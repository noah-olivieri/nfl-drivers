# QB strength: weekly opponent-adjusted QB ratings from a weighted ridge regression
# on dropback EPA, a rolling-average benchmark, and the starter adjustment: how much
# better or worse the actual starter is than the QBs behind the team's pass rating.
#
# params: list(qb_half_life = weeks, qb_lambda = ridge penalty, garbage_weight =
#              weight for plays with win probability outside 0.05-0.95)

QB_SEASONS_BACK <- 4L
EXPERIENCE_BINS <- c("0-99", "100-499", "500-1499", "1500+")

experience_bin <- function(n) {
  cut(n, breaks = c(-Inf, 99, 499, 1499, Inf), labels = EXPERIENCE_BINS)
}

# Dropbacks credited to a QB. Dropbacks with no player ID are left out and counted.
qb_dropbacks <- function(plays) {
  is_db <- plays$qb_dropback == 1
  out <- plays[is_db & !is.na(plays$qb_id), ]
  attr(out, "excluded") <- sum(is_db & is.na(plays$qb_id))
  out
}

# Each play's QB career dropbacks before that play's game (over all plays given).
add_career_dropbacks <- function(db) {
  per_game <- db |>
    dplyr::count(qb_id, game_date, name = "n_game") |>
    dplyr::arrange(qb_id, game_date) |>
    dplyr::group_by(qb_id) |>
    dplyr::mutate(career_before = cumsum(n_game) - n_game) |>
    dplyr::ungroup() |>
    dplyr::select(qb_id, game_date, career_before)
  dplyr::left_join(db, per_game, by = c("qb_id", "game_date"))
}

career_at <- function(db, qb_ids, cutoff_date) {
  tab <- table(db$qb_id[db$game_date < cutoff_date])
  n <- as.integer(tab[qb_ids])
  ifelse(is.na(n), 0L, n)
}

qb_play_weights <- function(db, cutoff_date, params) {
  weeks_ago <- as.numeric(cutoff_date - db$game_date) / 7
  garbage <- !is.na(db$wp) & (db$wp < 0.05 | db$wp > 0.95)
  0.5^(weeks_ago / params$qb_half_life) * ifelse(garbage, params$garbage_weight, 1)
}

# QB ratings to use for games on or after cutoff_date, from dropbacks dated before
# it in `season` and the QB_SEASONS_BACK seasons before. Fits
#   epa ~ qb + defense + home + experience bin
# with qb and defense penalized and home and the bins unpenalized, so a QB with few
# dropbacks is pulled toward the typical QB at his experience level. A rating is
# bin effect + own coefficient, centered so 0 = the window's average dropback.
# `starters` are rated too, even with no dropbacks yet. db needs career_before.
fit_qb_ratings_at <- function(db, season, cutoff_date, params, starters = character()) {
  p <- db[db$season >= season - QB_SEASONS_BACK & db$season <= season & db$game_date < cutoff_date, ]
  if (nrow(p) == 0) stop("No dropbacks before ", cutoff_date, " for season ", season)

  bin <- as.character(experience_bin(p$career_before))
  present <- EXPERIENCE_BINS[EXPERIENCE_BINS %in% bin]
  bin_cols <- paste0("exp_", present[-1])          # the first present bin is the reference
  qb_key <- paste0("qb_", p$qb_id)
  def_key <- paste0("def_", p$defteam)
  qb_cols <- sort(unique(qb_key))
  def_cols <- sort(unique(def_key))
  cols <- c(qb_cols, def_cols, "is_home_off", bin_cols)
  n <- nrow(p)
  home_rows <- which(p$posteam == p$home_team)
  bin_rows <- which(paste0("exp_", bin) %in% bin_cols)
  X <- Matrix::sparseMatrix(
    i = c(seq_len(n), seq_len(n), home_rows, bin_rows),
    j = c(match(qb_key, cols), match(def_key, cols),
          rep(match("is_home_off", cols), length(home_rows)),
          match(paste0("exp_", bin[bin_rows]), cols)),
    x = 1, dims = c(n, length(cols)), dimnames = list(NULL, cols))

  w <- qb_play_weights(p, cutoff_date, params)
  beta <- ridge_solve(X, p$epa, w, lambda = params$qb_lambda,
                      penalty_factor = c(rep(1, length(qb_cols) + length(def_cols)),
                                         rep(0, 1 + length(bin_cols))))

  bin_effect <- stats::setNames(numeric(length(EXPERIENCE_BINS)), EXPERIENCE_BINS)
  for (b in present[-1]) bin_effect[[b]] <- beta[[paste0("exp_", b)]]
  # A bin with no dropbacks in the window borrows the effect of the bin below it.
  for (i in seq_along(EXPERIENCE_BINS)[-1]) {
    if (!(EXPERIENCE_BINS[i] %in% present)) bin_effect[[i]] <- bin_effect[[i - 1]]
  }

  center <- sum(w * (bin_effect[bin] + beta[qb_key])) / sum(w)
  qbs <- union(sort(unique(p$qb_id)), starters[!is.na(starters)])
  own <- unname(beta[paste0("qb_", qbs)])
  own[is.na(own)] <- 0
  career <- career_at(db, qbs, cutoff_date)
  out <- tibble::tibble(qb_id = qbs,
                        qb_rating = unname(bin_effect[as.character(experience_bin(career))]) + own - center,
                        career_dropbacks = career)
  attr(out, "bin_ratings") <- bin_effect - center
  out
}

# One row per team per scheduled game: who the schedule lists as the starter.
scheduled_starters <- function(sched) {
  dplyr::bind_rows(
    dplyr::transmute(sched, season, week, team = home_team, qb_id = home_qb_id),
    dplyr::transmute(sched, season, week, team = away_team, qb_id = away_qb_id)
  )
}

# QB ratings for every season-week in `seasons`, covering every QB with dropbacks
# in the window plus that week's scheduled starters.
compute_qb_ratings <- function(plays, sched, params, seasons) {
  db <- add_career_dropbacks(qb_dropbacks(plays))
  starters <- scheduled_starters(sched)
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    s <- starters$qb_id[starters$season == season & starters$week == week]
    r <- fit_qb_ratings_at(db, season, cutoff_date, params, starters = s)
    tibble::add_column(r, season = season, week = week, .before = 1)
  })
}

# Share of scheduled starter IDs that appear on at least one dropback. Near 1 when
# schedule and play-by-play IDs agree; the pipeline stops if it is low.
starter_id_coverage <- function(sched, plays) {
  ids <- unique(stats::na.omit(c(sched$home_qb_id, sched$away_qb_id)))
  mean(ids %in% plays$qb_id[plays$qb_dropback == 1])
}

# Each team's QB mix for each season-week: the weighted mean QB rating over exactly
# the plays and weights that built its ridge pass_off (seasons s and s - 1, before
# the cutoff, play_weights() with the team params). Only play_type "pass" dropbacks
# count: scrambles are "run" plays, so they are in rush_off, not pass_off.
team_qb_mix <- function(plays, qb_ratings, sched, team_params, seasons) {
  db <- qb_dropbacks(plays)
  db <- db[db$play_type == "pass", ]
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    p <- db[db$season %in% c(season - 1L, season) & db$game_date < cutoff_date, ]
    r <- qb_ratings[qb_ratings$season == season & qb_ratings$week == week, ]
    p$w <- play_weights(p, season, cutoff_date, team_params)
    p$qb_rating <- r$qb_rating[match(p$qb_id, r$qb_id)]
    if (anyNA(p$qb_rating)) stop("QB ratings missing for the team mix in ", season, " week ", week)
    p |>
      dplyr::group_by(team = posteam) |>
      dplyr::summarise(qb_mix = sum(w * qb_rating) / sum(w), .groups = "drop") |>
      tibble::add_column(season = season, week = week, .before = 1)
  })
}

# Starter rating minus team QB mix for each side, and the home-minus-away gap.
# About 0 when the usual starter plays. A missing starter ID is assumed to be the
# usual starter (adjustment 0). qb = NULL gives NA columns.
add_qb_adjustments <- function(games, qb) {
  if (is.null(qb)) {
    return(dplyr::mutate(games, home_qb_adj = NA_real_, away_qb_adj = NA_real_, qb_adj_diff = NA_real_))
  }
  r <- dplyr::select(qb$ratings, season, week, qb_id, qb_rating)
  m <- dplyr::select(qb$mix, season, week, team, qb_mix)
  games |>
    dplyr::left_join(dplyr::rename(r, home_qb_id = qb_id, home_qb_rating = qb_rating),
                     by = c("season", "week", "home_qb_id")) |>
    dplyr::left_join(dplyr::rename(r, away_qb_id = qb_id, away_qb_rating = qb_rating),
                     by = c("season", "week", "away_qb_id")) |>
    dplyr::left_join(dplyr::rename(m, home_team = team, home_qb_mix = qb_mix),
                     by = c("season", "week", "home_team")) |>
    dplyr::left_join(dplyr::rename(m, away_team = team, away_qb_mix = qb_mix),
                     by = c("season", "week", "away_team")) |>
    dplyr::mutate(home_qb_adj = ifelse(is.na(home_qb_id), 0, home_qb_rating - home_qb_mix),
                  away_qb_adj = ifelse(is.na(away_qb_id), 0, away_qb_rating - away_qb_mix),
                  qb_adj_diff = home_qb_adj - away_qb_adj) |>
    dplyr::select(-home_qb_rating, -away_qb_rating, -home_qb_mix, -away_qb_mix)
}

# Benchmark: each QB's mean EPA over his last n_dropbacks dropbacks before the
# cutoff, across seasons and teams, with no opponent adjustment. A QB with fewer
# has the rest filled with the low-experience mean: mean EPA of dropbacks before the
# cutoff in seasons s-3 to s by QBs who had fewer than n_dropbacks career dropbacks
# at the time. Rates the same QBs as compute_qb_ratings().
compute_rolling_qb_ratings <- function(plays, sched, seasons, n_dropbacks = 300) {
  db <- add_career_dropbacks(qb_dropbacks(plays))
  db <- db[order(db$game_date), ]   # order() is stable, so plays keep their in-game order
  starters <- scheduled_starters(sched)
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    past <- db[db$game_date < cutoff_date, ]
    low <- past$epa[past$season >= season - 3L & past$career_before < n_dropbacks]
    low_mean <- if (length(low) > 0) mean(low) else 0
    s <- starters$qb_id[starters$season == season & starters$week == week]
    qbs <- union(sort(unique(past$qb_id[past$season >= season - QB_SEASONS_BACK])), s[!is.na(s)])
    last_n <- past[past$qb_id %in% qbs, ] |>
      dplyr::group_by(qb_id) |>
      dplyr::slice_tail(n = n_dropbacks) |>
      dplyr::summarise(sum_epa = sum(epa), k = dplyr::n(), .groups = "drop")
    i <- match(qbs, last_n$qb_id)
    sum_epa <- ifelse(is.na(i), 0, last_n$sum_epa[i])
    k <- ifelse(is.na(i), 0, last_n$k[i])
    tibble::tibble(season = season, week = week, qb_id = qbs,
                   qb_rating = (sum_epa + (n_dropbacks - k) * low_mean) / n_dropbacks,
                   career_dropbacks = career_at(db, qbs, cutoff_date))
  })
}

default_qb_grid <- function() {
  tidyr::expand_grid(qb_half_life = c(16, 32, 64, 128), qb_lambda = c(1e-3, 3e-3, 1e-2, 3e-2),
                     garbage_weight = 0.25)
}

# Among settings within `tol` RMSE points of the best all-games score (nearly flat),
# pick the one that does best on QB-change games.
select_qb_params <- function(scores, tol = 0.02) {
  near <- scores[scores$rmse <= min(scores$rmse) + tol, ]
  near[which.min(near$rmse_qb_change), ]
}

# Score each QB setting by how well that week's ratings explain that week's margins
# (team rating diffs + QB gap + home), on all games and on QB-change games, using
# only `seasons` (2010-2017 by default). Team rating params stay fixed. qb_fn takes
# (plays, sched, params, seasons), so it is for the ridge ratings; the rolling
# benchmark is never tuned.
tune_qb_params <- function(plays, sched, team_ratings, team_params, grid, seasons = 2010:2017,
                           qb_fn = compute_qb_ratings, tol = 0.02) {
  games <- sched |>
    dplyr::left_join(qb_flags(sched), by = "game_id") |>
    add_qb_change_game() |>
    dplyr::filter(season %in% seasons) |>
    dplyr::mutate(margin = home_score - away_score, home = as.numeric(location != "Neutral")) |>
    rating_diffs(team_ratings)
  scores <- purrr::pmap_dfr(grid, function(...) {
    params <- list(...)
    qbr <- qb_fn(plays, sched, params, seasons)
    mix <- team_qb_mix(plays, qbr, sched, team_params, seasons)
    d <- add_qb_adjustments(games, list(ratings = qbr, mix = mix))
    fit <- stats::lm(margin ~ pass_off_diff + rush_off_diff + pass_def_diff + rush_def_diff +
                       qb_adj_diff + home, data = d, na.action = stats::na.exclude)
    res <- stats::residuals(fit)
    tibble::as_tibble(params) |>
      dplyr::mutate(rmse = sqrt(mean(res^2, na.rm = TRUE)),
                    rmse_qb_change = sqrt(mean(res[d$qb_change_game == 1]^2, na.rm = TRUE)))
  })
  list(best = as.list(select_qb_params(scores, tol)[names(grid)]), scores = scores)
}

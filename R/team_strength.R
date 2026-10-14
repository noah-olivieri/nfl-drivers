# Team strength: weekly opponent-adjusted ratings from a weighted ridge regression
# on play-level EPA, plus a simple rolling-average baseline.
#
# params: list(half_life = weeks, offseason_weeks = extra decay for last season's
#              plays, lambda = ridge penalty, garbage_weight = weight for plays with
#              win probability outside 0.05-0.95)

# Earliest game date of each season-week; ratings for a week use plays before it.
week_cutoffs <- function(sched) {
  sched |>
    dplyr::group_by(season, week) |>
    dplyr::summarise(cutoff_date = min(as.Date(gameday)), .groups = "drop") |>
    dplyr::arrange(season, week)
}

# Exact weighted ridge solution, identical to glmnet(alpha = 0, standardize = FALSE)
# with the same lambda but without its iterative solver (milliseconds vs ~1 s per fit):
#   1/2 * sum(v * (y - b0 - X b)^2) + (lambda / sd_v(y)) / 2 * sum(pf * b^2)
# where v = w / sum(w), sd_v is the v-weighted SD of y, and pf is rescaled to sum
# to ncol(X), all as glmnet does internally. Returns c(intercept, coefficients).
ridge_solve <- function(X, y, w, lambda, penalty_factor = rep(1, ncol(X))) {
  v <- w / sum(w)
  sd_y <- sqrt(sum(v * (y - sum(v * y))^2))
  pf <- penalty_factor * ncol(X) / sum(penalty_factor)
  Z <- cbind(1, X)
  ZtV <- Matrix::t(Z * v)
  A <- as.matrix(ZtV %*% Z) + diag(c(0, lambda / sd_y * pf))
  b <- as.numeric(ZtV %*% y)
  stats::setNames(solve(A, b), c("(Intercept)", colnames(X)))
}

play_weights <- function(plays, season, cutoff_date, params) {
  weeks_ago <- as.numeric(cutoff_date - plays$game_date) / 7 +
    ifelse(plays$season < season, params$offseason_weeks, 0)
  garbage <- !is.na(plays$wp) & (plays$wp < 0.05 | plays$wp > 0.95)
  0.5^(weeks_ago / params$half_life) * ifelse(garbage, params$garbage_weight, 1)
}

# Ratings to use for games on or after cutoff_date, from plays in `season` and
# `season - 1` dated strictly before it. Defense is signed so higher = better.
fit_ratings_at <- function(plays, season, cutoff_date, params) {
  p <- plays[plays$season %in% c(season - 1L, season) & plays$game_date < cutoff_date, ]
  if (nrow(p) == 0) stop("No plays before ", cutoff_date, " for season ", season)

  off_key <- paste0("off_", p$posteam, "_", p$play_type)
  def_key <- paste0("def_", p$defteam, "_", p$play_type)
  cols <- c(sort(unique(off_key)), sort(unique(def_key)), "is_home_off", "is_pass")
  n <- nrow(p)
  home_rows <- which(p$posteam == p$home_team)
  pass_rows <- which(p$play_type == "pass")
  X <- Matrix::sparseMatrix(
    i = c(seq_len(n), seq_len(n), home_rows, pass_rows),
    j = c(match(off_key, cols), match(def_key, cols),
          rep(length(cols) - 1L, length(home_rows)), rep(length(cols), length(pass_rows))),
    x = 1, dims = c(n, length(cols)), dimnames = list(NULL, cols))

  beta <- ridge_solve(X, p$epa, play_weights(p, season, cutoff_date, params),
                      lambda = params$lambda,
                      penalty_factor = c(rep(1, length(cols) - 2), 0, 0))

  teams <- sort(unique(c(p$posteam, p$defteam)))
  get <- function(prefix, team, type) {
    v <- beta[paste0(prefix, "_", team, "_", type)]
    ifelse(is.na(v), 0, unname(v))
  }
  tibble::tibble(team = teams,
                 pass_off = get("off", teams, "pass"),
                 rush_off = get("off", teams, "run"),
                 pass_def = -get("def", teams, "pass"),
                 rush_def = -get("def", teams, "run"))
}

# Ratings for every season-week in `seasons`.
compute_ridge_ratings <- function(plays, sched, params, seasons) {
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    r <- fit_ratings_at(plays, season, cutoff_date, params)
    tibble::add_column(r, season = season, week = week, .before = 1)
  })
}

# Baseline: each team's mean EPA/play over its last n_games before each cutoff,
# split by pass and run, crossing season boundaries. No opponent adjustment.
compute_rolling_ratings <- function(plays, sched, seasons, n_games = 8) {
  side_sums <- function(team_col, side) {
    plays |>
      dplyr::group_by(game_id, game_date, team = .data[[team_col]], play_type) |>
      dplyr::summarise(s = sum(epa), n = dplyr::n(), .groups = "drop") |>
      dplyr::mutate(key = paste0(side, "_", play_type))
  }
  per_game <- dplyr::bind_rows(side_sums("posteam", "off"), side_sums("defteam", "def")) |>
    dplyr::select(-play_type) |>
    tidyr::pivot_wider(names_from = key, values_from = c(s, n), values_fill = 0)

  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  pooled <- function(s, n) ifelse(sum(n) > 0, sum(s) / sum(n), 0)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    per_game |>
      dplyr::filter(game_date < cutoff_date) |>
      dplyr::group_by(team) |>
      dplyr::slice_max(game_date, n = n_games, with_ties = FALSE) |>
      dplyr::summarise(pass_off = pooled(s_off_pass, n_off_pass),
                       rush_off = pooled(s_off_run, n_off_run),
                       pass_def = -pooled(s_def_pass, n_def_pass),
                       rush_def = -pooled(s_def_run, n_def_run)) |>
      tibble::add_column(season = season, week = week, .before = 1)
  })
}

# Join a week's ratings onto its games as home-minus-away differences.
rating_diffs <- function(games, ratings) {
  r <- dplyr::select(ratings, season, week, team, pass_off, rush_off, pass_def, rush_def)
  games |>
    dplyr::left_join(r, by = c("season", "week", "home_team" = "team")) |>
    dplyr::left_join(r, by = c("season", "week", "away_team" = "team"), suffix = c("_h", "_a")) |>
    dplyr::mutate(pass_off_diff = pass_off_h - pass_off_a, rush_off_diff = rush_off_h - rush_off_a,
                  pass_def_diff = pass_def_h - pass_def_a, rush_def_diff = rush_def_h - rush_def_a) |>
    dplyr::select(-dplyr::ends_with("_h"), -dplyr::ends_with("_a"))
}

default_rating_grid <- function() {
  tidyr::expand_grid(half_life = c(3, 5, 8, 12), offseason_weeks = c(8, 16, 26),
                     lambda = c(1e-4, 3e-4, 1e-3, 3e-3, 1e-2), garbage_weight = 0.25)
}

# Pick rating params by how well that week's ratings explain that week's margins,
# using only `seasons` (2010-2017 by default, so later test years stay untouched).
tune_rating_params <- function(plays, sched, grid, seasons = 2010:2017,
                               rating_fn = compute_ridge_ratings) {
  games <- sched |>
    dplyr::filter(season %in% seasons) |>
    dplyr::mutate(margin = home_score - away_score, home = as.numeric(location != "Neutral"))
  scores <- purrr::pmap_dfr(grid, function(...) {
    params <- list(...)
    d <- rating_diffs(games, rating_fn(plays, sched, params, seasons))
    fit <- stats::lm(margin ~ pass_off_diff + rush_off_diff + pass_def_diff + rush_def_diff + home,
                     data = d)
    tibble::as_tibble(params) |> dplyr::mutate(rmse = sqrt(mean(stats::residuals(fit)^2)))
  })
  best <- as.list(scores[which.min(scores$rmse), names(grid)])
  list(best = best, scores = scores)
}

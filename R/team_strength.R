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

  fit <- glmnet::glmnet(X, p$epa, weights = play_weights(p, season, cutoff_date, params),
                        alpha = 0, lambda = params$lambda, standardize = FALSE,
                        penalty.factor = c(rep(1, length(cols) - 2), 0, 0))
  beta <- as.matrix(stats::coef(fit))[, 1]

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

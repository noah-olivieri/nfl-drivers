test_that("rolling ratings use only the last n games before the cutoff", {
  p <- make_synthetic()
  s <- tibble::tibble(season = 2020L, week = 2L, gameday = as.Date("2020-10-01"))
  r <- compute_rolling_ratings(p, s, 2020L, n_games = 8)
  a_games <- p |>
    dplyr::filter(game_date < as.Date("2020-10-01"), posteam == "A" | defteam == "A") |>
    dplyr::distinct(game_id, game_date) |>
    dplyr::slice_max(game_date, n = 8, with_ties = FALSE)
  manual <- p |> dplyr::filter(game_id %in% a_games$game_id, posteam == "A", play_type == "pass")
  expect_equal(r$pass_off[r$team == "A"], mean(manual$epa))
  manual_def <- p |> dplyr::filter(game_id %in% a_games$game_id, defteam == "A", play_type == "run")
  expect_equal(r$rush_def[r$team == "A"], -mean(manual_def$epa))
})

test_that("rolling ratings have the same shape as ridge ratings", {
  p <- make_synthetic()
  s <- tibble::tibble(season = 2020L, week = 2L, gameday = as.Date("2020-10-01"))
  r <- compute_rolling_ratings(p, s, 2020L)
  expect_named(r, c("season", "week", "team", "pass_off", "rush_off", "pass_def", "rush_def"))
})

test_that("tuning never sees seasons outside the tuning window", {
  seen <- integer()
  recorder <- function(plays, sched, params, seasons) {
    seen <<- c(seen, seasons)
    tibble::tibble(season = integer(), week = integer(), team = character(),
                   pass_off = numeric(), rush_off = numeric(),
                   pass_def = numeric(), rush_def = numeric())
  }
  sched <- tibble::tibble(season = 2005:2022, week = 1L, gameday = "2015-09-10",
                          home_team = "A", away_team = "B", location = "Home",
                          home_score = 20, away_score = 17)
  try(tune_rating_params(tibble::tibble(), sched, default_rating_grid()[1:2, ],
                         rating_fn = recorder), silent = TRUE)
  expect_gt(length(seen), 0)
  expect_true(all(seen %in% 2010:2017))
})

test_that("tuning picks the params with the lowest RMSE", {
  # fake rating function: lambda = 1 gives ratings equal to the true strengths,
  # lambda = 2 gives pure noise
  set.seed(5)
  sched <- tibble::tibble(season = 2010L, week = rep(1:10, each = 30),
                          home_team = sample(c("A", "B"), 300, TRUE), location = "Home")
  sched$away_team <- ifelse(sched$home_team == "A", "B", "A")
  sched$home_score <- 20 + ifelse(sched$home_team == "A", 7, -7) + rnorm(300, 0, 3)
  sched$away_score <- 20
  fake <- function(plays, sched, params, seasons) {
    tidyr::expand_grid(season = 2010L, week = 1:10, team = c("A", "B")) |>
      dplyr::mutate(pass_off = if (params$lambda == 1) ifelse(team == "A", 1, -1) else rnorm(dplyr::n()),
                    rush_off = 0, pass_def = 0, rush_def = 0)
  }
  grid <- tibble::tibble(half_life = 5, offseason_weeks = 8, lambda = c(2, 1), garbage_weight = 0.25)
  out <- tune_rating_params(tibble::tibble(), sched, grid, seasons = 2010L, rating_fn = fake)
  expect_equal(out$best$lambda, 1)
  expect_equal(nrow(out$scores), 2)
})

wf_data <- function() {
  set.seed(7)
  tibble::tibble(game_id = 1:400, season = rep(2014:2021, each = 50),
                 week = rep(rep(1:5, each = 10), 8), x = rnorm(400),
                 home = 1, spread = rnorm(400, 2, 5)) |>
    dplyr::mutate(margin = 2 * x + rnorm(400, 0, 10), win = as.numeric(margin > 0))
}

test_that("walk-forward never trains on the test season or later", {
  seen <- list()
  recording_fit <- function(train, vars) {
    seen[[length(seen) + 1]] <<- unique(train$season)
    fit_models(train, vars)
  }
  walk_forward(wf_data(), "x", test_seasons = 2018:2021, train_start = 2014L, fit_fn = recording_fit)
  expect_length(seen, 4)
  for (i in seq_along(seen)) {
    expect_equal(sort(seen[[i]]), 2014:(2016 + i))
  }
})

test_that("walk-forward predicts every test-season game once", {
  out <- walk_forward(wf_data(), "x", test_seasons = 2018:2021, train_start = 2014L)
  expect_equal(sort(out$game_id), 201:400)
  expect_named(out, c("game_id", "season", "week", "margin", "win", "pred_margin", "pred_win"))
})

test_that("test seasons with no games are skipped", {
  out <- walk_forward(wf_data(), "x", test_seasons = 2020:2023, train_start = 2014L)
  expect_equal(sort(unique(out$season)), 2020:2021)
})

test_that("spread benchmark predicts the spread as the margin", {
  out <- spread_benchmark(wf_data(), test_seasons = 2020:2021, train_start = 2014L)
  d <- wf_data()
  expect_equal(out$pred_margin, d$spread[match(out$game_id, d$game_id)])
  expect_equal(out$week, d$week[match(out$game_id, d$game_id)])
  expect_true(all(out$pred_win > 0 & out$pred_win < 1))
})

test_that("scoring matches hand-computed values", {
  p <- tibble::tibble(season = 2020L, margin = c(3, -7), win = c(1, 0),
                      pred_margin = c(1, -3), pred_win = c(0.8, 0.4))
  s <- score_predictions(p)
  expect_equal(s$rmse, sqrt((4 + 16) / 2))
  expect_equal(s$mae, 3)
  expect_equal(s$brier, ((0.2)^2 + (0.4)^2) / 2)
  expect_equal(s$log_loss, -(log(0.8) + log(0.6)) / 2)
  expect_equal(s$accuracy, 1)
})

test_that("ties are excluded from accuracy but kept in log loss", {
  p <- tibble::tibble(season = 2020L, margin = c(0, 3), win = c(0.5, 1),
                      pred_margin = c(1, 1), pred_win = c(0.9, 0.9))
  s <- score_predictions(p)
  expect_equal(s$accuracy, 1)
  expect_equal(s$n_games, 2)
  expect_equal(s$log_loss, (-(0.5 * log(0.9) + 0.5 * log(0.1)) - log(0.9)) / 2)
})

test_that("log loss survives certain predictions", {
  p <- tibble::tibble(season = 2020L, margin = -3, win = 0, pred_margin = 1, pred_win = 1)
  expect_true(is.finite(score_predictions(p)$log_loss))
})

test_that("calibration bins compare predicted and actual win rates", {
  p <- tibble::tibble(pred_win = c(0.05, 0.08, 0.92, 0.95), win = c(0, 1, 1, 1))
  cal <- calibration_table(p, bins = 10)
  expect_equal(cal$n, c(2, 2))
  expect_equal(cal$actual_rate, c(0.5, 1))
  expect_equal(cal$mean_pred, c(0.065, 0.935))
})

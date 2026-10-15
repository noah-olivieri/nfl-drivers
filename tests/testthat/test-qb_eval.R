boot_row <- function(d_rmse, d_rmse_high, d_log_loss = -0.001, d_log_loss_high = -0.0001) {
  tibble::tibble(d_rmse = d_rmse, d_rmse_high = d_rmse_high,
                 d_log_loss = d_log_loss, d_log_loss_high = d_log_loss_high)
}
seasons_better <- function(k) {   # k of 8 seasons where the QB model has lower RMSE
  tibble::tibble(season = 2018:2025, rmse_ref = 13, rmse_qb = c(rep(12.9, k), rep(13.1, 8 - k)))
}
no_gain <- boot_row(0.01, 0.05)

test_that("path 1: all-games intervals below 0 on both metrics and better in 5+ of 8 seasons", {
  v <- qb_verdict(boot_row(-0.05, -0.01), no_gain, seasons_better(5))
  expect_true(v$path1_all_games)
  expect_true(v$earns_place)
  expect_false(qb_verdict(boot_row(-0.05, -0.01), no_gain, seasons_better(4))$path1_all_games)
  expect_false(qb_verdict(boot_row(-0.05, -0.01, -0.001, 0.0002), no_gain,
                          seasons_better(6))$path1_all_games)
})

test_that("path 2: subset RMSE interval below 0 and all-games point estimates no worse", {
  flat <- boot_row(0, 0.03, 0, 0.001)
  v <- qb_verdict(flat, boot_row(-0.3, -0.05), seasons_better(3))
  expect_false(v$path1_all_games)
  expect_true(v$path2_qb_change)
  expect_true(v$earns_place)
})

test_that("path 2 fails on its all-games guard, once on RMSE and once on log loss", {
  subset_gain <- boot_row(-0.3, -0.05)
  expect_false(qb_verdict(boot_row(0.001, 0.03, -0.001, 0.001), subset_gain, seasons_better(3))$path2_qb_change)
  expect_false(qb_verdict(boot_row(-0.001, 0.03, 0.0001, 0.001), subset_gain, seasons_better(3))$path2_qb_change)
})

test_that("neither path: no place earned", {
  v <- qb_verdict(no_gain, boot_row(-0.1, 0.02), seasons_better(2))
  expect_false(v$path1_all_games)
  expect_false(v$path2_qb_change)
  expect_false(v$earns_place)
  expect_equal(c(v$seasons_better, v$n_seasons), c(2, 8))
})

test_that("subset scores split each model's games by the QB-change marker", {
  preds <- list(m = tibble::tibble(game_id = 1:6, season = 2020L, week = 1L,
                                   margin = c(3, -3, 7, 0, 10, -1), win = c(1, 0, 1, 0.5, 1, 0),
                                   pred_margin = 1, pred_win = 0.6))
  features <- tibble::tibble(game_id = 1:6, qb_change_game = c(1, 1, 0, 0, 0, 0))
  s <- score_qb_subsets(preds, features, "m")
  expect_equal(s$n_games[s$subset == "qb_change"], 2)
  expect_equal(s$n_games[s$subset == "other"], 4)
  expect_equal(s$rmse[s$subset == "qb_change"], sqrt(mean(c(2, -4)^2)))
})

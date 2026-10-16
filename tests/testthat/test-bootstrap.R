boot_data <- function(n_weeks = 20, per_week = 8, err_sd = 10) {
  set.seed(11)
  n <- n_weeks * per_week
  tibble::tibble(game_id = seq_len(n), season = 2020L, week = rep(seq_len(n_weeks), each = per_week),
                 margin = rnorm(n, 0, 13)) |>
    dplyr::mutate(win = as.numeric(margin > 0),
                  pred_margin = margin + rnorm(n, 0, err_sd),
                  pred_win = plogis(pred_margin / 7))
}

test_that("identical predictions give a zero difference with a zero-width interval", {
  p <- boot_data()
  b <- paired_bootstrap(p, p, n_boot = 200)
  expect_equal(c(b$d_rmse, b$d_rmse_low, b$d_rmse_high), c(0, 0, 0))
  expect_equal(c(b$d_log_loss, b$d_log_loss_low, b$d_log_loss_high), c(0, 0, 0))
  expect_equal(b$share_worse_rmse, 0)
})

test_that("a clearly worse model gets an interval entirely above zero", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + rnorm(dplyr::n(), 0, 15),
                     pred_win = plogis(pred_margin / 7))
  r <- paired_bootstrap(a, b, n_boot = 500)
  expect_gt(r$d_rmse_low, 0)
  expect_gt(r$d_log_loss_low, 0)
  expect_gt(r$share_worse_rmse, 0.99)
  expect_true(r$d_rmse_low < r$d_rmse && r$d_rmse < r$d_rmse_high)
})

test_that("the point estimate is b's pooled error minus a's", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + 2, pred_win = pmin(pred_win + 0.1, 1))
  r <- paired_bootstrap(a, b, n_boot = 50)
  rmse <- function(p) sqrt(mean((p$margin - p$pred_margin)^2))
  ll <- function(p) {
    q <- pmin(pmax(p$pred_win, 1e-6), 1 - 1e-6)
    mean(-(p$win * log(q) + (1 - p$win) * log(1 - q)))
  }
  expect_equal(r$d_rmse, rmse(b) - rmse(a))
  expect_equal(r$d_log_loss, ll(b) - ll(a))
})

test_that("pairing is by game, not row order", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + rnorm(dplyr::n(), 0, 5))
  shuffled <- b[sample(nrow(b)), ]
  expect_equal(paired_bootstrap(a, shuffled, n_boot = 100), paired_bootstrap(a, b, n_boot = 100))
})

test_that("the same seed gives the same result", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + rnorm(dplyr::n(), 0, 5))
  expect_equal(paired_bootstrap(a, b, n_boot = 100, seed = 4),
               paired_bootstrap(a, b, n_boot = 100, seed = 4))
})

test_that("weeks are resampled whole, so one week gives every draw the point estimate", {
  a <- boot_data(n_weeks = 1, per_week = 16)
  b <- dplyr::mutate(a, pred_margin = pred_margin + rnorm(dplyr::n(), 0, 5))
  r <- paired_bootstrap(a, b, n_boot = 100)
  expect_equal(r$d_rmse_low, r$d_rmse)
  expect_equal(r$d_rmse_high, r$d_rmse)
  expect_equal(r$n_weeks, 1)
})

test_that("games missing a prediction in either model are dropped", {
  a <- boot_data()
  b <- a[-(1:3), ]
  b$pred_win[1] <- NA
  r <- paired_bootstrap(a, b, n_boot = 50)
  expect_equal(r$n_games, nrow(a) - 4)
})

test_that("compare_models labels each pair, b relative to a", {
  a <- boot_data()
  preds <- list(good = a, bad = dplyr::mutate(a, pred_margin = pred_margin + 10))
  cm <- compare_models(preds, list(c("good", "bad")), n_boot = 50)
  expect_equal(c(cm$model_a, cm$model_b), c("good", "bad"))
  expect_equal(cm$d_rmse, paired_bootstrap(preds$good, preds$bad, n_boot = 50)$d_rmse)
})

test_that("games without an outcome are left out of the comparison", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + 2)
  up <- tibble::tibble(game_id = 1000L + 1:3, season = 2020L, week = 21L, margin = NA_real_,
                       win = NA_real_, pred_margin = 3, pred_win = 0.6)
  expect_equal(paired_bootstrap(dplyr::bind_rows(a, up), dplyr::bind_rows(b, up), n_boot = 50),
               paired_bootstrap(a, b, n_boot = 50))
})

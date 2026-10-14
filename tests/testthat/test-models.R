test_that("scaler uses training rows only and skips binary flags", {
  train <- tibble::tibble(x = c(1, 2, 3), home = 1)
  test  <- tibble::tibble(x = c(100), home = 1)
  sc <- fit_scaler(train, c("x", "home"))
  expect_equal(sc$mean[["x"]], 2)
  expect_false("home" %in% names(sc$mean))
  expect_equal(apply_scaler(test, sc)$x, (100 - 2) / 1)
  expect_equal(apply_scaler(test, sc)$home, 1)
})

test_that("win model accepts ties and predicts probabilities", {
  set.seed(2)
  d <- tibble::tibble(game_id = 1:300, x = rnorm(300), home = 1,
                      margin = 3 * x + rnorm(300, 0, 10))
  d$win <- ifelse(d$margin > 0, 1, ifelse(d$margin < 0, 0, 0.5))
  d$win[1:5] <- 0.5
  fit <- fit_models(d, c("x"))
  p <- predict_models(fit, d)
  expect_named(p, c("game_id", "pred_margin", "pred_win"))
  expect_true(all(p$pred_win > 0 & p$pred_win < 1))
  expect_gt(coef(fit$margin)[["x"]], 0)
})

test_that("coefficients are per 1 SD of the training data", {
  set.seed(6)
  d <- tibble::tibble(game_id = 1:2000, x = rnorm(2000, 50, 10), home = 1)
  d$margin <- 0.3 * d$x + rnorm(2000, 0, 1)   # 0.3 points per unit = 3 points per SD
  d$win <- as.numeric(d$margin > 15)
  fit <- fit_models(d, "x")
  expect_equal(coef(fit$margin)[["x"]], 3, tolerance = 0.03)
})

test_that("a variable that is constant in training is dropped, not fatal", {
  d <- tibble::tibble(game_id = 1:50, home = 1, margin = rnorm(50, 2), win = 1)
  d$win[1:20] <- 0
  fit <- fit_models(d, "home")
  p <- predict_models(fit, d)
  expect_equal(unique(round(p$pred_margin, 8)), round(mean(d$margin), 8))
})

test_that("MODEL_SPECS: market is full plus the spread", {
  expect_equal(MODEL_SPECS$base, "home")
  expect_equal(setdiff(MODEL_SPECS$market, MODEL_SPECS$full), "spread")
})

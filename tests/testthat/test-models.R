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

test_that("MODEL_SPECS: QB specs add only the QB gap; baselines exclude it", {
  expect_false("qb_adj_diff" %in% MODEL_SPECS$full)
  expect_false("qb_adj_diff" %in% MODEL_SPECS$market)
  expect_equal(setdiff(MODEL_SPECS$full_qb, MODEL_SPECS$full), "qb_adj_diff")
  expect_equal(setdiff(MODEL_SPECS$market_qb, MODEL_SPECS$market), "qb_adj_diff")
  expect_equal(setdiff(MODEL_SPECS$market_qb, MODEL_SPECS$full_qb), "spread")
})

test_that("fit_models ignores games without an outcome", {
  set.seed(3)
  d <- tibble::tibble(game_id = 1:200, x = rnorm(200), home = 1)
  d$margin <- 2 * d$x + rnorm(200, 0, 5)
  d$win <- as.numeric(d$margin > 0)
  unplayed <- tibble::tibble(game_id = 201:210, x = rnorm(10, 5), home = 1,
                             margin = NA_real_, win = NA_real_)
  a <- fit_models(d, "x")
  b <- fit_models(dplyr::bind_rows(d, unplayed), "x")
  expect_equal(b$scaler, a$scaler)   # scaling must not see unplayed games either
  expect_equal(coef(b$margin), coef(a$margin))
  expect_equal(coef(b$win), coef(a$win))
})

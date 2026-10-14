imp_data <- function() {
  set.seed(3)
  n <- 16 * 250
  f <- tibble::tibble(game_id = seq_len(n), season = rep(2010:2025, each = 250),
                      week = rep(rep(1:10, each = 25), 16),
                      strong = rnorm(n), noise = rnorm(n), home = 1)
  f$margin <- 6 * f$strong + rnorm(n, 0, 12)
  f$win <- as.numeric(f$margin > 0)
  f
}

test_that("a strong variable hurts when dropped and noise does not", {
  s <- summarize_ablation(ablate(imp_data(), c("strong", "noise"),
                                 list(strong = "strong", noise = "noise"),
                                 test_seasons = 2018:2025))
  expect_equal(s$dropped[1], "strong")
  expect_gt(s$mean_d_rmse[s$dropped == "strong"], 1)
  expect_lt(abs(s$mean_d_rmse[s$dropped == "noise"]), 0.1)
  expect_equal(s$seasons_hurt[s$dropped == "strong"], 8)
  expect_equal(s$n_seasons[s$dropped == "strong"], 8)
})

test_that("ablation reports per-season changes for both metrics", {
  a <- ablate(imp_data(), c("strong", "noise"), list(noise = "noise"), test_seasons = 2024:2025)
  expect_named(a, c("dropped", "season", "d_rmse", "d_log_loss"))
  expect_equal(a$season, 2024:2025)
})

test_that("coef_table gives CIs that contain the estimate", {
  set.seed(4)
  f <- tibble::tibble(game_id = 1:2000, season = rep(2010:2025, length.out = 2000),
                      x = rnorm(2000), home = 1)
  f$margin <- 2 * f$x + rnorm(2000, 0, 10)
  f$win <- as.numeric(f$margin > 0)
  ct <- coef_table(f, "x")
  expect_named(ct, c("model", "term", "estimate", "conf_low", "conf_high"))
  row <- ct[ct$model == "margin" & ct$term == "x", ]
  expect_true(row$conf_low < row$estimate && row$estimate < row$conf_high)
  expect_true(all(c("margin", "win") %in% ct$model))
})

test_that("coef_table only uses the requested seasons", {
  f <- imp_data()
  f$strong[f$season == 2025] <- 1000   # would wreck the fit if 2025 leaked in
  ct <- coef_table(f, "strong", seasons = 2010:2024)
  est <- ct$estimate[ct$model == "margin" & ct$term == "strong"]
  expect_equal(est, 6, tolerance = 0.1)
})

test_that("the ablation bootstrap flags a strong variable but not noise", {
  ap <- ablation_preds(imp_data(), c("strong", "noise"), list(strong = "strong", noise = "noise"),
                       test_seasons = 2018:2025)
  b <- ablation_bootstrap(ap, n_boot = 300)
  expect_equal(b$dropped, c("strong", "noise"))
  expect_gt(b$d_rmse_low[b$dropped == "strong"], 0)
  expect_true(b$d_rmse_low[b$dropped == "noise"] < 0 && b$d_rmse_high[b$dropped == "noise"] > 0)
})

test_that("ablate() gives the same per-season deltas as ablation_deltas() on shared predictions", {
  sets <- list(noise = "noise")
  ap <- ablation_preds(imp_data(), c("strong", "noise"), sets, test_seasons = 2024:2025)
  expect_equal(ablation_deltas(ap),
               ablate(imp_data(), c("strong", "noise"), sets, test_seasons = 2024:2025))
})

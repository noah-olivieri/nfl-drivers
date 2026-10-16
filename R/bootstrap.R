# Paired block bootstrap: is the gap between two models' walk-forward errors bigger
# than noise? Both models are scored on the same resampled games, and whole
# season-weeks are resampled, since games in the same week share rating updates,
# weather and news.

# Per-game squared margin error and log loss, scored as in score_predictions()
# (played games only).
game_losses <- function(preds) {
  preds |>
    dplyr::filter(!is.na(margin), !is.na(win), !is.na(pred_margin), !is.na(pred_win)) |>
    dplyr::mutate(p = pmin(pmax(pred_win, 1e-6), 1 - 1e-6),
                  sq = (margin - pred_margin)^2,
                  ll = -(win * log(p) + (1 - win) * log(1 - p))) |>
    dplyr::select(game_id, season, week, sq, ll)
}

# Difference in pooled error, b minus a (positive = b is worse), with 95% percentile
# intervals and the share of draws in which b was worse.
paired_bootstrap <- function(a, b, n_boot = 2000, seed = 1) {
  wk <- dplyr::inner_join(game_losses(a), game_losses(b), by = c("game_id", "season", "week"),
                          suffix = c("_a", "_b")) |>
    dplyr::group_by(season, week) |>
    dplyr::summarise(n = dplyr::n(), sq_a = sum(sq_a), sq_b = sum(sq_b),
                     ll_a = sum(ll_a), ll_b = sum(ll_b), .groups = "drop")
  sums <- as.matrix(wk[c("n", "sq_a", "sq_b", "ll_a", "ll_b")])
  k <- nrow(wk)
  # Column j counts how often each week was drawn in bootstrap sample j.
  counts <- withr::with_seed(seed, apply(
    matrix(sample.int(k, k * n_boot, replace = TRUE), nrow = k), 2, tabulate, nbins = k))
  draws <- crossprod(matrix(counts, nrow = k), sums)
  d_rmse <- function(s) unname(sqrt(s[, "sq_b"] / s[, "n"]) - sqrt(s[, "sq_a"] / s[, "n"]))
  d_ll <- function(s) unname((s[, "ll_b"] - s[, "ll_a"]) / s[, "n"])
  total <- matrix(colSums(sums), nrow = 1, dimnames = list(NULL, colnames(sums)))
  ci <- function(x) unname(stats::quantile(x, c(0.025, 0.975)))
  boot_rmse <- d_rmse(draws)
  boot_ll <- d_ll(draws)
  tibble::tibble(
    d_rmse = d_rmse(total), d_rmse_low = ci(boot_rmse)[1], d_rmse_high = ci(boot_rmse)[2],
    share_worse_rmse = mean(boot_rmse > 0),
    d_log_loss = d_ll(total), d_log_loss_low = ci(boot_ll)[1], d_log_loss_high = ci(boot_ll)[2],
    share_worse_log_loss = mean(boot_ll > 0),
    n_games = as.integer(total[, "n"]), n_weeks = k
  )
}

# One paired_bootstrap() row per pair c(a, b) of names in `preds`.
compare_models <- function(preds, pairs, ...) {
  purrr::map_dfr(pairs, function(pair) {
    dplyr::bind_cols(tibble::tibble(model_a = pair[1], model_b = pair[2]),
                     paired_bootstrap(preds[[pair[1]]], preds[[pair[2]]], ...))
  })
}

# Importance: how much walk-forward accuracy gets worse when a variable (or a
# group of variables) is removed, plus coefficient sizes with confidence intervals.

# For each named set in drop_sets, rerun walk-forward without it and report the
# per-season change vs. the full model. Positive = removing it hurt accuracy.
ablate <- function(features, vars, drop_sets, ...) {
  full <- score_predictions(walk_forward(features, vars, ...))
  purrr::imap_dfr(drop_sets, function(drop, name) {
    reduced <- score_predictions(walk_forward(features, setdiff(vars, drop), ...))
    dplyr::inner_join(full, reduced, by = "season", suffix = c("_full", "_drop")) |>
      dplyr::transmute(dropped = name, season,
                       d_rmse = rmse_drop - rmse_full,
                       d_log_loss = log_loss_drop - log_loss_full)
  })
}

summarize_ablation <- function(abl) {
  abl |>
    dplyr::group_by(dropped) |>
    dplyr::summarise(mean_d_rmse = mean(d_rmse), mean_d_log_loss = mean(d_log_loss),
                     seasons_hurt = sum(d_rmse > 0), n_seasons = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(mean_d_rmse))
}

# Coefficients of both models fit on `seasons`, with 95% confidence intervals.
coef_table <- function(features, vars, seasons = 2010:2025) {
  fit <- fit_models(features[features$season %in% seasons, ], vars)
  tidy <- function(m, ci, name) {
    b <- stats::coef(m)
    tibble::tibble(model = name, term = names(b), estimate = unname(b),
                   conf_low = unname(ci[, 1]), conf_high = unname(ci[, 2]))
  }
  dplyr::bind_rows(tidy(fit$margin, stats::confint(fit$margin), "margin"),
                   tidy(fit$win, stats::confint.default(fit$win), "win"))
}

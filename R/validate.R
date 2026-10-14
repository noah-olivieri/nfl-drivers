# Walk-forward validation: for each test season Y, train on train_start..(Y - 1)
# and predict Y. Nothing from the test season (or later) is ever used in training.

walk_forward <- function(features, vars, test_seasons = 2018:2026, train_start = 2010L,
                         fit_fn = fit_models, predict_fn = predict_models) {
  purrr::map_dfr(test_seasons, function(y) {
    test <- features[features$season == y, ]
    if (nrow(test) == 0) return(NULL)
    train <- features[features$season >= train_start & features$season < y, ]
    preds <- predict_fn(fit_fn(train, vars), test)
    dplyr::left_join(dplyr::select(test, game_id, season, week, margin, win), preds, by = "game_id")
  })
}

# Benchmark: the closing spread as the margin prediction, turned into a win
# probability by a logistic fit on the training years.
spread_benchmark <- function(features, test_seasons = 2018:2026, train_start = 2010L) {
  purrr::map_dfr(test_seasons, function(y) {
    test <- features[features$season == y, ]
    if (nrow(test) == 0) return(NULL)
    train <- features[features$season >= train_start & features$season < y & !is.na(features$spread), ]
    fit <- stats::glm(win ~ spread, data = train, family = stats::quasibinomial())
    dplyr::transmute(test, game_id, season, week, margin, win, pred_margin = spread,
                     pred_win = unname(stats::predict(fit, newdata = test, type = "response")))
  })
}

score_predictions <- function(preds) {
  preds |>
    dplyr::filter(!is.na(pred_margin), !is.na(pred_win)) |>
    dplyr::mutate(p = pmin(pmax(pred_win, 1e-6), 1 - 1e-6)) |>
    dplyr::group_by(season) |>
    dplyr::summarise(
      rmse = sqrt(mean((margin - pred_margin)^2)),
      mae = mean(abs(margin - pred_margin)),
      log_loss = mean(-(win * log(p) + (1 - win) * log(1 - p))),
      brier = mean((win - pred_win)^2),
      accuracy = mean(((pred_win > 0.5) == (win == 1))[win != 0.5]),
      n_games = dplyr::n(),
      .groups = "drop"
    )
}

calibration_table <- function(preds, bins = 10) {
  preds |>
    dplyr::mutate(bin = cut(pred_win, breaks = seq(0, 1, length.out = bins + 1),
                            include.lowest = TRUE)) |>
    dplyr::group_by(bin) |>
    dplyr::summarise(mean_pred = mean(pred_win), actual_rate = mean(win), n = dplyr::n(),
                     .groups = "drop")
}

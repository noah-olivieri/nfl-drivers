# Models: point margin (linear regression) and win/loss (logistic, ties = 0.5).
# Numeric features are standardized with training-rows-only means and SDs, so each
# coefficient reads as points (or log-odds) per 1 SD; binary flags stay 0/1.

# full and market keep the variables they had before QB ratings, so they stay the
# baselines; the _qb specs add the QB rating gap.
FULL_VARS <- setdiff(unlist(FEATURE_GROUPS, use.names = FALSE), "qb_adj_diff")
MODEL_SPECS <- list(
  base      = "home",
  full      = FULL_VARS,
  full_qb   = c(FULL_VARS, "qb_adj_diff"),
  market    = c(FULL_VARS, "spread"),
  market_qb = c(FULL_VARS, "qb_adj_diff", "spread")
)

fit_scaler <- function(train, vars) {
  numeric_vars <- setdiff(vars, BINARY_FEATURES)
  list(mean = vapply(train[numeric_vars], mean, numeric(1)),
       sd = vapply(train[numeric_vars], stats::sd, numeric(1)))
}

apply_scaler <- function(df, scaler) {
  for (v in names(scaler$mean)) {
    s <- scaler$sd[[v]]
    df[[v]] <- (df[[v]] - scaler$mean[[v]]) / ifelse(is.na(s) || s == 0, 1, s)
  }
  df
}

fit_models <- function(train, vars) {
  # Drop anything with no variation in the training rows (e.g. `home` when every
  # training game was a true home game); those carry no information.
  vars <- vars[vapply(vars, function(v) length(unique(train[[v]])) > 1, logical(1))]
  scaler <- fit_scaler(train, vars)
  d <- apply_scaler(train, scaler)
  rhs <- if (length(vars) == 0) "1" else vars
  list(
    vars = vars,
    scaler = scaler,
    margin = stats::lm(stats::reformulate(rhs, response = "margin"), data = d),
    win = stats::glm(stats::reformulate(rhs, response = "win"), data = d,
                     family = stats::quasibinomial())
  )
}

predict_models <- function(fit, newdata) {
  d <- apply_scaler(newdata, fit$scaler)
  tibble::tibble(game_id = newdata$game_id,
                 pred_margin = unname(stats::predict(fit$margin, newdata = d)),
                 pred_win = unname(stats::predict(fit$win, newdata = d, type = "response")))
}

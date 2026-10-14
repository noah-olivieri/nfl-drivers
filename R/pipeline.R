# End-to-end pipeline: data -> ratings -> features -> models -> validation and
# importance. Saves everything the report needs to <cache_dir>/results.rds.

run_all <- function(cache_dir = "data/cache", refresh = 2026L,
                    stadiums_path = "data/stadiums.csv") {
  step <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)
  seasons <- 2010:2026
  test_seasons <- 2018:2026

  step("Loading data")
  sched_all <- clean_schedules(load_cached(2009:2026, "schedules", cache_dir, refresh),
                               completed_only = FALSE)   # includes the upcoming week
  sched <- dplyr::filter(sched_all, !is.na(home_score), !is.na(away_score))
  plays <- clean_plays(load_cached(2009:2026, "pbp", cache_dir, refresh))
  stadiums <- readr::read_csv(stadiums_path, show_col_types = FALSE)

  step("Tuning rating parameters on 2010-2017")
  tuning <- tune_rating_params(plays, sched, default_rating_grid(), seasons = 2010:2017)

  step("Computing ratings")
  # Ratings cover every completed week plus the next unplayed week (the one to forecast).
  sched_rate <- rating_schedule(sched_all)
  ratings_ridge <- compute_ridge_ratings(plays, sched_rate, tuning$best, seasons)
  ratings_rolling <- compute_rolling_ratings(plays, sched_rate, seasons)

  step("Building features")
  features_ridge <- build_features(sched, ratings_ridge, stadiums)
  features_rolling <- build_features(sched, ratings_rolling, stadiums)

  step("Walk-forward validation")
  preds <- list(
    base        = walk_forward(features_ridge, MODEL_SPECS$base, test_seasons),
    full_ridge  = walk_forward(features_ridge, MODEL_SPECS$full, test_seasons),
    full_rolling = walk_forward(features_rolling, MODEL_SPECS$full, test_seasons),
    market      = walk_forward(features_ridge, MODEL_SPECS$market, test_seasons),
    spread_only = spread_benchmark(features_ridge, test_seasons)
  )
  scores <- purrr::imap_dfr(preds, function(p, name) {
    dplyr::mutate(score_predictions(p), model = name, .before = 1)
  })

  step("Importance")
  full_vars <- MODEL_SPECS$full
  market_vars <- MODEL_SPECS$market
  ablation_groups <- ablate(features_ridge, full_vars, FEATURE_GROUPS, test_seasons = test_seasons)
  ablation_vars <- ablate(features_ridge, full_vars, as.list(stats::setNames(full_vars, full_vars)),
                          test_seasons = test_seasons)
  ablation_market_vars <- ablate(features_ridge, market_vars,
                                 as.list(stats::setNames(market_vars, market_vars)),
                                 test_seasons = test_seasons)

  results <- list(
    created = Sys.time(),
    n_games = dplyr::count(features_ridge, season, name = "games"),
    dropped = attr(features_ridge, "dropped"),
    tuning = tuning,
    ratings_ridge = ratings_ridge,
    ratings_rolling = ratings_rolling,
    features_ridge = features_ridge,
    features_rolling = features_rolling,
    preds = preds,
    scores = scores,
    calibration = calibration_table(preds$full_ridge),
    ablation_groups = ablation_groups,
    ablation_vars = ablation_vars,
    ablation_market_vars = ablation_market_vars,
    coefs = coef_table(features_ridge, full_vars),
    coefs_market = coef_table(features_ridge, market_vars)
  )
  saveRDS(results, file.path(cache_dir, "results.rds"))
  step("Saved ", file.path(cache_dir, "results.rds"))
  invisible(results)
}

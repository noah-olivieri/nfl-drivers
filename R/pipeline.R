# End-to-end pipeline: data -> team and QB ratings -> features -> models -> validation,
# importance and paired bootstrap. Saves everything the report needs to
# <cache_dir>/results.rds.

run_all <- function(cache_dir = "data/cache", refresh = 2026L,
                    stadiums_path = "data/stadiums.csv") {
  step <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)
  code <- code_state()   # recorded so a snapshot can check which code made these results
  seasons <- 2010:2026
  test_seasons <- 2018:2026
  verdict_seasons <- 2018:2025   # 2026 is a live check only

  step("Loading data")
  sched_all <- clean_schedules(load_cached(2009:2026, "schedules", cache_dir, refresh),
                               completed_only = FALSE)   # includes the upcoming week
  sched <- dplyr::filter(sched_all, !is.na(home_score), !is.na(away_score))
  # 2006-2008 plays only feed QB history and experience counts; team ratings reach
  # back one season (ridge) or 8 games (rolling), so they are unaffected.
  plays <- clean_plays(load_cached(2006:2026, "pbp", cache_dir, refresh))
  excluded_dropbacks <- attr(qb_dropbacks(plays), "excluded")
  coverage <- starter_id_coverage(dplyr::filter(sched, season >= 2010), plays)
  if (coverage < 0.99) {
    stop("Only ", round(100 * coverage, 1), "% of schedule starter IDs appear in play-by-play; ",
         "QB ratings would treat the rest as unseen.", call. = FALSE)
  }
  stadiums <- readr::read_csv(stadiums_path, show_col_types = FALSE)

  step("Tuning rating parameters on 2010-2017")
  tuning <- tune_rating_params(plays, sched, default_rating_grid(), seasons = 2010:2017)

  step("Computing ratings")
  # Ratings cover every completed week plus the forecast week (the rest of a week in
  # progress, or else the next week).
  sched_rate <- rating_schedule(sched_all)
  ratings_ridge <- compute_ridge_ratings(plays, sched_rate, tuning$best, seasons)
  ratings_rolling <- compute_rolling_ratings(plays, sched_rate, seasons)

  step("Tuning QB rating parameters on 2010-2017")
  qb_tuning <- tune_qb_params(plays, sched, ratings_ridge, tuning$best, default_qb_grid(),
                              seasons = 2010:2017)

  step("Computing QB ratings")
  qb_ratings_ridge <- compute_qb_ratings(plays, sched_rate, qb_tuning$best, seasons)
  qb_ratings_rolling <- compute_rolling_qb_ratings(plays, sched_rate, seasons)
  qb_mix_ridge <- team_qb_mix(plays, qb_ratings_ridge, sched_rate, tuning$best, seasons)
  qb_mix_rolling <- team_qb_mix(plays, qb_ratings_rolling, sched_rate, tuning$best, seasons)

  step("Building features")
  # Features cover completed games plus the forecast week's unplayed games (margin and
  # win NA). features_ridge carries the ridge QB gap, so every ridge run is scored on
  # the same games.
  features_ridge <- build_features(sched_rate, ratings_ridge, stadiums,
                                   qb = list(ratings = qb_ratings_ridge, mix = qb_mix_ridge))
  features_qb_rolling <- build_features(sched_rate, ratings_ridge, stadiums,
                                        qb = list(ratings = qb_ratings_rolling, mix = qb_mix_rolling))
  features_rolling <- build_features(sched_rate, ratings_rolling, stadiums)
  dropped <- attr(features_ridge, "dropped")
  upcoming_dropped <- attr(features_ridge, "dropped_upcoming")
  missing_starters <- attr(features_ridge, "missing_starters")

  step("Walk-forward validation")
  preds_all <- list(
    base            = walk_forward(features_ridge, MODEL_SPECS$base, test_seasons),
    full_ridge      = walk_forward(features_ridge, MODEL_SPECS$full, test_seasons),
    full_rolling    = walk_forward(features_rolling, MODEL_SPECS$full, test_seasons),
    full_qb_ridge   = walk_forward(features_ridge, MODEL_SPECS$full_qb, test_seasons),
    full_qb_rolling = walk_forward(features_qb_rolling, MODEL_SPECS$full_qb, test_seasons),
    market          = walk_forward(features_ridge, MODEL_SPECS$market, test_seasons),
    market_qb       = walk_forward(features_ridge, MODEL_SPECS$market_qb, test_seasons),
    spread_only     = spread_benchmark(features_ridge, test_seasons)
  )
  # Unplayed games get a prediction from every run. From here on, everything (scores,
  # comparisons, importance, the QB verdict and what is saved) uses completed games only.
  upcoming <- upcoming_predictions(preds_all, sched_rate)
  upcoming_feats <- upcoming_features(features_ridge, MODEL_SPECS$full_qb)
  stopifnot(setequal(upcoming_feats$game_id,
                     upcoming$game_id[upcoming$model == "full_qb_ridge"]))
  preds <- lapply(preds_all, function(p) p[!is.na(p$margin), ])
  completed <- function(f) f[!is.na(f$margin), ]
  features_ridge <- completed(features_ridge)
  features_qb_rolling <- completed(features_qb_rolling)
  features_rolling <- completed(features_rolling)
  unplayed <- is.na(sched_rate$home_score) | is.na(sched_rate$away_score)
  forecast_season <- if (any(unplayed)) sched_rate$season[unplayed][1] else NA_integer_
  scores <- purrr::imap_dfr(preds, function(p, name) {
    dplyr::mutate(score_predictions(p), model = name, .before = 1)
  })

  step("Importance")
  full_vars <- MODEL_SPECS$full
  market_vars <- MODEL_SPECS$market
  ap_groups <- ablation_preds(features_ridge, full_vars, FEATURE_GROUPS, test_seasons = test_seasons)
  ap_vars <- ablation_preds(features_ridge, full_vars, as.list(stats::setNames(full_vars, full_vars)),
                            test_seasons = test_seasons)
  ap_market_vars <- ablation_preds(features_ridge, market_vars,
                                   as.list(stats::setNames(market_vars, market_vars)),
                                   test_seasons = test_seasons)
  qb_vars <- MODEL_SPECS$full_qb
  market_qb_vars <- MODEL_SPECS$market_qb
  as_sets <- function(v) as.list(stats::setNames(v, v))
  ap_qb_groups <- ablation_preds(features_ridge, qb_vars, FEATURE_GROUPS, test_seasons = test_seasons)
  ap_qb_vars <- ablation_preds(features_ridge, qb_vars, as_sets(qb_vars), test_seasons = test_seasons)
  ap_market_qb_groups <- ablation_preds(features_ridge, market_qb_vars, FEATURE_GROUPS,
                                        test_seasons = test_seasons)
  ap_market_qb_vars <- ablation_preds(features_ridge, market_qb_vars, as_sets(market_qb_vars),
                                      test_seasons = test_seasons)

  step("Paired bootstrap")
  # Each pair is c(reference, challenger); positive differences mean the challenger is worse.
  model_pairs <- list(c("spread_only", "full_ridge"), c("spread_only", "market"),
                      c("base", "full_ridge"), c("full_ridge", "full_rolling"),
                      c("full_ridge", "full_qb_ridge"), c("full_qb_ridge", "full_qb_rolling"),
                      c("spread_only", "full_qb_ridge"), c("market", "market_qb"))
  model_comparisons <- compare_models(preds, model_pairs)
  ablation_groups_boot <- ablation_bootstrap(ap_groups)
  ablation_vars_boot <- ablation_bootstrap(ap_vars)
  ablation_market_vars_boot <- ablation_bootstrap(ap_market_vars)

  step("QB verdict")
  in_verdict <- function(p) p[p$season %in% verdict_seasons, ]
  change_ids <- features_ridge$game_id[features_ridge$qb_change_game == 1]
  only_change <- function(p) p[p$game_id %in% change_ids, ]
  ref <- in_verdict(preds$full_ridge)
  challenger <- in_verdict(preds$full_qb_ridge)
  qb_all_comparison <- paired_bootstrap(ref, challenger)
  qb_subset_comparison <- paired_bootstrap(only_change(ref), only_change(challenger))
  qb_season_scores <- qb_season_rmse(ref, challenger)
  verdict <- qb_verdict(qb_all_comparison, qb_subset_comparison, qb_season_scores)

  qb_names <- dplyr::bind_rows(
    dplyr::select(sched_all, qb_id = home_qb_id, qb_name = home_qb_name),
    dplyr::select(sched_all, qb_id = away_qb_id, qb_name = away_qb_name)
  ) |>
    dplyr::filter(!is.na(qb_id)) |>
    dplyr::distinct(qb_id, .keep_all = TRUE)

  results <- list(
    created = Sys.time(),
    code_sha = code$sha,
    code_dirty = code$dirty,
    n_games = dplyr::count(features_ridge, season, name = "games"),
    dropped = dropped,
    tuning = tuning,
    ratings_ridge = ratings_ridge,
    ratings_rolling = ratings_rolling,
    features_ridge = features_ridge,
    features_rolling = features_rolling,
    preds = preds,
    scores = scores,
    calibration = calibration_table(preds$full_ridge),
    model_comparisons = model_comparisons,
    ablation_groups = ablation_deltas(ap_groups),
    ablation_vars = ablation_deltas(ap_vars),
    ablation_market_vars = ablation_deltas(ap_market_vars),
    ablation_groups_boot = ablation_groups_boot,
    ablation_vars_boot = ablation_vars_boot,
    ablation_market_vars_boot = ablation_market_vars_boot,
    coefs = coef_table(features_ridge, full_vars),
    coefs_market = coef_table(features_ridge, market_vars),
    coefs_qb = coef_table(features_ridge, qb_vars),
    coefs_market_qb = coef_table(features_ridge, market_qb_vars),
    qb_tuning = qb_tuning,
    qb_ratings_ridge = qb_ratings_ridge,
    qb_ratings_rolling = qb_ratings_rolling,
    qb_mix_ridge = qb_mix_ridge,
    qb_mix_rolling = qb_mix_rolling,
    features_qb_rolling = features_qb_rolling,
    scores_qb_subset = score_qb_subsets(preds, features_ridge,
                                        c("full_ridge", "full_qb_ridge", "full_qb_rolling", "spread_only")),
    qb_all_comparison = qb_all_comparison,
    qb_subset_comparison = qb_subset_comparison,
    qb_season_scores = qb_season_scores,
    qb_verdict = verdict,
    ablation_qb_groups = ablation_deltas(ap_qb_groups),
    ablation_qb_groups_boot = ablation_bootstrap(ap_qb_groups),
    ablation_qb_vars = ablation_deltas(ap_qb_vars),
    ablation_qb_vars_boot = ablation_bootstrap(ap_qb_vars),
    ablation_market_qb_groups = ablation_deltas(ap_market_qb_groups),
    ablation_market_qb_groups_boot = ablation_bootstrap(ap_market_qb_groups),
    ablation_market_qb_vars = ablation_deltas(ap_market_qb_vars),
    ablation_market_qb_vars_boot = ablation_bootstrap(ap_market_qb_vars),
    excluded_dropbacks = excluded_dropbacks,
    missing_starters = missing_starters,
    starter_id_coverage = coverage,
    qb_names = qb_names,
    qb_starters = dplyr::filter(scheduled_starters(sched_rate), !is.na(qb_id)),
    upcoming = upcoming,
    upcoming_features = upcoming_feats,
    upcoming_dropped = upcoming_dropped,
    schedule_downloaded = schedule_download_time(cache_dir, forecast_season)
  )
  saveRDS(results, file.path(cache_dir, "results.rds"))
  step("Saved ", file.path(cache_dir, "results.rds"))
  invisible(results)
}

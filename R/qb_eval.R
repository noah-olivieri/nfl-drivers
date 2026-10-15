# Judging QB ratings: scores split by QB-change games, and the decision rule fixed in
# the spec before any results (docs/superpowers/specs/2026-10-15-qb-ratings-design.md,
# section 6).

# Season scores for each named model, split into games where a starter changed or a
# backup started ("qb_change") and all other games ("other").
score_qb_subsets <- function(preds, features, models) {
  marker <- dplyr::select(features, game_id, qb_change_game)
  purrr::map_dfr(models, function(m) {
    p <- dplyr::inner_join(preds[[m]], marker, by = "game_id")
    dplyr::bind_rows(
      dplyr::mutate(score_predictions(p[p$qb_change_game == 1, ]), subset = "qb_change"),
      dplyr::mutate(score_predictions(p[p$qb_change_game == 0, ]), subset = "other")
    ) |>
      dplyr::mutate(model = m, .before = 1)
  })
}

qb_season_rmse <- function(ref, qb) {
  dplyr::inner_join(score_predictions(ref), score_predictions(qb), by = "season",
                    suffix = c("_ref", "_qb")) |>
    dplyr::select(season, rmse_ref, rmse_qb)
}

# boot_all / boot_subset: paired_bootstrap(full_ridge, full_qb_ridge) rows on all
# games and on QB-change games (differences are QB model minus full_ridge, so
# negative = QB model better). season_rmse: qb_season_rmse() over 2018-2025.
#   Path 1: both all-games intervals entirely below 0, and lower RMSE in most
#           seasons (at least 5 of 8).
#   Path 2: subset RMSE interval entirely below 0, and all-games RMSE and log loss
#           point estimates no worse (difference <= 0).
qb_verdict <- function(boot_all, boot_subset, season_rmse) {
  better <- season_rmse$rmse_qb < season_rmse$rmse_ref
  path1 <- boot_all$d_rmse_high < 0 && boot_all$d_log_loss_high < 0 && sum(better) > length(better) / 2
  path2 <- boot_subset$d_rmse_high < 0 && boot_all$d_rmse <= 0 && boot_all$d_log_loss <= 0
  tibble::tibble(path1_all_games = path1, path2_qb_change = path2, earns_place = path1 || path2,
                 seasons_better = sum(better), n_seasons = length(better))
}

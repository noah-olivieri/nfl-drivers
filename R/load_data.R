# Data loading: download nflverse data once per season and cache it locally.

# Where the schedules file lives when nflreadr::load_schedules() can't reach it.
# On a normal machine, leave this unset and nflreadr is used directly.
schedules_source <- function() getOption("nfldrivers.games_rds", NA_character_)

fetch_schedules <- function(season) {
  path <- schedules_source()
  if (is.na(path)) return(tibble::as_tibble(nflreadr::load_schedules(season)))
  repo <- dirname(dirname(path))
  if (dir.exists(file.path(repo, ".git"))) {
    system2("git", c("-C", repo, "pull", "-q"), stdout = FALSE, stderr = FALSE)
  }
  games <- readRDS(path)
  tibble::as_tibble(games[games$season == season, ])
}

fetch_pbp <- function(season) tibble::as_tibble(nflreadr::load_pbp(season))

# Load seasons of `kind` ("pbp" or "schedules"), reading cache_dir/<kind>_<season>.rds
# when present. Seasons in `refresh` are always re-downloaded.
load_cached <- function(seasons, kind, cache_dir = "data/cache", refresh = 2026L, fetch = NULL) {
  kind <- match.arg(kind, c("pbp", "schedules"))
  if (is.null(fetch)) fetch <- if (kind == "pbp") fetch_pbp else fetch_schedules
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  out <- lapply(seasons, function(season) {
    path <- file.path(cache_dir, sprintf("%s_%d.rds", kind, season))
    if (file.exists(path) && !(season %in% refresh)) return(readRDS(path))
    data <- tryCatch(fetch(season), error = function(e) {
      stop("Failed to download ", kind, " for season ", season, ": ", conditionMessage(e),
           call. = FALSE)
    })
    if (nrow(data) == 0) {
      stop("Failed to download ", kind, " for season ", season, ": no rows returned",
           call. = FALSE)
    }
    saveRDS(data, path)
    data
  })
  dplyr::bind_rows(out)
}

clean_abbr <- function(x) nflreadr::clean_team_abbrs(x, current_location = TRUE)

# Relocated teams under one current abbreviation. By default only completed games
# are kept; completed_only = FALSE keeps unplayed games too, so week cutoffs (and
# ratings) exist for the week about to be played.
clean_schedules <- function(sched, completed_only = TRUE) {
  if (completed_only) sched <- dplyr::filter(sched, !is.na(home_score), !is.na(away_score))
  dplyr::mutate(sched, home_team = clean_abbr(home_team), away_team = clean_abbr(away_team))
}

# Schedule rows that ratings are computed for: every completed game, plus the
# games of the next week that has no completed games yet (the week to forecast).
rating_schedule <- function(sched_all) {
  done <- !is.na(sched_all$home_score) & !is.na(sched_all$away_score)
  key <- sched_all$season * 100 + sched_all$week
  last_done <- if (any(done)) max(key[done]) else -Inf
  upcoming <- key[!done & key > last_done]
  next_key <- if (length(upcoming)) min(upcoming) else NA
  sched_all[done | (!is.na(next_key) & key == next_key), ]
}

# Normalize the team columns of a plays table to current abbreviations.
clean_plays_teams <- function(df) {
  dplyr::mutate(df, posteam = clean_abbr(posteam), defteam = clean_abbr(defteam),
                home_team = clean_abbr(home_team))
}

# Run and pass plays with a valid EPA (drops kneels, spikes and no-play penalties).
# qb_id credits each dropback (pass, sack or scramble) to the QB; scrambles keep
# play_type "run", so team ratings still count them as runs.
clean_plays <- function(pbp) {
  pbp |>
    dplyr::filter(pass == 1 | rush == 1, !is.na(epa), play_type %in% c("pass", "run")) |>
    dplyr::transmute(season, week, game_id, game_date = as.Date(game_date),
                     posteam, defteam, home_team, play_type, epa, wp,
                     qb_dropback = dplyr::coalesce(as.numeric(qb_dropback), 0),
                     qb_id = ifelse(qb_dropback == 1, id, NA_character_)) |>
    clean_plays_teams()
}

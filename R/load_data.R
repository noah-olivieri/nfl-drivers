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

# Completed games only, with relocated teams under one current abbreviation.
clean_schedules <- function(sched) {
  sched |>
    dplyr::filter(!is.na(home_score), !is.na(away_score)) |>
    dplyr::mutate(home_team = clean_abbr(home_team), away_team = clean_abbr(away_team))
}

# Run and pass plays with a valid EPA (drops kneels, spikes and no-play penalties).
clean_plays <- function(pbp) {
  pbp |>
    dplyr::filter(pass == 1 | rush == 1, !is.na(epa), play_type %in% c("pass", "run")) |>
    dplyr::transmute(season, week, game_id, game_date = as.Date(game_date),
                     posteam = clean_abbr(posteam), defteam = clean_abbr(defteam),
                     home_team = clean_abbr(home_team), play_type, epa, wp)
}

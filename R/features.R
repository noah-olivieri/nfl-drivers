# Game features: one row per completed game, from the home team's view,
# using only information known before kickoff.

FEATURE_GROUPS <- list(
  strength    = c("pass_off_diff", "rush_off_diff", "pass_def_diff", "rush_def_diff"),
  situational = c("home", "rest_diff", "home_off_bye", "away_off_bye", "home_short_week",
                  "away_short_week", "away_travel_miles", "tz_crossed", "div_game", "primetime"),
  qb          = c("home_qb_changed", "away_qb_changed", "home_qb_backup", "away_qb_backup"),
  environment = c("dome", "temp", "wind", "turf")
)

BINARY_FEATURES <- c("home", "home_off_bye", "away_off_bye", "home_short_week", "away_short_week",
                     "div_game", "primetime", "home_qb_changed", "away_qb_changed",
                     "home_qb_backup", "away_qb_backup", "dome", "turf")

GRASS_SURFACES <- c("grass", "dessograss")   # dessograss is reinforced natural grass

# The most common value of x (ties go to the most recent, i.e. last, occurrence).
recent_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  counts <- table(x)
  top <- names(counts)[counts == max(counts)]
  x[max(which(x %in% top))]
}

# QB change and backup flags for every team-game, from each team's starters in
# date order across all games in `sched` (crossing seasons).
qb_flags <- function(sched, lookback = 8) {
  long <- dplyr::bind_rows(
    dplyr::transmute(sched, game_id, gameday = as.Date(gameday), team = home_team, qb = home_qb_id, side = "home"),
    dplyr::transmute(sched, game_id, gameday = as.Date(gameday), team = away_team, qb = away_qb_id, side = "away")
  )
  long |>
    dplyr::arrange(team, gameday) |>
    dplyr::group_by(team) |>
    dplyr::mutate(
      prev = dplyr::lag(qb),
      qb_changed = as.numeric(!is.na(qb) & !is.na(prev) & qb != prev),
      usual = purrr::map_chr(dplyr::row_number(), function(i) {
        if (i == 1) return(NA_character_)
        recent_mode(qb[max(1, i - lookback):(i - 1)])
      }),
      qb_backup = as.numeric(!is.na(qb) & !is.na(usual) & qb != usual)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(game_id, side, qb_changed, qb_backup) |>
    tidyr::pivot_wider(names_from = side, values_from = c(qb_changed, qb_backup),
                       names_glue = "{side}_{.value}")
}

build_features <- function(sched, ratings, stadiums) {
  qb <- qb_flags(sched)
  homes <- team_home_venue(sched)
  st <- dplyr::select(stadiums, stadium_id, lat, lon, tz)

  games <- sched |>
    dplyr::filter(season %in% unique(ratings$season)) |>
    dplyr::mutate(
      gameday = as.Date(gameday),
      kickoff = as.POSIXct(paste(gameday, gametime), tz = "America/New_York"),
      roof = trimws(roof), surface = trimws(surface)
    ) |>
    dplyr::left_join(st, by = "stadium_id") |>
    dplyr::left_join(homes, by = c("season", "away_team" = "team")) |>
    dplyr::left_join(dplyr::rename(st, away_lat = lat, away_lon = lon, away_tz = tz),
                     by = c("home_stadium_id" = "stadium_id"))

  # Fill missing outdoor weather: that stadium's median for the season, then the
  # stadium's median over all seasons, then the league's median that season.
  # (Retractable roofs left open often have no weather recorded at all.)
  fill_median <- function(df, ...) {
    df |>
      dplyr::group_by(dome, ...) |>
      dplyr::mutate(temp = dplyr::coalesce(temp, stats::median(temp, na.rm = TRUE)),
                    wind = dplyr::coalesce(wind, stats::median(wind, na.rm = TRUE))) |>
      dplyr::ungroup()
  }
  games <- games |>
    dplyr::mutate(dome = as.numeric(roof %in% c("dome", "closed")),
                  temp = as.numeric(temp), wind = as.numeric(wind)) |>
    fill_median(stadium_id, season) |>
    fill_median(stadium_id) |>
    fill_median(season)

  out <- games |>
    dplyr::mutate(
      margin = home_score - away_score,
      win = dplyr::case_when(margin > 0 ~ 1, margin < 0 ~ 0, TRUE ~ 0.5),
      home = as.numeric(location != "Neutral"),
      rest_diff = home_rest - away_rest,
      home_off_bye = as.numeric(home_rest >= 13), away_off_bye = as.numeric(away_rest >= 13),
      home_short_week = as.numeric(home_rest <= 5), away_short_week = as.numeric(away_rest <= 5),
      away_travel_miles = haversine_miles(away_lat, away_lon, lat, lon),
      tz_gap = abs(utc_offset_hours(tz, kickoff) - utc_offset_hours(away_tz, kickoff)),
      tz_crossed = pmin(tz_gap, 24 - tz_gap),
      div_game = as.numeric(div_game),
      primetime = as.numeric(weekday %in% c("Thursday", "Monday") | gametime >= "19:00"),
      temp = ifelse(dome == 1, 70, temp), wind = ifelse(dome == 1, 0, wind),
      turf = as.numeric(!(surface %in% c(GRASS_SURFACES, ""))),
      spread = spread_line
    ) |>
    rating_diffs(ratings) |>
    dplyr::left_join(qb, by = "game_id") |>
    dplyr::select(game_id, season, week, gameday, margin, win,
                  dplyr::all_of(unlist(FEATURE_GROUPS, use.names = FALSE)), spread)

  required <- c("margin", "win", unlist(FEATURE_GROUPS, use.names = FALSE))
  complete <- stats::complete.cases(out[required])
  out <- out[complete, ]
  attr(out, "dropped") <- sum(!complete)
  out
}

# Venue helpers: travel distance, time zone offsets, and each team's home stadium.

haversine_miles <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  a <- sin(dlat / 2)^2 + cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  2 * 3958.8 * asin(pmin(1, sqrt(a)))
}

# UTC offset in hours for each time zone at each moment (handles DST and Arizona).
utc_offset_hours <- function(tz, datetime) {
  datetime <- rep_len(as.POSIXct(datetime, tz = "UTC"), length(tz))
  mapply(function(z, t) {
    local_clock <- as.POSIXct(format(t, tz = z, usetz = FALSE), tz = "UTC")
    as.numeric(difftime(local_clock, t, units = "hours"))
  }, tz, as.list(datetime), USE.NAMES = FALSE)
}

# Each team's most frequent home stadium per season, ignoring neutral-site games.
team_home_venue <- function(sched) {
  sched |>
    dplyr::filter(location == "Home") |>
    dplyr::count(season, team = home_team, home_stadium_id = stadium_id) |>
    dplyr::group_by(season, team) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(season, team, home_stadium_id)
}

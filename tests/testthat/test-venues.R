test_that("every venue in the schedules has coordinates", {
  skip_if_not(file.exists("../../data/cache/schedules_2026.rds"), "data not cached")
  sched <- load_cached(2009:2026, "schedules", "../../data/cache", refresh = integer())
  st <- readr::read_csv("../../data/stadiums.csv", show_col_types = FALSE)
  expect_length(setdiff(unique(sched$stadium_id), st$stadium_id), 0)
  expect_false(anyNA(st[c("lat", "lon", "tz")]))
  expect_equal(anyDuplicated(st$stadium_id), 0)
})

test_that("haversine matches known distances", {
  # one degree of longitude on the equator = 2 * pi * 3958.8 / 360 miles
  expect_equal(haversine_miles(0, 0, 0, 1), 2 * pi * 3958.8 / 360)
  # Arrowhead to Hard Rock Stadium is roughly 1225 miles
  expect_equal(haversine_miles(39.05, -94.48, 25.96, -80.24), 1225, tolerance = 0.01)
})

test_that("Arizona has no DST", {
  summer <- as.POSIXct("2024-07-01 12:00", tz = "UTC")
  expect_equal(utc_offset_hours("America/Phoenix", summer), -7)
  expect_equal(utc_offset_hours("America/Los_Angeles", summer), -7)
  winter <- as.POSIXct("2024-12-01 12:00", tz = "UTC")
  expect_equal(utc_offset_hours("America/Los_Angeles", winter), -8)
})

test_that("team home venue ignores neutral-site games", {
  s <- tibble::tibble(season = 2022L, home_team = "ARI", location = c("Home", "Home", "Neutral"),
                      stadium_id = c("PHO00", "PHO00", "MEX00"))
  expect_equal(team_home_venue(s)$home_stadium_id, "PHO00")
})

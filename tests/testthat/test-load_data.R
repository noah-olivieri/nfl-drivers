test_that("load_cached reuses cache and refetches refresh seasons", {
  dir <- withr::local_tempdir()
  calls <- integer()
  fake <- function(s) { calls <<- c(calls, s); tibble::tibble(season = s, x = 1) }
  load_cached(c(2024L, 2026L), "schedules", dir, refresh = 2026L, fetch = fake)
  load_cached(c(2024L, 2026L), "schedules", dir, refresh = 2026L, fetch = fake)
  expect_equal(calls, c(2024L, 2026L, 2026L))
})

test_that("load_cached errors clearly naming the failed season", {
  dir <- withr::local_tempdir()
  bad <- function(s) stop("network down")
  expect_error(load_cached(2015L, "pbp", dir, fetch = bad), "2015")
})

test_that("clean_schedules drops unplayed and no-contest games", {
  s <- tibble::tibble(game_id = c("a", "b", "c"), home_team = c("SD", "KC", "BUF"),
                      away_team = c("OAK", "DEN", "CIN"),
                      home_score = c(20, NA, NA), away_score = c(17, NA, 3))
  out <- clean_schedules(s)
  expect_equal(out$game_id, "a")
  expect_equal(out$home_team, "LAC")
  expect_equal(out$away_team, "LV")
})

test_that("clean_plays keeps only pass/run plays with EPA", {
  p <- tibble::tibble(season = 2020L, week = 1L, game_id = "g", game_date = "2020-09-13",
                      posteam = "KC", defteam = "HOU", home_team = "KC",
                      play_type = c("pass", "run", "qb_kneel", "no_play", "pass"),
                      pass = c(1, 0, 0, 1, 1), rush = c(0, 1, 1, 0, 0),
                      epa = c(0.5, -0.2, -0.1, 0.3, NA), wp = 0.5)
  out <- clean_plays(p)
  expect_equal(nrow(out), 2)
  expect_s3_class(out$game_date, "Date")
})

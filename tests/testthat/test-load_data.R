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

test_that("clean_schedules can keep unplayed games so the next week gets ratings", {
  s <- tibble::tibble(season = 2026L, week = c(5L, 6L), gameday = c("2026-10-04", "2026-10-11"),
                      game_id = c("played", "upcoming"), home_team = c("OAK", "KC"),
                      away_team = c("DEN", "SD"), home_score = c(20, NA), away_score = c(17, NA))
  all_games <- clean_schedules(s, completed_only = FALSE)
  expect_equal(all_games$game_id, c("played", "upcoming"))
  expect_equal(all_games$away_team, c("DEN", "LAC"))
  expect_equal(week_cutoffs(all_games)$week, c(5L, 6L))
})

test_that("rating schedule is completed games plus the next unplayed week only", {
  s <- tibble::tibble(season = 2026L, week = c(5L, 6L, 6L, 7L),
                      game_id = c("w5", "w6a", "w6b", "w7"),
                      home_score = c(20, NA, NA, NA), away_score = c(17, NA, NA, NA))
  expect_equal(rating_schedule(s)$game_id, c("w5", "w6a", "w6b"))
})

# Week 6 with its Thursday game played and its Sunday and Monday games still to come.
midweek_sched <- function() {
  tibble::tibble(season = 2026L, week = c(5L, 6L, 6L, 6L, 7L),
                 gameday = c("2026-10-11", "2026-10-15", "2026-10-18", "2026-10-19", "2026-10-22"),
                 game_id = c("w5", "w6_thu", "w6_sun", "w6_mon", "w7"),
                 home_score = c(20, 24, NA, NA, NA), away_score = c(17, 10, NA, NA, NA))
}

test_that("mid-week, the rest of the week stays the forecast week", {
  expect_equal(rating_schedule(midweek_sched())$game_id, c("w5", "w6_thu", "w6_sun", "w6_mon"))
})

test_that("mid-week, the forecast week's cutoff is still its Thursday", {
  cw <- week_cutoffs(rating_schedule(midweek_sched()))
  expect_equal(cw$week, c(5L, 6L))
  expect_equal(cw$cutoff_date[cw$week == 6L], as.Date("2026-10-15"))
})

test_that("once a week is finished, the next week is the forecast week", {
  s <- tibble::tibble(season = 2026L, week = c(6L, 6L, 7L, 7L, 8L),
                      game_id = c("w6a", "w6b", "w7a", "w7b", "w8"),
                      home_score = c(20, 13, NA, NA, NA), away_score = c(17, 9, NA, NA, NA))
  expect_equal(rating_schedule(s)$game_id, c("w6a", "w6b", "w7a", "w7b"))
})

test_that("with every game played there is no forecast week", {
  s <- tibble::tibble(season = 2026L, week = c(21L, 22L), game_id = c("con", "sb"),
                      home_score = c(27, 31), away_score = c(24, 20))
  expect_equal(rating_schedule(s)$game_id, c("con", "sb"))
})

test_that("an unplayed game in a past week is a no-contest and dropped", {
  # BUF-CIN 2022 week 17 was never finished; week 18 was played after it.
  s <- tibble::tibble(season = 2022L, week = c(17L, 17L, 18L), game_id = c("buf_cin", "other", "w18"),
                      home_score = c(NA, 24, 20), away_score = c(NA, 21, 3))
  out <- rating_schedule(s)
  expect_equal(out$game_id, c("other", "w18"))
  expect_true(17L %in% out$week)   # the week keeps its ratings via its other games
})

test_that("schedule download time is the cache file's modification time", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "schedules_2026.rds")
  saveRDS(1, path)
  when <- as.POSIXct("2026-10-15 14:56:00", tz = "America/Los_Angeles")
  Sys.setFileTime(path, when)
  expect_equal(as.numeric(schedule_download_time(dir, 2026L)), as.numeric(when))
  expect_true(is.na(schedule_download_time(dir, 2025L)))
  expect_true(is.na(schedule_download_time(dir, NA_integer_)))
})

test_that("a season that isn't refreshed keeps its old download time", {
  dir <- withr::local_tempdir()
  fake <- function(s) tibble::tibble(season = s, x = 1)
  load_cached(2026L, "schedules", dir, refresh = 2026L, fetch = fake)
  old <- as.POSIXct("2026-10-01 09:00:00", tz = "UTC")
  Sys.setFileTime(file.path(dir, "schedules_2026.rds"), old)
  load_cached(2026L, "schedules", dir, refresh = integer(), fetch = fake)
  expect_equal(as.numeric(schedule_download_time(dir, 2026L)), as.numeric(old))
  load_cached(2026L, "schedules", dir, refresh = 2026L, fetch = fake)
  expect_gt(as.numeric(schedule_download_time(dir, 2026L)), as.numeric(old))
})

test_that("clean_plays keeps only pass/run plays with EPA", {
  p <- tibble::tibble(season = 2020L, week = 1L, game_id = "g", game_date = "2020-09-13",
                      posteam = "KC", defteam = "HOU", home_team = "KC",
                      play_type = c("pass", "run", "qb_kneel", "no_play", "pass"),
                      pass = c(1, 0, 0, 1, 1), rush = c(0, 1, 1, 0, 0),
                      epa = c(0.5, -0.2, -0.1, 0.3, NA), wp = 0.5,
                      qb_dropback = c(1, 0, 0, 1, 1), id = c("qb1", "rb1", "qb1", "qb1", "qb1"))
  out <- clean_plays(p)
  expect_equal(nrow(out), 2)
  expect_s3_class(out$game_date, "Date")
})

test_that("clean_plays credits dropbacks, including scrambles, to the QB's id", {
  p <- tibble::tibble(season = 2020L, week = 1L, game_id = "g", game_date = "2020-09-13",
                      posteam = "KC", defteam = "HOU", home_team = "KC",
                      play_type = c("pass", "run", "run"),   # pass, scramble, designed run
                      pass = c(1, 1, 0), rush = c(0, 0, 1), epa = c(0.5, 0.8, -0.2), wp = 0.5,
                      qb_dropback = c(1, 1, 0), id = c("qb1", "qb1", "rb1"))
  out <- clean_plays(p)
  expect_equal(out$qb_dropback, c(1, 1, 0))
  expect_equal(out$qb_id, c("qb1", "qb1", NA))
  expect_equal(out$play_type, c("pass", "run", "run"))   # scrambles stay "run" for team ratings
})

test_that("code_state reports HEAD's SHA and whether the tree has uncommitted changes", {
  dir <- withr::local_tempdir()
  git <- function(...) system2("git", c("-C", dir, ...), stdout = TRUE, stderr = TRUE)
  git("init", "-q")
  git("config", "user.email", "t@example.com")
  git("config", "user.name", "t")
  writeLines("a", file.path(dir, "a.R"))
  git("add", "a.R")
  git("commit", "-q", "-m", "init")
  st <- code_state(dir)
  expect_equal(st$sha, git("rev-parse", "HEAD"))
  expect_false(st$dirty)
  writeLines("b", file.path(dir, "a.R"))
  expect_true(code_state(dir)$dirty)
})

test_that("code_state is NA outside a git repo", {
  st <- code_state(withr::local_tempdir())
  expect_true(is.na(st$sha))
  expect_true(is.na(st$dirty))
})

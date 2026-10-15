db_of <- function(plays) add_career_dropbacks(qb_dropbacks(plays))
cutoff_of <- function(sched, season, week) {
  min(as.Date(sched$gameday[sched$season == season & sched$week == week]))
}

test_that("experience bins use career dropbacks before the game", {
  expect_equal(as.character(experience_bin(c(0, 99, 100, 499, 500, 1499, 1500))),
               c("0-99", "0-99", "100-499", "100-499", "500-1499", "500-1499", "1500+"))
})

test_that("dropbacks with no player id are excluded and counted", {
  p <- qb_fixture()$plays[1:70, ]
  p$qb_id[1:3] <- NA
  db <- qb_dropbacks(p)
  expect_equal(nrow(db), 47)
  expect_equal(attr(db, "excluded"), 3)
})

test_that("career dropbacks count only earlier games", {
  db <- db_of(qb_fixture()$plays)
  good <- db[db$qb_id == "qb_good", ]
  expect_equal(unique(good$career_before[good$season == 2013L & good$week == 1L]), 0)
  expect_equal(unique(good$career_before[good$season == 2013L & good$week == 3L]), 100)
  expect_equal(career_at(db, c("qb_good", "nobody"), as.Date("2013-09-24")), c(100L, 0L))
})

test_that("QB ratings recover the known order", {
  fx <- qb_fixture()
  r <- fit_qb_ratings_at(db_of(fx$plays), 2019L, cutoff_of(fx$sched, 2019L, 16L), QB_TEST_PARAMS)
  starters <- r[r$qb_id %in% names(QB_TRUE), ]
  expect_equal(starters$qb_id[order(-starters$qb_rating)], c("qb_star", "qb_good", "qb_avg", "qb_bad"))
})

test_that("QB ratings for a week ignore plays from that week onward", {
  fx <- qb_fixture()
  cut <- cutoff_of(fx$sched, 2018L, 9L)
  full <- fit_qb_ratings_at(db_of(fx$plays), 2018L, cut, QB_TEST_PARAMS, starters = "qb_new")
  trimmed <- fit_qb_ratings_at(db_of(fx$plays[fx$plays$game_date < cut, ]), 2018L, cut,
                               QB_TEST_PARAMS, starters = "qb_new")
  expect_equal(full, trimmed)
})

test_that("an unseen starter gets the 0-99 bin's rating and zero career dropbacks", {
  fx <- qb_fixture()
  r <- fit_qb_ratings_at(db_of(fx$plays), 2019L, cutoff_of(fx$sched, 2019L, 10L), QB_TEST_PARAMS,
                         starters = c("qb_good", "qb_new", NA))
  expect_false(anyNA(r$qb_id))
  expect_equal(r$qb_rating[r$qb_id == "qb_new"], unname(attr(r, "bin_ratings")["0-99"]))
  expect_equal(r$career_dropbacks[r$qb_id == "qb_new"], 0L)
  expect_lt(attr(r, "bin_ratings")[["0-99"]], attr(r, "bin_ratings")[["1500+"]])
})

test_that("an experience bin with no dropbacks in the window borrows the bin below it", {
  fx <- qb_fixture()
  # Before 2014 week 15 no play is yet in the 1500+ bin, but starters reach 1500 at the cutoff.
  r <- fit_qb_ratings_at(db_of(fx$plays), 2014L, cutoff_of(fx$sched, 2014L, 15L), QB_TEST_PARAMS)
  expect_true(all(is.finite(r$qb_rating)))
  expect_equal(r$career_dropbacks[r$qb_id == "qb_good"], 1500L)
  b <- attr(r, "bin_ratings")
  expect_equal(b[["1500+"]], b[["500-1499"]])
})

test_that("compute_qb_ratings covers each week and its scheduled starters, skipping missing IDs", {
  fx <- qb_fixture()
  s <- fx$sched[fx$sched$season == 2019L & fx$sched$week %in% 9:10, ]
  s$home_qb_id[s$week == 10L][1] <- NA
  s$away_qb_id[s$week == 10L][1] <- "qb_new"
  r <- compute_qb_ratings(fx$plays, s, QB_TEST_PARAMS, 2019L)
  expect_named(r, c("season", "week", "qb_id", "qb_rating", "career_dropbacks"))
  expect_equal(sort(unique(r$week)), 9:10)
  expect_false(anyNA(r$qb_id))
  expect_true("qb_new" %in% r$qb_id[r$week == 10L])
  expect_false("qb_new" %in% r$qb_id[r$week == 9L])
})

test_that("starter ID coverage flags schedules whose IDs don't match play IDs", {
  fx <- qb_fixture()
  expect_equal(starter_id_coverage(fx$sched, fx$plays), 1)
  bad <- dplyr::mutate(fx$sched, home_qb_id = paste0("x", home_qb_id), away_qb_id = paste0("x", away_qb_id))
  expect_equal(starter_id_coverage(bad, fx$plays), 0)
})

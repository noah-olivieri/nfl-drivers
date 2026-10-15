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

week_of <- function(sched, season, week) sched[sched$season == season & sched$week == week, ]
side_adj <- function(g, team) {
  c(g$home_qb_adj[g$home_team == team], g$away_qb_adj[g$away_team == team])
}
ridge_qb <- function(plays, s, season) {
  r <- compute_qb_ratings(plays, s, QB_TEST_PARAMS, season)
  list(ratings = r, mix = team_qb_mix(plays, r, s, TEAM_TEST_PARAMS, season))
}

test_that("the team mix is the weighted rating over the team's pass dropbacks only", {
  fx <- qb_fixture()
  p <- fx$plays
  p$play_type[p$qb_id %in% "qb_avg" & p$season == 2019L] <- "run"   # make them all scrambles
  p$wp[p$qb_id %in% "rookie_2018_8"] <- 0.02                         # one QB's plays in garbage time
  team_params <- modifyList(TEAM_TEST_PARAMS, list(offseason_weeks = 8))
  s <- week_of(fx$sched, 2019L, 13L)
  r_all <- compute_qb_ratings(p, s, QB_TEST_PARAMS, 2019L)
  mix <- team_qb_mix(p, r_all, s, team_params, 2019L)
  cut <- min(as.Date(s$gameday))
  m <- p[p$posteam == "C" & p$qb_dropback == 1 & p$play_type == "pass" &
           p$season %in% 2018:2019 & p$game_date < cut, ]
  w <- play_weights(m, 2019L, cut, team_params)
  r <- r_all$qb_rating[match(m$qb_id, r_all$qb_id)]
  expect_false(any(m$qb_id == "qb_avg" & m$season == 2019L))
  expect_equal(mix$qb_mix[mix$team == "C"], sum(w * r) / sum(w))
})

test_that("QB play weights decay by age and count garbage time at the garbage weight", {
  db <- tibble::tibble(game_date = as.Date(c("2020-10-01", "2020-10-01", "2020-09-03")),
                       wp = c(0.5, 0.02, 0.5))
  w <- qb_play_weights(db, as.Date("2020-10-08"), QB_TEST_PARAMS)
  expect_equal(w[2] / w[1], QB_TEST_PARAMS$garbage_weight)
  expect_equal(w[3] / w[1], 0.5^(4 / QB_TEST_PARAMS$qb_half_life))   # 4 weeks older
})

test_that("a team whose only QB starts gets qb_adj of exactly 0 (ridge)", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2019L, 10L)
  g <- add_qb_adjustments(s, ridge_qb(fx$plays, s, 2019L))
  expect_lt(abs(side_adj(g, "B")), 1e-12)
})

test_that("an unseen backup starting gets a negative qb_adj (ridge)", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2019L, 10L)
  s$home_qb_id[s$home_team == "B"] <- "qb_new"
  s$away_qb_id[s$away_team == "B"] <- "qb_new"
  qb <- ridge_qb(fx$plays, s, 2019L)
  g <- add_qb_adjustments(s, qb)
  new_rating <- qb$ratings$qb_rating[qb$ratings$qb_id == "qb_new"]
  expect_lt(side_adj(g, "B"), 0)
  expect_equal(side_adj(g, "B"), new_rating - qb$mix$qb_mix[qb$mix$team == "B"])
})

test_that("a QB who changes teams keeps his own rating (ridge)", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2018L, 1L)               # star now starts for D, bad for A
  qb <- ridge_qb(fx$plays, s, 2018L)
  g <- add_qb_adjustments(s, qb)
  expect_gt(side_adj(g, "D"), 0)
  expect_lt(side_adj(g, "A"), 0)
  relabeled <- dplyr::mutate(s, home_team = chartr("CD", "DC", home_team),
                             away_team = chartr("CD", "DC", away_team))
  expect_equal(compute_qb_ratings(fx$plays, relabeled, QB_TEST_PARAMS, 2018L), qb$ratings)
})

test_that("the team mix for a week ignores plays from that week onward", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2018L, 9L)
  cut <- min(as.Date(s$gameday))
  expect_equal(ridge_qb(fx$plays, s, 2018L), ridge_qb(fx$plays[fx$plays$game_date < cut, ], s, 2018L))
})

test_that("the week to forecast gets ratings and a mix for every team; a missing starter gets 0", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2019L, 16L)
  nxt <- dplyr::mutate(s, week = 17L, gameday = as.character(as.Date(gameday) + 7),
                       home_score = NA, away_score = NA)
  nxt$home_qb_id[1] <- NA
  qb <- ridge_qb(fx$plays, dplyr::bind_rows(s, nxt), 2019L)
  expect_false(anyNA(qb$ratings$qb_id))
  expect_setequal(qb$mix$team[qb$mix$week == 17L], c("A", "B", "C", "D"))
  g <- add_qb_adjustments(nxt, qb)
  expect_equal(g$home_qb_adj[1], 0)
  expect_false(anyNA(g$qb_adj_diff))
})

test_that("no QB input gives NA adjustment columns", {
  s <- week_of(qb_fixture()$sched, 2019L, 10L)
  g <- add_qb_adjustments(s, NULL)
  expect_true(all(is.na(c(g$home_qb_adj, g$away_qb_adj, g$qb_adj_diff))))
  expect_equal(nrow(g), nrow(s))
})

rolling_qb <- function(plays, s, season) {
  r <- compute_rolling_qb_ratings(plays, s, season)
  list(ratings = r, mix = team_qb_mix(plays, r, s, TEAM_TEST_PARAMS, season))
}

test_that("rolling QB rating is the mean of his last 300 dropbacks", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2019L, 10L)
  r <- compute_rolling_qb_ratings(fx$plays, s, 2019L)
  cut <- min(as.Date(s$gameday))
  g <- fx$plays[fx$plays$qb_id %in% "qb_good" & fx$plays$game_date < cut, ]
  g <- g[order(g$game_date), ]
  expect_equal(r$qb_rating[r$qb_id == "qb_good"], mean(utils::tail(g$epa, 300)))
  expect_named(r, c("season", "week", "qb_id", "qb_rating", "career_dropbacks"))
})

test_that("short histories are padded with the low-experience mean; unseen QBs get exactly it", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2019L, 10L)
  s$home_qb_id[s$home_team == "B"] <- "qb_new"
  s$away_qb_id[s$away_team == "B"] <- "qb_new"
  r <- compute_rolling_qb_ratings(fx$plays, s, 2019L)
  cut <- min(as.Date(s$gameday))
  db <- add_career_dropbacks(qb_dropbacks(fx$plays))
  low <- db$epa[db$game_date < cut & db$season >= 2016L & db$career_before < 300]
  expect_equal(r$qb_rating[r$qb_id == "qb_new"], mean(low))
  rk <- fx$plays$epa[fx$plays$qb_id %in% "rookie_2019_8"]   # 50 dropbacks
  expect_equal(r$qb_rating[r$qb_id == "rookie_2019_8"], (sum(rk) + 250 * mean(low)) / 300)
})

test_that("rolling QB ratings ignore plays from that week onward", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2018L, 9L)
  cut <- min(as.Date(s$gameday))
  expect_equal(compute_rolling_qb_ratings(fx$plays, s, 2018L),
               compute_rolling_qb_ratings(fx$plays[fx$plays$game_date < cut, ], s, 2018L))
})

test_that("rolling: only QB gives 0, unseen backup is negative, a moved QB keeps his rating", {
  fx <- qb_fixture()
  s10 <- week_of(fx$sched, 2019L, 10L)
  expect_lt(abs(side_adj(add_qb_adjustments(s10, rolling_qb(fx$plays, s10, 2019L)), "B")), 1e-12)

  s_new <- s10
  s_new$home_qb_id[s_new$home_team == "B"] <- "qb_new"
  s_new$away_qb_id[s_new$away_team == "B"] <- "qb_new"
  expect_lt(side_adj(add_qb_adjustments(s_new, rolling_qb(fx$plays, s_new, 2019L)), "B"), 0)

  s1 <- week_of(fx$sched, 2018L, 1L)
  qb <- rolling_qb(fx$plays, s1, 2018L)
  g <- add_qb_adjustments(s1, qb)
  expect_gt(side_adj(g, "D"), 0)
  expect_lt(side_adj(g, "A"), 0)
  relabeled <- dplyr::mutate(s1, home_team = chartr("CD", "DC", home_team),
                             away_team = chartr("CD", "DC", away_team))
  expect_equal(compute_rolling_qb_ratings(fx$plays, relabeled, 2018L), qb$ratings)
})

test_that("the rolling-method team mix ignores plays from that week onward", {
  fx <- qb_fixture()
  s <- week_of(fx$sched, 2018L, 9L)
  cut <- min(as.Date(s$gameday))
  expect_equal(rolling_qb(fx$plays, s, 2018L), rolling_qb(fx$plays[fx$plays$game_date < cut, ], s, 2018L))
})

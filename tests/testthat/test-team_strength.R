test_that("ratings recover the known offensive order", {
  r <- fit_ratings_at(make_synthetic(), 2020L, as.Date("2021-06-01"),
                      list(half_life = 1000, offseason_weeks = 0, lambda = 1e-4, garbage_weight = 0.25))
  expect_equal(r$team[order(-r$pass_off)], c("A", "B", "C", "D"))
  expect_equal(r$team[order(-r$rush_off)], c("A", "B", "C", "D"))
})

test_that("ratings for a week ignore plays from that week onward", {
  p <- make_synthetic()
  params <- list(half_life = 5, offseason_weeks = 8, lambda = 1e-3, garbage_weight = 0.25)
  cut <- as.Date("2020-11-01")
  full <- fit_ratings_at(p, 2020L, cut, params)
  trimmed <- fit_ratings_at(dplyr::filter(p, game_date < cut), 2020L, cut, params)
  expect_equal(full, trimmed)
})

test_that("a relocated team keeps one continuous identity", {
  p <- make_synthetic() |>
    dplyr::mutate(posteam = dplyr::recode(posteam, A = "LV"),
                  defteam = dplyr::recode(defteam, A = "LV"),
                  home_team = dplyr::recode(home_team, A = "LV"))
  p$posteam[1:200] <- ifelse(p$posteam[1:200] == "LV", "OAK", p$posteam[1:200])
  p <- clean_plays_teams(p)
  r <- fit_ratings_at(p, 2020L, as.Date("2021-06-01"),
                      list(half_life = 1000, offseason_weeks = 0, lambda = 1e-4, garbage_weight = 0.25))
  expect_false("OAK" %in% r$team)
})

test_that("closed-form ridge matches glmnet's solution", {
  set.seed(9)
  n <- 3000
  X <- Matrix::rsparsematrix(n, 20, density = 0.15, rand.x = function(k) rep(1, k))
  y <- as.numeric(X %*% rnorm(20, 0, 0.3)) + rnorm(n)
  w <- runif(n)
  pf <- c(rep(1, 18), 0, 0)
  ours <- ridge_solve(X, y, w, lambda = 0.01, penalty_factor = pf)
  ref <- glmnet::glmnet(X, y, weights = w, alpha = 0, lambda = 0.01, standardize = FALSE,
                        penalty.factor = pf, thresh = 1e-14)
  expect_equal(unname(ours), unname(as.matrix(coef(ref))[, 1]), tolerance = 1e-4)
})

test_that("week cutoffs are the earliest game date of each season-week", {
  s <- tibble::tibble(season = 2020L, week = c(1L, 1L, 2L),
                      gameday = c("2020-09-10", "2020-09-13", "2020-09-17"))
  cw <- week_cutoffs(s)
  expect_equal(cw$cutoff_date, as.Date(c("2020-09-10", "2020-09-17")))
})

test_that("garbage-time plays count less", {
  clean <- make_synthetic()
  junk <- clean$posteam == "D" & seq_len(nrow(clean)) %% 2 == 0   # half of D's plays
  clean$wp[junk] <- 0.01
  p <- clean
  p$epa[junk] <- p$epa[junk] + 2                                   # inflated in garbage time
  base <- list(half_life = 1000, offseason_weeks = 0, lambda = 1e-4, garbage_weight = 0.25)
  d_of <- function(plays, gw) {
    r <- fit_ratings_at(plays, 2020L, as.Date("2021-06-01"), modifyList(base, list(garbage_weight = gw)))
    r$pass_off[r$team == "D"]
  }
  shift_full <- d_of(p, 1) - d_of(clean, 1)
  shift_down <- d_of(p, 0.25) - d_of(clean, 0.25)
  expect_gt(shift_full, 0)
  expect_lt(shift_down, 0.6 * shift_full)   # expected ratio ~0.4
})

test_that("team ratings are identical with or without older seasons' plays", {
  fx <- qb_fixture()
  recent <- fx$plays[fx$plays$season >= 2016L, ]
  expect_equal(compute_ridge_ratings(fx$plays, fx$sched, TEAM_TEST_PARAMS, 2017L),
               compute_ridge_ratings(recent, fx$sched, TEAM_TEST_PARAMS, 2017L))
  expect_equal(compute_rolling_ratings(fx$plays, fx$sched, 2017L),
               compute_rolling_ratings(recent, fx$sched, 2017L))
})

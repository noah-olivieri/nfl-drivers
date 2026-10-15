feats <- function() build_features(toy_sched(), toy_ratings(), toy_stadiums())
row_of <- function(f, id) f[f$game_id == id, ]

test_that("one row per completed game with correct outcomes", {
  f <- feats()
  expect_equal(nrow(f), dplyr::n_distinct(toy_sched()$game_id))
  expect_equal(row_of(f, "g1")$margin, 14)
  expect_equal(row_of(f, "g1")$win, 1)
  tie <- row_of(f, "tie_game")
  expect_equal(tie$margin, 0)
  expect_equal(tie$win, 0.5)
})

test_that("columns match the spec's feature table", {
  expect_named(feats(), c("game_id", "season", "week", "gameday", "margin", "win",
                          FEATURE_GROUPS$strength, FEATURE_GROUPS$situational,
                          FEATURE_GROUPS$qb, FEATURE_GROUPS$environment, "spread",
                          "home_qb_adj", "away_qb_adj", "qb_change_game"),
               ignore.order = TRUE)
})

test_that("rating differences are home minus away", {
  g1 <- row_of(feats(), "g1")   # KC home vs HOU
  expect_equal(g1$pass_off_diff, 0.2)
  expect_equal(g1$rush_def_diff, 0.1)
})

test_that("neutral-site games: home = 0 and travel goes to the real venue", {
  lon <- row_of(feats(), "london_game")
  expect_equal(lon$home, 0)
  expect_gt(lon$away_travel_miles, 3000)
  expect_equal(lon$tz_crossed, 6)   # Kansas City (CDT, -5) to London (BST, +1)
})

test_that("travel, time zones and rest flags", {
  f <- feats()
  g1 <- row_of(f, "g1")         # HOU travels to KC
  expect_equal(g1$away_travel_miles, haversine_miles(29.68, -95.41, 39.05, -94.48))
  expect_equal(g1$tz_crossed, 0)
  jh <- row_of(f, "jax_home")   # HOU to JAX, one zone, on short rest
  expect_equal(jh$tz_crossed, 1)
  expect_equal(jh$away_short_week, 1)
  tie <- row_of(f, "tie_game")
  expect_equal(tie$home_off_bye, 1)
  expect_equal(tie$rest_diff, 7)
})

test_that("primetime flags Thursday and night kickoffs only", {
  f <- feats()
  expect_equal(row_of(f, "g1")$primetime, 1)
  expect_equal(row_of(f, "g2")$primetime, 0)
})

test_that("playoff weeks get features", {
  f <- feats()
  expect_true("playoff_game" %in% f$game_id)
  expect_false(anyNA(row_of(f, "playoff_game")[FEATURE_GROUPS$strength]))
})

test_that("dome games get neutral weather and surfaces are classified", {
  f <- feats()
  tie <- row_of(f, "tie_game")
  expect_equal(c(tie$dome, tie$temp, tie$wind, tie$turf), c(1, 70, 0, 1))
  expect_equal(row_of(f, "jax_home")$turf, 0)   # "grass " with a trailing space
})

test_that("missing outdoor weather is filled with the stadium's season median", {
  g2 <- row_of(feats(), "g2")   # KAN00 outdoor temps that season: 80, 70, 60, 30
  expect_equal(g2$temp, 65)
  expect_equal(g2$wind, 9)
})

test_that("weather falls back to the stadium's all-season median, then the league's", {
  s <- toy_sched()
  s$roof[s$game_id == "jax_home"] <- "open"   # retractable roof open, no weather recorded
  s$temp[s$game_id == "jax_home"] <- NA
  s$wind[s$game_id == "jax_home"] <- NA
  f <- build_features(s, toy_ratings(), toy_stadiums())
  jh <- row_of(f, "jax_home")
  expect_false(is.na(jh$temp))
  expect_equal(attr(f, "dropped"), 0)
})

test_that("time zones crossed wrap around the globe", {
  s <- toy_sched()
  s$stadium_id[s$game_id == "london_game"] <- "MEL00"
  st <- dplyr::bind_rows(toy_stadiums(),
                         tibble::tibble(stadium_id = "MEL00", lat = -37.82, lon = 144.98,
                                        tz = "Australia/Melbourne"))
  f <- build_features(s, toy_ratings(), st)
  # Oct 18: Kansas City CDT (-5), Melbourne AEDT (+11) -> 16 apart -> 8 the short way
  expect_equal(row_of(f, "london_game")$tz_crossed, 8)
})

test_that("missing QB IDs and first games give 0 flags, not NA", {
  f <- feats()
  expect_false(anyNA(f[setdiff(FEATURE_GROUPS$qb, "qb_adj_diff")]))
  expect_equal(row_of(f, "g1")$home_qb_changed, 0)
  expect_equal(row_of(f, "tie_game")$home_qb_backup, 0)
})

test_that("QB change and backup flags", {
  f <- feats()
  expect_equal(row_of(f, "backup_game")$home_qb_changed, 1)
  expect_equal(row_of(f, "backup_game")$home_qb_backup, 1)
  expect_equal(row_of(f, "tie_game")$away_qb_changed, 1)   # back from Henne to Mahomes
  expect_equal(row_of(f, "tie_game")$away_qb_backup, 0)
})

test_that("games without ratings for their season are left out, not counted as dropped", {
  s <- dplyr::bind_rows(toy_sched(), dplyr::mutate(toy_sched()[1, ], game_id = "old", season = 2019L))
  f <- build_features(s, toy_ratings(), toy_stadiums())
  expect_false("old" %in% f$game_id)
  expect_equal(attr(f, "dropped"), 0)
})

test_that("week 1 of a new season uses last season's home stadium for travel", {
  wk1 <- toy_sched()[1, ] |>
    dplyr::mutate(game_id = "2021_wk1", season = 2021L, week = 1L, gameday = "2021-09-12",
                  away_team = "JAX", away_qb_id = "minshew")   # JAX hasn't hosted in 2021 yet
  s <- dplyr::bind_rows(toy_sched(), wk1)
  r <- dplyr::bind_rows(toy_ratings(), dplyr::mutate(toy_ratings()[toy_ratings()$week == 1, ], season = 2021L))
  f <- build_features(s, r, toy_stadiums())
  expect_equal(attr(f, "dropped"), 0)
  expect_equal(row_of(f, "2021_wk1")$away_travel_miles,
               haversine_miles(30.32, -81.64, 39.05, -94.48))   # JAX00 -> KAN00
})

test_that("games missing a required feature are dropped and counted", {
  s <- toy_sched()
  s$home_rest[s$game_id == "g3"] <- NA
  f <- build_features(s, toy_ratings(), toy_stadiums())
  expect_false("g3" %in% f$game_id)
  expect_equal(attr(f, "dropped"), 1)
})

feats_qb <- function(qb = toy_qb()) build_features(toy_sched(), toy_ratings(), toy_stadiums(), qb = qb)
qb_cols <- c("home_qb_adj", "away_qb_adj", "qb_adj_diff")

test_that("QB adjustment is starter rating minus team mix, home minus away", {
  bg <- row_of(feats_qb(), "backup_game")   # Henne (-0.2) for KC (mix 0.3) vs Minshew for JAX
  expect_equal(c(bg$home_qb_adj, bg$away_qb_adj, bg$qb_adj_diff), c(-0.5, 0, -0.5))
  expect_equal(row_of(feats_qb(), "g1")$qb_adj_diff, 0)
})

test_that("a missing starter ID gives adjustment 0 and is counted", {
  f <- feats_qb()
  tie <- row_of(f, "tie_game")             # home QB ID is NA
  expect_equal(tie$home_qb_adj, 0)
  expect_equal(attr(f, "missing_starters"), 1)
})

test_that("QB-change games are those with any change or backup flag", {
  f <- feats_qb()
  expect_equal(row_of(f, "backup_game")$qb_change_game, 1)
  expect_equal(row_of(f, "tie_game")$qb_change_game, 1)   # Mahomes back from Henne
  expect_equal(row_of(f, "g1")$qb_change_game, 0)
  expect_equal(f$qb_change_game,
               as.numeric(f$home_qb_changed == 1 | f$away_qb_changed == 1 |
                            f$home_qb_backup == 1 | f$away_qb_backup == 1))
})

test_that("without QB input, rows and existing columns are unchanged", {
  f0 <- feats()
  f1 <- feats_qb()
  expect_equal(f0$game_id, f1$game_id)
  expect_true(all(is.na(unlist(f0[qb_cols]))))
  for (v in setdiff(names(f0), qb_cols)) expect_equal(f0[[v]], f1[[v]], info = v)
})

test_that("with QB input, a game missing its team mix is dropped and counted", {
  qb <- toy_qb()
  qb$mix <- qb$mix[!(qb$mix$team == "JAX" & qb$mix$week == 6L), ]
  f <- feats_qb(qb)
  expect_false("london_game" %in% f$game_id)
  expect_equal(attr(f, "dropped"), 1)
})

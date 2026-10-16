# Upcoming-Week Predictions Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Every pipeline run also gives each model run's win probability for the forecast week's unplayed games, shown in a "Next week" report table, without changing any scored result.

**Architecture:** `rating_schedule()` gets a corrected forecast-week rule, so a week whose Thursday game is done stays the forecast week. `build_features()` keeps the forecast week's unplayed games with `margin`/`win` = `NA`. Training (`fit_models()`, `spread_benchmark()`) and scoring (`score_predictions()`, `game_losses()`, `calibration_table()`) ignore rows without an outcome. A new `upcoming_predictions()` collects the unplayed rows from walk-forward output. `R/pipeline.R` splits predictions and features into upcoming and completed right after walk-forward, so everything downstream sees exactly what it sees today. `report.qmd` gets a "9. Next week" section.

**Tech Stack:** R, dplyr, tidyr, purrr, tibble, testthat, Quarto. No new packages.

**Spec:** `docs/superpowers/specs/2026-10-15-upcoming-week-design.md`

## Global Constraints

- Forecast week: the earliest week with an unplayed game whose key `season * 100 + week` is **≥** the latest key with a completed game. An unplayed game is one with `home_score` or `away_score` `NA`.
- Every completed game's features, and every score, comparison, ablation, coefficient and the QB verdict, must be identical to a run without unplayed games (verified in Task 5).
- Training and scoring use only rows with non-`NA` outcomes.
- Each run's upcoming predictions come from its walk-forward run for the forecast season (trained on 2010 through the previous season). No separate fit.
- `attr(, "dropped")` counts completed games only; `attr(, "dropped_upcoming")` counts unplayed forecast-week games missing a required feature.
- Headline model: `full_qb_ridge`. The note under the table names `market`, `market_qb` and `spread_only` as using the current line, not the closing line.
- The table shows each team's projected starter ("not listed" when missing) and the schedule download time.
- Run names: `base`, `full_ridge`, `full_rolling`, `full_qb_ridge`, `full_qb_rolling`, `market`, `market_qb`, `spread_only`.
- `.R` files define functions only; nothing runs on `source()`. Match the existing style: `pkg::fn()` calls, native pipe `|>`, short comments.
- Report text written by Claude describes method and what the table shows, nothing more.
- Tests: all with `Rscript -e 'testthat::test_dir("tests/testthat")'`; one file with `Rscript -e 'testthat::test_dir("tests/testthat", filter = "<name>")'` (e.g. `filter = "load_data"`). The project uses testthat edition 2 (no `ignore_attr`).
- Commit messages end with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  ```

## Review Focus

1. **Thursday played, Sunday and Monday not (mid-week):** the Sunday and Monday games stay in the forecast and week 6's cutoff is still Thursday's date. Pinned in Task 1 (`midweek_sched()` tests).
2. **A projected starter is missing for an upcoming game:** the game is kept with that side's `qb_adj = 0`, and the table says "not listed". Pinned in Task 2 ("no listed starter" test). The table handles it in Task 6 with `coalesce()`.
3. **No line yet for an upcoming game** (2026 week 7's Monday game has `spread_line = NA` today): the game is kept, `spread` runs predict `NA`, and the table shows "–" rather than `NA%`. Pinned in Task 2 (`spread_line = NA` in the "kept" test) and Task 4 (NA prediction kept). The table handles it in Task 6 with `pct()`.
4. **Stale schedule** (the current season left out of `refresh`): the download time shown is the old one. Pinned in Task 1 ("isn't refreshed keeps its old download time").
5. **No forecast week** (season over): `upcoming` is empty, the download time is `NA`, and the report says there is nothing to predict instead of erroring. Pinned in Task 1 ("every game played"), Task 4 (empty result) and Task 6 (empty-results render check).

---

### Task 1: Forecast week and schedule download time

**Files:**
- Modify: `R/load_data.R` (`rating_schedule()`, new `schedule_download_time()`)
- Test: `tests/testthat/test-load_data.R`

**Interfaces:**
- Consumes: `week_cutoffs(sched)` from `R/team_strength.R` (tests only), `load_cached()`.
- Produces:
  - `rating_schedule(sched_all) -> sched rows`: completed games plus the forecast week's unplayed games (same signature, new rule).
  - `schedule_download_time(cache_dir, season) -> POSIXct` (length 1): modification time of `<cache_dir>/schedules_<season>.rds`, `NA` if `season` is `NA` or the file is missing.

- [ ] **Step 1: Write the failing tests**

In `tests/testthat/test-load_data.R`, replace the test `"rating schedule keeps a no-contest game's week via its other games"` with the version below, and add the new tests after `"rating schedule is completed games plus the next unplayed week only"`:

```r
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
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "load_data")'`
Expected: FAIL. "mid-week, the rest of the week…" returns `w5, w6_thu, w7`, the cutoff test finds week 7, and both download-time tests fail with `could not find function "schedule_download_time"`. The other new tests pass (they pin behavior the change must keep).

- [ ] **Step 3: Implement**

In `R/load_data.R`, replace `rating_schedule()` and its comment with:

```r
# Schedule rows that ratings are computed for: every completed game, plus the
# unplayed games of the forecast week, the earliest week with an unplayed game
# counting from the latest week with a completed game. A week whose Thursday game
# is done stays the forecast week until its last game; an unplayed game in an
# earlier week is a no-contest and is dropped.
rating_schedule <- function(sched_all) {
  done <- !is.na(sched_all$home_score) & !is.na(sched_all$away_score)
  key <- sched_all$season * 100 + sched_all$week
  last_done <- if (any(done)) max(key[done]) else -Inf
  upcoming <- key[!done & key >= last_done]
  next_key <- if (length(upcoming)) min(upcoming) else NA
  sched_all[done | (!is.na(next_key) & key == next_key), ]
}

# When a season's schedule was last downloaded: its cache file's modification time
# (load_cached() writes the file right after each download). NA if there is none.
schedule_download_time <- function(cache_dir, season) {
  if (length(season) != 1 || is.na(season)) return(as.POSIXct(NA))
  path <- file.path(cache_dir, sprintf("schedules_%d.rds", season))
  if (!file.exists(path)) return(as.POSIXct(NA))
  file.mtime(path)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "load_data")'`
Expected: PASS, all tests.

- [ ] **Step 5: Commit**

```bash
git add R/load_data.R tests/testthat/test-load_data.R
git commit -m "Keep a partly played week as the forecast week; record schedule download time

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: `build_features()` keeps the forecast week's unplayed games

**Files:**
- Modify: `R/features.R` (header comment, `build_features()`)
- Test: `tests/testthat/test-features.R`

**Interfaces:**
- Consumes: `toy_sched()`, `toy_ratings()`, `toy_stadiums()`, `toy_qb()` from `tests/testthat/helper-toy.R`; `row_of()` and `feats()` at the top of `test-features.R`.
- Produces: `build_features(sched, ratings, stadiums, qb = NULL)`, with the same signature and columns. Unplayed games whose (season, week) is in `ratings` are kept with `margin`/`win` `NA`. Attributes: `dropped` (completed only), new `dropped_upcoming`, and `missing_starters` (completed only).

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-features.R`:

```r
# An unplayed game the week after the last toy game (KC hosts HOU in week 21), and
# ratings that cover its week.
upcoming_game <- function() {
  toy_sched() |>
    dplyr::filter(game_id == "playoff_game") |>
    dplyr::mutate(game_id = "upcoming", week = 21L, game_type = "CON", gameday = "2021-01-24",
                  gametime = "18:40", home_score = NA_real_, away_score = NA_real_,
                  temp = NA_real_, wind = NA_real_)
}
ratings_through_21 <- function() {
  r <- toy_ratings()
  dplyr::bind_rows(r, dplyr::mutate(r[r$week == 20L, ], week = 21L))
}
qb_through_21 <- function() {
  qb <- toy_qb()
  week21 <- function(df) dplyr::bind_rows(df, dplyr::mutate(df[df$week == 20L, ], week = 21L))
  list(ratings = week21(qb$ratings), mix = week21(qb$mix))
}

test_that("an unplayed game in a rated week is kept with NA outcomes", {
  s <- dplyr::bind_rows(toy_sched(), dplyr::mutate(upcoming_game(), spread_line = NA_real_))
  f <- build_features(s, ratings_through_21(), toy_stadiums())
  up <- row_of(f, "upcoming")
  expect_equal(nrow(up), 1)
  expect_true(is.na(up$margin))
  expect_true(is.na(up$win))    # not 0.5
  expect_true(is.na(up$spread)) # no line yet is not a reason to drop it
  expect_false(anyNA(up[setdiff(unlist(FEATURE_GROUPS), "qb_adj_diff")]))
  expect_equal(c(up$temp, up$wind), c(65, 9))   # KAN00 2020 medians from played games
  expect_equal(attr(f, "dropped"), 0)
  expect_equal(attr(f, "dropped_upcoming"), 0)
})

test_that("completed games' features don't change when unplayed games are added", {
  jax_london <- function(id) {
    dplyr::mutate(upcoming_game(), game_id = id, home_team = "JAX", away_team = "HOU",
                  stadium_id = "LON00", home_qb_id = "minshew", away_qb_id = "watson")
  }
  extra <- dplyr::bind_rows(
    dplyr::mutate(upcoming_game(), temp = 5, wind = 40),   # recorded weather must not feed the medians
    # Two unplayed JAX "home" games in London would make LON00 JAX's most-used home
    # stadium if unplayed games counted, moving the travel for JAX's road games.
    jax_london("jax_lon1"), jax_london("jax_lon2")
  )
  base <- build_features(toy_sched(), ratings_through_21(), toy_stadiums())
  with_up <- build_features(dplyr::bind_rows(toy_sched(), extra), ratings_through_21(), toy_stadiums())
  done <- with_up[!is.na(with_up$margin), ]
  expect_equal(nrow(with_up), nrow(base) + 3)
  expect_equal(done$game_id, base$game_id)
  for (v in names(base)) expect_equal(done[[v]], base[[v]], info = v)
})

test_that("an unplayed game in a week without ratings is left out, not counted", {
  s <- dplyr::bind_rows(toy_sched(), upcoming_game())
  f <- build_features(s, toy_ratings(), toy_stadiums())   # no week-21 ratings
  expect_false("upcoming" %in% f$game_id)
  expect_equal(attr(f, "dropped"), 0)
  expect_equal(attr(f, "dropped_upcoming"), 0)
})

test_that("an unplayed game missing a required feature is counted separately", {
  s <- dplyr::bind_rows(toy_sched(), dplyr::mutate(upcoming_game(), home_rest = NA_integer_))
  f <- build_features(s, ratings_through_21(), toy_stadiums())
  expect_false("upcoming" %in% f$game_id)
  expect_equal(attr(f, "dropped"), 0)
  expect_equal(attr(f, "dropped_upcoming"), 1)
})

test_that("missing starters are counted over completed games only", {
  s <- dplyr::bind_rows(toy_sched(), dplyr::mutate(upcoming_game(), home_qb_id = NA_character_))
  f <- build_features(s, ratings_through_21(), toy_stadiums())
  expect_equal(attr(f, "missing_starters"), 1)   # tie_game only
})

test_that("an upcoming game with no listed starter is kept with adjustment 0", {
  s <- dplyr::bind_rows(toy_sched(), dplyr::mutate(upcoming_game(), away_qb_id = NA_character_))
  f <- build_features(s, ratings_through_21(), toy_stadiums(), qb = qb_through_21())
  up <- row_of(f, "upcoming")
  expect_equal(nrow(up), 1)
  expect_equal(up$away_qb_adj, 0)
  expect_equal(attr(f, "dropped_upcoming"), 0)
})

test_that("an upcoming week-1 game uses last season's home stadium for travel", {
  wk1 <- toy_sched()[1, ] |>
    dplyr::mutate(game_id = "2021_wk1", season = 2021L, week = 1L, gameday = "2021-09-12",
                  away_team = "JAX", away_qb_id = "minshew",
                  home_score = NA_real_, away_score = NA_real_)
  s <- dplyr::bind_rows(toy_sched(), wk1)
  r <- dplyr::bind_rows(toy_ratings(), dplyr::mutate(toy_ratings()[toy_ratings()$week == 1, ], season = 2021L))
  f <- build_features(s, r, toy_stadiums())
  expect_equal(attr(f, "dropped_upcoming"), 0)
  expect_equal(row_of(f, "2021_wk1")$away_travel_miles,
               haversine_miles(30.32, -81.64, 39.05, -94.48))   # JAX00 -> KAN00
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "features")'`
Expected: FAIL in every new test. Today unplayed rows are dropped and counted in `dropped` (so `nrow(up)` is 0 and `dropped` is 1, or 3 in the "don't change" test), `dropped_upcoming` is `NULL`, and `missing_starters` counts the unplayed game's missing ID (2, not 1).

- [ ] **Step 3: Implement**

In `R/features.R`, replace the two header comment lines with:

```r
# Game features: one row per game, from the home team's view, using only information
# known before kickoff. Unplayed games of a rated week (the forecast week) are kept
# with margin and win NA.
```

Replace the whole of `build_features()` (from its comment through the closing brace) with:

```r
# qb: NULL, or list(ratings = <QB ratings>, mix = <team_qb_mix() output>) to add the
# starter adjustment (qb_adj_diff). Without it the QB columns are NA.
# Unplayed games (either score NA) are kept, with margin and win NA, when their
# season-week has ratings; other unplayed games are left out. Home venues, weather
# medians and the missing-starter count come from completed games only, so adding
# unplayed games never changes a completed game's row.
build_features <- function(sched, ratings, stadiums, qb = NULL) {
  played <- !is.na(sched$home_score) & !is.na(sched$away_score)
  rated <- paste(sched$season, sched$week) %in% paste(ratings$season, ratings$week)
  sched <- sched[played | rated, ]
  flags <- qb_flags(sched)   # looks back only, so later unplayed games can't move it
  homes <- team_home_venue(sched[!is.na(sched$home_score) & !is.na(sched$away_score), ])
  st <- dplyr::select(stadiums, stadium_id, lat, lon, tz)

  games <- sched |>
    dplyr::filter(season %in% unique(ratings$season)) |>
    dplyr::mutate(
      is_played = !is.na(home_score) & !is.na(away_score),
      gameday = as.Date(gameday),
      kickoff = as.POSIXct(paste(gameday, gametime), tz = "America/New_York"),
      roof = trimws(roof), surface = trimws(surface)
    ) |>
    dplyr::left_join(st, by = "stadium_id") |>
    # Away team's home stadium: from this season if it has hosted yet, otherwise
    # from the most recent season it did (needed for early-week live runs).
    dplyr::left_join(dplyr::rename(homes, home_season = season),
                     dplyr::join_by(away_team == team, closest(season >= home_season))) |>
    dplyr::left_join(dplyr::rename(st, away_lat = lat, away_lon = lon, away_tz = tz),
                     by = c("home_stadium_id" = "stadium_id"))

  # Fill missing outdoor weather: that stadium's median for the season, then the
  # stadium's median over all seasons, then the league's median that season.
  # (Retractable roofs left open often have no weather recorded at all.) Medians
  # come from played games only.
  fill_median <- function(df, ...) {
    df |>
      dplyr::group_by(dome, ...) |>
      dplyr::mutate(temp = dplyr::coalesce(temp, stats::median(temp[is_played], na.rm = TRUE)),
                    wind = dplyr::coalesce(wind, stats::median(wind[is_played], na.rm = TRUE))) |>
      dplyr::ungroup()
  }
  games <- games |>
    dplyr::mutate(dome = as.numeric(roof %in% c("dome", "closed")),
                  temp = as.numeric(temp), wind = as.numeric(wind)) |>
    fill_median(stadium_id, season) |>
    fill_median(stadium_id) |>
    fill_median(season)
  n_missing_starters <- sum(is.na(games$home_qb_id[games$is_played])) +
    sum(is.na(games$away_qb_id[games$is_played]))

  out <- games |>
    dplyr::mutate(
      margin = home_score - away_score,
      win = dplyr::case_when(margin > 0 ~ 1, margin < 0 ~ 0, margin == 0 ~ 0.5),
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
    dplyr::left_join(flags, by = "game_id") |>
    add_qb_change_game() |>
    add_qb_adjustments(qb) |>
    dplyr::select(game_id, season, week, gameday, margin, win,
                  dplyr::all_of(unlist(FEATURE_GROUPS, use.names = FALSE)), spread,
                  home_qb_adj, away_qb_adj, qb_change_game)

  feature_vars <- unlist(FEATURE_GROUPS, use.names = FALSE)
  # Without QB input the QB gap is NA by design and not required. Outcomes are NA
  # exactly for unplayed games, so they are not checked here.
  required <- if (is.null(qb)) setdiff(feature_vars, "qb_adj_diff") else feature_vars
  complete <- stats::complete.cases(out[required])
  unplayed <- is.na(out$margin)
  out <- out[complete, ]
  attr(out, "dropped") <- sum(!complete & !unplayed)
  attr(out, "dropped_upcoming") <- sum(!complete & unplayed)
  attr(out, "missing_starters") <- n_missing_starters
  out
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "features")'`
Expected: PASS, all tests, including the existing ones (row counts, ties at `0.5`, weather fills, the README-sourcing test).

- [ ] **Step 5: Run the whole suite**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: PASS. `tune_qb_params()` and the pipeline helpers still get completed-only schedules, so nothing else moves.

- [ ] **Step 6: Commit**

```bash
git add R/features.R tests/testthat/test-features.R
git commit -m "Keep the forecast week's unplayed games in build_features with NA outcomes

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Training and scoring ignore games without an outcome

**Files:**
- Modify: `R/models.R` (`fit_models()`), `R/validate.R` (`spread_benchmark()`, `score_predictions()`, `calibration_table()`), `R/bootstrap.R` (`game_losses()`)
- Test: `tests/testthat/test-models.R`, `tests/testthat/test-validate.R`, `tests/testthat/test-bootstrap.R`

**Interfaces:**
- Consumes: nothing new.
- Produces: the same signatures. Rows with `NA` `margin` (training) or `NA` `margin`/`win` (scoring) are ignored.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-models.R`:

```r
test_that("fit_models ignores games without an outcome", {
  set.seed(3)
  d <- tibble::tibble(game_id = 1:200, x = rnorm(200), home = 1)
  d$margin <- 2 * d$x + rnorm(200, 0, 5)
  d$win <- as.numeric(d$margin > 0)
  unplayed <- tibble::tibble(game_id = 201:210, x = rnorm(10, 5), home = 1,
                             margin = NA_real_, win = NA_real_)
  a <- fit_models(d, "x")
  b <- fit_models(dplyr::bind_rows(d, unplayed), "x")
  expect_equal(b$scaler, a$scaler)   # scaling must not see unplayed games either
  expect_equal(coef(b$margin), coef(a$margin))
  expect_equal(coef(b$win), coef(a$win))
})
```

Append to `tests/testthat/test-validate.R`:

```r
# wf_data() with its last week (2021 week 5) not yet played.
wf_unplayed <- function() {
  d <- wf_data()
  last <- d$season == 2021 & d$week == 5
  d$margin[last] <- NA
  d$win[last] <- NA
  d
}

test_that("walk-forward predicts unplayed games and never trains on them", {
  d <- wf_unplayed()
  saw_na <- logical()
  recording_fit <- function(train, vars) {
    saw_na <<- c(saw_na, anyNA(train$margin))
    fit_models(train, vars)
  }
  out <- walk_forward(d, "x", test_seasons = 2020:2021, train_start = 2014L, fit_fn = recording_fit)
  expect_false(any(saw_na))
  up <- out[is.na(out$margin), ]
  expect_equal(sort(up$game_id), d$game_id[is.na(d$margin)])
  expect_false(anyNA(up$pred_win))
})

test_that("spread benchmark predicts unplayed games without fitting on them", {
  d <- wf_unplayed()
  out <- spread_benchmark(d, test_seasons = 2021, train_start = 2014L)
  played <- spread_benchmark(d[!is.na(d$margin), ], test_seasons = 2021, train_start = 2014L)
  expect_equal(out$pred_win[!is.na(out$margin)], played$pred_win)
  expect_false(anyNA(out$pred_win))
})

test_that("scoring and calibration ignore games without an outcome", {
  p <- tibble::tibble(season = 2020L, margin = c(3, -7), win = c(1, 0),
                      pred_margin = c(1, -3), pred_win = c(0.8, 0.4))
  up <- tibble::tibble(season = 2020L, margin = NA_real_, win = NA_real_,
                       pred_margin = 4, pred_win = 0.6)
  expect_equal(score_predictions(dplyr::bind_rows(p, up)), score_predictions(p))
  expect_equal(calibration_table(dplyr::bind_rows(p, up)), calibration_table(p))
})
```

Append to `tests/testthat/test-bootstrap.R`:

```r
test_that("games without an outcome are left out of the comparison", {
  a <- boot_data()
  b <- dplyr::mutate(a, pred_margin = pred_margin + 2)
  up <- tibble::tibble(game_id = 1000L + 1:3, season = 2020L, week = 21L, margin = NA_real_,
                       win = NA_real_, pred_margin = 3, pred_win = 0.6)
  expect_equal(paired_bootstrap(dplyr::bind_rows(a, up), dplyr::bind_rows(b, up), n_boot = 50),
               paired_bootstrap(a, b, n_boot = 50))
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "models|validate|bootstrap")'`
Expected: FAIL in `fit_models ignores…` (scaler mean includes the unplayed `x`), `scoring and calibration…` (`rmse`, `accuracy`, the calibration bin's `actual_rate` become `NA`) and `games without an outcome…` (an extra week and `NA` sums). The two walk-forward/spread tests already pass. They pin that unplayed rows get predictions.

- [ ] **Step 3: Implement**

In `R/models.R`, make the first lines of `fit_models()`:

```r
fit_models <- function(train, vars) {
  train <- train[!is.na(train$margin), ]   # unplayed games have no outcome to learn from
  # Drop anything with no variation in the training rows (e.g. `home` when every
```

(the rest of the function unchanged).

In `R/validate.R`, in `spread_benchmark()` replace the `train <-` line with:

```r
    train <- features[features$season >= train_start & features$season < y &
                        !is.na(features$spread) & !is.na(features$win), ]
```

In `score_predictions()` replace the first `dplyr::filter(...)` with:

```r
    dplyr::filter(!is.na(margin), !is.na(win), !is.na(pred_margin), !is.na(pred_win)) |>
```

In `calibration_table()` make the start:

```r
calibration_table <- function(preds, bins = 10) {
  preds |>
    dplyr::filter(!is.na(win)) |>
    dplyr::mutate(bin = cut(pred_win, breaks = seq(0, 1, length.out = bins + 1),
```

In `R/bootstrap.R`, in `game_losses()` replace the `dplyr::filter(...)` with:

```r
    dplyr::filter(!is.na(margin), !is.na(win), !is.na(pred_margin), !is.na(pred_win)) |>
```

and change its comment to:

```r
# Per-game squared margin error and log loss, scored as in score_predictions()
# (played games only).
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: PASS, whole suite.

- [ ] **Step 5: Commit**

```bash
git add R/models.R R/validate.R R/bootstrap.R tests/testthat/test-models.R tests/testthat/test-validate.R tests/testthat/test-bootstrap.R
git commit -m "Train and score on completed games only

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: `upcoming_predictions()`

**Files:**
- Modify: `R/validate.R` (new function at the end)
- Test: `tests/testthat/test-validate.R`

**Interfaces:**
- Consumes: walk-forward/spread-benchmark outputs (`game_id, season, week, margin, win, pred_margin, pred_win`); a schedule with `game_id, gameday, gametime, away_team, home_team, away_qb_name, home_qb_name, spread_line`.
- Produces: `upcoming_predictions(preds, sched) -> tibble` with columns `model, game_id, season, week, gameday, gametime, away_team, home_team, away_qb_name, home_qb_name, spread, pred_margin, pred_win`: one row per run per game with `NA` `margin`. `NA` predictions and `NA` starters are kept. Zero rows (same columns) when nothing is unplayed.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-validate.R`:

```r
up_sched <- function() {
  tibble::tibble(game_id = c("done", "next1", "next2"), gameday = "2026-10-18",
                 gametime = c("13:00", "13:00", "20:20"), away_team = c("A", "C", "E"),
                 home_team = c("B", "D", "F"), away_qb_name = c("qa", "qc", NA),
                 home_qb_name = c("qb", "qd", "qf"), spread_line = c(3, -1.5, NA))
}
up_preds <- function() {
  tibble::tibble(game_id = c("done", "next1", "next2"), season = 2026L, week = 6L,
                 margin = c(7, NA, NA), win = c(1, NA, NA),
                 pred_margin = c(2, 1, NA), pred_win = c(0.6, 0.55, NA))   # next2 has no line yet
}
up_cols <- c("model", "game_id", "season", "week", "gameday", "gametime", "away_team", "home_team",
             "away_qb_name", "home_qb_name", "spread", "pred_margin", "pred_win")

test_that("upcoming predictions are the unplayed games, one row per run, with schedule info", {
  preds <- list(m1 = up_preds(), m2 = dplyr::mutate(up_preds(), pred_win = pred_win + 0.1))
  out <- upcoming_predictions(preds, up_sched())
  expect_named(out, up_cols)
  expect_equal(nrow(out), 4)
  expect_setequal(out$game_id, c("next1", "next2"))
  expect_equal(out$pred_win[out$model == "m2" & out$game_id == "next1"], 0.65)
  expect_equal(out$home_team[out$game_id == "next1"], c("D", "D"))
  expect_equal(out$spread[out$game_id == "next1"], c(-1.5, -1.5))
  expect_true(all(is.na(out$away_qb_name[out$game_id == "next2"])))   # starter not listed
  expect_true(all(is.na(out$pred_win[out$game_id == "next2"])))       # kept, not dropped
})

test_that("upcoming predictions are empty when every game has been played", {
  p <- up_preds()[1, ]
  out <- upcoming_predictions(list(m1 = p), up_sched())
  expect_equal(nrow(out), 0)
  expect_named(out, up_cols)
})
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "validate")'`
Expected: FAIL with `could not find function "upcoming_predictions"`.

- [ ] **Step 3: Implement**

Append to `R/validate.R`:

```r
# Predictions for games not yet played (margin NA), one row per run per game, with
# the schedule's teams, kickoff, projected starters and current line. pred_margin and
# pred_win are from the home team's view. preds: named list of walk_forward() /
# spread_benchmark() outputs.
upcoming_predictions <- function(preds, sched) {
  info <- dplyr::select(sched, game_id, gameday, gametime, away_team, home_team,
                        away_qb_name, home_qb_name, spread = spread_line)
  purrr::imap_dfr(preds, function(p, name) {
    p <- p[is.na(p$margin), c("game_id", "season", "week", "pred_margin", "pred_win")]
    dplyr::mutate(p, model = name, .before = 1)
  }) |>
    dplyr::left_join(info, by = "game_id") |>
    dplyr::select(model, game_id, season, week, gameday, gametime, away_team, home_team,
                  away_qb_name, home_qb_name, spread, pred_margin, pred_win)
}
```

- [ ] **Step 4: Run the tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "validate")'`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/validate.R tests/testthat/test-validate.R
git commit -m "Add upcoming_predictions for the forecast week's unplayed games

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: Pipeline wiring, verified against a baseline run

**Files:**
- Modify: `R/pipeline.R`

**Interfaces:**
- Consumes: `rating_schedule()`, `schedule_download_time()` (Task 1); `build_features()` attributes `dropped`, `dropped_upcoming`, `missing_starters` (Task 2); `upcoming_predictions()` (Task 4).
- Produces: `results` gains `upcoming` (Task 4's tibble), `upcoming_dropped` (integer), `schedule_downloaded` (`POSIXct`, may be `NA`). Every existing key is unchanged.

`run_all()` has no unit test (it needs the full data). The check is a before/after run on the same cache.

- [ ] **Step 1: Make a baseline with the pre-change code**

The plan's own commit holds the old code. Run it on the current cache without re-downloading (about 13 minutes):

```bash
REPO=$(pwd)
BASE_SHA=$(git log --format=%H -1 -- docs/superpowers/plans/2026-10-15-upcoming-week.md)
BASE_DIR="${TMPDIR:-/tmp}/nfl-drivers-baseline"
git worktree add --detach "$BASE_DIR" "$BASE_SHA"
(cd "$BASE_DIR" && REPO_CACHE="$REPO/data/cache" Rscript -e 'suppressPackageStartupMessages(library(dplyr)); for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f); run_all(cache_dir = Sys.getenv("REPO_CACHE"), refresh = integer())')
cp "$REPO/data/cache/results.rds" "${TMPDIR:-/tmp}/nfl-baseline_results.rds"
git worktree remove "$BASE_DIR"
```

Expected: `Saved .../data/cache/results.rds` at the end.

- [ ] **Step 2: Wire the pipeline**

In `R/pipeline.R`, replace the block from `step("Building features")` through the closing `)` of `preds <- list(...)` with:

```r
  step("Building features")
  # Features cover completed games plus the forecast week's unplayed games (margin and
  # win NA). features_ridge carries the ridge QB gap, so every ridge run is scored on
  # the same games.
  features_ridge <- build_features(sched_rate, ratings_ridge, stadiums,
                                   qb = list(ratings = qb_ratings_ridge, mix = qb_mix_ridge))
  features_qb_rolling <- build_features(sched_rate, ratings_ridge, stadiums,
                                        qb = list(ratings = qb_ratings_rolling, mix = qb_mix_rolling))
  features_rolling <- build_features(sched_rate, ratings_rolling, stadiums)
  dropped <- attr(features_ridge, "dropped")
  upcoming_dropped <- attr(features_ridge, "dropped_upcoming")
  missing_starters <- attr(features_ridge, "missing_starters")

  step("Walk-forward validation")
  preds_all <- list(
    base            = walk_forward(features_ridge, MODEL_SPECS$base, test_seasons),
    full_ridge      = walk_forward(features_ridge, MODEL_SPECS$full, test_seasons),
    full_rolling    = walk_forward(features_rolling, MODEL_SPECS$full, test_seasons),
    full_qb_ridge   = walk_forward(features_ridge, MODEL_SPECS$full_qb, test_seasons),
    full_qb_rolling = walk_forward(features_qb_rolling, MODEL_SPECS$full_qb, test_seasons),
    market          = walk_forward(features_ridge, MODEL_SPECS$market, test_seasons),
    market_qb       = walk_forward(features_ridge, MODEL_SPECS$market_qb, test_seasons),
    spread_only     = spread_benchmark(features_ridge, test_seasons)
  )
  # Unplayed games get a prediction from every run. From here on, everything (scores,
  # comparisons, importance, the QB verdict and what is saved) uses completed games only.
  upcoming <- upcoming_predictions(preds_all, sched_rate)
  preds <- lapply(preds_all, function(p) p[!is.na(p$margin), ])
  completed <- function(f) f[!is.na(f$margin), ]
  features_ridge <- completed(features_ridge)
  features_qb_rolling <- completed(features_qb_rolling)
  features_rolling <- completed(features_rolling)
  unplayed <- is.na(sched_rate$home_score) | is.na(sched_rate$away_score)
  forecast_season <- if (any(unplayed)) sched_rate$season[unplayed][1] else NA_integer_
```

In the `results <- list(...)` call, change `dropped = attr(features_ridge, "dropped"),` to `dropped = dropped,` and `missing_starters = attr(features_ridge, "missing_starters"),` to `missing_starters = missing_starters,`. Then replace the last entry, `qb_starters = dplyr::filter(scheduled_starters(sched_rate), !is.na(qb_id))`, with:

```r
    qb_starters = dplyr::filter(scheduled_starters(sched_rate), !is.na(qb_id)),
    upcoming = upcoming,
    upcoming_dropped = upcoming_dropped,
    schedule_downloaded = schedule_download_time(cache_dir, forecast_season)
```

Also update the comment above `sched_rate <- rating_schedule(sched_all)` to:

```r
  # Ratings cover every completed week plus the forecast week (the rest of a week in
  # progress, or else the next week).
```

- [ ] **Step 3: Run the suite**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: PASS.

- [ ] **Step 4: Run the new pipeline on the same cache**

Run: `Rscript -e 'suppressPackageStartupMessages(library(dplyr)); for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f); run_all(refresh = integer())'`
Expected: `Saved data/cache/results.rds`.

- [ ] **Step 5: Compare with the baseline**

```bash
BASELINE="${TMPDIR:-/tmp}/nfl-baseline_results.rds" Rscript -e '
strip <- function(x) {
  if (is.data.frame(x)) for (a in c("dropped", "dropped_upcoming", "missing_starters")) attr(x, a) <- NULL
  x
}
old <- readRDS(Sys.getenv("BASELINE")); new <- readRDS("data/cache/results.rds")
for (k in setdiff(names(old), "created")) {
  ok <- isTRUE(all.equal(strip(old[[k]]), strip(new[[k]])))
  cat(sprintf("%-32s %s\n", k, if (ok) "same" else "DIFFERENT"))
}
cat("new keys:", setdiff(names(new), names(old)), "\n")
print(table(new$upcoming$model))
cat("upcoming_dropped:", new$upcoming_dropped,
    " schedule_downloaded:", format(new$schedule_downloaded, "%Y-%m-%d %H:%M %Z"), "\n")'
```

Expected: every key `same`. New keys: `upcoming upcoming_dropped schedule_downloaded`. Each of the 8 runs has one row per unplayed forecast-week game (14 each on the 2026-10-15 cache), and `upcoming_dropped` is 0. The download time matches `ls -l data/cache/schedules_2026.rds`.

**Exception:** if the cache was saved mid-week (a forecast-week game already has a score), the old code picked a different forecast week. Then only `ratings_ridge`, `ratings_rolling`, `qb_ratings_ridge`, `qb_ratings_rolling`, `qb_mix_ridge`, `qb_mix_rolling` and `qb_starters` may differ, in their forecast-week rows only. Every score, comparison, ablation, coefficient and verdict key must still be `same`. Any other `DIFFERENT` is a bug: stop and investigate.

- [ ] **Step 6: Commit**

```bash
git add R/pipeline.R
git commit -m "Predict the forecast week from every run; save upcoming predictions and download time

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: "Next week" report section and docs

**Files:**
- Modify: `report.qmd` (new section 9, caveats renumbered to 10, one caveat line)
- Modify: `README.md` ("Using it in the Forecast Lab")
- Modify: `docs/superpowers/specs/2026-10-14-nfl-drivers-design.md` (section 1 pointer)

**Interfaces:**
- Consumes: `res$upcoming`, `res$upcoming_dropped`, `res$schedule_downloaded` (Task 5); `model_labels` and `kable()` from the report's setup chunk.
- Produces: the rendered "9. Next week" section.

- [ ] **Step 1: Add the section**

In `report.qmd`, replace the line `## 9. Caveats and next steps` with the block below. It ends with the renamed `## 10. Caveats and next steps` heading.

````markdown
## 9. Next week

Win probabilities for the forecast week's games that are still to be played, from
every model run.

```{r next-week}
#| output: asis
up <- res$upcoming
dl <- res$schedule_downloaded
dl_text <- if (is.null(dl) || is.na(dl)) "unknown" else format(dl, "%Y-%m-%d %H:%M %Z")
if (is.null(up) || nrow(up) == 0) {
  cat("No unplayed games are on the schedule, so there is nothing to predict. ",
      "Schedule downloaded ", dl_text, ".\n", sep = "")
} else {
  headline <- "full_qb_ridge"
  runs <- c(headline, setdiff(names(model_labels), headline))
  runs <- runs[runs %in% up$model]
  run_labels <- model_labels
  run_labels["spread_only"] <- "Spread alone (current line)"
  col_labels <- ifelse(runs == headline, paste0("★ ", run_labels[runs]), run_labels[runs])
  pct <- function(p) ifelse(is.na(p), "–", sprintf("%.0f%%", 100 * p))
  signed <- function(x) ifelse(is.na(x), "–", sprintf("%+.1f", x))

  probs <- up |>
    mutate(model = factor(model, levels = runs), pred_win = pct(pred_win)) |>
    arrange(model) |>
    select(game_id, model, pred_win) |>
    pivot_wider(names_from = model, values_from = pred_win)
  names(probs)[-1] <- col_labels[match(names(probs)[-1], runs)]
  margin <- up |>
    filter(model == headline) |>
    transmute(game_id, `★ Predicted margin` = signed(pred_margin))
  tbl <- up |>
    distinct(game_id, gameday, gametime, away_team, home_team, away_qb_name, home_qb_name, spread) |>
    arrange(gameday, gametime, game_id) |>
    left_join(margin, by = "game_id") |>
    left_join(probs, by = "game_id")
  shown <- bind_cols(
    transmute(tbl, Game = paste(away_team, "@", home_team),
              Kickoff = paste(format(as.Date(gameday), "%a %b %d"), gametime, "ET"),
              `Away QB` = coalesce(away_qb_name, "not listed"),
              `Home QB` = coalesce(home_qb_name, "not listed"),
              `Line (current)` = signed(spread)),
    select(tbl, `★ Predicted margin`, all_of(unname(col_labels)))
  )

  season_up <- up$season[1]
  cat(sprintf("Predictions for **%d week %d** (%d games). Schedule downloaded **%s**; the projected starters and the line are as of then.\n\n",
              season_up, up$week[1], nrow(tbl), dl_text))
  cat(kable(shown), sep = "\n")
  cat("\n\nProbabilities are the **home team's** chance to win. The line and the margin are points ",
      "from the home team's view (+ = home favored). Every run is trained on 2010–", season_up - 1,
      ", as in its scored ", season_up, " walk-forward run.\n\n", sep = "")
  cat("- **★ marks the headline model,** ", model_labels[[headline]], ".\n", sep = "")
  cat("- **The market runs use the current line, not the closing line.** ",
      model_labels[["market"]], ", ", model_labels[["market_qb"]], " and Spread alone were ",
      "trained and scored on closing lines, so for these games their numbers are not comparable ",
      "to their scored accuracy, and they move as the line moves.\n", sep = "")
  cat("- **Check the starters before using these numbers.** \"not listed\" means the schedule ",
      "has no projected starter, and the model then assumes the usual starter. If a starter is ",
      "wrong or the download time above is stale, rerun `Rscript run.R`.\n", sep = "")
  if (isTRUE(res$upcoming_dropped > 0)) {
    cat("- ", res$upcoming_dropped, " forecast-week game(s) are missing a feature and are not shown.\n",
        sep = "")
  }
}
```

## 10. Caveats and next steps
````

In the caveats, replace:

```markdown
- **The upcoming week uses projected starters** from the schedule, which can change
  before kickoff.
```

with:

```markdown
- **The upcoming week uses projected starters** from the schedule, which can change
  before kickoff. Section 9 lists them with the time the schedule was downloaded.
```

- [ ] **Step 2: Render and check the section**

Run: `quarto render report.qmd && grep -c "Next week\|Schedule downloaded\|★ Full model + QB ratings (ridge)\|not the closing line" report.html`
Expected: the render succeeds and the count is at least 4. Open `report.html` and check that section 9 has 14 rows ordered by kickoff (SEA @ DEN first on the 2026-10-15 cache), Away QB / Home QB columns, `Line (current)`, the ★ margin and probability columns, and the three notes.

- [ ] **Step 3: Check the empty case renders**

```bash
cp data/cache/results.rds "${TMPDIR:-/tmp}/results_full.rds"
Rscript -e 'r <- readRDS("data/cache/results.rds"); r$upcoming <- r$upcoming[0, ]; r$schedule_downloaded <- as.POSIXct(NA); saveRDS(r, "data/cache/results.rds")'
quarto render report.qmd && grep -c "nothing to predict" report.html
cp "${TMPDIR:-/tmp}/results_full.rds" data/cache/results.rds
quarto render report.qmd
```

Expected: the first render succeeds with count 1, and the results file is restored and re-rendered.

- [ ] **Step 4: Update the docs**

In `README.md`, append to the "Using it in the Forecast Lab" paragraph (after `to `build_features()`.`):

```markdown
`build_features()` also returns the forecast week's unplayed games (those whose week
has ratings) with `margin` and `win` set to `NA`. `fit_models()` skips them; filter on
`!is.na(margin)` before fitting anything else.
```

In `docs/superpowers/specs/2026-10-14-nfl-drivers-design.md`, replace:

```markdown
- Games only: regular season and playoffs, excluding games without a final score (unplayed 2026 games).
```

with:

```markdown
- Games only: regular season and playoffs, excluding games without a final score (unplayed 2026 games). The forecast week's unplayed games are kept for prediction only; see `2026-10-15-upcoming-week-design.md`.
```

- [ ] **Step 5: Run the suite once more**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add report.qmd README.md docs/superpowers/specs/2026-10-14-nfl-drivers-design.md
git commit -m "Add Next week section to the report; document unplayed games in build_features

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

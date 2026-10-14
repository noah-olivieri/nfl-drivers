# NFL Game Drivers Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** An R project that computes opponent-adjusted team ratings, builds pre-game features for every 2010–2026 NFL game, fits margin and win/loss regressions, and measures which variables matter with walk-forward validation, rendered as a Quarto report.

**Architecture:** Plain `.R` files in `R/` define functions only (nothing runs on `source()`). `R/pipeline.R` chains them and caches results to `data/cache/results.rds`; `report.qmd` reads that file and renders tables, charts, and Noah's written reasoning. Data flows one way: raw data → ratings → features → models → validation and importance → report.

**Tech Stack:** R, nflreadr, dplyr, tidyr, purrr, glmnet, Matrix, lubridate, ggplot2, testthat, Quarto CLI.

**Spec:** `docs/superpowers/specs/2026-10-14-nfl-drivers-design.md`

## Global Constraints

- Modeling seasons 2010–2025; 2026 completed games are a live check, labeled as a partial season. 2009 is loaded only for 2010 carryover.
- Walk-forward test seasons 2018–2026; for test season Y, train on 2010 through Y − 1. Never shuffle across years.
- Every feature must be known before kickoff. Ratings for a game use only plays from games dated strictly before that game's week cutoff (the earliest `gameday` in that season-week).
- Rating hyperparameters are tuned on 2010–2017 only, then fixed.
- Ties: `win = 0.5`; the win model uses `family = quasibinomial`. Ties are excluded from win accuracy only.
- Numeric features are standardized with training-rows-only means and SDs; binary flags are not standardized.
- Team abbreviations are normalized with `nflreadr::clean_team_abbrs(x, current_location = TRUE)` everywhere teams are joined, so relocated teams keep one identity.
- `.R` files define functions only. No top-level code runs on `source()`.
- Report reasoning sections are Noah's; the report holds clearly marked placeholders (`<!-- NOAH: ... -->`) where his interpretation goes. Claude does not write them.
- Commit messages end with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01ANLiiWbdUn6hh5XbpnRKFL
  ```

## Review Focus

1. **Relocated teams (STL→LA, SD→LAC, OAK→LV).** A team keeps continuous ratings across a move; no rating restarts from zero mid-history. Test in Task 3.
2. **Playoff and week-18 games.** Postseason weeks (19–22 from 2021, 18–21 before) get ratings and feature rows like any regular-season week. Test in Task 5.
3. **Mid-season 2026 runs.** Unplayed games and no-contest games (NA scores, e.g. 2022 BUF@CIN) are excluded without erroring, and the newest week still gets ratings. Test in Task 1.
4. **Neutral-site and international games.** `home = 0`, and away travel is measured to the actual venue, not the nominal home team's stadium. Test in Task 5.
5. **Missing QB IDs or a team's first game in the data.** QB flags default to 0 rather than NA, so the game isn't dropped. Test in Task 5.

---

### Task 1: Project setup and data loading

**Files:**
- Create: `R/load_data.R`, `tests/testthat/helper-source.R`, `tests/testthat/test-load_data.R`, `DESCRIPTION` (dependency list only, `Package: nfldrivers`, `Imports:` the tech stack)

**Interfaces:**
- Produces:
  - `load_cached(seasons, kind, cache_dir = "data/cache", refresh = 2026L, fetch = NULL) -> tibble` — `kind` is `"pbp"` or `"schedules"`. Per season, reads `cache_dir/<kind>_<season>.rds` if present and the season is not in `refresh`; otherwise calls `fetch(season)` (default: `nflreadr::load_pbp` / `nflreadr::load_schedules`), saves, and returns. Binds all seasons.
  - `clean_schedules(sched) -> tibble` — keeps games with non-NA `home_score` and `away_score`; normalizes `home_team`, `away_team` with `clean_team_abbrs`.
  - `clean_plays(pbp) -> tibble` — columns `season, week, game_id, game_date (Date), posteam, defteam, home_team, play_type ("pass"|"run"), epa, wp`; keeps `pass == 1 | rush == 1`, non-NA `epa`, `play_type %in% c("pass","run")` (this drops kneels, spikes, and no-play penalties); normalizes team abbrs.

- [ ] **Step 1: Write `helper-source.R`** sourcing every file in `../../R/` with `for (f in list.files("../../R", full.names = TRUE)) source(f)`, plus `library(dplyr)`.

- [ ] **Step 2: Write the failing tests**

```r
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
  s <- tibble::tibble(game_id = c("a","b","c"), home_team = c("SD","KC","BUF"),
                      away_team = c("OAK","DEN","CIN"),
                      home_score = c(20, NA, NA), away_score = c(17, NA, 3))
  out <- clean_schedules(s)
  expect_equal(out$game_id, "a")
  expect_equal(out$home_team, "LAC")
  expect_equal(out$away_team, "LV")
})

test_that("clean_plays keeps only pass/run plays with EPA", {
  p <- tibble::tibble(season = 2020L, week = 1L, game_id = "g", game_date = "2020-09-13",
                      posteam = "KC", defteam = "HOU", home_team = "KC",
                      play_type = c("pass","run","qb_kneel","no_play","pass"),
                      pass = c(1,0,0,1,1), rush = c(0,1,1,0,0),
                      epa = c(0.5,-0.2,-0.1,0.3,NA), wp = 0.5)
  out <- clean_plays(p)
  expect_equal(nrow(out), 2)
  expect_s3_class(out$game_date, "Date")
})
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-load_data.R")'`
Expected: FAIL with `could not find function "load_cached"`.

- [ ] **Step 4: Implement the three functions in `R/load_data.R`**

Wrap each `fetch(season)` in `tryCatch` and re-raise as `stop("Failed to download ", kind, " for season ", season, ": ", msg)`.

- [ ] **Step 5: Run tests to verify they pass**

Run: same command. Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add DESCRIPTION R/load_data.R tests/testthat/
git commit -m "Add cached data loading and cleaning"
```

---

### Task 2: Stadium table and venue lookup

**Files:**
- Create: `data/stadiums.csv`, `R/venues.R`, `tests/testthat/test-venues.R`

**Interfaces:**
- Consumes: `load_cached`, `clean_schedules` (Task 1)
- Produces:
  - `data/stadiums.csv` with columns `stadium_id, stadium, lat, lon, tz` (IANA zone, e.g. `America/Phoenix`), one row per `stadium_id` appearing in 2009–2026 schedules, including international venues.
  - `team_home_venue(sched) -> tibble(season, team, home_stadium_id)` — each team's most frequent home `stadium_id` per season among non-neutral games.
  - `haversine_miles(lat1, lon1, lat2, lon2) -> numeric`
  - `utc_offset_hours(tz, datetime) -> numeric` — offset at that moment, via `lubridate::with_tz` (handles DST and Arizona).

- [ ] **Step 1: Build `stadiums.csv`.** Run `Rscript -e 'source("R/load_data.R"); s <- load_cached(2009:2026, "schedules"); print(dplyr::distinct(s, stadium_id, stadium), n = Inf)'`, then fill coordinates and IANA zone for every row. Coordinates to 2 decimal places are enough.

- [ ] **Step 2: Write the failing tests**

```r
test_that("every venue in the schedules has coordinates", {
  skip_if_not(file.exists("../../data/cache/schedules_2026.rds"), "data not cached")
  sched <- load_cached(2009:2026, "schedules", "../../data/cache", refresh = integer())
  st <- readr::read_csv("../../data/stadiums.csv", show_col_types = FALSE)
  expect_length(setdiff(unique(sched$stadium_id), st$stadium_id), 0)
  expect_false(anyNA(st[c("lat","lon","tz")]))
})

test_that("haversine matches a known distance", {
  # Kansas City to Miami ~ 1240 miles
  expect_equal(haversine_miles(39.05, -94.48, 25.96, -80.24), 1240, tolerance = 15)
})

test_that("Arizona has no DST", {
  summer <- as.POSIXct("2024-07-01 12:00", tz = "UTC")
  expect_equal(utc_offset_hours("America/Phoenix", summer), -7)
  expect_equal(utc_offset_hours("America/Los_Angeles", summer), -7)
  winter <- as.POSIXct("2024-12-01 12:00", tz = "UTC")
  expect_equal(utc_offset_hours("America/Los_Angeles", winter), -8)
})

test_that("team home venue ignores neutral-site games", {
  s <- tibble::tibble(season = 2022L, home_team = "ARI", location = c("Home","Home","Neutral"),
                      stadium_id = c("PHO00","PHO00","MEX00"))
  expect_equal(team_home_venue(s)$home_stadium_id, "PHO00")
})
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-venues.R")'`
Expected: FAIL with `could not find function "haversine_miles"`.

- [ ] **Step 4: Implement the functions in `R/venues.R`.** Earth radius 3958.8 miles.

- [ ] **Step 5: Run tests to verify they pass.** Expected: all PASS (coverage test may SKIP until data is cached; run it once with data before committing).

- [ ] **Step 6: Commit**

```bash
git add data/stadiums.csv R/venues.R tests/testthat/test-venues.R
git commit -m "Add stadium table and venue helpers"
```

---

### Task 3: Ridge team ratings

**Files:**
- Create: `R/team_strength.R`, `tests/testthat/test-team_strength.R`

**Interfaces:**
- Consumes: `clean_plays`, `clean_schedules` output (Task 1)
- Produces:
  - `week_cutoffs(sched) -> tibble(season, week, cutoff_date)` — earliest `gameday` per season-week, regular season and playoffs.
  - `fit_ratings_at(plays, season, cutoff_date, params) -> tibble(team, pass_off, rush_off, pass_def, rush_def)`
  - `compute_ridge_ratings(plays, sched, params, seasons) -> tibble(season, week, team, pass_off, rush_off, pass_def, rush_def)`
  - `params` is a list: `half_life` (weeks), `offseason_weeks`, `lambda`, `garbage_weight = 0.25`.

`fit_ratings_at` uses plays with `game_date < cutoff_date` from `season` and `season - 1`. Design matrix (sparse, `Matrix::sparse.model.matrix` or hand-built `sparseMatrix`): one column per `off_<team>_<pass|run>`, one per `def_<team>_<pass|run>`, `is_home_off` (posteam == home_team), and `is_pass`. `is_pass` and `is_home_off` get `penalty.factor = 0`. Weights:

```r
weeks_ago <- as.numeric(cutoff_date - game_date) / 7 +
             ifelse(play_season < season, params$offseason_weeks, 0)
w <- 0.5 ^ (weeks_ago / params$half_life) *
     ifelse(wp < 0.05 | wp > 0.95, params$garbage_weight, 1)
```

Fit `glmnet(X, epa, weights = w, alpha = 0, lambda = params$lambda, standardize = FALSE)`. Offense rating = coefficient; defense rating = **negated** coefficient (higher = better defense). Teams with no plays in the window get 0.

- [ ] **Step 1: Write the failing tests.** Put `make_synthetic` in `tests/testthat/helper-synthetic.R` (later tasks reuse it); the tests go in `test-team_strength.R`.

```r
make_synthetic <- function(n_games = 400, seed = 1) {
  set.seed(seed)
  teams <- c("A","B","C","D")
  true_off <- c(A = 0.3, B = 0.1, C = -0.1, D = -0.3)
  g <- tibble::tibble(game_id = seq_len(n_games),
                      game_date = as.Date("2020-09-01") + seq_len(n_games) %/% 4,
                      home_team = sample(teams, n_games, TRUE))
  g$away_team <- vapply(g$home_team, function(h) sample(setdiff(teams, h), 1), "")
  plays <- g |> tidyr::uncount(40) |>
    dplyr::mutate(posteam = ifelse(dplyr::row_number() %% 2 == 0, home_team, away_team),
                  defteam = ifelse(posteam == home_team, away_team, home_team),
                  play_type = sample(c("pass","run"), dplyr::n(), TRUE),
                  epa = true_off[posteam] + rnorm(dplyr::n(), 0, 1),
                  wp = 0.5, season = 2020L, week = 1L)
  plays
}

test_that("ratings recover the known offensive order", {
  r <- fit_ratings_at(make_synthetic(), 2020L, as.Date("2021-06-01"),
                      list(half_life = 1000, offseason_weeks = 0, lambda = 1e-4, garbage_weight = 0.25))
  expect_equal(r$team[order(-r$pass_off)], c("A","B","C","D"))
  expect_equal(r$team[order(-r$rush_off)], c("A","B","C","D"))
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
  p <- make_synthetic() |> dplyr::mutate(posteam = dplyr::recode(posteam, A = "LV"),
                                         defteam = dplyr::recode(defteam, A = "LV"),
                                         home_team = dplyr::recode(home_team, A = "LV"))
  p$posteam[1:200] <- ifelse(p$posteam[1:200] == "LV", "OAK", p$posteam[1:200])
  p <- clean_plays_teams(p)   # helper that applies clean_team_abbrs to team columns
  r <- fit_ratings_at(p, 2020L, as.Date("2021-06-01"),
                      list(half_life = 1000, offseason_weeks = 0, lambda = 1e-4, garbage_weight = 0.25))
  expect_false("OAK" %in% r$team)
})
```

Also add `clean_plays_teams(df) -> df` to `R/load_data.R` (applies `clean_team_abbrs` to `posteam`, `defteam`, `home_team`) and have `clean_plays` call it.

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_file("tests/testthat/test-team_strength.R")'`
Expected: FAIL with `could not find function "fit_ratings_at"`.

- [ ] **Step 3: Implement `week_cutoffs`, `fit_ratings_at`, `compute_ridge_ratings` in `R/team_strength.R`.** `compute_ridge_ratings` maps `fit_ratings_at` over every `week_cutoffs` row in `seasons` with `purrr::pmap_dfr`.

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/team_strength.R R/load_data.R tests/testthat/test-team_strength.R
git commit -m "Add opponent-adjusted ridge team ratings"
```

---

### Task 4: Rolling baseline and rating tuning

**Files:**
- Modify: `R/team_strength.R`
- Create: `tests/testthat/test-rolling_tuning.R`

**Interfaces:**
- Consumes: `week_cutoffs`, `compute_ridge_ratings` (Task 3)
- Produces:
  - `compute_rolling_ratings(plays, sched, seasons, n_games = 8) -> tibble(season, week, team, pass_off, rush_off, pass_def, rush_def)` — same shape as ridge. For each cutoff, each team's mean EPA/play over its last `n_games` games before the cutoff (crossing seasons), split by play type; defense = negated mean EPA allowed. No opponent adjustment.
  - `tune_rating_params(plays, sched, grid, seasons = 2010:2017) -> list(best = params, scores = tibble)` — for each grid row: compute ridge ratings for `seasons`, join to that week's games, score with in-sample RMSE of `lm(margin ~ pass_off_diff + rush_off_diff + pass_def_diff + rush_def_diff + home)` pooled over `seasons`. Returns the lowest-RMSE params.
  - `default_rating_grid() -> tibble` = `tidyr::expand_grid(half_life = c(3, 5, 8, 12), offseason_weeks = c(8, 16, 26), lambda = c(1e-4, 3e-4, 1e-3, 3e-3, 1e-2), garbage_weight = 0.25)`.

- [ ] **Step 1: Write the failing tests**

```r
test_that("rolling ratings use only the last n games before the cutoff", {
  p <- make_synthetic()
  s <- tibble::tibble(season = 2020L, week = 2L, gameday = as.Date("2020-10-01"))
  r <- compute_rolling_ratings(p, s, 2020L, n_games = 8)
  a_games <- p |> dplyr::filter(game_date < as.Date("2020-10-01"),
                                posteam == "A" | defteam == "A") |>
    dplyr::distinct(game_id, game_date) |> dplyr::slice_max(game_date, n = 8)
  manual <- p |> dplyr::filter(game_id %in% a_games$game_id, posteam == "A", play_type == "pass")
  expect_equal(r$pass_off[r$team == "A"], mean(manual$epa))
})

test_that("tuning never sees seasons outside the tuning window", {
  seen <- integer()
  testthat::local_mocked_bindings(
    compute_ridge_ratings = function(plays, sched, params, seasons) {
      seen <<- c(seen, seasons); tibble::tibble()
    })
  try(tune_rating_params(tibble::tibble(), tibble::tibble(), default_rating_grid()[1, ]), silent = TRUE)
  expect_true(all(seen %in% 2010:2017))
})
```

- [ ] **Step 2: Run tests to verify they fail.** Run: `Rscript -e 'testthat::test_file("tests/testthat/test-rolling_tuning.R")'`. Expected: FAIL, function not found.

- [ ] **Step 3: Implement the three functions.**

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/team_strength.R tests/testthat/
git commit -m "Add rolling baseline ratings and rating parameter tuning"
```

---

### Task 5: Game features

**Files:**
- Create: `R/features.R`, `tests/testthat/test-features.R`

**Interfaces:**
- Consumes: cleaned schedules (Task 1); `team_home_venue`, `haversine_miles`, `utc_offset_hours`, `data/stadiums.csv` (Task 2); a ratings tibble of the Task 3/4 shape.
- Produces:
  - `build_features(sched, ratings, stadiums) -> tibble` — one row per completed game, columns exactly as the spec's feature table: `game_id, season, week, gameday, margin, win, pass_off_diff, rush_off_diff, pass_def_diff, rush_def_diff, home, rest_diff, home_off_bye, away_off_bye, home_short_week, away_short_week, away_travel_miles, tz_crossed, div_game, primetime, home_qb_changed, away_qb_changed, home_qb_backup, away_qb_backup, dome, temp, wind, turf, spread`, plus attribute `"dropped"` = number of games dropped for missing required features.
  - `FEATURE_GROUPS` — named list:
    ```r
    FEATURE_GROUPS <- list(
      strength    = c("pass_off_diff","rush_off_diff","pass_def_diff","rush_def_diff"),
      situational = c("home","rest_diff","home_off_bye","away_off_bye","home_short_week",
                      "away_short_week","away_travel_miles","tz_crossed","div_game","primetime"),
      qb          = c("home_qb_changed","away_qb_changed","home_qb_backup","away_qb_backup"),
      environment = c("dome","temp","wind","turf"))
    BINARY_FEATURES <- c("home","home_off_bye","away_off_bye","home_short_week","away_short_week",
                         "div_game","primetime","home_qb_changed","away_qb_changed",
                         "home_qb_backup","away_qb_backup","dome","turf")
    ```

Definitions (from the spec): `home = location != "Neutral"`; `rest_diff = home_rest - away_rest`; off bye = rest ≥ 13; short week = rest ≤ 5; `away_travel_miles` from the away team's `home_stadium_id` that season to the game's `stadium_id`; `tz_crossed = abs(offset(venue tz) - offset(away home tz))` at kickoff; `primetime = weekday %in% c("Thursday","Monday") | gametime >= "19:00"`; `dome = roof %in% c("dome","closed")`; `turf = surface != "grass"`; dome games get `temp = 70`, `wind = 0`; outdoor NA temp/wind filled with that stadium's median in the same season, then rows still NA dropped. QB flags computed from each team's sequence of starter IDs across all games sorted by `gameday` (crossing seasons): changed = differs from previous game's starter; backup = not the modal starter over the previous 8 games (ties → most recent). NA starter or no prior games → flag 0. `spread = spread_line`.

- [ ] **Step 1: Write the failing tests** using a small hand-built schedule (~6 games) and a constant ratings tibble:

```r
test_that("one row per completed game with correct outcomes", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  expect_equal(nrow(f), dplyr::n_distinct(toy_sched()$game_id))
  tie <- f[f$game_id == "tie_game", ]
  expect_equal(tie$margin, 0); expect_equal(tie$win, 0.5)
})

test_that("neutral-site games: home = 0 and travel goes to the real venue", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  lon <- f[f$game_id == "london_game", ]
  expect_equal(lon$home, 0)
  expect_gt(lon$away_travel_miles, 3000)
})

test_that("playoff weeks get features", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  expect_true("playoff_game" %in% f$game_id)
  expect_false(anyNA(f[f$game_id == "playoff_game", FEATURE_GROUPS$strength]))
})

test_that("dome games get neutral weather", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  d <- f[f$dome == 1, ]
  expect_true(all(d$temp == 70 & d$wind == 0))
})

test_that("missing QB IDs and first games give 0 flags, not NA", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  expect_false(anyNA(f[FEATURE_GROUPS$qb]))
})

test_that("QB change and backup flags", {
  f <- build_features(toy_sched(), toy_ratings(), toy_stadiums())
  expect_equal(f$home_qb_changed[f$game_id == "backup_game"], 1)
  expect_equal(f$home_qb_backup[f$game_id == "backup_game"], 1)
})
```

Put `toy_sched()`, `toy_ratings()`, `toy_stadiums()` in `tests/testthat/helper-toy.R`. `toy_sched()` includes games named `tie_game`, `london_game` (`location = "Neutral"`, London venue), `playoff_game` (`game_type = "DIV"`, week 20), a dome game, and `backup_game` where the home team's starter differs from its previous 3 games, plus one game with `home_qb_id = NA`.

- [ ] **Step 2: Run tests to verify they fail.** Run: `Rscript -e 'testthat::test_file("tests/testthat/test-features.R")'`. Expected: FAIL, function not found.

- [ ] **Step 3: Implement `build_features`, `FEATURE_GROUPS`, `BINARY_FEATURES` in `R/features.R`.** Ratings join on `(season, week, team)` twice (home and away), then diff.

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/features.R tests/testthat/
git commit -m "Add pre-game feature builder"
```

---

### Task 6: Models

**Files:**
- Create: `R/models.R`, `tests/testthat/test-models.R`

**Interfaces:**
- Consumes: `FEATURE_GROUPS`, `BINARY_FEATURES` (Task 5)
- Produces:
  - `MODEL_SPECS` — `list(base = "home", full = unlist(FEATURE_GROUPS, use.names = FALSE), market = c(unlist(FEATURE_GROUPS, use.names = FALSE), "spread"))`
  - `fit_scaler(train, vars) -> list(mean = named numeric, sd = named numeric)` — numeric non-binary vars only.
  - `apply_scaler(df, scaler) -> df`
  - `fit_models(train, vars) -> list(scaler, margin = lm, win = glm)` — margin: `lm(margin ~ vars)`; win: `glm(win ~ vars, family = quasibinomial)`; both on scaled data.
  - `predict_models(fit, newdata) -> tibble(game_id, pred_margin, pred_win)` — `pred_win` on probability scale.

- [ ] **Step 1: Write the failing tests**

```r
test_that("scaler uses training rows only", {
  train <- tibble::tibble(x = c(1, 2, 3), home = 1)
  test  <- tibble::tibble(x = c(100), home = 1)
  sc <- fit_scaler(train, c("x", "home"))
  expect_equal(sc$mean[["x"]], 2)
  expect_false("home" %in% names(sc$mean))
  expect_equal(apply_scaler(test, sc)$x, (100 - 2) / 1)
})

test_that("win model accepts ties and predicts probabilities", {
  set.seed(2)
  d <- tibble::tibble(game_id = 1:300, x = rnorm(300), home = 1,
                      margin = 3 * x + rnorm(300, 0, 10))
  d$win <- ifelse(d$margin > 0, 1, ifelse(d$margin < 0, 0, 0.5)); d$win[1:5] <- 0.5
  fit <- fit_models(d, c("x"))
  p <- predict_models(fit, d)
  expect_true(all(p$pred_win > 0 & p$pred_win < 1))
  expect_gt(coef(fit$margin)[["x"]], 0)
})
```

- [ ] **Step 2: Run tests to verify they fail.** Run: `Rscript -e 'testthat::test_file("tests/testthat/test-models.R")'`. Expected: FAIL, function not found.

- [ ] **Step 3: Implement in `R/models.R`.** Build formulas with `reformulate(vars, response = ...)`. When `vars == "home"` and all training rows have `home == 1`, fit intercept-only (`margin ~ 1`) so the base model still works.

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/models.R tests/testthat/test-models.R
git commit -m "Add margin and win/loss regression models"
```

---

### Task 7: Walk-forward validation and benchmarks

**Files:**
- Create: `R/validate.R`, `tests/testthat/test-validate.R`

**Interfaces:**
- Consumes: `fit_models`, `predict_models`, `MODEL_SPECS` (Task 6)
- Produces:
  - `walk_forward(features, vars, test_seasons = 2018:2026, train_start = 2010L) -> tibble(game_id, season, margin, win, pred_margin, pred_win)` — for each Y, trains on `season %in% train_start:(Y-1)`, predicts `season == Y`.
  - `spread_benchmark(features, test_seasons = 2018:2026, train_start = 2010L) -> same shape` — `pred_margin = spread`; `pred_win` from `glm(win ~ spread, quasibinomial)` fit on the training years each Y.
  - `score_predictions(preds) -> tibble(season, rmse, mae, log_loss, brier, accuracy, n_games)` — log loss clamps probabilities to `[1e-6, 1 - 1e-6]` and uses `-(y log p + (1-y) log(1-p))`, valid for y = 0.5; accuracy excludes ties.
  - `calibration_table(preds, bins = 10) -> tibble(bin, mean_pred, actual_rate, n)`

- [ ] **Step 1: Write the failing tests**

```r
test_that("walk-forward never trains on the test season or later", {
  seen <- list()
  testthat::local_mocked_bindings(fit_models = function(train, vars) {
    seen[[length(seen) + 1]] <<- unique(train$season); structure(list(), class = "stub") })
  testthat::local_mocked_bindings(predict_models = function(fit, newdata)
    tibble::tibble(game_id = newdata$game_id, pred_margin = 0, pred_win = 0.5))
  f <- tibble::tibble(game_id = 1:40, season = rep(2014:2021, each = 5), margin = 1, win = 1, home = 1)
  walk_forward(f, "home", test_seasons = 2018:2021, train_start = 2014L)
  for (i in seq_along(seen)) expect_true(max(seen[[i]]) < 2017 + i)
})

test_that("scoring matches hand-computed values", {
  p <- tibble::tibble(season = 2020L, margin = c(3, -7), win = c(1, 0),
                      pred_margin = c(1, -3), pred_win = c(0.8, 0.4))
  s <- score_predictions(p)
  expect_equal(s$rmse, sqrt((4 + 16) / 2))
  expect_equal(s$brier, ((0.2)^2 + (0.4)^2) / 2)
  expect_equal(s$log_loss, -(log(0.8) + log(0.6)) / 2)
  expect_equal(s$accuracy, 1)
})

test_that("ties are excluded from accuracy but kept in log loss", {
  p <- tibble::tibble(season = 2020L, margin = c(0, 3), win = c(0.5, 1),
                      pred_margin = c(1, 1), pred_win = c(0.9, 0.9))
  s <- score_predictions(p)
  expect_equal(s$accuracy, 1)
  expect_equal(s$n_games, 2)
})
```

- [ ] **Step 2: Run tests to verify they fail.** Run: `Rscript -e 'testthat::test_file("tests/testthat/test-validate.R")'`. Expected: FAIL, function not found.

- [ ] **Step 3: Implement in `R/validate.R`.**

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/validate.R tests/testthat/test-validate.R
git commit -m "Add walk-forward validation, scoring, and spread benchmark"
```

---

### Task 8: Importance

**Files:**
- Create: `R/importance.R`, `tests/testthat/test-importance.R`

**Interfaces:**
- Consumes: `walk_forward`, `score_predictions` (Task 7); `FEATURE_GROUPS` (Task 5); `fit_models` (Task 6)
- Produces:
  - `ablate(features, vars, drop_sets, ...) -> tibble(dropped, season, d_rmse, d_log_loss)` — `drop_sets` is a named list of variable vectors; for each, reruns `walk_forward` without them and reports per-season change vs. the full `vars` (positive = removing it hurt). `...` passes to `walk_forward`.
  - `summarize_ablation(abl) -> tibble(dropped, mean_d_rmse, mean_d_log_loss, seasons_hurt, n_seasons)` — `seasons_hurt` counts seasons with `d_rmse > 0`; sorted by `mean_d_rmse` descending.
  - `coef_table(features, vars, seasons = 2010:2025) -> tibble(model, term, estimate, conf_low, conf_high)` — fits `fit_models` on `seasons`; margin CIs from `confint`, win CIs via `confint.default`.

Group ablation = `ablate(f, MODEL_SPECS$full, FEATURE_GROUPS)`; variable ablation = `ablate(f, MODEL_SPECS$full, as.list(setNames(vars, vars)))`.

- [ ] **Step 1: Write the failing tests**

```r
test_that("a strong variable hurts when dropped and noise does not", {
  set.seed(3)
  n <- 16 * 250
  f <- tibble::tibble(game_id = seq_len(n), season = rep(2010:2025, each = 250),
                      strong = rnorm(n), noise = rnorm(n), home = 1)
  f$margin <- 6 * f$strong + rnorm(n, 0, 12); f$win <- as.numeric(f$margin > 0)
  s <- summarize_ablation(ablate(f, c("strong","noise"),
                                 list(strong = "strong", noise = "noise"),
                                 test_seasons = 2018:2025))
  expect_equal(s$dropped[1], "strong")
  expect_gt(s$mean_d_rmse[s$dropped == "strong"], 1)
  expect_lt(abs(s$mean_d_rmse[s$dropped == "noise"]), 0.1)
  expect_equal(s$seasons_hurt[s$dropped == "strong"], 8)
})

test_that("coef_table gives CIs that contain the estimate", {
  set.seed(4)
  f <- tibble::tibble(game_id = 1:2000, season = rep(2010:2025, length.out = 2000),
                      x = rnorm(2000), home = 1)
  f$margin <- 2 * f$x + rnorm(2000, 0, 10); f$win <- as.numeric(f$margin > 0)
  ct <- coef_table(f, "x")
  row <- ct[ct$model == "margin" & ct$term == "x", ]
  expect_true(row$conf_low < row$estimate && row$estimate < row$conf_high)
})
```

- [ ] **Step 2: Run tests to verify they fail.** Run: `Rscript -e 'testthat::test_file("tests/testthat/test-importance.R")'`. Expected: FAIL, function not found.

- [ ] **Step 3: Implement in `R/importance.R`.**

- [ ] **Step 4: Run tests to verify they pass.** Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/importance.R tests/testthat/test-importance.R
git commit -m "Add ablation importance and coefficient tables"
```

---

### Task 9: Pipeline run on real data

**Files:**
- Create: `R/pipeline.R`, `run.R` (the only script with top-level code: `source` all of `R/`, then `run_all()`)

**Interfaces:**
- Consumes: everything above.
- Produces: `run_all(cache_dir = "data/cache") -> invisible(results)`, saving `data/cache/results.rds` as a list with elements: `n_games` (per season), `dropped`, `tuning` (`tune_rating_params` output), `ratings_ridge`, `ratings_rolling`, `features_ridge`, `features_rolling`, `scores` (tibble with a `model` column: `base`, `full_ridge`, `full_rolling`, `market`, `spread_only`), `calibration` (for `full_ridge`), `ablation_groups`, `ablation_vars`, `ablation_market_vars`, `coefs`.

Steps inside `run_all`: load 2009–2026 pbp and schedules → clean → tune on 2010–2017 with `default_rating_grid()` → ridge ratings 2010–2026 with best params → rolling ratings 2010–2026 → features for both → walk-forward for every model → calibration → three ablations → coefficient table → save. Print a one-line progress message per stage with `message()`.

- [ ] **Step 1: Implement `run_all` and `run.R`.**

- [ ] **Step 2: Run the full test suite.** Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`. Expected: all PASS (the stadium coverage test runs now that data is cached after Step 3; rerun after Step 3 if it skipped).

- [ ] **Step 3: Run the pipeline.** Run: `Rscript run.R`. Expected: completes, `data/cache/results.rds` exists, and these sanity checks hold:
  - `results$scores` for `full_ridge` has average margin RMSE roughly 13–14 points; the closing spread is in the same range and should be at or slightly below `full_ridge`.
  - `full_ridge` beats `base` on RMSE and log loss in every test season.
  - `results$dropped` is under 1% of games.
  If any check fails, stop and debug with superpowers:systematic-debugging before continuing.

- [ ] **Step 4: Run the full test suite again.** Same command. Expected: all PASS with no skips.

- [ ] **Step 5: Commit**

```bash
git add R/pipeline.R run.R
git commit -m "Add end-to-end pipeline"
```

---

### Task 10: Quarto report

**Files:**
- Create: `report.qmd`, `README.md` (how to run: `Rscript run.R`, then `quarto render report.qmd`; how to run tests)

**Interfaces:**
- Consumes: `data/cache/results.rds` (Task 9). The report does no fitting; it only reads results and plots.

Sections, in the spec's order: (1) question and data, with games per season and the dropped count; (2) how team strength is calculated, with chosen tuning parameters and ridge vs. rolling accuracy; (3) coefficients as a dot-and-whisker chart, margin model in points per 1 SD; (4) walk-forward accuracy by season, line chart with all benchmarks, 2026 labeled "partial season"; (5) calibration plot with a 45° reference line; (6) group and variable ablation, bar charts sorted by mean ΔRMSE with `seasons_hurt / n_seasons` labels; (7) after the market, the market-run ablation; (8) caveats (the QB starter caveat from the spec) and next steps (xgboost check, Forecast Lab integration).

Every section ends with a `<!-- NOAH: your interpretation here -->` placeholder. Section text written by Claude is limited to factual descriptions of method and what each chart shows.

- [ ] **Step 1: Write `report.qmd` and `README.md`.** Before writing charts, read the dataviz skill.

- [ ] **Step 2: Render.** Run: `quarto render report.qmd`. Expected: `report.html` is produced with no errors or warnings, and every chart appears.

- [ ] **Step 3: Check the output.** Open `report.html` and confirm each of the 8 sections has its chart or table and a NOAH placeholder, and the 2026 season is labeled partial.

- [ ] **Step 4: Commit**

```bash
git add report.qmd README.md
git commit -m "Add Quarto report"
```

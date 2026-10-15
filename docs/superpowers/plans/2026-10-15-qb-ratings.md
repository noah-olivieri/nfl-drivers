# Individual QB Ratings Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rate every QB weekly (opponent-adjusted ridge, with a rolling benchmark), turn the gap between the actual starter and the QBs behind the team's pass rating into a `qb_adj_diff` feature, and judge it against `full_ridge` with a decision rule fixed in advance.

**Architecture:** A new `R/qb_strength.R` holds the QB ridge, the rolling benchmark, the shared team QB mix and the starter adjustment. Team ratings (`R/team_strength.R`) are not changed. `build_features()` gains an optional `qb` argument, `MODEL_SPECS` gains `full_qb` / `market_qb` variable lists, and a new `R/qb_eval.R` holds subset scoring and `qb_verdict()`. `R/pipeline.R` wires it together and `report.qmd` gets a "QB ratings" section.

**Tech Stack:** R, dplyr, tidyr, purrr, tibble, Matrix, testthat, Quarto. No new packages.

**Spec:** `docs/superpowers/specs/2026-10-15-qb-ratings-design.md`

## Global Constraints

- QB ratings, career dropback counts and the team mix for week *w* use only plays dated strictly before week *w*'s cutoff (`week_cutoffs()`: earliest `gameday` in that season-week).
- QB params are tuned on **2010–2017 only**: grid `qb_half_life` ∈ {16, 32, 64, 128} × `qb_lambda` ∈ {1e-3, 3e-3, 1e-2, 3e-2}, `garbage_weight = 0.25`. Selection: settings within **0.02** of the best all-games RMSE, then lowest QB-change RMSE.
- Experience bins on career dropbacks before the game: `0-99`, `100-499`, `500-1499`, `1500+`.
- The QB ridge window is the current season plus up to **4** prior seasons. Rolling benchmark: last **300** dropbacks, padded with the low-experience mean over seasons *s*−3 through *s*.
- pbp loads from **2006**. Team ratings must give identical output with or without 2006–2008 plays.
- Run names: `base`, `full_ridge`, `full_rolling`, `full_qb_ridge`, `full_qb_rolling`, `market`, `market_qb`, `spread_only`. `MODEL_SPECS` keys (`full`, `full_qb`, `market`, `market_qb`) appear only in code.
- The existing `full` and `market` variable lists must not change (baselines stay fixed).
- Decision rule (spec Section 6) on seasons 2018–2025 only; 2026 never counts.
- `.R` files define functions only; nothing runs on `source()`. Match the existing style: `pkg::fn()` calls, native pipe `|>`, short comments.
- Report interpretation is Noah's: add `<!-- NOAH: your interpretation here -->` placeholders, never written interpretation.
- Commit messages end with:
  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  ```

## How scrambles are handled

nflfastR codes a scramble as `play_type == "run"`, `pass == 1`, `qb_dropback == 1` (checked on 2025: all 1,149 scrambles). That gives three rules:

1. **Team ratings (unchanged):** scrambles are `run` plays, so they count toward team **`rush_off`**, as they do today.
2. **QB fit:** credits **all dropbacks**, scrambles included, as the spec says. A QB's rating covers his passing, sacks and scrambles.
3. **Team QB mix:** uses only dropbacks with **`play_type == "pass"`**, because those are the plays that built `pass_off`. The mix weights each QB by his share of the plays behind the team's pass rating. Scrambles carry no weight there, but the QB's rating (which includes his scrambling) is what gets averaged.

**Consequence (stated in the report caveats):** when the usual starter plays, `qb_adj ≈ 0`, so nothing is double-counted. When a scrambler is replaced, `qb_adj` captures the full drop, including lost scramble value, while team `rush_off` still contains the starter's scrambles for a few weeks. So the backup's team looks slightly better on the ground than it is. The regression learns the weight on `qb_adj_diff` from such games, which partly absorbs this. Moving scrambles out of `rush_off` would change team ratings, which the spec rules out. Task 3 has a test that pins rule 3 (scrambles excluded from the mix).

## Review Focus

1. **Schedule starter IDs that don't match pbp IDs.** If `home_qb_id` / `away_qb_id` used a different ID system in some seasons, every starter would look "unseen" and get the rookie rating, and nothing would error. Expected: the pipeline stops if fewer than 99% of 2010+ starter IDs appear on a dropback. Test: `starter_id_coverage()` in Task 2, plus the guard in Task 8.
2. **Upcoming week with a missing projected starter.** The 2026 schedule lists starters for only some unplayed games. Expected: QB ratings don't error or emit an `NA` QB row, and that side's `qb_adj` is 0. Test in Task 3.
3. **An experience bin with no dropbacks in the fit window** (e.g. starters crossing 1500 career dropbacks before any 1500+ play is in the window). An unpenalized all-zero column makes the ridge singular. Expected: no error; the empty bin borrows the nearest lower bin's effect. Test in Task 2.
4. **Scrambles in the team mix.** Expected: the mix uses only `play_type == "pass"` dropbacks (the plays behind `pass_off`). Test in Task 3.
5. **The week to forecast has no plays yet.** Expected: QB ratings and a mix row exist for every team in that week, so the live forecast gets `qb_adj`. Test in Task 3.

---

### Task 1: Dropback columns in plays, QB fixture, team-rating invariance

**Files:**
- Modify: `R/load_data.R` (`clean_plays()`)
- Create: `tests/testthat/helper-qb.R`
- Modify: `tests/testthat/test-load_data.R`, `tests/testthat/test-team_strength.R`

**Interfaces:**
- Produces:
  - `clean_plays(pbp)` now also returns `qb_dropback` (numeric 0/1) and `qb_id` (character: the play's `id` when `qb_dropback == 1`, else `NA`).
  - Test helpers: `make_qb_fixture(seasons = 2013:2019, n_weeks = 16L, seed = 1) -> list(plays, sched)`, `qb_fixture()` (memoized), `QB_TRUE`, `DEF_TRUE`, `QB_TEST_PARAMS`, `TEAM_TEST_PARAMS`.

- [ ] **Step 1: Write the QB fixture helper**

Create `tests/testthat/helper-qb.R`:

```r
# Multi-season synthetic league with known QB strengths (EPA per dropback):
#   qb_star (+0.4): A in 2013-2017, D from 2018
#   qb_good (+0.2): B throughout (B's only QB)
#   qb_avg  ( 0.0): C, except weeks 8 and 12, when C starts a one-game rookie
#   qb_bad  (-0.2): D in 2013-2017, A from 2018
#   rookie_<season>_<week> (-0.6): one start each, so low experience means worse
# Every team-game has 50 dropbacks (45 passes + 5 scrambles coded as "run") and
# 20 designed runs. Starters pass 1500 career dropbacks during 2014.
QB_TRUE <- c(qb_star = 0.4, qb_good = 0.2, qb_avg = 0, qb_bad = -0.2)
DEF_TRUE <- c(A = 0.1, B = 0, C = -0.1, D = 0)   # EPA allowed per dropback
QB_TEST_PARAMS <- list(qb_half_life = 32, qb_lambda = 1e-2, garbage_weight = 0.25)
TEAM_TEST_PARAMS <- list(half_life = 16, offseason_weeks = 0, lambda = 1e-3, garbage_weight = 0.25)

fixture_qb_true <- function(q) unname(ifelse(q %in% names(QB_TRUE), QB_TRUE[q], -0.6))

fixture_starter <- function(team, season, week) {
  if (team == "A") return(if (season < 2018) "qb_star" else "qb_bad")
  if (team == "D") return(if (season < 2018) "qb_bad" else "qb_star")
  if (team == "B") return("qb_good")
  if (week %in% c(8L, 12L)) paste0("rookie_", season, "_", week) else "qb_avg"
}

make_qb_fixture <- function(seasons = 2013:2019, n_weeks = 16L, seed = 1) {
  set.seed(seed)
  pairings <- list(c("A", "B", "C", "D"), c("A", "C", "B", "D"), c("A", "D", "B", "C"))
  sched <- purrr::map_dfr(seasons, function(s) purrr::map_dfr(seq_len(n_weeks), function(w) {
    pr <- pairings[[(w - 1) %% 3 + 1]]
    flip <- w %% 2 == 0
    home <- if (flip) pr[c(2, 4)] else pr[c(1, 3)]
    away <- if (flip) pr[c(1, 3)] else pr[c(2, 4)]
    tibble::tibble(season = s, week = w, game_id = paste(s, w, home, away, sep = "_"),
                   gameday = as.character(as.Date(paste0(s, "-09-10")) + 7 * (w - 1)),
                   home_team = home, away_team = away, location = "Home",
                   home_qb_id = vapply(home, fixture_starter, "", season = s, week = w),
                   away_qb_id = vapply(away, fixture_starter, "", season = s, week = w))
  }))
  sched$home_score <- round(21 + 25 * (fixture_qb_true(sched$home_qb_id) -
                                         fixture_qb_true(sched$away_qb_id)) + rnorm(nrow(sched), 0, 7))
  sched$away_score <- 21

  side_plays <- function(g, side) {
    other <- if (side == "home") "away" else "home"
    def <- g[[paste0(other, "_team")]]
    qb <- g[[paste0(side, "_qb_id")]]
    tibble::tibble(
      season = g$season, week = g$week, game_id = g$game_id, game_date = as.Date(g$gameday),
      posteam = g[[paste0(side, "_team")]], defteam = def, home_team = g$home_team,
      play_type = c(rep("pass", 45), rep("run", 5), rep("run", 20)),
      qb_dropback = c(rep(1, 50), rep(0, 20)),
      qb_id = c(rep(qb, 50), rep(NA_character_, 20)),
      epa = c(fixture_qb_true(qb) + DEF_TRUE[[def]] + 0.05 * (side == "home") + rnorm(50, 0, 1),
              rnorm(20, 0, 1)),
      wp = 0.5)
  }
  plays <- purrr::map_dfr(seq_len(nrow(sched)), function(i) {
    dplyr::bind_rows(side_plays(sched[i, ], "home"), side_plays(sched[i, ], "away"))
  })
  list(plays = plays, sched = sched)
}

# Built once per test run; the fixture takes about a second.
qb_fixture <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- make_qb_fixture()
    cache
  }
})
```

- [ ] **Step 2: Write the failing tests**

In `tests/testthat/test-load_data.R`, replace the existing `"clean_plays keeps only pass/run plays with EPA"` test with:

```r
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
```

Append to `tests/testthat/test-team_strength.R`:

```r
test_that("team ratings are identical with or without older seasons' plays", {
  fx <- qb_fixture()
  recent <- fx$plays[fx$plays$season >= 2016L, ]
  expect_equal(compute_ridge_ratings(fx$plays, fx$sched, TEAM_TEST_PARAMS, 2017L),
               compute_ridge_ratings(recent, fx$sched, TEAM_TEST_PARAMS, 2017L))
  expect_equal(compute_rolling_ratings(fx$plays, fx$sched, 2017L),
               compute_rolling_ratings(recent, fx$sched, 2017L))
})
```

- [ ] **Step 3: Run tests to verify the new ones fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "load_data|team_strength")'`
Expected: FAIL in `"clean_plays credits dropbacks..."` (no `qb_dropback` / `qb_id` columns). The invariance test should already PASS: it guards existing behavior, so a pass here is correct.

- [ ] **Step 4: Implement**

In `R/load_data.R`, replace `clean_plays()` with:

```r
# Run and pass plays with a valid EPA (drops kneels, spikes and no-play penalties).
# qb_id credits each dropback (pass, sack or scramble) to the QB; scrambles keep
# play_type "run", so team ratings still count them as runs.
clean_plays <- function(pbp) {
  pbp |>
    dplyr::filter(pass == 1 | rush == 1, !is.na(epa), play_type %in% c("pass", "run")) |>
    dplyr::transmute(season, week, game_id, game_date = as.Date(game_date),
                     posteam, defteam, home_team, play_type, epa, wp,
                     qb_dropback = dplyr::coalesce(as.numeric(qb_dropback), 0),
                     qb_id = ifelse(qb_dropback == 1, id, NA_character_)) |>
    clean_plays_teams()
}
```

- [ ] **Step 5: Run the full suite**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: all PASS (FAIL 0).

- [ ] **Step 6: Commit**

```bash
git add R/load_data.R tests/testthat/helper-qb.R tests/testthat/test-load_data.R tests/testthat/test-team_strength.R
git commit -m "Keep QB dropback IDs in plays; add multi-season QB test fixture

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 2: QB ridge ratings

**Files:**
- Create: `R/qb_strength.R`
- Create: `tests/testthat/test-qb_strength.R`

**Interfaces:**
- Consumes: `ridge_solve(X, y, w, lambda, penalty_factor)` and `week_cutoffs(sched)` from `R/team_strength.R`; `clean_plays()` columns from Task 1.
- Produces (all in `R/qb_strength.R`):
  - `QB_SEASONS_BACK` (4L), `EXPERIENCE_BINS` (`c("0-99", "100-499", "500-1499", "1500+")`)
  - `experience_bin(n) -> factor` with levels `EXPERIENCE_BINS`
  - `qb_dropbacks(plays) -> tibble`: rows with `qb_dropback == 1` and non-NA `qb_id`; `attr(, "excluded")` = count of dropbacks with NA `qb_id`
  - `add_career_dropbacks(db) -> tibble`: adds `career_before` (the QB's dropbacks in games dated before this play's game)
  - `career_at(db, qb_ids, cutoff_date) -> integer`
  - `fit_qb_ratings_at(db, season, cutoff_date, params, starters = character()) -> tibble(qb_id, qb_rating, career_dropbacks)`, with `attr(, "bin_ratings")` = named numeric (rating of a QB with no own coefficient, per bin). `db` must already have `career_before`.
  - `scheduled_starters(sched) -> tibble(season, week, team, qb_id)`
  - `compute_qb_ratings(plays, sched, params, seasons) -> tibble(season, week, qb_id, qb_rating, career_dropbacks)`
  - `starter_id_coverage(sched, plays) -> numeric` in [0, 1]
  - `params` is `list(qb_half_life, qb_lambda, garbage_weight)`.

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-qb_strength.R`:

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: FAIL with `could not find function "experience_bin"` (and similar).

- [ ] **Step 3: Implement**

Create `R/qb_strength.R`:

```r
# QB strength: weekly opponent-adjusted QB ratings from a weighted ridge regression
# on dropback EPA, a rolling-average benchmark, and the starter adjustment: how much
# better or worse the actual starter is than the QBs behind the team's pass rating.
#
# params: list(qb_half_life = weeks, qb_lambda = ridge penalty, garbage_weight =
#              weight for plays with win probability outside 0.05-0.95)

QB_SEASONS_BACK <- 4L
EXPERIENCE_BINS <- c("0-99", "100-499", "500-1499", "1500+")

experience_bin <- function(n) {
  cut(n, breaks = c(-Inf, 99, 499, 1499, Inf), labels = EXPERIENCE_BINS)
}

# Dropbacks credited to a QB. Dropbacks with no player ID are left out and counted.
qb_dropbacks <- function(plays) {
  is_db <- plays$qb_dropback == 1
  out <- plays[is_db & !is.na(plays$qb_id), ]
  attr(out, "excluded") <- sum(is_db & is.na(plays$qb_id))
  out
}

# Each play's QB career dropbacks before that play's game (over all plays given).
add_career_dropbacks <- function(db) {
  per_game <- db |>
    dplyr::count(qb_id, game_date, name = "n_game") |>
    dplyr::arrange(qb_id, game_date) |>
    dplyr::group_by(qb_id) |>
    dplyr::mutate(career_before = cumsum(n_game) - n_game) |>
    dplyr::ungroup() |>
    dplyr::select(qb_id, game_date, career_before)
  dplyr::left_join(db, per_game, by = c("qb_id", "game_date"))
}

career_at <- function(db, qb_ids, cutoff_date) {
  tab <- table(db$qb_id[db$game_date < cutoff_date])
  n <- as.integer(tab[qb_ids])
  ifelse(is.na(n), 0L, n)
}

qb_play_weights <- function(db, cutoff_date, params) {
  weeks_ago <- as.numeric(cutoff_date - db$game_date) / 7
  garbage <- !is.na(db$wp) & (db$wp < 0.05 | db$wp > 0.95)
  0.5^(weeks_ago / params$qb_half_life) * ifelse(garbage, params$garbage_weight, 1)
}

# QB ratings to use for games on or after cutoff_date, from dropbacks dated before
# it in `season` and the QB_SEASONS_BACK seasons before. Fits
#   epa ~ qb + defense + home + experience bin
# with qb and defense penalized and home and the bins unpenalized, so a QB with few
# dropbacks is pulled toward the typical QB at his experience level. A rating is
# bin effect + own coefficient, centered so 0 = the window's average dropback.
# `starters` are rated too, even with no dropbacks yet. db needs career_before.
fit_qb_ratings_at <- function(db, season, cutoff_date, params, starters = character()) {
  p <- db[db$season >= season - QB_SEASONS_BACK & db$season <= season & db$game_date < cutoff_date, ]
  if (nrow(p) == 0) stop("No dropbacks before ", cutoff_date, " for season ", season)

  bin <- as.character(experience_bin(p$career_before))
  present <- EXPERIENCE_BINS[EXPERIENCE_BINS %in% bin]
  bin_cols <- paste0("exp_", present[-1])          # the first present bin is the reference
  qb_key <- paste0("qb_", p$qb_id)
  def_key <- paste0("def_", p$defteam)
  qb_cols <- sort(unique(qb_key))
  def_cols <- sort(unique(def_key))
  cols <- c(qb_cols, def_cols, "is_home_off", bin_cols)
  n <- nrow(p)
  home_rows <- which(p$posteam == p$home_team)
  bin_rows <- which(paste0("exp_", bin) %in% bin_cols)
  X <- Matrix::sparseMatrix(
    i = c(seq_len(n), seq_len(n), home_rows, bin_rows),
    j = c(match(qb_key, cols), match(def_key, cols),
          rep(match("is_home_off", cols), length(home_rows)),
          match(paste0("exp_", bin[bin_rows]), cols)),
    x = 1, dims = c(n, length(cols)), dimnames = list(NULL, cols))

  w <- qb_play_weights(p, cutoff_date, params)
  beta <- ridge_solve(X, p$epa, w, lambda = params$qb_lambda,
                      penalty_factor = c(rep(1, length(qb_cols) + length(def_cols)),
                                         rep(0, 1 + length(bin_cols))))

  bin_effect <- stats::setNames(numeric(length(EXPERIENCE_BINS)), EXPERIENCE_BINS)
  for (b in present[-1]) bin_effect[[b]] <- beta[[paste0("exp_", b)]]
  # A bin with no dropbacks in the window borrows the effect of the bin below it.
  for (i in seq_along(EXPERIENCE_BINS)[-1]) {
    if (!(EXPERIENCE_BINS[i] %in% present)) bin_effect[[i]] <- bin_effect[[i - 1]]
  }

  center <- sum(w * (bin_effect[bin] + beta[qb_key])) / sum(w)
  qbs <- union(sort(unique(p$qb_id)), starters[!is.na(starters)])
  own <- unname(beta[paste0("qb_", qbs)])
  own[is.na(own)] <- 0
  career <- career_at(db, qbs, cutoff_date)
  out <- tibble::tibble(qb_id = qbs,
                        qb_rating = unname(bin_effect[as.character(experience_bin(career))]) + own - center,
                        career_dropbacks = career)
  attr(out, "bin_ratings") <- bin_effect - center
  out
}

# One row per team per scheduled game: who the schedule lists as the starter.
scheduled_starters <- function(sched) {
  dplyr::bind_rows(
    dplyr::transmute(sched, season, week, team = home_team, qb_id = home_qb_id),
    dplyr::transmute(sched, season, week, team = away_team, qb_id = away_qb_id)
  )
}

# QB ratings for every season-week in `seasons`, covering every QB with dropbacks
# in the window plus that week's scheduled starters.
compute_qb_ratings <- function(plays, sched, params, seasons) {
  db <- add_career_dropbacks(qb_dropbacks(plays))
  starters <- scheduled_starters(sched)
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    s <- starters$qb_id[starters$season == season & starters$week == week]
    r <- fit_qb_ratings_at(db, season, cutoff_date, params, starters = s)
    tibble::add_column(r, season = season, week = week, .before = 1)
  })
}

# Share of scheduled starter IDs that appear on at least one dropback. Near 1 when
# schedule and play-by-play IDs agree; the pipeline stops if it is low.
starter_id_coverage <- function(sched, plays) {
  ids <- unique(stats::na.omit(c(sched$home_qb_id, sched$away_qb_id)))
  mean(ids %in% plays$qb_id[plays$qb_dropback == 1])
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: all PASS. If `"QB ratings recover the known order"` fails, print `r` and check the fixture: starters should be about 0.2 apart.

- [ ] **Step 5: Commit**

```bash
git add R/qb_strength.R tests/testthat/test-qb_strength.R
git commit -m "Add weekly QB ridge ratings with an experience prior

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 3: Team QB mix and starter adjustment

**Files:**
- Modify: `R/qb_strength.R` (append)
- Modify: `tests/testthat/test-qb_strength.R` (append)

**Interfaces:**
- Consumes: `qb_dropbacks()`, `compute_qb_ratings()` (Task 2); `play_weights(plays, season, cutoff_date, params)`, `week_cutoffs()` from `R/team_strength.R`.
- Produces:
  - `team_qb_mix(plays, qb_ratings, sched, team_params, seasons) -> tibble(season, week, team, qb_mix)`. Works with ridge or rolling QB ratings (any table with `season, week, qb_id, qb_rating`).
  - `add_qb_adjustments(games, qb) -> games` plus `home_qb_adj`, `away_qb_adj`, `qb_adj_diff`. `games` needs `season, week, home_team, away_team, home_qb_id, away_qb_id`. `qb` is `NULL` (all three columns `NA`) or `list(ratings = <QB ratings>, mix = <team_qb_mix output>)`. A missing starter ID gives that side `qb_adj = 0`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-qb_strength.R`:

```r
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
  s <- week_of(fx$sched, 2019L, 13L)
  qb <- ridge_qb(p, s, 2019L)
  cut <- min(as.Date(s$gameday))
  m <- p[p$posteam == "C" & p$qb_dropback == 1 & p$play_type == "pass" &
           p$season %in% 2018:2019 & p$game_date < cut, ]
  w <- play_weights(m, 2019L, cut, TEAM_TEST_PARAMS)
  r <- qb$ratings$qb_rating[qb$ratings$week == 13L][match(m$qb_id, qb$ratings$qb_id[qb$ratings$week == 13L])]
  expect_false(any(m$qb_id == "qb_avg" & m$season == 2019L))
  expect_equal(qb$mix$qb_mix[qb$mix$team == "C"], sum(w * r) / sum(w))
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: the new tests FAIL with `could not find function "team_qb_mix"`.

- [ ] **Step 3: Implement**

Append to `R/qb_strength.R`:

```r
# Each team's QB mix for each season-week: the weighted mean QB rating over exactly
# the plays and weights that built its ridge pass_off (seasons s and s - 1, before
# the cutoff, play_weights() with the team params). Only play_type "pass" dropbacks
# count: scrambles are "run" plays, so they are in rush_off, not pass_off.
team_qb_mix <- function(plays, qb_ratings, sched, team_params, seasons) {
  db <- qb_dropbacks(plays)
  db <- db[db$play_type == "pass", ]
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    p <- db[db$season %in% c(season - 1L, season) & db$game_date < cutoff_date, ]
    r <- qb_ratings[qb_ratings$season == season & qb_ratings$week == week, ]
    p$w <- play_weights(p, season, cutoff_date, team_params)
    p$qb_rating <- r$qb_rating[match(p$qb_id, r$qb_id)]
    if (anyNA(p$qb_rating)) stop("QB ratings missing for the team mix in ", season, " week ", week)
    p |>
      dplyr::group_by(team = posteam) |>
      dplyr::summarise(qb_mix = sum(w * qb_rating) / sum(w), .groups = "drop") |>
      tibble::add_column(season = season, week = week, .before = 1)
  })
}

# Starter rating minus team QB mix for each side, and the home-minus-away gap.
# About 0 when the usual starter plays. A missing starter ID is assumed to be the
# usual starter (adjustment 0). qb = NULL gives NA columns.
add_qb_adjustments <- function(games, qb) {
  if (is.null(qb)) {
    return(dplyr::mutate(games, home_qb_adj = NA_real_, away_qb_adj = NA_real_, qb_adj_diff = NA_real_))
  }
  r <- dplyr::select(qb$ratings, season, week, qb_id, qb_rating)
  m <- dplyr::select(qb$mix, season, week, team, qb_mix)
  games |>
    dplyr::left_join(dplyr::rename(r, home_qb_id = qb_id, home_qb_rating = qb_rating),
                     by = c("season", "week", "home_qb_id")) |>
    dplyr::left_join(dplyr::rename(r, away_qb_id = qb_id, away_qb_rating = qb_rating),
                     by = c("season", "week", "away_qb_id")) |>
    dplyr::left_join(dplyr::rename(m, home_team = team, home_qb_mix = qb_mix),
                     by = c("season", "week", "home_team")) |>
    dplyr::left_join(dplyr::rename(m, away_team = team, away_qb_mix = qb_mix),
                     by = c("season", "week", "away_team")) |>
    dplyr::mutate(home_qb_adj = ifelse(is.na(home_qb_id), 0, home_qb_rating - home_qb_mix),
                  away_qb_adj = ifelse(is.na(away_qb_id), 0, away_qb_rating - away_qb_mix),
                  qb_adj_diff = home_qb_adj - away_qb_adj) |>
    dplyr::select(-home_qb_rating, -away_qb_rating, -home_qb_mix, -away_qb_mix)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/qb_strength.R tests/testthat/test-qb_strength.R
git commit -m "Add team QB mix and starter adjustment

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 4: Rolling QB ratings benchmark

**Files:**
- Modify: `R/qb_strength.R` (append)
- Modify: `tests/testthat/test-qb_strength.R` (append)

**Interfaces:**
- Consumes: `qb_dropbacks()`, `add_career_dropbacks()`, `career_at()`, `scheduled_starters()`, `QB_SEASONS_BACK` (Task 2); `team_qb_mix()`, `add_qb_adjustments()` (Task 3).
- Produces: `compute_rolling_qb_ratings(plays, sched, seasons, n_dropbacks = 300) -> tibble(season, week, qb_id, qb_rating, career_dropbacks)`, the same shape as `compute_qb_ratings()`.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-qb_strength.R`:

```r
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
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: new tests FAIL with `could not find function "compute_rolling_qb_ratings"`.

- [ ] **Step 3: Implement**

Append to `R/qb_strength.R`:

```r
# Benchmark: each QB's mean EPA over his last n_dropbacks dropbacks before the
# cutoff, across seasons and teams, with no opponent adjustment. A QB with fewer
# has the rest filled with the low-experience mean: mean EPA of dropbacks before the
# cutoff in seasons s-3 to s by QBs who had fewer than n_dropbacks career dropbacks
# at the time. Rates the same QBs as compute_qb_ratings().
compute_rolling_qb_ratings <- function(plays, sched, seasons, n_dropbacks = 300) {
  db <- add_career_dropbacks(qb_dropbacks(plays))
  db <- db[order(db$game_date), ]   # order() is stable, so plays keep their in-game order
  starters <- scheduled_starters(sched)
  cw <- dplyr::filter(week_cutoffs(sched), season %in% seasons)
  purrr::pmap_dfr(cw, function(season, week, cutoff_date) {
    past <- db[db$game_date < cutoff_date, ]
    low <- past$epa[past$season >= season - 3L & past$career_before < n_dropbacks]
    low_mean <- if (length(low) > 0) mean(low) else 0
    s <- starters$qb_id[starters$season == season & starters$week == week]
    qbs <- union(sort(unique(past$qb_id[past$season >= season - QB_SEASONS_BACK])), s[!is.na(s)])
    last_n <- past[past$qb_id %in% qbs, ] |>
      dplyr::group_by(qb_id) |>
      dplyr::slice_tail(n = n_dropbacks) |>
      dplyr::summarise(sum_epa = sum(epa), k = dplyr::n(), .groups = "drop")
    i <- match(qbs, last_n$qb_id)
    sum_epa <- ifelse(is.na(i), 0, last_n$sum_epa[i])
    k <- ifelse(is.na(i), 0, last_n$k[i])
    tibble::tibble(season = season, week = week, qb_id = qbs,
                   qb_rating = (sum_epa + (n_dropbacks - k) * low_mean) / n_dropbacks,
                   career_dropbacks = career_at(db, qbs, cutoff_date))
  })
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/qb_strength.R tests/testthat/test-qb_strength.R
git commit -m "Add rolling QB ratings benchmark

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 5: Features and model specs

**Files:**
- Modify: `R/features.R` (`FEATURE_GROUPS`, new `add_qb_change_game()`, `build_features()`)
- Modify: `R/models.R` (`MODEL_SPECS`)
- Modify: `tests/testthat/helper-toy.R` (add `toy_qb()`)
- Modify: `tests/testthat/test-features.R`, `tests/testthat/test-models.R`

**Interfaces:**
- Consumes: `add_qb_adjustments(games, qb)` (Task 3).
- Produces:
  - `FEATURE_GROUPS$qb` = `c("home_qb_changed", "away_qb_changed", "home_qb_backup", "away_qb_backup", "qb_adj_diff")`
  - `add_qb_change_game(df) -> df` plus `qb_change_game` (1 if any of the four QB flags is 1, else 0)
  - `build_features(sched, ratings, stadiums, qb = NULL)`: output gains `qb_adj_diff`, `home_qb_adj`, `away_qb_adj`, `qb_change_game`, and `attr(, "missing_starters")` (count of NA starter IDs among games considered)
  - `MODEL_SPECS$full` (unchanged list), `$full_qb` (= full + `qb_adj_diff`), `$market` (unchanged), `$market_qb` (= market + `qb_adj_diff`)

- [ ] **Step 1: Add the toy QB input**

Append to `tests/testthat/helper-toy.R`:

```r
# QB ratings and team mixes for the toy schedule. Mahomes is KC's usual starter, so
# his adjustment is 0; Henne is 0.5 worse than KC's mix.
toy_qb <- function() {
  weeks <- c(1:7, 20L)
  rating <- c(mahomes = 0.3, henne = -0.2, watson = 0.1, minshew = 0)
  mix <- c(KC = 0.3, HOU = 0.1, JAX = 0)
  list(
    ratings = tidyr::expand_grid(season = 2020L, week = weeks, qb_id = names(rating)) |>
      dplyr::mutate(qb_rating = unname(rating[qb_id]), career_dropbacks = 1000L),
    mix = tidyr::expand_grid(season = 2020L, week = weeks, team = names(mix)) |>
      dplyr::mutate(qb_mix = unname(mix[team]))
  )
}
```

- [ ] **Step 2: Write the failing tests**

In `tests/testthat/test-features.R`, replace `"columns match the spec's feature table"` with:

```r
test_that("columns match the spec's feature table", {
  expect_named(feats(), c("game_id", "season", "week", "gameday", "margin", "win",
                          FEATURE_GROUPS$strength, FEATURE_GROUPS$situational,
                          FEATURE_GROUPS$qb, FEATURE_GROUPS$environment, "spread",
                          "home_qb_adj", "away_qb_adj", "qb_change_game"),
               ignore.order = TRUE)
})
```

In the same file, in `"missing QB IDs and first games give 0 flags, not NA"`, change
`expect_false(anyNA(f[FEATURE_GROUPS$qb]))` to
`expect_false(anyNA(f[setdiff(FEATURE_GROUPS$qb, "qb_adj_diff")]))`.

Append to `tests/testthat/test-features.R`:

```r
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
```

In `tests/testthat/test-models.R`, append:

```r
test_that("MODEL_SPECS: QB specs add only the QB gap; baselines exclude it", {
  expect_false("qb_adj_diff" %in% MODEL_SPECS$full)
  expect_false("qb_adj_diff" %in% MODEL_SPECS$market)
  expect_equal(setdiff(MODEL_SPECS$full_qb, MODEL_SPECS$full), "qb_adj_diff")
  expect_equal(setdiff(MODEL_SPECS$market_qb, MODEL_SPECS$market), "qb_adj_diff")
  expect_equal(setdiff(MODEL_SPECS$market_qb, MODEL_SPECS$full_qb), "spread")
})
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "features|models")'`
Expected: FAIL (unused argument `qb`, missing columns, no `full_qb` spec).

- [ ] **Step 4: Implement**

In `R/features.R`, change the `qb` entry of `FEATURE_GROUPS` to:

```r
  qb          = c("home_qb_changed", "away_qb_changed", "home_qb_backup", "away_qb_backup",
                  "qb_adj_diff"),
```

Add after `qb_flags()`:

```r
# 1 when either team's starter changed or a backup started: the games where QB
# ratings should matter most.
add_qb_change_game <- function(df) {
  dplyr::mutate(df, qb_change_game = as.numeric(home_qb_changed == 1 | away_qb_changed == 1 |
                                                  home_qb_backup == 1 | away_qb_backup == 1))
}
```

In `build_features()`:

1. Change the signature to `build_features <- function(sched, ratings, stadiums, qb = NULL) {`.
2. `qb` is now the argument name, so rename the local QB-flag table: change the first line of the body, `qb <- qb_flags(sched)`, to `flags <- qb_flags(sched)`.
3. Right after the `games <- games |> dplyr::mutate(dome = ...) |> fill_median(...)` block, add:
   ```r
   n_missing_starters <- sum(is.na(games$home_qb_id)) + sum(is.na(games$away_qb_id))
   ```
4. Replace the tail of the `out <- games |> ...` pipeline, from `rating_diffs(ratings) |>` to the end of the `select()`, with:
   ```r
       rating_diffs(ratings) |>
       dplyr::left_join(flags, by = "game_id") |>
       add_qb_change_game() |>
       add_qb_adjustments(qb) |>
       dplyr::select(game_id, season, week, gameday, margin, win,
                     dplyr::all_of(unlist(FEATURE_GROUPS, use.names = FALSE)), spread,
                     home_qb_adj, away_qb_adj, qb_change_game)
   ```
5. Replace the `required <- ...` line with:
   ```r
   feature_vars <- unlist(FEATURE_GROUPS, use.names = FALSE)
   # Without QB input the QB gap is NA by design and not required.
   required <- c("margin", "win", if (is.null(qb)) setdiff(feature_vars, "qb_adj_diff") else feature_vars)
   ```
6. After `attr(out, "dropped") <- sum(!complete)`, add `attr(out, "missing_starters") <- n_missing_starters`.

In `R/models.R`, replace `MODEL_SPECS` with:

```r
# full and market keep the variables they had before QB ratings, so they stay the
# baselines; the _qb specs add the QB rating gap.
FULL_VARS <- setdiff(unlist(FEATURE_GROUPS, use.names = FALSE), "qb_adj_diff")
MODEL_SPECS <- list(
  base      = "home",
  full      = FULL_VARS,
  full_qb   = c(FULL_VARS, "qb_adj_diff"),
  market    = c(FULL_VARS, "spread"),
  market_qb = c(FULL_VARS, "qb_adj_diff", "spread")
)
```

- [ ] **Step 5: Run the full suite**

Run: `Rscript -e 'testthat::test_dir("tests/testthat")'`
Expected: all PASS.

- [ ] **Step 6: Commit**

```bash
git add R/features.R R/models.R tests/testthat/helper-toy.R tests/testthat/test-features.R tests/testthat/test-models.R
git commit -m "Add QB rating gap feature, QB-change marker and QB model specs

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 6: QB parameter tuning

**Files:**
- Modify: `R/qb_strength.R` (append)
- Modify: `tests/testthat/test-qb_strength.R` (append)

**Interfaces:**
- Consumes: `compute_qb_ratings()`, `team_qb_mix()`, `add_qb_adjustments()` (Tasks 2–3); `qb_flags()`, `add_qb_change_game()` (`R/features.R`); `rating_diffs()`, `compute_ridge_ratings()` (`R/team_strength.R`).
- Produces:
  - `default_qb_grid() -> tibble(qb_half_life, qb_lambda, garbage_weight)` (16 rows)
  - `select_qb_params(scores, tol = 0.02) -> one-row tibble`
  - `tune_qb_params(plays, sched, team_ratings, team_params, grid, seasons = 2010:2017, qb_fn = compute_qb_ratings, tol = 0.02) -> list(best = list(qb_half_life, qb_lambda, garbage_weight), scores = tibble(<grid cols>, rmse, rmse_qb_change))`

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-qb_strength.R`:

```r
test_that("the selector takes the best QB-change setting within 0.02 of the best overall", {
  scores <- tibble::tibble(qb_half_life = c(16, 32, 64, 128), qb_lambda = 0.01,
                           rmse = c(13.00, 13.01, 13.02, 13.05),
                           rmse_qb_change = c(14.0, 13.6, 13.5, 13.0))
  expect_equal(select_qb_params(scores)$qb_half_life, 64)   # 128 is 0.05 off: excluded
  expect_equal(select_qb_params(scores, tol = 0)$qb_half_life, 16)
})

test_that("default grid is the spec's 4 x 4 grid", {
  g <- default_qb_grid()
  expect_equal(nrow(g), 16)
  expect_setequal(g$qb_half_life, c(16, 32, 64, 128))
  expect_setequal(g$qb_lambda, c(1e-3, 3e-3, 1e-2, 3e-2))
})

test_that("tuning scores both subsets and never uses later seasons", {
  fx <- qb_fixture()
  grid <- tibble::tibble(qb_half_life = c(16, 64), qb_lambda = 1e-2, garbage_weight = 0.25)
  team_r <- compute_ridge_ratings(fx$plays, fx$sched, TEAM_TEST_PARAMS, 2016:2017)
  full <- tune_qb_params(fx$plays, fx$sched, team_r, TEAM_TEST_PARAMS, grid, seasons = 2016:2017)
  early <- tune_qb_params(fx$plays[fx$plays$season < 2018L, ], fx$sched[fx$sched$season < 2018L, ],
                          team_r, TEAM_TEST_PARAMS, grid, seasons = 2016:2017)
  expect_equal(full, early)
  expect_named(full$scores, c("qb_half_life", "qb_lambda", "garbage_weight", "rmse", "rmse_qb_change"))
  expect_true(all(is.finite(full$scores$rmse_qb_change)))
  expect_named(full$best, c("qb_half_life", "qb_lambda", "garbage_weight"))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: new tests FAIL with `could not find function "select_qb_params"`.

- [ ] **Step 3: Implement**

Append to `R/qb_strength.R`:

```r
default_qb_grid <- function() {
  tidyr::expand_grid(qb_half_life = c(16, 32, 64, 128), qb_lambda = c(1e-3, 3e-3, 1e-2, 3e-2),
                     garbage_weight = 0.25)
}

# Among settings within `tol` RMSE points of the best all-games score (nearly flat),
# pick the one that does best on QB-change games.
select_qb_params <- function(scores, tol = 0.02) {
  near <- scores[scores$rmse <= min(scores$rmse) + tol, ]
  near[which.min(near$rmse_qb_change), ]
}

# Score each QB setting by how well that week's ratings explain that week's margins
# (team rating diffs + QB gap + home), on all games and on QB-change games, using
# only `seasons` (2010-2017 by default). Team rating params stay fixed.
tune_qb_params <- function(plays, sched, team_ratings, team_params, grid, seasons = 2010:2017,
                           qb_fn = compute_qb_ratings, tol = 0.02) {
  games <- sched |>
    dplyr::left_join(qb_flags(sched), by = "game_id") |>
    add_qb_change_game() |>
    dplyr::filter(season %in% seasons) |>
    dplyr::mutate(margin = home_score - away_score, home = as.numeric(location != "Neutral")) |>
    rating_diffs(team_ratings)
  scores <- purrr::pmap_dfr(grid, function(...) {
    params <- list(...)
    qbr <- qb_fn(plays, sched, params, seasons)
    mix <- team_qb_mix(plays, qbr, sched, team_params, seasons)
    d <- add_qb_adjustments(games, list(ratings = qbr, mix = mix))
    fit <- stats::lm(margin ~ pass_off_diff + rush_off_diff + pass_def_diff + rush_def_diff +
                       qb_adj_diff + home, data = d, na.action = stats::na.exclude)
    res <- stats::residuals(fit)
    tibble::as_tibble(params) |>
      dplyr::mutate(rmse = sqrt(mean(res^2, na.rm = TRUE)),
                    rmse_qb_change = sqrt(mean(res[d$qb_change_game == 1]^2, na.rm = TRUE)))
  })
  list(best = as.list(select_qb_params(scores, tol)[names(grid)]), scores = scores)
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_strength")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/qb_strength.R tests/testthat/test-qb_strength.R
git commit -m "Add QB rating parameter tuning with QB-change tie-break

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 7: Subset scoring and the decision rule

**Files:**
- Create: `R/qb_eval.R`
- Create: `tests/testthat/test-qb_eval.R`

**Interfaces:**
- Consumes: `score_predictions(preds)` (`R/validate.R`); `paired_bootstrap()` output columns `d_rmse, d_rmse_high, d_log_loss, d_log_loss_high` (`R/bootstrap.R`).
- Produces:
  - `score_qb_subsets(preds, features, models) -> tibble(model, season, rmse, mae, log_loss, brier, accuracy, n_games, subset)` with `subset` ∈ `"qb_change"`, `"other"`
  - `qb_season_rmse(ref, qb) -> tibble(season, rmse_ref, rmse_qb)`
  - `qb_verdict(boot_all, boot_subset, season_rmse) -> tibble(path1_all_games, path2_qb_change, earns_place, seasons_better, n_seasons)`

- [ ] **Step 1: Write the failing tests**

Create `tests/testthat/test-qb_eval.R`:

```r
boot_row <- function(d_rmse, d_rmse_high, d_log_loss = -0.001, d_log_loss_high = -0.0001) {
  tibble::tibble(d_rmse = d_rmse, d_rmse_high = d_rmse_high,
                 d_log_loss = d_log_loss, d_log_loss_high = d_log_loss_high)
}
seasons_better <- function(k) {   # k of 8 seasons where the QB model has lower RMSE
  tibble::tibble(season = 2018:2025, rmse_ref = 13, rmse_qb = c(rep(12.9, k), rep(13.1, 8 - k)))
}
no_gain <- boot_row(0.01, 0.05)

test_that("path 1: all-games intervals below 0 on both metrics and better in 5+ of 8 seasons", {
  v <- qb_verdict(boot_row(-0.05, -0.01), no_gain, seasons_better(5))
  expect_true(v$path1_all_games)
  expect_true(v$earns_place)
  expect_false(qb_verdict(boot_row(-0.05, -0.01), no_gain, seasons_better(4))$path1_all_games)
  expect_false(qb_verdict(boot_row(-0.05, -0.01, -0.001, 0.0002), no_gain,
                          seasons_better(6))$path1_all_games)
})

test_that("path 2: subset RMSE interval below 0 and all-games point estimates no worse", {
  flat <- boot_row(0, 0.03, 0, 0.001)
  v <- qb_verdict(flat, boot_row(-0.3, -0.05), seasons_better(3))
  expect_false(v$path1_all_games)
  expect_true(v$path2_qb_change)
  expect_true(v$earns_place)
})

test_that("path 2 fails on its all-games guard, once on RMSE and once on log loss", {
  subset_gain <- boot_row(-0.3, -0.05)
  expect_false(qb_verdict(boot_row(0.001, 0.03, -0.001, 0.001), subset_gain, seasons_better(3))$path2_qb_change)
  expect_false(qb_verdict(boot_row(-0.001, 0.03, 0.0001, 0.001), subset_gain, seasons_better(3))$path2_qb_change)
})

test_that("neither path: no place earned", {
  v <- qb_verdict(no_gain, boot_row(-0.1, 0.02), seasons_better(2))
  expect_false(v$path1_all_games)
  expect_false(v$path2_qb_change)
  expect_false(v$earns_place)
  expect_equal(c(v$seasons_better, v$n_seasons), c(2, 8))
})

test_that("subset scores split each model's games by the QB-change marker", {
  preds <- list(m = tibble::tibble(game_id = 1:6, season = 2020L, week = 1L,
                                   margin = c(3, -3, 7, 0, 10, -1), win = c(1, 0, 1, 0.5, 1, 0),
                                   pred_margin = 1, pred_win = 0.6))
  features <- tibble::tibble(game_id = 1:6, qb_change_game = c(1, 1, 0, 0, 0, 0))
  s <- score_qb_subsets(preds, features, "m")
  expect_equal(s$n_games[s$subset == "qb_change"], 2)
  expect_equal(s$n_games[s$subset == "other"], 4)
  expect_equal(s$rmse[s$subset == "qb_change"], sqrt(mean(c(2, -4)^2)))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_eval")'`
Expected: FAIL with `could not find function "qb_verdict"`.

- [ ] **Step 3: Implement**

Create `R/qb_eval.R`:

```r
# Judging QB ratings: scores split by QB-change games, and the decision rule fixed in
# the spec before any results (docs/superpowers/specs/2026-10-15-qb-ratings-design.md,
# section 6).

# Season scores for each named model, split into games where a starter changed or a
# backup started ("qb_change") and all other games ("other").
score_qb_subsets <- function(preds, features, models) {
  marker <- dplyr::select(features, game_id, qb_change_game)
  purrr::map_dfr(models, function(m) {
    p <- dplyr::inner_join(preds[[m]], marker, by = "game_id")
    dplyr::bind_rows(
      dplyr::mutate(score_predictions(p[p$qb_change_game == 1, ]), subset = "qb_change"),
      dplyr::mutate(score_predictions(p[p$qb_change_game == 0, ]), subset = "other")
    ) |>
      dplyr::mutate(model = m, .before = 1)
  })
}

qb_season_rmse <- function(ref, qb) {
  dplyr::inner_join(score_predictions(ref), score_predictions(qb), by = "season",
                    suffix = c("_ref", "_qb")) |>
    dplyr::select(season, rmse_ref, rmse_qb)
}

# boot_all / boot_subset: paired_bootstrap(full_ridge, full_qb_ridge) rows on all
# games and on QB-change games (differences are QB model minus full_ridge, so
# negative = QB model better). season_rmse: qb_season_rmse() over 2018-2025.
#   Path 1: both all-games intervals entirely below 0, and lower RMSE in most
#           seasons (at least 5 of 8).
#   Path 2: subset RMSE interval entirely below 0, and all-games RMSE and log loss
#           point estimates no worse (difference <= 0).
qb_verdict <- function(boot_all, boot_subset, season_rmse) {
  better <- season_rmse$rmse_qb < season_rmse$rmse_ref
  path1 <- boot_all$d_rmse_high < 0 && boot_all$d_log_loss_high < 0 && sum(better) > length(better) / 2
  path2 <- boot_subset$d_rmse_high < 0 && boot_all$d_rmse <= 0 && boot_all$d_log_loss <= 0
  tibble::tibble(path1_all_games = path1, path2_qb_change = path2, earns_place = path1 || path2,
                 seasons_better = sum(better), n_seasons = length(better))
}
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'testthat::test_dir("tests/testthat", filter = "qb_eval")'`
Expected: all PASS.

- [ ] **Step 5: Commit**

```bash
git add R/qb_eval.R tests/testthat/test-qb_eval.R
git commit -m "Add QB-change subset scoring and the pre-registered QB verdict

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 8: Pipeline

**Files:**
- Modify: `R/pipeline.R`

**Interfaces:**
- Consumes: everything above.
- Produces: `results.rds` with the existing elements unchanged plus `qb_tuning`, `qb_ratings_ridge`, `qb_ratings_rolling`, `qb_mix_ridge`, `qb_mix_rolling`, `features_qb_rolling`, `scores_qb_subset`, `qb_all_comparison`, `qb_subset_comparison`, `qb_season_scores`, `qb_verdict`, `ablation_qb_groups(_boot)`, `ablation_qb_vars(_boot)`, `ablation_market_qb_groups(_boot)`, `ablation_market_qb_vars(_boot)`, `coefs_qb`, `coefs_market_qb`, `excluded_dropbacks`, `missing_starters`, `starter_id_coverage`, `qb_names`, `qb_starters`.

The pipeline has no unit tests (it is a script over real data). It is verified by running it in Step 3.

- [ ] **Step 1: Edit `run_all()`**

Make these changes in `R/pipeline.R`:

1. Update the header comment's first line to `# End-to-end pipeline: data -> team and QB ratings -> features -> models -> validation,`.

2. After `test_seasons <- 2018:2026`, add `verdict_seasons <- 2018:2025   # 2026 is a live check only`.

3. Replace the `plays <- ...` line with:
   ```r
   # 2006-2008 plays only feed QB history and experience counts; team ratings reach
   # back one season (ridge) or 8 games (rolling), so they are unaffected.
   plays <- clean_plays(load_cached(2006:2026, "pbp", cache_dir, refresh))
   excluded_dropbacks <- attr(qb_dropbacks(plays), "excluded")
   coverage <- starter_id_coverage(dplyr::filter(sched, season >= 2010), plays)
   if (coverage < 0.99) {
     stop("Only ", round(100 * coverage, 1), "% of schedule starter IDs appear in play-by-play; ",
          "QB ratings would treat the rest as unseen.", call. = FALSE)
   }
   ```

4. After the `ratings_rolling <- ...` line, add:
   ```r
   step("Tuning QB rating parameters on 2010-2017")
   qb_tuning <- tune_qb_params(plays, sched, ratings_ridge, tuning$best, default_qb_grid(),
                               seasons = 2010:2017)

   step("Computing QB ratings")
   qb_ratings_ridge <- compute_qb_ratings(plays, sched_rate, qb_tuning$best, seasons)
   qb_ratings_rolling <- compute_rolling_qb_ratings(plays, sched_rate, seasons)
   qb_mix_ridge <- team_qb_mix(plays, qb_ratings_ridge, sched_rate, tuning$best, seasons)
   qb_mix_rolling <- team_qb_mix(plays, qb_ratings_rolling, sched_rate, tuning$best, seasons)
   ```

5. Replace the two `features_* <- build_features(...)` lines with:
   ```r
   # features_ridge carries the ridge QB gap, so every ridge run is scored on the same games.
   features_ridge <- build_features(sched, ratings_ridge, stadiums,
                                    qb = list(ratings = qb_ratings_ridge, mix = qb_mix_ridge))
   features_qb_rolling <- build_features(sched, ratings_ridge, stadiums,
                                         qb = list(ratings = qb_ratings_rolling, mix = qb_mix_rolling))
   features_rolling <- build_features(sched, ratings_rolling, stadiums)
   ```

6. Replace the `preds <- list(...)` block with:
   ```r
   preds <- list(
     base            = walk_forward(features_ridge, MODEL_SPECS$base, test_seasons),
     full_ridge      = walk_forward(features_ridge, MODEL_SPECS$full, test_seasons),
     full_rolling    = walk_forward(features_rolling, MODEL_SPECS$full, test_seasons),
     full_qb_ridge   = walk_forward(features_ridge, MODEL_SPECS$full_qb, test_seasons),
     full_qb_rolling = walk_forward(features_qb_rolling, MODEL_SPECS$full_qb, test_seasons),
     market          = walk_forward(features_ridge, MODEL_SPECS$market, test_seasons),
     market_qb       = walk_forward(features_ridge, MODEL_SPECS$market_qb, test_seasons),
     spread_only     = spread_benchmark(features_ridge, test_seasons)
   )
   ```

7. In the `step("Importance")` block, after `ap_market_vars <- ...`, add:
   ```r
   qb_vars <- MODEL_SPECS$full_qb
   market_qb_vars <- MODEL_SPECS$market_qb
   as_sets <- function(v) as.list(stats::setNames(v, v))
   ap_qb_groups <- ablation_preds(features_ridge, qb_vars, FEATURE_GROUPS, test_seasons = test_seasons)
   ap_qb_vars <- ablation_preds(features_ridge, qb_vars, as_sets(qb_vars), test_seasons = test_seasons)
   ap_market_qb_groups <- ablation_preds(features_ridge, market_qb_vars, FEATURE_GROUPS,
                                         test_seasons = test_seasons)
   ap_market_qb_vars <- ablation_preds(features_ridge, market_qb_vars, as_sets(market_qb_vars),
                                       test_seasons = test_seasons)
   ```

8. Replace `model_pairs <- list(...)` with:
   ```r
   model_pairs <- list(c("spread_only", "full_ridge"), c("spread_only", "market"),
                       c("base", "full_ridge"), c("full_ridge", "full_rolling"),
                       c("full_ridge", "full_qb_ridge"), c("full_qb_ridge", "full_qb_rolling"),
                       c("spread_only", "full_qb_ridge"), c("market", "market_qb"))
   ```

9. Before `results <- list(`, add:
   ```r
   step("QB verdict")
   in_verdict <- function(p) p[p$season %in% verdict_seasons, ]
   change_ids <- features_ridge$game_id[features_ridge$qb_change_game == 1]
   only_change <- function(p) p[p$game_id %in% change_ids, ]
   ref <- in_verdict(preds$full_ridge)
   challenger <- in_verdict(preds$full_qb_ridge)
   qb_all_comparison <- paired_bootstrap(ref, challenger)
   qb_subset_comparison <- paired_bootstrap(only_change(ref), only_change(challenger))
   qb_season_scores <- qb_season_rmse(ref, challenger)
   verdict <- qb_verdict(qb_all_comparison, qb_subset_comparison, qb_season_scores)

   qb_names <- dplyr::bind_rows(
     dplyr::select(sched_all, qb_id = home_qb_id, qb_name = home_qb_name),
     dplyr::select(sched_all, qb_id = away_qb_id, qb_name = away_qb_name)
   ) |>
     dplyr::filter(!is.na(qb_id)) |>
     dplyr::distinct(qb_id, .keep_all = TRUE)
   ```

10. Add to the `results <- list(...)` (after `coefs_market = ...`, keeping every existing element):
    ```r
       coefs_qb = coef_table(features_ridge, qb_vars),
       coefs_market_qb = coef_table(features_ridge, market_qb_vars),
       qb_tuning = qb_tuning,
       qb_ratings_ridge = qb_ratings_ridge,
       qb_ratings_rolling = qb_ratings_rolling,
       qb_mix_ridge = qb_mix_ridge,
       qb_mix_rolling = qb_mix_rolling,
       features_qb_rolling = features_qb_rolling,
       scores_qb_subset = score_qb_subsets(preds, features_ridge,
                                           c("full_ridge", "full_qb_ridge", "full_qb_rolling", "spread_only")),
       qb_all_comparison = qb_all_comparison,
       qb_subset_comparison = qb_subset_comparison,
       qb_season_scores = qb_season_scores,
       qb_verdict = verdict,
       ablation_qb_groups = ablation_deltas(ap_qb_groups),
       ablation_qb_groups_boot = ablation_bootstrap(ap_qb_groups),
       ablation_qb_vars = ablation_deltas(ap_qb_vars),
       ablation_qb_vars_boot = ablation_bootstrap(ap_qb_vars),
       ablation_market_qb_groups = ablation_deltas(ap_market_qb_groups),
       ablation_market_qb_groups_boot = ablation_bootstrap(ap_market_qb_groups),
       ablation_market_qb_vars = ablation_deltas(ap_market_qb_vars),
       ablation_market_qb_vars_boot = ablation_bootstrap(ap_market_qb_vars),
       excluded_dropbacks = excluded_dropbacks,
       missing_starters = attr(features_ridge, "missing_starters"),
       starter_id_coverage = coverage,
       qb_names = qb_names,
       qb_starters = dplyr::filter(scheduled_starters(sched_rate), !is.na(qb_id))
    ```
    (Add a comma after the existing `coefs_market = coef_table(features_ridge, market_vars)` line.)

- [ ] **Step 2: Check it parses and the suite still passes**

Run: `Rscript -e 'source("R/pipeline.R"); testthat::test_dir("tests/testthat")'`
Expected: no parse error; all tests PASS.

- [ ] **Step 3: Run the pipeline**

Run (in the background; the first run downloads 2006–2008 pbp and runs both tunings, which takes a while): `Rscript run.R`
Expected: finishes with `Saved data/cache/results.rds` and no error from the starter-ID guard.

- [ ] **Step 4: Verify the results against the spec**

Run:
```bash
Rscript -e '
r <- readRDS("data/cache/results.rds")
cat("dropped:", r$dropped, " missing starters:", r$missing_starters,
    " excluded dropbacks:", r$excluded_dropbacks, " starter coverage:", r$starter_id_coverage, "\n")
print(r$qb_tuning$best)
f <- r$features_ridge
usual <- f$home_qb_changed == 0 & f$home_qb_backup == 0
cat("median |home_qb_adj| usual starter:", median(abs(f$home_qb_adj[usual])),
    " backup starts:", median(f$home_qb_adj[f$home_qb_backup == 1]), "\n")
s <- dplyr::filter(r$scores, season <= 2025)
print(dplyr::summarise(dplyr::group_by(s, model), rmse = mean(rmse), log_loss = mean(log_loss)))
print(r$qb_verdict)
'
```
Expected:
- `dropped` = 0 (same as before this change), and `full_ridge` mean RMSE 2018–2025 still ≈ 13.2 (baseline unchanged).
- Starter coverage ≥ 0.99.
- Median |adj| for usual starters is near 0 and clearly smaller than for backups. Median backup-start adj is negative.
- `qb_verdict` prints one row. Report whatever it says; do not tune anything to change it.

- [ ] **Step 5: Commit**

```bash
git add R/pipeline.R
git commit -m "Wire QB ratings, QB runs, subset scoring and verdict into the pipeline

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

---

### Task 9: Report, README, spec pointer

**Files:**
- Modify: `report.qmd`
- Modify: `README.md`
- Modify: `docs/superpowers/specs/2026-10-14-nfl-drivers-design.md` (one line)

**Interfaces:**
- Consumes: the `results.rds` elements listed in Task 8.

- [ ] **Step 1: Setup chunk changes in `report.qmd`**

Replace the `model_labels` / `model_cols` definitions with:

```r
model_labels <- c(
  base = "Home field only",
  full_rolling = "Full model, rolling ratings",
  full_ridge = "Full model, ridge ratings",
  market = "Full model + spread",
  spread_only = "Closing spread alone",
  full_qb_ridge = "Full model + QB ratings (ridge)",
  full_qb_rolling = "Full model + QB ratings (rolling)",
  market_qb = "Full model + spread + QB ratings"
)
model_cols <- setNames(pal, names(model_labels)[1:5])   # section 5 charts the original five
```

In `var_labels`, add `qb_adj_diff = "QB rating gap (starter vs. team mix)",` after `away_qb_backup`, and change the `qb` entry to `qb = "QB (status flags; + rating gap in QB models)",`.

- [ ] **Step 2: Renumber sections and keep section 5's chart to the original models**

Change headings `## 3. Effect sizes` … `## 8. Caveats and next steps` to `## 4.` … `## 9.` respectively. In the ablation intro, change "as in section 4" to "as in section 5". In the reading-the-intervals paragraph, change "Sections 6 and 7 test about 50 comparisons between them, so even if none of these variables mattered, a couple of 95% intervals would exclude 0 by chance" to "Sections 7 and 8 test about 100 comparisons between them, so even if none of these variables mattered, about five 95% intervals would exclude 0 by chance". In "After the market", change "The same caution as in section 6 applies: with about 50 intervals across the two sections, expect a couple to exclude 0 by chance" to "The same caution as in section 7 applies: with about 100 intervals across the two sections, expect about five to exclude 0 by chance".

In the `accuracy` chunk, change the first line to:
```r
sc <- res$scores |>
  filter(model %in% names(model_cols)) |>
  mutate(Model = factor(model_labels[model], levels = model_labels[names(model_cols)]))
```

- [ ] **Step 3: Add section 3, "QB ratings", after section 2's NOAH placeholder**

````markdown
## 3. QB ratings

Team pass ratings blend every QB who played for a team. This section rates QBs
individually, so a backup start shows up as a weaker passing game.

Each week, before that week's games, one weighted ridge regression is fit on every
dropback (pass, sack or scramble) from the current and previous four seasons:

> dropback EPA = QB strength + defense strength + home-field + experience level

A QB's experience level is his career dropbacks before the game (0–99, 100–499,
500–1499, 1500+, counted from 2006). QB strengths are shrunk toward zero, so a QB
with few dropbacks is rated close to the typical QB at his experience level, which
for 0–99 dropbacks is well below average. Ratings follow the QB, not the team.

The **starter adjustment** compares the starter with the QBs whose plays built the
team's pass rating: starter rating minus the average rating of those QBs, weighted the
same way the team rating weights their plays. When the usual starter plays it is about
zero. The model uses the home-minus-away difference, the **QB rating gap**.

A simpler benchmark rates each QB by his plain mean EPA over his last 300 dropbacks
(padded with the average of low-experience QBs when he has fewer), with the same
starter adjustment.

QB half-life and ridge penalty were chosen on 2010–2017: among settings within 0.02
RMSE of the best on all games, the one best on games where a starter changed or a
backup started.

```{r qb-tuning}
qb_best <- res$qb_tuning$best
res$qb_tuning$scores |>
  arrange(rmse) |>
  transmute(`Half-life (weeks)` = qb_half_life, `Ridge penalty` = qb_lambda,
            `RMSE, all games` = rmse, `RMSE, QB-change games` = rmse_qb_change,
            Chosen = ifelse(qb_half_life == qb_best$qb_half_life & qb_lambda == qb_best$qb_lambda,
                            "✓", "")) |>
  kable(digits = 4)
```

```{r qb-latest}
qb_season <- max(res$qb_ratings_ridge$season)
qb_week <- max(res$qb_ratings_ridge$week[res$qb_ratings_ridge$season == qb_season])
starters_now <- res$qb_starters |>
  filter(season == qb_season) |>
  arrange(week) |>
  group_by(qb_id) |>
  summarise(team = last(team), .groups = "drop")
qb_table <- res$qb_ratings_ridge |>
  filter(season == qb_season, week == qb_week) |>
  inner_join(starters_now, by = "qb_id") |>
  left_join(res$qb_names, by = "qb_id") |>
  arrange(desc(qb_rating)) |>
  transmute(QB = coalesce(qb_name, qb_id), Team = team, `Rating (EPA/dropback)` = qb_rating,
            `Career dropbacks` = career_dropbacks)
f_qb <- res$features_ridge
backup_adj <- c(f_qb$home_qb_adj[f_qb$home_qb_backup == 1], f_qb$away_qb_adj[f_qb$away_qb_backup == 1])
```

Ratings for **`r qb_season` week `r qb_week`**, among QBs who have started a game this
season. Rating 0 = the average dropback.

```{r qb-top-bottom}
head(qb_table, 10) |> kable()
tail(qb_table, 10) |> kable()
```

Sanity check: in **`r sprintf("%.0f%%", 100 * mean(backup_adj < 0))`** of
`r length(backup_adj)` backup starts (2010 onward), the backup's adjustment is
negative. Dropbacks without a player ID left out: `r res$excluded_dropbacks`. Games
with a missing starter ID (adjustment set to 0): `r res$missing_starters`.

### Accuracy with QB ratings

RMSE by test season, on all games and on games where either team's starter changed
or a backup started (2026 is partial):

```{r qb-accuracy}
qb_models <- c("full_ridge", "full_qb_ridge", "full_qb_rolling", "spread_only")
qb_season_table <- function(df) {
  df |>
    filter(model %in% qb_models) |>
    transmute(Season = partial_label(season), model = model_labels[model], rmse) |>
    pivot_wider(names_from = model, values_from = rmse) |>
    kable()
}
qb_season_table(res$scores)
qb_season_table(filter(res$scores_qb_subset, subset == "qb_change"))
bind_rows(mutate(res$scores, subset = "All games"),
          mutate(filter(res$scores_qb_subset, subset == "qb_change"), subset = "QB-change games")) |>
  filter(model %in% qb_models, season <= 2025) |>
  group_by(Games = subset, Model = model_labels[model]) |>
  summarise(`Margin RMSE` = mean(rmse), `Log loss` = mean(log_loss),
            `Games per season` = mean(n_games), .groups = "drop") |>
  kable()
```

### Do QB ratings earn their place?

The rule was fixed before any results. Seasons 2018–2025 count; 2026 does not. QB
ratings earn their place if either:

- **Path 1 (all games):** the QB model beats the full model on both RMSE and log loss
  (95% interval of the difference entirely below 0), and has lower RMSE in at least 5
  of the 8 seasons; or
- **Path 2 (QB-change games):** on QB-change games, the QB model beats the full model
  on RMSE (interval entirely below 0), and on all games its RMSE and log loss are no
  worse.

```{r qb-verdict}
v <- res$qb_verdict
bind_rows(mutate(res$qb_all_comparison, Games = "All games"),
          mutate(res$qb_subset_comparison, Games = "QB-change games")) |>
  transmute(Subset = Games,
            `Δ RMSE [95% interval]` = fmt_ci(d_rmse, d_rmse_low, d_rmse_high, 3),
            `Δ log loss [95% interval]` = fmt_ci(d_log_loss, d_log_loss_low, d_log_loss_high, 4),
            `Games scored` = n_games) |>
  kable()
```

Δ is the QB model (ridge) minus the full model, 2018–2025; negative means the QB model
is better. Seasons with lower RMSE: **`r v$seasons_better` of `r v$n_seasons`**.
Path 1: **`r ifelse(v$path1_all_games, "passed", "not passed")`**. Path 2:
**`r ifelse(v$path2_qb_change, "passed", "not passed")`**. Verdict: QB ratings
**`r ifelse(v$earns_place, "earn", "do not earn")`** their place.

Other comparisons (all test seasons, 2026 included):

```{r qb-comparisons}
res$model_comparisons |>
  filter(model_b %in% c("full_qb_ridge", "full_qb_rolling", "market_qb")) |>
  transmute(Model = model_labels[model_b], `Compared with` = model_labels[model_a],
            `Δ RMSE [95% interval]` = fmt_ci(d_rmse, d_rmse_low, d_rmse_high, 3),
            `Δ log loss [95% interval]` = fmt_ci(d_log_loss, d_log_loss_low, d_log_loss_high, 4)) |>
  kable()
```

```{r qb-coef}
res$coefs_qb |>
  filter(term == "qb_adj_diff") |>
  transmute(Model = model, Estimate = estimate, `95% low` = conf_low, `95% high` = conf_high) |>
  kable()
```

The QB rating gap coefficient is per 1 SD of the gap (2010–2025 fit): points of
margin in the margin model, log-odds in the win model. Ablation for the QB models is
in sections 7 and 8.

<!-- NOAH: your interpretation here -->
````

- [ ] **Step 4: QB ablation in sections 7 and 8**

In section 7, after the `ablation-vars` chunk and before its NOAH placeholder, add:

````markdown
### QB model (full model + QB ratings)

```{r ablation-qb, fig.height=7.5}
ablation_plot(res$ablation_qb_groups, res$ablation_qb_groups_boot, "Variable groups, QB model")
ablation_table(res$ablation_qb_groups, res$ablation_qb_groups_boot)
ablation_plot(res$ablation_qb_vars, res$ablation_qb_vars_boot, "Individual variables, QB model")
ablation_table(res$ablation_qb_vars, res$ablation_qb_vars_boot)
```
````

In section 8, after the existing `ablation-market` chunk and before its NOAH placeholder, add:

````markdown
### With QB ratings

```{r ablation-market-qb, fig.height=7.5}
ablation_plot(res$ablation_market_qb_groups, res$ablation_market_qb_groups_boot,
              "Variable groups, given the spread, QB model")
ablation_table(res$ablation_market_qb_groups, res$ablation_market_qb_groups_boot)
ablation_plot(res$ablation_market_qb_vars, res$ablation_market_qb_vars_boot,
              "Variables, given the spread, QB model")
ablation_table(res$ablation_market_qb_vars, res$ablation_market_qb_vars_boot)
```
````

- [ ] **Step 5: Caveats**

In section 9's **Caveats** list, after the "Backup QB" bullet, add:

```markdown
- **Scrambles count twice in one case.** Team rush ratings include QB scrambles, and
  QB ratings include them too. When the usual starter plays this cancels out (his
  adjustment is about zero). When a scrambling starter is replaced, the team's rush
  rating still carries his scrambles for a few weeks, so the backup's team looks a
  little better on the ground than it is.
- **QB history starts in 2006.** Career dropback counts for QBs who began before 2006
  are undercounted, but by 2010 they are all in the top experience level anyway.
- **The upcoming week uses projected starters** from the schedule, which can change
  before kickoff.
```

- [ ] **Step 6: README and spec pointer**

In `README.md`'s layout table, add after the `R/team_strength.R` row:

```markdown
| `R/qb_strength.R` | Weekly QB ratings (ridge and rolling), team QB mix, starter adjustment, QB tuning |
| `R/qb_eval.R` | QB-change subset scoring and the QB-ratings decision rule |
```

In the "Using it in the Forecast Lab" paragraph, add a sentence at the end: `For QB-adjusted features, also source `R/qb_strength.R` and pass `qb = list(ratings = compute_qb_ratings(...), mix = team_qb_mix(...))` to `build_features()`.`

In the intro paragraph, change "builds opponent-adjusted team ratings from play-by-play data" to "builds opponent-adjusted team and QB ratings from play-by-play data", and "The first run downloads 18 seasons of play-by-play (about 230 MB" to "The first run downloads 21 seasons of play-by-play (about 270 MB".

In `docs/superpowers/specs/2026-10-14-nfl-drivers-design.md`, change the out-of-scope line `- Individual QB quality ratings` to `- Individual QB quality ratings (designed separately in `2026-10-15-qb-ratings-design.md`)`.

- [ ] **Step 7: Render and check**

Run: `quarto render report.qmd`
Expected: renders with no error. Open `report.html` and check:
- section 3 tables are filled in, with no `NA` model column names
- the verdict sentence reads correctly
- the section 5 chart shows exactly the original five models
- sections 7 and 8 show the QB ablation charts

- [ ] **Step 8: Commit**

```bash
git add report.qmd report.html README.md docs/superpowers/specs/2026-10-14-nfl-drivers-design.md
git commit -m "Add QB ratings section to the report; update README and spec pointer

Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>"
```

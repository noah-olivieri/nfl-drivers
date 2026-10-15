# Upcoming-Week Predictions (Design Spec)

Date: 2026-10-15
Status: Draft for review
Follows: `2026-10-14-nfl-drivers-design.md` (section 1 kept only games with a final score) and `2026-10-15-qb-ratings-design.md`

## Goal

Each pipeline run also gives a win probability for every unplayed game in the forecast week, from every model run, without changing any scored result.

- **Why:** ratings already cover the week about to be played (`rating_schedule()`), but `build_features()` drops games without a score, so nothing predicts them.
- **Done when:** `results$upcoming` holds one row per unplayed forecast-week game per model run, the report shows them in a "Next week" table, and every existing result (scores, bootstrap, ablation, coefficients, QB verdict) is identical to a run without unplayed games.

## Decisions

| Topic | Decision |
|---|---|
| Unplayed games in `build_features()` | Kept, with `margin` and `win` set to `NA` |
| Which unplayed games | Only those whose (season, week) has ratings, i.e. the forecast week. Others are left out and not counted as dropped. |
| Forecast week | The earliest week with an unplayed game, counting from the latest week that has a completed game. A week that's partly played stays the forecast week. |
| Training | Completed games only, enforced in `fit_models()` and `spread_benchmark()` |
| Scoring | Completed games only, enforced in `score_predictions()`, `game_losses()` and `calibration_table()` |
| Models | Every run in `preds` (option A). Each is trained exactly as its walk-forward run for the forecast season (2010 through the previous season). |
| Headline model | `full_qb_ridge` (it passed the pre-registered QB verdict) |
| Spread for upcoming games | `spread_line` as downloaded: the current line, not the closing line |

## 1. Forecast week (`R/load_data.R`)

`rating_schedule(sched_all)` returns every completed game plus every unplayed game of the forecast week.

**Current bug:** the forecast week is the first week after the latest week with a completed game. Once a Thursday game is played, its week counts as "done," so that week's Sunday and Monday games are dropped and week 7 becomes the forecast week. Checked on a test schedule: `w5, w6_thu (played), w6_sun, w6_mon, w7` returns `w5, w6_thu, w7`.

**New rule:** the forecast week is the earliest week with an unplayed game whose (season, week) key is **≥** the latest completed key (was **>**).

| State | Forecast week |
|---|---|
| Week 5 finished, week 6 not started | Week 6 (unchanged) |
| Week 6 Thursday played, Sunday and Monday not | Week 6: its unplayed games are kept and week 7 waits |
| A past week has an unplayed game (BUF–CIN 2022) | That game is a no-contest and is dropped (unchanged) |
| Season over, no unplayed games | None: only completed games |

**No leakage mid-week.** A week's cutoff is its earliest `gameday`, so week 6's ratings use only plays before Thursday. That's true in the backtest too, where every game in a week shares the pre-Thursday ratings. Sunday predictions therefore never see Thursday's plays, the same as the scored games.

**Known gap:** a game postponed past the start of the next week drops out of the forecast once a game in that next week has a score, and comes back only as a completed game. Its pre-game prediction is never reported. This is rare enough to accept.

## 2. Features (`R/features.R`)

`build_features(sched, ratings, stadiums, qb = NULL)`. Inputs and output columns stay the same.

- **Rows:** every completed game in a season that has ratings (as today), plus every unplayed game whose (season, week) appears in `ratings`. An unplayed game is one where `home_score` or `away_score` is `NA`.
- **Outcomes:** `margin = NA` and `win = NA` for unplayed games. `win` is computed only when `margin` is not `NA`. Today the `case_when` default would turn an unplayed game into a tie (`0.5`).
- **Required columns:** a row is kept when every required feature is present. `margin` and `win` are required only for completed games.
- **Counts:** `attr(, "dropped")` still counts completed games only, so the report's dropped figure keeps its meaning. A new `attr(, "dropped_upcoming")` counts unplayed games with ratings that are missing a required feature.
- **Completed rows don't move.** Two inputs are computed from completed games only, so adding unplayed rows can't shift a completed game's features:
  - `team_home_venue()` input: the away team's home stadium. An early-week game still falls back to the most recent season the team hosted.
  - The weather medians in `fill_median()`. Unplayed games are filled from them, but they don't feed them.
  - `qb_flags()` only looks back in date order, so it is unaffected.
- **Upcoming games use:** the schedule's projected starters (for the QB flags, `qb_adj_diff` and `qb_change_game`), the current line as `spread`, and weather filled from stadium medians (nflverse has no forecast temperature or wind).

Callers that pass a completed-only schedule (the Forecast Lab today) get exactly the same output as before.

## 3. Training and scoring (`R/models.R`, `R/validate.R`, `R/bootstrap.R`)

The outcome guard sits where training and scoring happen, so no caller can leak unplayed rows into either:

- `fit_models(train, vars)` drops rows with `NA` `margin` before scaling and fitting. The scaler's means and SDs therefore also come only from completed games. This covers `walk_forward()`, ablation and `coef_table()`.
- `spread_benchmark()` fits its logistic curve on training rows with non-`NA` `win`.
- `score_predictions()`, `game_losses()` (so `paired_bootstrap()`, `compare_models()` and the ablation bootstrap) and `calibration_table()` drop rows with `NA` `margin` or `win`.
- `walk_forward()` and `spread_benchmark()` are otherwise unchanged. Test-season rows include the unplayed games, so they get `pred_margin` and `pred_win` with `NA` outcomes.

New `upcoming_predictions(preds, sched)` in `R/validate.R`: for each run in `preds`, the rows with `NA` `margin`, joined to `sched` for teams, kickoff and projected starters. It returns one long tibble:

`model, game_id, season, week, gameday, gametime, away_team, home_team, away_qb_name, home_qb_name, spread, pred_margin, pred_win`

`pred_margin` and `pred_win` are from the home team's view, matching every other prediction in the project.

## 4. Pipeline (`R/pipeline.R`)

1. `build_features()` gets `sched_rate` (completed games plus the forecast week) instead of `sched` for all three feature sets.
2. Walk-forward runs on those full feature sets, as now, so the forecast-season runs also predict the unplayed games.
3. `results$upcoming <- upcoming_predictions(preds_all, sched_rate)`, then `preds` keeps completed games only. Everything stored under `preds`, `features_ridge`, `features_rolling` and `features_qb_rolling` holds completed games only, so the report and every comparison see exactly what they see today.
4. Ablation, coefficients, the bootstrap and the QB verdict run on the completed-only features and predictions. The guards in section 3 make this belt and braces, not the only line of defense.
5. New `results$upcoming_dropped = attr(features_ridge, "dropped_upcoming")`.

Tuning (`tune_rating_params()`, `tune_qb_params()`) still gets the completed-only `sched` and is unchanged.

## 5. Report (`report.qmd`)

A new **"9. Next week"** section before "Caveats and next steps" (which becomes 10):

- Heading line: season and week, with the number of games. If `results$upcoming` is empty (season over), one sentence says so and there is no table.
- One table, one row per game, ordered by kickoff: **Game** (away @ home), **Kickoff**, **Starters**, **Line (current)**, then the home win probability for each run. `full_qb_ridge` comes first, is marked as the headline model, and also shows its predicted margin.
- Note under the table: `market`, `market_qb` and `spread_only` use the current line for these games, not the closing line they were trained and scored on, so their numbers here aren't comparable to their scored accuracy. They will move as the line moves.
- If `results$upcoming_dropped` > 0, a line saying how many forecast-week games are missing a feature and so aren't shown.
- The existing caveat "The upcoming week uses projected starters" stays and now points to this section.

## 6. Docs

- Main spec, section 1: the "excluding games without a final score" line points here for the forecast week.
- README, "Using it in the Forecast Lab": `build_features()` keeps unplayed games that have ratings, with `NA` outcomes, and filters on `!is.na(margin)` before fitting anything outside `fit_models()`.

## 7. Testing (`tests/testthat/`)

**Forecast week**

- Mid-week: `w5, w6_thu (played), w6_sun, w6_mon, w7` gives `w5, w6_thu, w6_sun, w6_mon`.
- Week finished: week 6 all played gives week 6 plus all of week 7.
- The existing BUF–CIN no-contest test and "next unplayed week only" test still pass.

**Features**

- An unplayed toy game in a week with ratings is kept, with `margin` and `win` `NA` (not `0.5`) and every required feature present.
- Completed rows are identical with and without unplayed rows added, including an unplayed game with recorded weather and one that would change a team's most-used home stadium.
- An unplayed game in a week without ratings is left out and counted in neither `dropped` nor `dropped_upcoming`.
- An unplayed game missing a required feature adds 1 to `dropped_upcoming` and 0 to `dropped`.
- An upcoming game early in a season still gets travel from the away team's previous-season stadium.

**Training and scoring**

- `fit_models()` gives the same coefficients and scaler with and without `NA`-outcome rows added.
- `walk_forward()` predicts unplayed test-season rows, and no training set it builds contains an `NA` outcome (recording `fit_fn`, as in the existing leakage test).
- `spread_benchmark()` fit is unchanged by `NA`-outcome rows, and it predicts them.
- `score_predictions()`, `game_losses()` and `calibration_table()` give identical output with and without `NA`-outcome rows.
- `upcoming_predictions()` returns only the `NA`-margin rows, one per model per game, with team names and starters from the schedule.

## Out of scope

- Closing lines for upcoming games (they don't exist until kickoff), or line-movement tracking
- Training the forecast on completed games from the current season (option C)
- Predicting beyond the forecast week
- Weather forecasts, injury news or late starter changes
- Showing a played forecast-week game's result alongside the predictions

# Individual QB Ratings (Design Spec)

Date: 2026-10-15
Status: Draft for review
Follows: `2026-10-14-nfl-drivers-design.md` (which listed individual QB ratings as out of scope)

## Goal

Make a team's pass rating reflect the QB who actually starts, not the team's blended history.

- **Why:** `pass_off_diff` is the largest coefficient in the model, but it is a team number. A backup start is seen only through the `qb_changed` / `qb_backup` flags. The market prices QBs individually, and `full_ridge` (13.2 RMSE, 2018–2025) trails `spread_only` (12.7).
- **Scorecard:** walk-forward accuracy against `full_ridge`, judged with the paired week-block bootstrap, plus accuracy on games where a starter changed or a backup started. The decision rule is fixed below, before any results.

## Decisions

| Topic | Decision |
|---|---|
| Main method | Separate weekly QB ridge with multi-season memory and an experience prior (`R/qb_strength.R`) |
| Benchmark | Rolling QB EPA/dropback (no opponent adjustment), differing from the main method only in how individual QBs are rated |
| What a QB is credited with | Dropbacks only (passes, sacks, scrambles). Designed QB runs stay in team `rush_off`. |
| How it enters the model | Starter adjustment `qb_adj` = starter rating − rating of the QBs whose plays built the team's pass rating; feature `qb_adj_diff` = home − away |
| Team ratings | Unchanged. `compute_ridge_ratings()` and `compute_rolling_ratings()` keep their inputs, outputs and tuning. |
| Existing QB flags | Kept; ablation shows whether they become redundant |
| Existing model runs | Kept unchanged, so baselines do not move |
| Data | pbp loaded from 2006 for QB history and experience counts; team ratings still effectively use 2009+ |

## Model run names

Specs, code comments and the report refer to models only by these run names (the keys of `preds`).

| Run | Team ratings | QB ratings | Variables |
|---|---|---|---|
| `base` | none | none | home only |
| `full_ridge` | ridge | none | current full set |
| `full_rolling` | rolling | none | current full set |
| `full_qb_ridge` | ridge | ridge (main method) | full set + `qb_adj_diff` |
| `full_qb_rolling` | ridge | rolling (benchmark) | full set + `qb_adj_diff` |
| `market` | ridge | none | full set + `spread` |
| `market_qb` | ridge | ridge (main method) | full set + `spread` + `qb_adj_diff` |
| `spread_only` | none | none | `spread` |

Variable lists live in `MODEL_SPECS` and are named only in code.

## 1. Plays (`R/load_data.R`)

- `clean_plays()` also keeps `qb_dropback` and adds `qb_id`: the play's `id` when `qb_dropback == 1`, otherwise `NA`. `id` is used rather than `passer_player_id` because it is filled on sacks too (`passer_player_id` is missing on about 10% of pass plays).
- Dropbacks with a missing `id` are excluded from the QB fit and the team mix, and their count is reported.
- The pipeline loads pbp for 2006–2026 with the existing cached loader (about 40 MB more, downloaded once).

## 2. QB ratings, main method (`R/qb_strength.R`)

### Model

For each season and week *w*, fit one weighted ridge regression on dropbacks dated **before week *w*'s cutoff** (`week_cutoffs()`), from the current season and up to 4 prior seasons:

```
dropback EPA ~ qb + defense_team + home + experience_bin
```

- `qb` and `defense_team` columns are penalized; `home` and the experience bins are unpenalized (`penalty_factor = 0`), so the bins act as the prior a QB is shrunk toward.
- **Experience bin:** the QB's career dropbacks before that play's game, counted over all loaded plays (2006 onward): `0–99`, `100–499`, `500–1499`, `1500+`.
- **Weights:** `0.5 ^ (calendar_weeks_ago / qb_half_life)` × the team model's garbage-time weight (0.25 when pre-snap win probability is below 0.05 or above 0.95). There is no separate offseason term, since calendar weeks already include the offseason.
- Solved with the existing `ridge_solve()`.

### Rating

- A QB's rating for week *w* = the effect of his experience bin at the cutoff + his own coefficient (0 if he has no dropbacks in the window), in EPA per dropback.
- Ratings are produced for every QB with dropbacks in the window **and** every scheduled starter for week *w*, including starters with no dropbacks yet. An unseen QB's rating is the `0–99` bin's effect.
- Ratings are keyed by QB, not team, so a QB keeps his rating when he changes teams.

### Output

`compute_qb_ratings(plays, sched, params, seasons)` returns a tibble `season, week, qb_id, qb_rating, career_dropbacks`, covering every season-week in `sched` for `seasons` (including the upcoming week from `rating_schedule()`).

### Tuning

`tune_qb_params()` grid-searches `qb_half_life` ∈ {16, 32, 64, 128} × `qb_lambda` ∈ {1e-3, 3e-3, 1e-2, 3e-2} over **2010–2017 only**, with the team rating params fixed at their tuned values.

- Each setting is scored by the RMSE of `lm(margin ~ pass_off_diff + rush_off_diff + pass_def_diff + rush_def_diff + qb_adj_diff + home)`, fit and scored in-sample on that week's ratings, as in `tune_rating_params()`. Two scores are recorded: RMSE on all games, and RMSE on QB-change games (Section 4) from the same fit.
- **Selection rule:** take every setting whose all-games RMSE is within **0.02** of the best all-games RMSE, and among those choose the lowest QB-change RMSE.
- The chosen values are fixed for all later years. The report shows both scores for every setting.

## 3. Rolling QB ratings, benchmark (`R/qb_strength.R`)

`compute_rolling_qb_ratings(plays, sched, seasons, n_dropbacks = 300)` returns the same shape as `compute_qb_ratings()`.

- A QB's rating for week *w* is his mean EPA over his **last 300 dropbacks** before the cutoff, across seasons and teams, unweighted and with no opponent adjustment.
- **Short histories:** a QB with fewer than 300 dropbacks has the missing ones filled with the **low-experience mean**: mean EPA of dropbacks before the cutoff in seasons *s* − 3 through *s* (the current season so far plus the 3 before it), by QBs who had fewer than 300 career dropbacks at the time. An unseen starter's rating is exactly this value.
- `n_dropbacks` is fixed, not tuned, like the team baseline's `n_games = 8`.

## 4. Team QB mix and features

### Team QB mix (`team_qb_mix()`, shared by both QB methods)

`team_qb_mix(plays, qb_ratings, sched, team_params, seasons)` returns `season, week, team, qb_mix`.

- For each season, week and team: the weighted mean of `qb_rating` (as of that week) over the QBs on the team's dropbacks, using **exactly the plays and weights that built the team's ridge `pass_off`**: seasons *s* and *s* − 1, dated before the cutoff, weighted by `play_weights()` with the tuned team params.
- The same plays and weights are used whichever QB ratings are passed in, so `full_qb_ridge` and `full_qb_rolling` differ only in how individual QBs are rated.

### Starter adjustment

- `qb_adj` = starter's `qb_rating` − team `qb_mix`, for each side. When the usual starter plays it is about zero, and exactly zero when he threw every weighted dropback, so his value is not counted twice.
- If a schedule starter ID is ever missing, `qb_adj = 0` (assume the usual starter) and the count is reported. Currently 0 of 4,442 completed 2010–2026 games are missing one.

### Features (`R/features.R`)

- `build_features(sched, ratings, stadiums, qb = NULL)`: `qb` is `list(ratings = <QB ratings>, mix = <team_qb_mix output>)`.
- New columns: `home_qb_adj`, `away_qb_adj`, and **`qb_adj_diff` = home − away**, added to `FEATURE_GROUPS$qb`.
- New non-model column **`qb_change_game`** = 1 when any of `home_qb_changed`, `away_qb_changed`, `home_qb_backup`, `away_qb_backup` is 1. This defines the QB-change subset.
- With `qb = NULL` the three QB-adjustment columns are `NA` and are not required for a row to be kept, so existing callers and the Forecast Lab work unchanged.
- `qb_adj_diff` is numeric and standardized like the other numeric features.
- `MODEL_SPECS` keeps its current entries unchanged and adds the variable lists for the `full_qb_*` runs (full + `qb_adj_diff`) and `market_qb` (market + `qb_adj_diff`).

**Why a separate feature instead of folding the adjustment into `pass_off_diff`:** the team and QB ratings come from different ridge fits with different shrinkage, so their units do not line up one-to-one. A separate term lets the regression learn what a QB gap is worth in points, and its coefficient and ablation answer directly whether QB ratings help. `home_qb_adj` / `away_qb_adj` stay available for a starter-adjusted pass rating in the Forecast Lab.

## 5. Pipeline (`R/pipeline.R`)

After team tuning and team ratings:

1. `tune_qb_params()` on 2010–2017.
2. `compute_qb_ratings()` and `compute_rolling_qb_ratings()` for 2010–2026 on `rating_schedule(sched_all)`, so the upcoming week uses the schedule's projected starters.
3. `team_qb_mix()` for each set of QB ratings.
4. Features:
   - `features_ridge` is built with the ridge QB ratings and serves `base`, `full_ridge`, `full_qb_ridge`, `market`, `market_qb` and `spread_only`, so those comparisons are paired on identical games.
   - `features_qb_rolling` (ridge team ratings + rolling QB ratings) serves `full_qb_rolling`.
   - `features_rolling` (rolling team ratings) still serves `full_rolling`.
5. Walk-forward: `preds` gains `full_qb_ridge`, `full_qb_rolling` and `market_qb`. All existing runs are kept.
6. Paired bootstrap pairs, each `c(reference, challenger)`:
   - (`full_ridge`, `full_qb_ridge`): the main test
   - (`full_qb_ridge`, `full_qb_rolling`)
   - (`spread_only`, `full_qb_ridge`)
   - (`market`, `market_qb`)

   The existing pairs are kept.
7. **QB-change subset:** `score_predictions()` on `qb_change_game == 1` and on `qb_change_game == 0`, for `full_ridge`, `full_qb_ridge`, `full_qb_rolling` and `spread_only`. The main pair is also bootstrapped on the subset by filtering both prediction sets to those game IDs (the existing week-block resampling is unchanged).
8. **Importance:** group ablation, variable ablation and coefficient tables for `full_qb_ridge` and `market_qb`, added alongside the existing ones.
9. `qb_verdict()` (Section 6) on the bootstrap rows.

`results.rds` gains: `qb_tuning`, `qb_ratings_ridge`, `qb_ratings_rolling`, `qb_mix_ridge`, `qb_mix_rolling`, `features_qb_rolling`, `scores_qb_subset`, `qb_subset_comparison`, the new ablation and coefficient tables, `qb_verdict`, and counts of excluded dropbacks and missing starter IDs.

## 6. Decision rule (fixed before any results)

Seasons 2018–2025. 2026 is reported only as a live check and does not count. `qb_verdict()` applies this rule and reports which path passed.

QB ratings earn their place if **either** path holds:

- **Path 1 (all games):** on all games, `full_qb_ridge` beats `full_ridge` on RMSE **and** log loss in the paired bootstrap (the 95% interval for the difference excludes 0 on the improving side), **and** `full_qb_ridge` has lower RMSE than `full_ridge` in at least 5 of the 8 seasons.
- **Path 2 (QB-change games):** on the QB-change subset, `full_qb_ridge` beats `full_ridge` on RMSE in the paired bootstrap (95% interval excludes 0 on the improving side), **and** on all games the point estimates of both RMSE and log loss for `full_qb_ridge` are no worse than for `full_ridge` (difference ≤ 0).

If neither path holds, the report says so, and the subset results are shown either way.

## 7. Report (`report.qmd`)

New section "QB ratings", after the team strength section:

1. How QB ratings and the starter adjustment are calculated
2. Tuning table: every grid setting with all-games and QB-change RMSE, the chosen setting marked
3. Top and bottom 10 QBs by rating as of the latest rated week
4. Sanity check: share of backup starts with negative `qb_adj`
5. Accuracy by season for `full_ridge`, `full_qb_ridge`, `full_qb_rolling` and `spread_only`, on all games and on QB-change games
6. Paired bootstrap results and the `qb_verdict()` outcome
7. `qb_adj_diff` coefficient, and ablation for `full_qb_ridge` and `market_qb`
8. Placeholders for Noah's interpretation

Also:
- README layout table gets a row for `R/qb_strength.R`.
- The original spec's "Individual QB quality ratings" out-of-scope line gets a pointer to this spec.
- The known caveat still applies: schedule QB IDs record the actual starter, so rare surprise scratches leak a little game-day information.

## 8. Testing (`tests/testthat/test-qb_strength.R`, plus additions to existing test files)

All QB tests run on small synthetic fixtures.

**Required cases**

1. **Only QB, every game:** a team with a single QB on all of its dropbacks gets `qb_adj == 0` (tolerance 1e-12), with both ridge and rolling QB ratings.
2. **Unseen backup:** in a fixture where low-experience QBs are clearly worse, a starter with no prior dropbacks gets a negative `qb_adj`. His rating equals the `0–99` bin's effect (ridge) or the low-experience mean (rolling), and his `qb_adj` equals that rating minus the team mix.
3. **QB changes teams:** a strong QB moves from team 1 to team 2.
   - His `qb_rating` is identical whichever team schedules him.
   - Team 2's `qb_adj` with him starting is positive.
   - Team 1's `qb_adj` with its replacement starting is negative.

**Leakage**

- QB ratings, career dropback counts and the team mix for week *w* are unchanged when all plays from week *w* onward are deleted, for both ridge and rolling QB ratings.
- `tune_qb_params()` gives the same result when 2018+ games and plays are removed.

**Integrity**

- **Recovery:** QBs of known strength rotating across teams with known defenses are recovered in the right order.
- **Team ratings unchanged:** `compute_ridge_ratings()` and `compute_rolling_ratings()` give identical output whether plays start in 2006 or 2009.
- **Backward compatibility:** `build_features(..., qb = NULL)` returns the same rows and existing columns as today.
- **Markers and missing data:** `qb_change_game` matches the flags. A missing starter ID gives `qb_adj = 0` and is counted. Dropbacks with no `id` are excluded and counted.
- **Tuning rule:** on a toy score table, the selector picks the best QB-change setting among those within 0.02 of the best all-games RMSE.
- **`qb_verdict()`:** toy bootstrap rows cover Path 1 passing, Path 2 passing, neither path passing, and Path 2 failing on its all-games guard (once on RMSE, once on log loss).

## Out of scope

- QB rushing value (designed runs stay in team `rush_off`)
- Other individual players (receivers, offensive line, pass rushers)
- Folding `qb_adj` into `pass_off` inside the team ratings
- Predicting surprise scratches or injury probabilities for the upcoming week
- Tuning the rolling benchmark

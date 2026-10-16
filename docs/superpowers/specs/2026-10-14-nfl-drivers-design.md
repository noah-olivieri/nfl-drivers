# NFL Game Drivers: Regression Analysis (Design Spec)

Date: 2026-10-14
Status: Draft for review

## Goal

Find which pre-game variables best predict NFL game outcomes, and explain why, using regression models in R.

- **Overall goal:** the best possible model for predicting NFL games. This regression analysis is step one.
- **Standalone first:** this is its own project. Its rating and feature functions are built so the Forecast Lab can source them later without changes.
- **Scorecard:** walk-forward (out-of-sample) accuracy. Every later step (xgboost, market comparison, Forecast Lab integration) must beat these numbers to earn its place.

## Decisions

| Topic | Decision |
|---|---|
| Purpose | Interpretable reasoning now, reused as Forecast Lab model inputs later |
| Outcomes | Point margin (primary, linear regression) and win/loss (check, logistic regression) |
| Variable families | Team strength, situational, QB, environment; closing spread only in a separate "after the market" run |
| Team strength | Calculated in-house: opponent-adjusted ridge ratings, compared against a rolling-average baseline. Published power rankings are not used as inputs. |
| Seasons | 2010–2025 for modeling; 2026 games to date as a live check |
| Validation | Walk-forward by season, never shuffling across years |
| Importance method | Regression coefficients (size) + group and variable ablation (predictive value). No ML model in this step. |
| Output | Quarto report (`report.qmd`) that calls plain `.R` function files |

## Project structure

```
nfl-drivers/
├── R/
│   ├── load_data.R       # download + cache nflreadr data
│   ├── team_strength.R   # weekly ridge ratings + rolling baseline
│   ├── features.R        # one pre-game feature row per game
│   ├── models.R          # margin lm + win/loss logistic
│   ├── validate.R        # walk-forward loop + metrics
│   └── importance.R      # group / variable ablation
├── data/
│   ├── stadiums.csv      # venue lat/lon + time zone (hand-built, committed)
│   └── cache/            # downloaded data (git-ignored)
├── tests/testthat/       # leakage + integrity tests
├── report.qmd            # tables, charts, written reasoning
└── docs/superpowers/specs/
```

`.R` files only define functions; nothing runs on `source()`. Data flows one way:
raw data → team ratings → game features → models → validation and importance → report.

## 1. Data loading (`R/load_data.R`)

- Source: `nflreadr` play-by-play (`load_pbp`) and schedules (`load_schedules`) for seasons 2009–2026. 2009 is loaded only so 2010 week 1 ratings have prior-season carryover.
- Cached to `data/cache/` as `.rds`. A cached season is reused; the current season (2026) is always re-downloaded.
- If a download fails, stop with a clear error naming the season. Never continue on partial data.
- Games only: regular season and playoffs, excluding games without a final score (unplayed 2026 games). The forecast week's unplayed games are kept for prediction only; see `2026-10-15-upcoming-week-design.md`.

## 2. Team strength ratings (`R/team_strength.R`)

### Ridge ratings (main method)

For each season and week *w*, fit one weighted ridge regression on all qualifying plays **before week *w*** of that season, plus the prior season:

```
play EPA ~ offense_team:play_type + defense_team:play_type + home_indicator
```

- `play_type` is pass or run, giving four ratings per team: pass offense, rush offense, pass defense, rush defense.
- Fitted with `glmnet` (`alpha = 0`), using one-hot team-by-play-type columns.
- **Qualifying plays:** `pass == 1` or `rush == 1`, non-missing EPA, excluding penalties with no play, kneels and spikes.
- **Garbage time:** plays with pre-snap win probability below 0.05 or above 0.95 get weight × 0.25.
- **Recency weight:** `0.5 ^ (weeks_ago / half_life)`, where `weeks_ago` counts calendar weeks between the play and week *w*. The offseason counts as extra decay through `offseason_weeks` added to prior-season plays. This produces the preseason carryover: week 1 ratings are last season's, pulled toward average.
- **Tuning:** `half_life`, `offseason_weeks` and the ridge penalty `lambda` are chosen by grid search, scored by the RMSE of a simple `lm(margin ~ four rating diffs + home)` predicting each week's games from that week's ratings, over **2010–2017 only**. The chosen values are then fixed for all later years.
- Output: tibble `season, week, team, pass_off, rush_off, pass_def, rush_def`, where each value is the rating to use **for games in that week**. Defense ratings are signed so higher = better defense.

### Rolling baseline

- Mean offensive and defensive EPA/play (pass and run split the same way) over each team's previous 8 games, crossing season boundaries. No opponent adjustment and no ridge shrinkage.
- Same output shape as the ridge ratings, so the two are interchangeable inputs to `features.R`.

## 3. Game features (`R/features.R`)

One row per game, from the **home team's view** (home minus away where applicable). Neutral-site games set `home = 0`.

| Group | Variable | Definition |
|---|---|---|
| Outcome | `margin` | home score − away score |
| Outcome | `win` | 1 home win, 0 loss, 0.5 tie |
| Strength | `pass_off_diff`, `rush_off_diff`, `pass_def_diff`, `rush_def_diff` | home rating − away rating for that week |
| Situational | `home` | 1 unless neutral site |
| Situational | `rest_diff` | home rest days − away rest days (from schedules) |
| Situational | `home_off_bye`, `away_off_bye` | rest ≥ 13 days |
| Situational | `home_short_week`, `away_short_week` | rest ≤ 5 days |
| Situational | `away_travel_miles` | great-circle miles from the away team's home stadium to the venue (`stadiums.csv`) |
| Situational | `tz_crossed` | absolute time zone difference for the away team |
| Situational | `div_game` | from schedules |
| Situational | `primetime` | Thursday or Monday game, or kickoff 19:00 ET or later |
| QB | `home_qb_changed`, `away_qb_changed` | starter differs from the team's previous game's starter |
| QB | `home_qb_backup`, `away_qb_backup` | starter is not the team's most frequent starter over its previous 8 games |
| Environment | `dome` | roof is dome or closed |
| Environment | `temp`, `wind` | from schedules; set to 70°F and 0 mph for dome games |
| Environment | `turf` | surface is not grass |
| Market (separate run) | `spread` | closing spread from home view (`spread_line`) |

**Missing values:** outdoor games missing temperature or wind get the season median for that stadium; any game still missing a required feature is dropped and counted in the report.

**Known caveat:** schedule QB IDs record the *actual* starter. Rare surprise scratches leak a little game-day information. This is noted in the report, not hidden.

All effects are linear with no interactions in this step.

## 4. Models (`R/models.R`)

- **Margin:** `lm(margin ~ features)`.
- **Win/loss:** `glm(win ~ features, family = quasibinomial)`, so ties at 0.5 are valid.
- Numeric features are standardized using **training-rows-only** means and SDs, so coefficients read as points (or log-odds) per 1 SD. Binary flags are not standardized.
- Three model specs:
  1. `base`: home only (benchmark)
  2. `full`: all non-market features
  3. `market`: `full` + `spread`
- Each spec runs with ridge ratings, and `full` also runs with rolling-baseline ratings.

## 5. Walk-forward validation (`R/validate.R`)

- Test seasons 2018–2026. For test season *Y*, train on all games from 2010 through *Y − 1*.
- 2026 includes completed games only and is reported as a live check, labeled as a partial season.
- Metrics per test season:
  - Margin: RMSE, MAE
  - Win: log loss, Brier score, accuracy (ties excluded from accuracy)
- Calibration: pooled predicted win probability in 10 bins vs. actual win rate.
- Benchmarks in every results table:
  1. `base` (home field only)
  2. `full` with rolling-baseline ratings
  3. Closing spread alone, converted to a margin prediction and to win probability with a logistic fit on training years

## 6. Importance (`R/importance.R`)

- **Group ablation:** remove one group (strength, situational, QB, environment) from `full`, rerun the full walk-forward, and report the change in RMSE and log loss for each test season and on average.
- **Variable ablation:** the same drop-one procedure for each individual variable.
- A group or variable counts as useful only if removing it worsens accuracy in most test seasons, not just on average.
- **Coefficient table:** final `full` model fit on 2010–2025, with 95% confidence intervals.
- **After the market:** the same ablation run on the `market` spec, showing which variables still add value once the spread is known.

## 7. Report (`report.qmd`)

Renders to HTML. Sections:

1. Question and data (seasons, game counts, dropped games)
2. How team strength is calculated, with ridge vs. rolling baseline accuracy
3. Coefficients: effect sizes with confidence intervals
4. Walk-forward accuracy by season vs. benchmarks
5. Calibration plot
6. Importance: group and variable ablation rankings
7. After the market: what still matters given the spread
8. Caveats and next steps (xgboost check, Forecast Lab integration)

Written reasoning in the report is Noah's; placeholders mark where his interpretation goes.

## 8. Testing (`tests/testthat/`)

- **No leakage in ratings:** ratings for week *w* are unchanged when all plays from week *w* onward are deleted.
- **No leakage in walk-forward:** no training row comes from the test season or later.
- **No leakage in scaling:** standardization parameters come only from training rows.
- **Stadium coverage:** every venue in the schedules (2010–2026) has a row in `stadiums.csv`.
- **Feature integrity:** exactly one feature row per completed game; outcome columns match the scores.
- **Rating sanity:** ridge ratings for a small synthetic dataset recover the known team strengths in the right order.

## Packages

`nflreadr`, `dplyr`, `tidyr`, `purrr`, `glmnet`, `ggplot2`, `testthat`, `quarto` (CLI for rendering).

## Out of scope for this step

- xgboost / SHAP check
- Interaction or nonlinear terms
- Individual QB quality ratings (designed separately in `2026-10-15-qb-ratings-design.md`)
- Kalshi / Polymarket prices
- Forecast Lab integration

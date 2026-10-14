# NFL Game Drivers

Which pre-game variables best predict NFL game outcomes, and why? This project builds
opponent-adjusted team ratings from play-by-play data, turns every 2010–2026 game into
a row of pre-game features, and fits regression models for point margin and win/loss.
Every model is scored with walk-forward validation: each season is predicted only by a
model trained on earlier seasons.

It's step one toward the best possible game-prediction model, and its rating and
feature functions are built to be reused in the Forecast Lab.

## Run it

Requires R (4.3+) and [Quarto](https://quarto.org).

```r
install.packages(c("nflreadr", "dplyr", "tidyr", "purrr", "tibble", "readr",
                   "Matrix", "lubridate", "ggplot2", "ggrepel", "knitr", "rmarkdown",
                   "glmnet", "testthat", "withr"))
```

```bash
Rscript run.R                 # download data, fit everything -> data/cache/results.rds
quarto render report.qmd      # build report.html from those results
```

The first run downloads 18 seasons of play-by-play (about 230 MB, cached in
`data/cache/`) and tunes the rating parameters, which takes a while. Later runs reuse
cached seasons and only re-download the current season, so rerunning weekly picks up
new games.

## Tests

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
```

The tests focus on leakage: no rating, scaling step or model may use information from
the game being predicted or anything after it.

## Layout

| File | Job |
|---|---|
| `R/load_data.R` | Download and cache nflverse data; clean schedules and plays |
| `R/venues.R` | Travel distance, time zones, each team's home stadium |
| `R/team_strength.R` | Weekly ridge ratings, rolling baseline, rating tuning |
| `R/features.R` | One pre-game feature row per game |
| `R/models.R` | Margin and win/loss regressions |
| `R/validate.R` | Walk-forward validation, scoring, spread benchmark, calibration |
| `R/importance.R` | Ablation importance and coefficient tables |
| `R/bootstrap.R` | Paired week-block bootstrap for comparing models |
| `R/pipeline.R` | Runs everything and saves the results |
| `data/stadiums.csv` | Venue coordinates and time zones |
| `report.qmd` | The report |
| `docs/superpowers/` | Design spec and implementation plan |

## Using it in the Forecast Lab

`source("R/team_strength.R")` and `source("R/features.R")` (plus `R/load_data.R` and
`R/venues.R`, which they use) give you `compute_ridge_ratings()` and
`build_features()` with no other setup.

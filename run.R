# Run the full analysis: Rscript run.R
# Downloads (and caches) nflverse data, fits everything, and saves
# data/cache/results.rds for report.qmd. First run takes a while; later runs
# reuse cached seasons and only re-download the current season.

suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
run_all()

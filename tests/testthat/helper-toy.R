# A tiny 2020 schedule covering the feature edge cases.
toy_sched <- function() {
  g <- tibble::tribble(
    ~game_id,       ~week, ~game_type, ~gameday,     ~weekday,   ~gametime, ~home_team, ~away_team, ~home_score, ~away_score, ~location, ~stadium_id, ~roof,      ~surface,     ~temp, ~wind, ~home_qb_id, ~away_qb_id, ~home_rest, ~away_rest,
    "g1",           1L,    "REG",      "2020-09-10", "Thursday", "20:20",   "KC",       "HOU",      34,          20,          "Home",    "KAN00",     "outdoors", "grass",      80,    5,     "mahomes",   "watson",    7L,         7L,
    "g2",           2L,    "REG",      "2020-09-20", "Sunday",   "13:00",   "KC",       "JAX",      23,          20,          "Home",    "KAN00",     "outdoors", "grass",      NA,    NA,    "mahomes",   "minshew",   10L,        7L,
    "g3",           3L,    "REG",      "2020-09-27", "Sunday",   "13:00",   "KC",       "HOU",      31,          10,          "Home",    "KAN00",     "outdoors", "grass",      70,    8,     "mahomes",   "watson",    7L,         7L,
    "backup_game",  4L,    "REG",      "2020-10-04", "Sunday",   "16:25",   "KC",       "JAX",      17,          14,          "Home",    "KAN00",     "outdoors", "grass",      60,    10,    "henne",     "minshew",   7L,         7L,
    "tie_game",     5L,    "REG",      "2020-10-11", "Sunday",   "13:00",   "HOU",      "KC",       20,          20,          "Home",    "HOU00",     "closed",   "matrixturf", NA,    NA,    NA,          "mahomes",   14L,        7L,
    "london_game",  6L,    "REG",      "2020-10-18", "Sunday",   "09:30",   "JAX",      "KC",       13,          27,          "Neutral", "LON00",     "outdoors", "grass",      55,    12,    "minshew",   "mahomes",   7L,         7L,
    "jax_home",     7L,    "REG",      "2020-10-25", "Sunday",   "13:00",   "JAX",      "HOU",      24,          21,          "Home",    "JAX00",     "outdoors", "grass ",     85,    6,     "minshew",   "watson",    7L,         4L,
    "playoff_game", 20L,   "DIV",      "2021-01-17", "Sunday",   "15:05",   "KC",       "HOU",      22,          17,          "Home",    "KAN00",     "outdoors", "grass",      30,    15,    "mahomes",   "watson",    7L,         7L
  )
  g |> dplyr::mutate(season = 2020L, div_game = as.integer(away_team == "HOU" & home_team == "JAX"),
                     spread_line = 3.5, .after = game_id)
}

toy_ratings <- function() {
  tidyr::expand_grid(season = 2020L, week = c(1:7, 20L), team = c("KC", "HOU", "JAX")) |>
    dplyr::mutate(pass_off = ifelse(team == "KC", 0.2, 0), rush_off = 0.05,
                  pass_def = 0, rush_def = ifelse(team == "HOU", -0.1, 0))
}

toy_stadiums <- function() {
  tibble::tribble(
    ~stadium_id, ~lat,  ~lon,    ~tz,
    "KAN00",     39.05, -94.48,  "America/Chicago",
    "HOU00",     29.68, -95.41,  "America/Chicago",
    "JAX00",     30.32, -81.64,  "America/New_York",
    "LON00",     51.56, -0.28,   "Europe/London"
  )
}

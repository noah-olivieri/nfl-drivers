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
                   home_qb_id = vapply(home, fixture_starter, "", season = s, week = w, USE.NAMES = FALSE),
                   away_qb_id = vapply(away, fixture_starter, "", season = s, week = w, USE.NAMES = FALSE))
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

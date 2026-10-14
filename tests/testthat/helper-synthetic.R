# Synthetic plays with known offensive strengths: A > B > C > D.
make_synthetic <- function(n_games = 400, seed = 1) {
  set.seed(seed)
  teams <- c("A", "B", "C", "D")
  true_off <- c(A = 0.3, B = 0.1, C = -0.1, D = -0.3)
  g <- tibble::tibble(game_id = seq_len(n_games),
                      game_date = as.Date("2020-09-01") + seq_len(n_games) %/% 4,
                      home_team = sample(teams, n_games, TRUE))
  g$away_team <- vapply(g$home_team, function(h) sample(setdiff(teams, h), 1), "")
  g |>
    tidyr::uncount(40) |>
    dplyr::mutate(posteam = ifelse(dplyr::row_number() %% 2 == 0, home_team, away_team),
                  defteam = ifelse(posteam == home_team, away_team, home_team),
                  play_type = sample(c("pass", "run"), dplyr::n(), TRUE),
                  epa = unname(true_off[posteam]) + rnorm(dplyr::n(), 0, 1),
                  wp = 0.5, season = 2020L, week = 1L)
}

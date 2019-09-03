library(tidyverse)
library(httr)
library(glue)

league_id <- 252353
url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{league_id}")

response <- GET(url, query = list(view = "mMatchup"))
content <- content(response)

format_matchup <- function(z) {
  tibble(
    game = z$id,
    week = z$matchupPeriodId,
    home = c(FALSE, TRUE),
    team = c(z$away$teamId, z$home$teamId),
    score = c(z$away$totalPoints, z$home$totalPoints)
  )
}

matchup_history <- rep(list(NA), length(content))
for (i in seq_along(content)) {
  matchups <- map(content[[i]]$schedule, format_matchup)
  matchup_history[[i]] <- bind_rows(matchups) %>% 
    mutate(year = content[[i]]$seasonId) %>% 
    select(year, game, week, home, team, score)
}

response <- GET(url, query = list(view = "mTeam"))
content <- content(response)

team_history <- rep(list(NA), length(content))
for (i in seq_along(content)) {
  abbrevs <- unlist(transpose(content[[i]]$teams)$abbrev)
  teams <- unlist(transpose(content[[i]]$teams)$id)
  team_history[[i]] <- tibble(team = teams, abbrev = abbrevs) %>% 
    mutate(year = content[[i]]$seasonId)
}

team_history <- bind_rows(team_history)

team_history <- tribble(
  ~team, ~abbrev,
  1,  "AGUS",
  2,  "ROWN",
  3,  "PEPE",
  4,  "BILL",
  5,  "CART",
  6,  "KIER",
  7,  "GRIZ",
  8,  "CHAR",
  9,  "COLN",
  10, "NICK",
  11, "KYLE",
)

bind_rows(matchup_history) %>% 
  left_join(team_history, by = "team") %>% 
  select(
    year,
    game,
    week,
    home,
    team,
    abbrev,
    score
  ) %>% 
  group_by(year, game, week) %>% 
  mutate(win = score == max(score))

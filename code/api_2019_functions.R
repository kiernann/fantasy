library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

fantasy_data <- function(lid, ...) {
  api <- str_c("https://fantasy.espn.com/apis/v3/games/ffl/seasons/2019/segments/0/leagues/", lid)
  data <- content(GET(url = api, query = list(...)))
  return(data)
}

roster_entry <- function(entry) {
  stats <- entry$playerPoolEntry$player$stats
  names(stats) <- map(stats, use_series, "id")
  names <- names(stats)
  names(stats)[names == max(names[str_which(names, "112019")])] <- "proj"
  names(stats)[names == max(names[str_which(names, "014011")])] <- "score"
  tibble(
    year    = stats$score$seasonId,
    week    = stats$score$scoringPeriodId,
    player  = as.character(abs(entry$playerId)),
    first   = entry$playerPoolEntry$player$firstName,
    last    = entry$playerPoolEntry$player$lastName,
    pos     = entry$playerPoolEntry$player$defaultPositionId,
    team    = entry$playerPoolEntry$player$proTeamId,
    owned   = entry$playerPoolEntry$player$ownership$percentOwned/100,
    start   = entry$playerPoolEntry$player$ownership$percentStarted/100,
    slot    = entry$lineupSlotId,
    proj    = stats$proj$appliedTotal,
    score   = stats$score$appliedTotal
  )
}

team_roster <- function(team) {
  abb <- team$abbrev
  roster <- team$roster$entries %>% 
    map_dfr(roster_entry) %>% 
    arrange(slot) %>% 
    mutate(
      slot = slot %>%
        recode(
          "0"  = "QB", 
          "2"  = "RB", 
          "4"  = "WR", 
          "6"  = "TE", 
          "16" = "DF", 
          "17" = "K", 
          "20" = "BE", 
          "23" = "FX"
        ),
      pos = pos %>% 
        recode(
          "1"  = "QB",
          "2"  = "RB", 
          "3"  = "WR",
          "4"  = "TE", 
          "5"  = "K",
          "16" = "D"
        )
    )
  roster %>% 
    mutate(
      abbrev = abb,
      player = str_pad(player, max(nchar(player)), "left", "0")
    ) %>% 
    select(abbrev, everything())
}


data <- fantasy_data(
  lid = 252353, 
  view = "roster", 
  scoringPeriodId = 2
)

score_history <- map_df(data$teams, team_roster)

write_csv(score_history, path = "data/score_history.csv")

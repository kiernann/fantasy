library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

fantasy_data <- function(lid, ...) {
  api <- str_c("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/", lid)
  data <- content(GET(url = api, query = list(...)))
  return(data)
}

roster_entry <- function(entry) {
  tibble(
    player  = entry$playerId,
    slot    = entry$lineupSlotId,
    team    = entry$playerPoolEntry$player$proTeamId,
    first   = entry$playerPoolEntry$player$firstName,
    last    = entry$playerPoolEntry$player$lastName,
    owned   = entry$playerPoolEntry$player$ownership$percentOwned/100,
    started = entry$playerPoolEntry$player$ownership$percentStarted/100
  ) %>% 
    mutate(
      slot = slot %>%
        recode(
          "0"  = "QB", 
          "2"  = "RB", 
          "4"  = "WR", 
          "6"  = "TE", 
          "16" = "DF", 
          "17" = "KI", 
          "20" = "BE", 
          "23" = "FX"
        )
    )
}

x <- fantasy_data(lid = 252353, view = "roster", scoringPeriodId = 1)

map_df(x[[1]]$teams[[2]]$roster$entries, roster_entry)

week <- rep(list(NA), length(x[[1]]$teams))
for (i in seq_along(x[[1]]$teams)) {
  week[[i]] <- 
    x[[1]]$teams[[i]]$roster$entries %>% 
    map_df(roster_entry) %>% 
    arrange(slot)
}

enframe(unlist(x[[1]]$teams[[2]]$roster$entries[[1]]$playerPoolEntry$player$stats[[1]]$stats))

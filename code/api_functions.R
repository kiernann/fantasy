library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

league_id <- 252353
year <- 2018      #change year
mngr <- 1         #change manager

url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{league_id}")
f <- GET(url, query = list(view = "roster", scoringPeriodId = 1))
x <- content(f)

roster_entry <- function(entry) {
  tibble(
    player  = entry$playerId,
    slot    = entry$lineupSlotId,
    team    = entry$playerPoolEntry$player$proTeamId,
    first   = entry$playerPoolEntry$player$firstName,
    last    = entry$playerPoolEntry$player$lastName,
    owned   = entry$playerPoolEntry$player$ownership$percentOwned/100,
    started = entry$playerPoolEntry$player$ownership$percentStarted/100
  )
}

map_df(x[[1]]$teams[[2]]$roster$entries, roster_entry) %>% arrange(slot)

week <- rep(list(NA), length(x[[1]]$teams))
for (i in seq_along(x[[1]]$teams)) {
  week[[i]] <- 
    x[[1]]$teams[[i]]$roster$entries %>% 
    map_df(roster_entry) %>% 
    arrange(slot)
}

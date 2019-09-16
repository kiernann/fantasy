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
  find_proj_stat <- function(stats_list) {
    names <- map(stats_list, names)
    str_detect_any <- function(...) any(str_detect(...))
    which(map_lgl(names, str_detect_any, "projectionAnalysis"))
  }
  stats <- entry$playerPoolEntry$player$stats
  stats <- stats[[find_proj_stat(stats)]]
  tibble(
    id      = entry$playerId,
    name    = entry$playerPoolEntry$player$fullName,
    slot    = entry$lineupSlotId,
    pos     = entry$playerPoolEntry$player$defaultPositionId,
    team    = entry$playerPoolEntry$player$proTeamId,
    owned   = entry$playerPoolEntry$player$ownership$percentOwned/100,
    start   = entry$playerPoolEntry$player$ownership$percentStarted/100,
    points  = 
    proj    = stats$appliedTotal,
    ceil    = stats$appliedTotalCeiling,
    vola    = ifelse(
      is.null(stats$projectionAnalysis$volatility), NA, stats$projectionAnalysis$volatility
    )
  )
}

team_roster <- function(team) {
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
          "20" = "Bench", 
          "23" = "Flex"
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
    mutate(abbrev = team$abbrev) %>% 
    select(abbrev, everything())
}


period <- 1
x <- fantasy_data(252353, view = "roster", scoringPeriodId = period)
team_roster(x$teams[[5]])

rosters <- rep(list(NA), length(x$teams))
for (i in seq_along(x$teams)) {
  roster <- team_roster(x$teams[[i]]$roster$entries)
}

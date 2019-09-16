library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

league_id <- 252353
r <- 1

mngrId <- rep(NA)
plyr <- rep(NA)
proj <- rep(NA)
points <- rep(NA)
week <- rep(NA)
len <- rep(NA)
slot <- rep(NA)
pos <- rep(NA)

for (weekId in 1:2) {
  url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/",year,"/segments/0/leagues/",league_id)
  f <- GET(url, query = list("scoringPeriodId" = weekId, view = "roster"))
  x <- content(f)

  for (mngr in 1:length(x$teams)) {
    nPlyrs <- length(x$teams[[mngr]]$roster$entries)
    for (pl in 1:nPlyrs) {
      plyr[r] <- x$teams[[mngr]]$roster$entries[[pl]]$playerPoolEntry$player$fullName
      y <- x$teams[[mngr]]$roster$entries[[pl]]$playerPoolEntry$player$stats
      for (j in 1:length(y)) {
        if (y[[j]]$scoringPeriodId == weekId){
          type <- y[[j]]$statSourceId
          if (type == 1) {proj[r] <- y[[j]]$appliedTotal}
          else {points[r] <- y[[j]]$appliedTotal}
          week[r] <- y[[j]]$scoringPeriodId
        }
      }
      if (is.na(points[r])) points[r] <- NA
      mngrId[r] <- x$teams[[mngr]]$id
      slot[r] <- x$teams[[mngr]]$roster$entries[[pl]]$lineupSlotId
      pos[r] <- x$teams[[mngr]]$roster$entries[[pl]]$playerPoolEntry$player$defaultPositionId
      r <- r+1
    }
  }
}
team <- tibble(mngrId, week, plyr, proj, points, slot, pos)
team$slot <- as.integer(recode(as.character(team$slot), "23" = "19"))
team <- arrange(team, week, mngrId, slot)
team$slot <- recode(team$slot, "0" = "QB", "2" = "RB", "4" = "WR", "6" = "TE", "16" = "D", "17" = "K", "20" = "Bench", "19" = "Flex")
team$pos <- recode(team$pos, "1" = "QB", "2" = "RB", "3"= "WR", "4" = "TE", "5" = "K", "16" = "D")
print(team)

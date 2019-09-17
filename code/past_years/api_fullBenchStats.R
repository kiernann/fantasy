library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

league_id <- 252353
year <- 2018      #change year
mngr <- 1         #change manager

url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{league_id}")
f <- GET(url, query = list("seasonId" = year, view = "roster"))
x <- content(f)[[1]]

plyr <- rep(NA)
proj <- rep(NA)
points <- rep(NA)
week <- rep(NA)
len <- rep(NA)
pos <- rep(NA)

week <- x$scoringPeriodId
nPlyrs <- length(x$teams[[mngr]]$roster$entries)
for (i in 1:nPlyrs) {
  pl <- i
  plyr[i] <- x$teams[[mngr]]$roster$entries[[pl]]$playerPoolEntry$player$fullName
  y <- x$teams[[mngr]]$roster$entries[[pl]]$playerPoolEntry$player$stats
  for (j in 1:length(y)) {
    if (y[[j]]$scoringPeriodId == week){
      type <- y[[j]]$statSourceId
      if (type == 1) {proj[i] <- y[[j]]$appliedTotal}
      else {points[i] <- y[[j]]$appliedTotal}
      week[i] <- y[[j]]$scoringPeriodId
    }
  }
  pos[i] <- x$teams[[mngr]]$roster$entries[[pl]]$lineupSlotId
}

team <- tibble(plyr, proj, points, pos, week)
team$pos <- as.integer(recode(as.character(team$pos), "23" = "19"))
team <- arrange(team, pos)
team$pos <- recode(team$pos, "0" = "QB", "2" = "RB", "4" = "WR", "6" = "TE", "16" = "D", "17" = "K", "20" = "Bench", "19" = "Flex")
print(team)

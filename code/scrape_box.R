library(tidyverse)
library(rvest)
seasons  <- c("2015", "2016", "2017", "2018")
team <- 
  team <- 
    player <- 
      points <- 
        season <- 
          week <- rep(NA, 9 * length(seasons) * 11 * 12)
start <- 1
for (s in 1:length(seasons)) {
  year <- seasons[s]
  for (weeks in 1:12) {
    for (i in 1:11) {
      end <- start + 8
      url <- paste0("http://games.espn.com/ffl/boxscorequick?leagueId=252353&teamId=", 
                    i, 
                    "&scoringPeriodId=", 
                    weeks, "&seasonId=", 
                    year, 
                    "&view=scoringperiod&version=quick")
      file <- read_html(url)
      teams <- html_text(html_nodes(file, css = "div.teamInfoOwnerData"))
      if (length(teams) == 1) {
        teams <- "Justin Grizwold"
      }
      team[start:end] <- teams[1]
      week[start:end] <- weeks
      season[start:end] <- year
      players <- html_text(html_nodes(file, css = "td.playertablePlayerName"))
      point <- html_text(html_nodes(file, css = "td.playertableStat.appliedPoints"))
      for (a in 1:9) {
        player[start] <- players[a]
        points[start] <- point[a + 1]
        start <- start + 1
      }
    }
  }
}
scores <- tibble(team, player, points, week, season)
scores <- filter(scores, !is.na(player))
print(scores)

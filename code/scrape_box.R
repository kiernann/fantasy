library(tidyverse)
library(rvest)
seasons <- c("2015", "2016", "2017", "2018")
team <- rep(NA, 9*length(seasons)*11*12)
player <- rep(NA, 9*length(seasons)*11*12)
points <- rep(NA, 9*length(seasons)*11*12)
season <- rep(NA, 9*length(seasons)*11*12)
week <- rep(NA, 9*length(seasons)*11*12)
start <- 1
for (s in 1:length(seasons)) {
  year <- seasons[s]
  for (weeks in 1:12) {
    for (i in 1:11) {
      end <- start + 8
      url <- str_glue("http://games.espn.com/ffl/boxscorequick?leagueId=252353&teamId=", i, "&scoringPeriodId=", weeks, "&seasonId=", year, "&view=scoringperiod&version=quick")
      file <- read_html(url)
      teams <- html_text(html_nodes(file, css = "div.teamInfoOwnerData"))
      if (length(teams) == 1) {
        teams <- "Justin Grizwold"
      }
      team[start:end] <- teams[1]
      week[start:end] <- weeks
      season[start:end] <- year
      data <- html_table(html_nodes(file, css = "table.playerTableTable.tableBody"), fill = TRUE)
      if (length(data) == 0) {data <- list(matrix(NA, 12, 5))}
      data <- data[[1]]
      col <- 1
      if (isTRUE(data[3,1] == "SLOT")) {
        col <- col + 1
      }
      players <- data[4:12,col]
      players[match("Total", players)] <- NA
      point <- data[4:12, col + 3]
      for (a in 1:9) {
        player[start] <- players[a]
        points[start] <- point[a]
        start <- start + 1
      }
    }
  }
}
scores <- tibble(team, player, points, week, season)
scores$points <- as.numeric(scores$points)
scores$season <- as.numeric(scores$season)
scores <- filter(scores, !is.na(player))
scores <- filter(scores, !is.na(points))
print(scores)

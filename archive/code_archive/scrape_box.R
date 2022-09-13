library(tidyverse)
library(rvest)
#input the seasons to analyze
seasons <- c("2015", "2016", "2017", "2018")
#pre-allocate vector sizes
team <- rep(NA, 9*length(seasons)*11*12)
player <- rep(NA, 9*length(seasons)*11*12)
pos <- rep(NA, 9*length(seasons)*11*12)
points <- rep(NA, 9*length(seasons)*11*12)
season <- rep(NA, 9*length(seasons)*11*12)
week <- rep(NA, 9*length(seasons)*11*12)
start <- 1
#loop through years
for (s in 1:length(seasons)) {
  year <- seasons[s]
  #loop through each week
  for (weeks in 1:12) {
    #loop through each team
    for (i in 1:11) {
      end <- start + 8
      #generate quick box score url
      url <- str_glue("http://games.espn.com/ffl/boxscorequick?leagueId=252353&teamId=", i, "&scoringPeriodId=", weeks, "&seasonId=", year, "&view=scoringperiod&version=quick")
      file <- read_html(url)
      #find the team name
      teams <- html_text(html_nodes(file, css = "div.teamInfoOwnerData"))
      if (length(teams) == 1) {
        teams <- "Justin Grizwold"
      }
      team[start:end] <- teams[1]
      week[start:end] <- weeks
      season[start:end] <- year
      #extract espn table
      data <- html_table(html_nodes(file, css = "table.playerTableTable.tableBody"), fill = TRUE)
      #create NA matrix if the team doesnt exist (get's around errors)
      if (length(data) == 0) {data <- list(matrix(NA, 12, 5))}
      data <- data[[1]]
      col <- 1
      #change which column to pul from depending on the table format
      if (isTRUE(data[3,1] == "SLOT")) {col <- col + 1}
      #extract players
      players <- data[4:12,col]
      #remove row for Total Score
      players[match("Total", players)] <- NA
      #extract points
      point <- data[4:12, col + 3]
      for (a in 1:9) {
        #separate player from position
        p <- unlist(str_split(players[a], ","))
        posish <- p[2]
        #separate position from team
        posish <- unlist(str_split(posish, intToUtf8(160)))
        #fix NA from D/ST
        if (is.na(posish[1])) {posish[2] <- "D/ST"}
        pos[start] <- posish[2]
        player[start] <- p[1]
        points[start] <- point[a]
        start <- start + 1
      }
    }
  }
}
scores <- tibble(team, player, pos, points, week, season)
scores$points <- as.numeric(scores$points)
scores$season <- as.numeric(scores$season)
#remove NA's
scores <- filter(scores, !is.na(player))
scores <- filter(scores, !is.na(points))
print(scores)

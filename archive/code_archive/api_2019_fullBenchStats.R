library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

league_id <- 252353
year <- 2019
r <- 1

mngrId <- rep(NA)
plyr <- rep(NA)
proj <- rep(NA)
points <- rep(NA)
week <- rep(NA)
len <- rep(NA)
slot <- rep(NA)
pos <- rep(NA)
projId <- rep(NA)
actId <- rep(NA)

for (weekId in 1:3) {
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
          if (type == 1) {
            proj[r] <- y[[j]]$appliedTotal
            projId[r] <- y[[j]]$id
            }
          else {
            points[r] <- y[[j]]$appliedTotal
            actId[r] <- y[[j]]$id
            }
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
data <- tibble(mngrId, week, plyr, proj, points, slot, pos)
data$slot <- as.integer(recode(as.character(data$slot), "23" = "19"))
data <- arrange(data, week, mngrId, slot)
data$slot <- recode(data$slot, "0" = "QB", "2" = "RB", "4" = "WR", "6" = "TE", "16" = "D", "17" = "K", "20" = "Bench", "19" = "Flex")
data$pos <- recode(data$pos, "1" = "QB", "2" = "RB", "3"= "WR", "4" = "TE", "5" = "K", "16" = "D")
data$mngrId <- recode(data$mngrId, "1" = "AGUS", "3" = "PEPE", "4" = "BILL", "5" = "CART", "6" = "KIER", "8" = "CORE", "10" = "NICK", "11" = "KYLE")

mngr <- rep(NA)
week <- rep(NA)
score <- rep(NA)
bestScore <- rep(NA)
r <- 1
mngrs <- c("AGUS", "PEPE", "BILL", "CART", "KIER","CORE", "NICK", "KYLE")

for (i in 1:max(data$week)) {
  for(j in 1:length(unique(data$mngrId))) {
    d <- filter(data, mngrId == mngrs[j] & week == i)
    temp <- filter(d, slot != "Bench")
    mngr[r] <- d$mngrId[1]
    week[r] <- i
    score[r] <- sum(temp$points) 
    temp <- arrange(d, desc(points))
    qb <- filter(temp, pos == "QB")
    rb <- filter(temp, pos == "RB")
    wr <- filter(temp, pos == "WR")
    te <- filter(temp, pos == "TE")
    notFlex <- c(rb$plyr[1:2], wr$plyr[1:2], te$plyr[1])
    flex <- filter(temp, pos == "RB" | pos == "WR" | pos == "TE")
    flex <- filter(flex, plyr != notFlex)
    k <- filter(temp, pos == "K")
    D <- filter(temp, pos == "D")
    bestScore[r] <- qb$points[1] + sum(rb$points[1:2]) + sum(wr$points[1:2]) + flex$points[1] + te$points[1] + k$points[1] + D$points[1]
    r <- r+1
  }
}
coaching <- tibble(mngr, week, score, bestScore)
coaching <- mutate(coaching, percTotal = coaching$score/coaching$bestScore)
avCoach <- aggregate(percTotal~mngr, data = coaching, FUN=function(x) c(mean=mean(x)))
avBest <- aggregate(bestScore~mngr, data = coaching, FUN=function(x) c(mean=mean(x)))
avScore <- aggregate(score~mngr, data = coaching, FUN=function(x) c(mean=mean(x)))

#percent possible per week
coaching %>% 
  mutate(week = as_factor(as.character(coaching$week))) %>% 
  ggplot(mapping = aes(x = mngr, y = percTotal)) +
  geom_col(mapping = aes(fill = week), position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Title", x = "Manager", y = "Percent Possible")

#average percent of best possible scores
avCoach %>% 
  ggplot(mapping = aes(x = mngr, y = percTotal)) +
  geom_col(mapping = aes(position = "dodge")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  coord_cartesian(ylim=c(.5,.9))+
  labs(title = "Avereage Percent of Best Possible Score", x = "Manager", y = "Percent Possible")

#stacked best score each week
coaching %>% 
  mutate(week = as_factor(as.character(coaching$week))) %>% 
  ggplot(mapping = aes(x = mngr, y = bestScore)) +
  geom_col(mapping = aes(fill = week)) +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  labs(title = "Best possible Scores", x = "Manager", y = "Points")

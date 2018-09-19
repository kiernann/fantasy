library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(ggplot2)

# Define values -----------------------------------------------------------

n.players <- 1008 # Total player pool
proj.urls <- as.list(rep(NA, ceiling(n.players / 40))) # Empty list for URLS
indexes <- seq(0, n.players, 40) # New page every 40 players
projected.url <- "http://games.espn.com/ffl/tools/projections?" # Base URL

# Create function ---------------------------------------------------------

projUrlGenerator <- function(week, year, league, sort, index){
  scoring.period <- paste0("&scoringPeriodId=", week)
  season.id <- paste0("&seasonId=", year)
  league.id <- paste0("&leagueId=", league)
  sort.map <- paste0("&sortMap=", sort)
  start.index <- paste0("&startIndex=", index)
  paste0(projected.url, # Append base URL with...
         scoring.period,
         season.id,
         league.id,
         sort.map,
         start.index)
}

# Run function for week 1, 2018 -------------------------------------------

alpha <- "AAAAARgAAAADAQAIY2F0ZWdvcnkDAAAAAwEABmNvbHVtbgMAAAABAQAJZGlyZWN0aW9uAwAAAAE%3D"
for (i in 1:ceiling(n.players / 40)) { # Loop, changing index, changing URL
  proj.urls[i] <- projUrlGenerator( # Puts new URL in list slot
    week = 1,
    year = 2018,
    league = 252353,
    sort = alpha,
    index = indexes[i]
  )
}

print(proj.urls[ceiling(n.players / 40)]) # Check if the URL looks right

# scrape html -------------------------------------------------------------

proj.tables <- as.list(rep(NA, ceiling(n.players / 40))) # create empty list

for (i in 1:ceiling(n.players / 40)) { # take each url and scrape html table
  proj.tables[[i]] <- as.data.frame(read_html(unlist(proj.urls[i])) %>%
                                      html_nodes("table") %>%
                                      .[(2)] %>%
                                      html_table(fill = TRUE) %>%
                                      `[[`(1))[-1, ]
}

projections <- bind_rows(proj.tables[1:ceiling(n.players / 40)]) # combine list

# clean up ----------------------------------------------------------------

names(projections) <- tolower(names(projections)) # lowercase variables
ff <- projections # new object named ff
ff <- rename(ff, projected = total, player = players) # change variable names
ff$player <- str_split_fixed(ff$player, ",", 2)[, 1] # take only player name
ff <- select(ff, player, projected) # take only names and proj score
ff <- as_data_frame(ff) # make a tibble
ff$projected <- as.numeric(ff$projected) # make scores numeric
ff <- ff[- grep("D/ST", ff$player), ] # remove defenses

# Define values -----------------------------------------------------------

scores.urls <- as.list(rep(NA, ceiling(n.players/50))) # Empty list for URLS
scores.indexes <- seq(0, n.players, 40) # New page every 50 players
scores.url <- "http://games.espn.com/ffl/leaders?" # Base URL

# Create function ---------------------------------------------------------

actualUrlGenerator <- function(week, year, league, sort, index){
  scoring.period <- paste0("&scoringPeriodId=", week)
  season.id <- paste0("&seasonId=", year)
  league.id <- paste0("&leagueId=", league)
  sort.map <- paste0("&sortMap=", sort)
  start.index <- paste0("&startIndex=", index)
  paste0(scores.url, # Append base URL with...
         scoring.period,
         season.id,
         league.id,
         sort.map,
         start.index)
}

# Run function for week 1, 2018 -------------------------------------------

for (i in 1:ceiling(n.players / 50)) { # Loop, changing index, changing URL
  scores.urls[i] <- actualUrlGenerator( # Puts new URL in list slot
    week = 1,
    year = 2018,
    league = 252353,
    sort = alpha,
    index = scores.indexes[i]
  )
}

print(scores.urls[ceiling(n.players / 50)]) # Check if the URL looks right

# scrape html -------------------------------------------------------------

scores.tables <- as.list(rep(NA, ceiling(n.players / 50))) # create empty list

for (i in 1:ceiling(n.players / 50)) { # take each url and scrape html table
  scores.tables[[i]] <- as.data.frame(read_html(unlist(scores.urls[i])) %>%
                                        html_nodes("table") %>%
                                        .[(2)] %>%
                                        html_table(fill = TRUE) %>%
                                        `[[`(1))[-1, ]
}

scores <- bind_rows(scores.tables[1:ceiling(n.players / 50)]) # combine list

# clean up ----------------------------------------------------------------

scores <- scores[-1, ] # espn uses double headers, get rid of one
ff2 <- scores # new object named ff
ff2 <- rename(ff2, actual = X27, player = X1) # change variable names
ff2$player <- str_split_fixed(ff2$player, ",", 2)[, 1] # take only player name
ff2 <- select(ff2, player, actual) # take only names and proj score
ff2 <- as_data_frame(ff2) # make a tibble
ff2$actual <- as.numeric(ff2$actual) # make scores numeric

mean(ff$projected, na.rm = TRUE) # check to see if it works... should be ~2.5
mean(ff2$actual, na.rm = TRUE) # check to see if it works... should be ~2.4

espn.ff.proj <- distinct(left_join(ff, ff2)) # combine both data frames!!
head(espn.ff.proj) # make sure it's not all kinds of screwed up


# compre proj and actual --------------------------------------------------

ggplot(data = espn.ff.proj) +
  geom_point(mapping = aes(x = projected, y = actual)) +
  geom_abline(color = "green") +
  ggtitle("Comparing projected and actual fantasy football scores, week 1") +
  xlab("Score projected by ESPN") +
  ylab("Actual score")
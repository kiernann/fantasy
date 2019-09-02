library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

league_id <- 252353
year <- 2018
url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{league_id}")
f <- GET(url, query = list("seasonId" = year, view = "mMatchup"))
x <- content(f)[[1]]

make_week <- function(z) {
  tibble(
    week = z$matchupPeriodId,
    home = c(FALSE, TRUE),
    team = c(z$away$teamId, z$home$teamId),
    score = c(z$away$totalPoints, z$home$totalPoints)
  )
}

z <- bind_rows(map(x$schedule, make_week))

a <- GET(url, query = list("seasonId" = year, view = "mBoxscore"))
b <- content(a)[[1]]

scoring_settings <- 
  transpose(b$settings$scoringSettings$scoringItems) %>% 
  as_tibble() %>% 
  unnest(cols = c(isReverseItem, points, statId))

t <- GET(url, query = list("seasonId" = year, view = "mRoster"))
t <- content(t)[[1]]

t$teams %>% map(as_tibble, .name_repair = "unique")

f <- GET(url, query = list(view = "mMatchup"))
x <- content(f)[[1]]

make_week <- function(z) {
  tibble(
    game = z$id,
    week = z$matchupPeriodId,
    home = c(FALSE, TRUE),
    team = c(z$away$teamId, z$home$teamId),
    score = c(z$away$totalPoints, z$home$totalPoints)
  )
}

z <- x$schedule %>% 
  map(make_week) %>% 
  map(mutate, year = x$seasonId) %>% 
  bind_rows() %>% 
  select(
    year, game, week, home, team, score
  )

write_csv(z, "data/matchups_2015.csv")

x$teams[[1]]$roster$entries[[1]]$playerPoolEntry$player

t <- GET(url, query = list(view = "mTeam"))
t <- content(t)
t <- transpose(t)
t$seasonId
t$members %>% glimpse()

t$members[[1]] %>% map(as_tibble) %>% bind_rows()

map(.x = t$members, .f = function(x) bind_rows(map(x, as_tibble)))

t$members %>% 
  map(map, as_tibble) %>% 
  map(bind_rows)

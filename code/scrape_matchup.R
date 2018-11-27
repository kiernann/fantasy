library(tidyverse)
matchups <-
  read_html("http://games.espn.com/ffl/schedule?leagueId=252353") %>%
  html_node("table") %>%
  html_table(fill = TRUE) %>%
  as_tibble() %>%
  select(X2, X5, X6) %>%
  rename(away_team = X2,
         home_team = X5,
         result = X6) %>%
  separate(col = result,
           into = c("away_score", "home_score"),
           sep = "-") %>%
  na.omit() %>%
  .[-c(56, 57), ] %>%
  mutate(away_team = recode(away_team,
                       "Angus Guider"     = "agus",
                       "Brian Pepe"       = "pepe",
                       "Carter Beeman"    = "cart",
                       "Charles Belisle"  = "char",
                       "Colin Compagna"   = "coln",
                       "Justin Griswold"  = "griz",
                       "Kiernan Nicholls" = "kier",
                       "Kiley Moody"      = "kyle",
                       "Nick Audet"       = "nick",
                       "Rowen Lovell"     = "rown",
                       "William Slocum"   = "bill"),
         home_team = recode(home_team,
                       "Angus Guider"     = "agus",
                       "Brian Pepe"       = "pepe",
                       "Carter Beeman"    = "cart",
                       "Charles Belisle"  = "char",
                       "Colin Compagna"   = "coln",
                       "Justin Griswold"  = "griz",
                       "Kiernan Nicholls" = "kier",
                       "Kiley Moody"      = "kyle",
                       "Nick Audet"       = "nick",
                       "Rowen Lovell"     = "rown",
                       "William Slocum"   = "bill"),
         week = rep(1:11, each = 5),
         winner = if_else(condition = away_score > home_score,
                          true = away_team,
                          false = home_team),
         loser = if_else(condition = away_score < home_score,
                         true = away_team,
                         false = home_team))

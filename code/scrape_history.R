# Kiernan Nicholls
# Sun Dec 29 18:20:52 2019
# Scrape API for league history

library(tidyverse)
library(magrittr)
library(fflr)

lid <- 252353

# matchups ----------------------------------------------------------------

# scrape past years
matchup_data <- ff_matchup(lid, current = FALSE)
matchups <- tibble()
for (i in seq_along(matchup_data)) {
  matchup_data[[i]]$schedule %>% 
    map_dfr(form_matchup) %>%
    mutate(year = matchup_data[[i]]$seasonId) %>% 
    select(year, everything()) %>% 
    bind_rows(matchups) -> matchups
}

# scrape current year
matchup_2019 <- ff_matchup(lid, current = TRUE)
matchup_2019$schedule %>% 
  map_dfr(form_matchup) %>%
  mutate(year = matchup_2019$seasonId) %>% 
  select(year, everything()) %>% 
  bind_rows(matchups) -> matchups

# add team abbreviations
teams <- select(form_teams(ff_members(lid)), id, abbrev)
teams <- teams %>% 
  bind_rows(
    tribble(
      ~id, ~abbrev,
      2L, "ROWN",
      7L, "GRIZ",
      9L, "COLN"
    )
  ) %>% 
  arrange(id) %>% 
  expand_grid(year = c(2015:2019))

teams$abbrev[teams$id == 8L & teams$year != 2019L] <- "CHAR"
matchups <- left_join(matchups, teams)
matchups <- select(matchups, -id, id = abbrev)

# write to disk
write_csv(matchups, "data/matchups.csv")
write_csv(teams, "data/teams.csv")

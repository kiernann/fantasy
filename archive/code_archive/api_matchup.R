library(tidyverse)
library(httr)
library(glue)

# query matchup api --------------------------------------------------------------------------

league_id <- 252353
url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/{league_id}")

response <- GET(url, query = list(view = "mMatchup"))
content <- content(response)

# create function to tibble matchup
format_matchup <- function(z) {
  tibble(
    game = z$id,
    week = z$matchupPeriodId,
    home = c(FALSE, TRUE),
    team = c(z$away$teamId, z$home$teamId),
    score = c(z$away$totalPoints, z$home$totalPoints)
  )
}

# extract and format every matchup
matchup_history <- rep(list(NA), length(content))
for (i in seq_along(content)) {
  matchups <- map(content[[i]]$schedule, format_matchup)
  matchup_history[[i]] <- bind_rows(matchups) %>% 
    mutate(year = content[[i]]$seasonId) %>% 
    select(year, game, week, home, team, score)
}

# query team history -------------------------------------------------------------------------

response <- GET(url, query = list(view = "mTeam"))
content <- content(response)

team_history <- rep(list(NA), length(content))
for (i in seq_along(content)) {
  abbrevs <- unlist(transpose(content[[i]]$teams)$abbrev)
  teams <- unlist(transpose(content[[i]]$teams)$id)
  team_history[[i]] <- tibble(team = teams, abbrev = abbrevs) %>% 
    mutate(year = content[[i]]$seasonId)
}

team_history <- bind_rows(team_history)

# create best abbrevs by hand
team_history <- tribble(
  ~team, ~abbrev,
  1,  "AGUS",
  2,  "ROWN",
  3,  "PEPE",
  4,  "BILL",
  5,  "CART",
  6,  "KIER",
  7,  "GRIZ",
  8,  "CHAR",
  9,  "COLN",
  10, "NICK",
  11, "KYLE",
)

# join abbrevs and calc wins
matchup_history <- 
  bind_rows(matchup_history) %>% 
  left_join(team_history, by = "team") %>% 
  group_by(year, game, week) %>% 
  mutate(win = score == max(score)) %>% 
  ungroup()

# save matchup history
write_csv(matchup_history, "data/matchup_history.csv")

# plot win history
wins_history <- 
  matchup_history %>%
  arrange(year, game) %>% 
  group_by(year, team) %>% 
  mutate(wins = cumsum(win)) %>% 
  ggplot(aes(x = game, y = wins)) +
  geom_line(aes(color = abbrev), size = 2) + 
  facet_wrap(~year) +
  labs(
    title = "Wins Over Time",
    x = "Game of Season",
    y = "Total Wins",
    color = "Team"
  )

# save matchup history plot
ggsave(
  filename = "plots/wins_history.png",
  plot = wins_history,
  dpi = "retina",
  width = 9,
  height = 5
)

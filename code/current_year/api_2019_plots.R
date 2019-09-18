library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)

id <- 252353
year <- 2019
url <- glue("https://fantasy.espn.com/apis/v3/games/ffl/seasons/{year}/segments/0/leagues/{id}")
response <- GET(url, query = list(view = "mMatchup"))
# also try: mTeam, mBoxscore, mRoster, mSettings, kona_player_info, player_wl, mSchedule
content <- content(response)

format_matchup <- function(matchup) {
  tibble(
    game = matchup$id,
    week = matchup$matchupPeriodId,
    home = c(FALSE, TRUE),
    team = c(matchup$away$teamId, matchup$home$teamId),
    score = c(matchup$away$totalPoints, matchup$home$totalPoints)
  )
}

teams <- tribble(
  ~team, ~abbrev,
  1,  "AGUS",
  2,  "ROWN",
  3,  "PEPE",
  4,  "BILL",
  5,  "CART",
  6,  "KIER",
  7,  "GRIZ",
  8,  "CORY",
  9,  "COLN",
  10, "NICK",
  11, "KYLE",
)

scores <- content$schedule %>% 
  map_df(format_matchup) %>% 
  left_join(teams) %>%
  select(-team) %>% 
  select(week, game, team = abbrev, everything()) %>% 
  filter(score != 0) %>% 
  mutate(week = fct_rev(as_factor(week)))

ff_median <- median(scores$score)
ff_mean <- mean(scores$score)
ff_dev <- sd(scores$score)

scores %>% 
  group_by(game) %>% 
  arrange(game, score) %>% 
  mutate(won = c(F, T)) %>% 
  group_by(team) %>% 
  summarise(wins = sum(won))

scores_plot <- scores %>% 
  ggplot(aes(x = team, y = score)) +
  geom_col(aes(fill = week)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "2019 GAA FFL Scores") +
  scale_fill_brewer(palette = "Dark2")

ggsave(
  filename = glue("plots/{Sys.Date()}_scores.png"),
  plot = scores_plot,
  dpi = "retina",
  width = 9,
  height = 5
)

power_wins <- rep(NA, length(scores$score))
for (i in seq_along(scores$score)) {
  power_wins[i] <- sum(scores$score[i] > scores$score[-i])
}

power_plot <- scores %>% 
  group_by(week) %>% 
  mutate(power = map_int(score, ~ sum(.x > score))) %>% 
  ggplot(aes(x = team, y = power)) +
  geom_col(aes(fill = week)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "2019 GAA FFL Power Wins") +
  scale_fill_brewer(palette = "Dark2")

ggsave(
  filename = glue("plots/{Sys.Date()}_power.png"),
  plot = power_plot,
  dpi = "retina",
  width = 9,
  height = 5
)

library(fflr)
library(tidyverse)

teams <- league_teams(252353)
matchups <- weekly_matchups(252353)
matchups <- left_join(matchups, teams[, 1:2])
matchups %>% 
  filter(as.integer(week) < 13) %>% 
  group_by(abbrev) %>% 
  summarise(wins = mean(winner, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(abbrev, wins), y = wins)) + 
  geom_col(aes(fill = abbrev)) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") + 
  scale_y_continuous(label = scales::percent) + 
  labs(
    title = "Win Rate by Team",
    x = "Team", 
    y = "Rate"
  )

ggsave(
  filename = "plots/win-rate.png",
  plot = last_plot(),
  dpi = "retina",
  height = 5,
  width = 9
)

matchups %>% 
  filter(as.integer(week) < 13, score > 0) %>% 
  group_by(abbrev) %>% 
  summarise(mean = mean(score, na.rm = TRUE)) %>% 
  ggplot(aes(x = reorder(abbrev, mean), y = mean)) + 
  geom_col(aes(fill = abbrev)) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") + 
  labs(
    title = "Mean Score by Team",
    subtitle = "Scoring and number of teams changing year to year",
    x = "Team", 
    y = "Rate"
  )

ggsave(
  filename = "plots/avg-score.png",
  plot = last_plot(),
  dpi = "retina",
  height = 5,
  width = 9
)

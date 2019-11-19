library(tidyverse)
library(magrittr)
library(glue)
library(fflr)

teams <- 
  fantasy_members(252353) %>% 
  form_teams() %>% 
  select(id, abbrev)

scores <- 
  fantasy_matchup(252353) %>% 
  use_series(schedule) %>% 
  map_df(form_matchup) %>% 
  left_join(teams) %>% 
  filter(score != 0) %>% 
  mutate(week = fct_rev(week))

total_wins <- scores %>% 
  group_by(week, game) %>% 
  mutate(won = map_dbl(score, ~ sum(.x > score))) %>% 
  group_by(abbrev) %>% 
  summarize(wins = sum(won)) %>% 
  arrange(desc(wins)) %>% 
  ggplot(aes(x = reorder(abbrev, wins), y = wins)) +
  geom_col(aes(fill = abbrev)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(
    title = "2019 GAA FFL Regular Wins",
    y = "Total Wins",
    x = "abbrev"
    )

ggsave(
  filename = glue("plots/{Sys.Date()}_wins.png"),
  plot = total_wins,
  dpi = "retina",
  width = 9,
  height = 5
)

scores_plot <- scores %>% 
  ggplot(aes(x = reorder(abbrev, score), y = score)) +
  geom_col(aes(fill = week)) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme(legend.position = "bottom") +
  scale_fill_fantasy() +
  coord_flip() +
  labs(
    title = "2019 GAA FFL Points For",
    y = "Points For",
    x = "abbrev",
    fill = "Week"
  )

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
  ggplot(aes(x = reorder(abbrev, power), y = power)) +
  geom_col(aes(fill = week)) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme(legend.position = "bottom") +
  scale_fill_fantasy() +
  coord_flip() +
  labs(
    title = "2019 GAA FFL Power Wins",
    y = "Power Wins",
    x = "abbrev",
    fill = "Week"
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_power.png"),
  plot = power_plot,
  dpi = "retina",
  width = 9,
  height = 5
)

against_plot <- scores %>% 
  group_by(week, game) %>% 
  mutate(against = coalesce(lag(score), lead(score))) %>% 
  ggplot(aes(x = reorder(abbrev, score), y = against)) +
  geom_col(aes(fill = week)) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE)) +
  theme(legend.position = "bottom") +
  scale_fill_fantasy() +
  coord_flip() +
  labs(
    title = "2019 GAA FFL Points Against",
    y = "Points Against",
    x = "abbrev",
    fill = "Week"
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_against.png"),
  plot = against_plot,
  dpi = "retina",
  width = 9,
  height = 5
)

for_against_plot <- scores %>% 
  group_by(week, game) %>% 
  mutate(against = coalesce(lag(score), lead(score))) %>% 
  group_by(abbrev) %>% 
  summarize(pf = sum(score), pa = sum(against)) %>% 
  ggplot(aes(x = pf, y = pa)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_label(aes(label = abbrev, fill = abbrev), size = 6) +
  scale_fill_brewer(
    palette = "Dark2", 
    guide = FALSE
  ) +
  labs(
    title = "2019 GAA FFL Points Relationship",
    y = "Points Against",
    x = "Points For"
  ) +
  coord_cartesian(
    xlim = c(850, 1300),
    ylim = c(850, 1300)
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_for_against.png"),
  plot = for_against_plot,
  dpi = "retina",
  width = 7,
  height = 7
)

matchup_plot <- scores %>% 
  pivot_wider(
    id_cols = c(week, game, home, score),
    names_from = home,
    values_from = score
  ) %>% 
  ggplot(aes(x = `TRUE`, y = `FALSE`)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(aes(color = week), size = 10, alpha = 0.75) +
  guides(color = guide_legend(nrow = 1)) +
  theme(legend.position = "none") +
  labs(
    title = "2019 GAA FFL Matchups",
    x = "Home Score",
    y = "Away Score",
    color = "Week"
  ) +
  coord_cartesian(
    xlim = c(50, 160),
    ylim = c(50, 160)
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_matchup.png"),
  plot = matchup_plot,
  dpi = "retina",
  width = 7,
  height = 7
)

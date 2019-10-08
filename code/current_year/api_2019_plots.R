library(tidyverse)
library(jsonlite)
library(httr)
library(glue)
library(rvest)
library(fflr)

content <- fantasy_matchup(252353)

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

total_wins <- scores %>% 
  group_by(week, game) %>% 
  mutate(won = map_dbl(score, ~ sum(.x > score))) %>% 
  group_by(team) %>% 
  summarize(wins = sum(won)) %>% 
  arrange(desc(wins)) %>% 
  ggplot(aes(x = reorder(team, wins), y = wins)) +
  geom_col(aes(fill = team)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  labs(
    title = "2019 GAA FFL Regular Wins",
    y = "Total Wins",
    x = "Team"
    )

ggsave(
  filename = glue("plots/{Sys.Date()}_wins.png"),
  plot = total_wins,
  dpi = "retina",
  width = 9,
  height = 5
)

scores_plot <- scores %>% 
  ggplot(aes(x = reorder(team, score), y = score)) +
  geom_col(aes(fill = week)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(
    palette = "Dark2", 
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "2019 GAA FFL Points For",
    y = "Points For",
    x = "Team",
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
  ggplot(aes(x = reorder(team, power), y = power)) +
  geom_col(aes(fill = week)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(
    palette = "Dark2", 
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "2019 GAA FFL Power Wins",
    y = "Power Wins",
    x = "Team",
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
  ggplot(aes(x = reorder(team, score), y = against)) +
  geom_col(aes(fill = week)) +
  coord_flip() +
  theme(legend.position = "bottom") +
  scale_fill_brewer(
    palette = "Dark2", 
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "2019 GAA FFL Points Against",
    y = "Points Against",
    x = "Team",
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
  group_by(team) %>% 
  summarize(pf = sum(score), pa = sum(against)) %>% 
  ggplot(aes(x = pf, y = pa)) +
  geom_label(aes(label = team, fill = team), size = 5) +
  scale_fill_brewer(
    palette = "Dark2", 
    guide = FALSE
  ) +
  labs(
    title = "2019 GAA FFL Points Relationship",
    y = "Points Against",
    x = "Points For"
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_for_against.png"),
  plot = for_against_plot,
  dpi = "retina",
  width = 9,
  height = 5
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
  theme(legend.position = "bottom") +
  scale_color_brewer(
    palette = "Dark2", 
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = "2019 GAA FFL Matchups",
    x = "Home Score",
    y = "Away Score",
    color = "Week"
  )

ggsave(
  filename = glue("plots/{Sys.Date()}_matchup.png"),
  plot = matchup_plot,
  dpi = "retina",
  width = 9,
  height = 5
)

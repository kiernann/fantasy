remotes::install_github("kiernann/fflr")
library(tidyverse)
library(fflr)
data <- fantasy_matchup(fflr:::gaa)
m <- map_df(data$schedule, form_matchup)

members <- fantasy_members(fflr:::gaa)
teams <- form_teams(members) %>% select(id, abbrev)

m <- m %>% 
  left_join(teams) %>% 
  select(-id) %>% 
  rename(id = abbrev)

stats <- m %>% 
  filter(score != 0) %>% 
  group_by(id) %>%
  summarise(
    min = min(score),
    mean = mean(score),
    median = median(score),
    max = max(score),
    sd = sd(score)
  )

stats %>% 
  pivot_longer(cols = -1) %>% 
  ggplot(aes(x = reorder(id, value), y = value)) +
  geom_col(aes(fill = id)) +
  facet_wrap(~name, ncol = 1, scales = "free") +
  scale_fill_brewer(palette = "Dark2", guide = FALSE)

x <- m %>% 
  filter(score == 0) %>% 
  group_by(game) %>% 
  right_join(stats) %>% 
  arrange(game)

for (i in x$game) {
  n <- 10000
  week <- unique(x$week[x$game == i])
  home_team <- x$id[x$game == i & x$home]
  away_team <- x$id[x$game == i & !x$home]
  home_sims <- rnorm(
    n = n, 
    mean = x$mean[x$game == i & x$home],
    sd = x$sd[x$game == i & x$home]
  )
  away_sims <- rnorm(
    n = n, 
    mean = x$mean[x$game == i & !x$home],
    sd = x$sd[x$game == i & !x$home]
  )
  home_prob <- mean(home_sims > away_sims)
  away_prob <- 1 - home_prob
}

sims <- tibble(
  game = seq_along(home_sims),
  home = home_sims,
  away = away_sims
)

sims %>% 
  ggplot(aes(x = home, y = away)) +
  geom_bin2d() +
  geom_abline(
    slope = 1, 
    intercept = 0, 
    color = "black",
    size = 2,
    linetype = 2
  ) +
  scale_fill_viridis_c(guide = FALSE) +
  coord_cartesian(
    xlim = c(0, 200),
    ylim = c(0, 200)
  ) +
  labs(
    title = glue::glue("Simulated Week {week} Fantasy Matchups"),
    subtitle = glue::glue("{scales::comma(nrow(sims))} games from a random normal distribtion around the team mean and standard deviation"),
    x = glue::glue("{home_team} Scores ({scales::percent(home_prob)})"),
    y = glue::glue("{away_team} Scores ({scales::percent(away_prob)})")
  )

ggsave(
  filename = "~/Pictures/score_heat.png",
  height = 8,
  width = 8,
  dpi = "retina"
)

# Kiernan Nicholls
# Tue Sep 15 21:19:14 2020 ------------------------------

library(tidyverse)
library(ggrepel)
library(fflr)

# get coaching data -----------------------------------------------------------

w <- 2
rosters <- team_roster(252353, week = w)
best <- map(rosters, best_roster)

actual_scores <- map_dbl(rosters, roster_score)
best_scores <- map_dbl(best, roster_score)

teams <- league_teams(252353)

coaching <- tibble(
  week = w,
  abbrev = teams$abbrev,
  best = best_scores
)

# get matchup data ------------------------------------------------------------

matchups <- weekly_matchups(252353)
matchups <- matchups %>%
  filter(week == w) %>%
  left_join(teams[, 1:2]) %>%
  mutate(week = as.numeric(week))

diff <-
  left_join(
    x = matchups,
    y = coaching,
    by = c("week", "abbrev")
  ) %>%
  mutate(coach = score/best)

x <- diff %>%
  pivot_longer(
    cols = c(score, best),
    names_to = "type",
    values_to = "score"
  )

x <- diff %>%
  group_by(id) %>%
  mutate(total = sum(score)) %>%
  ungroup() %>%
  mutate(
    opp = total - score,
    real_diff = score - opp,
    ideal_diff = best - opp,
    diff_diff = ideal_diff - real_diff
  )

x %>%
  ggplot(aes(x = real_diff, y = ideal_diff)) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  scale_size_continuous(range = c(5, 8), guide = FALSE) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100)) +
  geom_label(aes(label = abbrev, fill = abbrev, size = diff_diff))

x %>%
  mutate(
    id = as.character(id),
    coulda = ideal_diff > 0
  ) %>%
  ggplot(aes(x = real_diff, y = ideal_diff)) +
  geom_label(
    label.size = 0,
    alpha = 0,
    size = 12,
    color = "grey30",
    mapping = aes(
      label = "Coulda Won",
      x = -60,
      y = 60
    )
  ) +
  geom_label(
    label.size = 0,
    alpha = 0,
    size = 12,
    color = "grey30",
    mapping = aes(
      label = "No Chance",
      x = -60,
      y = -60
    )
  ) +
  geom_label(
    label.size = 0,
    alpha = 0,
    size = 12,
    color = "grey30",
    mapping = aes(
      label = "Did Win",
      x = 60,
      y = 60
    )
  ) +
  scale_fill_brewer(palette = "Dark2", guide = FALSE) +
  scale_shape_manual(values = c(15, 16, 17, 18), guide = FALSE) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  coord_cartesian(xlim = c(-100, 100), ylim = c(-100, 100)) +
  # geom_line(aes(group = id), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(aes(color = coulda), size = 2) +
  geom_label_repel(
    segment.size = 1,
    mapping = aes(fill = abbrev, label = abbrev),
    size = 7
  ) +
  scale_color_manual(values = c("red", "black"), guide = FALSE)
scale_linetype(guide = FALSE) +
  labs(
    title = paste("Best Roster Potential", sprintf("Week %s", w), sep = ": "),
    x = "Real Matchup Difference",
    y = "Ideal Score Difference"
  )

ggsave(
  "~/Pictures/diff_plot2.png",
  height = 7,
  width = 8,
  dpi = "retina"
)

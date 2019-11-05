# Kiernan Nicholls
# Tue Nov  5 13:26:36 2019

# get coaching data --------------------------------------------------------------------------

roster_data <- fantasy_roster(gaa, scoringPeriodId = 9)
rosters <- map(roster_data$teams, form_roster)
best <- map(rosters, best_roster)

actual_scores <- map_dbl(rosters, roster_score)
best_scores <- map_dbl(best, roster_score)

tran_data <- transpose(roster_data$teams)

coaching <- tibble(
  week = 8,
  abbrev = unlist(tran_data$abbrev),
  best = best_scores,
)

# get matchup data ---------------------------------------------------------------------------

teams <- fantasy_members(gaa) %>%
  form_teams() %>%
  select(id, abbrev)

match_data <- fantasy_matchup(gaa, scoringPeriodId = 9)
matchups <- match_data$schedule %>%
  map_dfr(form_matchup) %>%
  filter(week == 9) %>%
  left_join(teams) %>%
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
  group_by(game) %>%
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
  mutate(game = as.character(game)) %>%
  ggplot(aes(x = real_diff, y = ideal_diff)) +
  geom_label(
    label.size = 0,
    alpha = 0,
    size = 12,
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
  geom_line(aes(group = game), size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_label(aes(shape = game, fill = abbrev, label = abbrev), size = 7) +
  scale_linetype(guide = FALSE) +
  labs(
    title = "Real Scores vs Best Scores",
    subtitle = "Week 9",
    x = "Real Matchup Difference",
    y = "Ideal Score Difference"
  )

ggsave(
  "~/Pictures/diff_plot.png",
  height = 7,
  width = 8,
  dpi = "retina"
)

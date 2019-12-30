matchups %>% 
  group_by(id) %>% 
  summarise(wins = mean(winner, na.rm = )) %>% 
  ggplot(aes(x = reorder(id, wins), y = wins)) + 
  geom_col(aes(fill = id)) + scale_fill_fantasy() +
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
  group_by(id) %>% 
  summarise(mean = mean(score, na.rm = )) %>% 
  ggplot(aes(x = reorder(id, mean), y = mean)) + 
  geom_col(aes(fill = id)) + scale_fill_fantasy() +
  theme(legend.position = "none") + 
  coord_cartesian(ylim = c(80, 120)) +
  labs(
    title = "Mean Score by Team",
    subtitle = "Scoring settings and number of teams changing",
    x = "Team", 
    y = "Rate"
  )

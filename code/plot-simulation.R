library(tidyverse)
library(fflr)

x <- league_simulation()

x %>% 
  mutate(lbl_odds = percent(playoffPct, accuracy = 1)) %>%  
  ggplot(aes(reorder(abbrev, -playoffPct), playoffPct)) + 
  geom_col(aes(fill = playoffPct), color = "black", width = 0.8) + 
  theme_classic() + 
  scale_fill_viridis_c(option = "C", end = 0.95, guide = "none", begin = 0.05) + 
  geom_vline(xintercept = 4.5, linetype = 2) + 
  labs(x = NULL, y = NULL, title = "Playoff Odds", subtitle = Sys.Date()) + 
  scale_y_continuous(labels = scales::label_percent()) + 
  coord_cartesian(ylim = c(0, 0.7)) + 
  geom_text(aes(label = lbl_odds), nudge_y = 0.02, size = 3) +
  theme(
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    text = element_text(family = "Ubuntu mono"),
    plot.title.position = "plot",
    plot.title = element_text(
      hjust = 0.9,
      vjust = 1,
      margin = margin(b = -50, t = 30, r = 30),
      size = 24
    ),
    plot.subtitle = element_text(
      hjust = 0.92,
      vjust = 1,
      margin = margin(b = -60, t = 60, r = 35),
      size = 16,
      color = "grey40"
    )
  )

ggsave("~/Pictures/playoff_odds.png", height = 7, width = 5, dpi = 320)

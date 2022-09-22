library(tidyverse)
library(fflr)

# data --------------------------------------------------------------------

x <- tidy_scores(useMatchup = FALSE)

# funs --------------------------------------------------------------------

theme_gaa <- function(dat) {
  theme_classic() +
    theme(
      legend.position = "bottom",
      text = element_text(family = "Ubuntu mono"),
      panel.background = element_rect(),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

# score -------------------------------------------------------------------

x %>% 
  mutate(
    scoringPeriodId = fct_rev(as.factor(scoringPeriodId))
  ) %>% 
  group_by(abbrev) %>% 
  mutate(
    lbl_y = cumsum(points),
    lbl_txt = round(points)
  ) %>% 
  ggplot(
    aes(
      x = reorder(abbrev, points),
      y = points,
      fill = scoringPeriodId
    )
  ) + 
  geom_col(
    color = "black"
  ) +
  geom_text(
    mapping = aes(
      y = lbl_y,
      label = lbl_txt
    ),
    nudge_y = -4.5
  ) +
  coord_flip() +
  scale_fill_brewer(
    palette = "Dark2",
    direction = -1
  ) +
  theme_gaa() +
  labs(
    title = "Points Scored by Team (2022)",
    x = "Team",
    y = "Points",
    fill = "Week"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave(
  filename = sprintf("plots/scores-week_%s.png", Sys.Date()),
  height = 5,
  width = 10,
  dpi = 300
)


# wins --------------------------------------------------------------------


x %>% 
  mutate(
    scoringPeriodId = fct_rev(as.factor(scoringPeriodId))
  ) %>% 
  group_by(abbrev) %>% 
  mutate(
    lbl_y = cumsum(expectedWins),
    lbl_txt = ifelse(
      test = expectedWins > 0,
      yes = as.character(round(expectedWins, 2)),
      no = ""
    )
  ) %>% 
  ggplot(
    aes(
      x = reorder(abbrev, expectedWins),
      y = expectedWins,
      fill = scoringPeriodId
    )
  ) + 
  geom_col(
    color = "black"
  ) +
  geom_text(
    mapping = aes(
      y = lbl_y,
      label = lbl_txt
    ),
    nudge_y = -0.04
  ) +
  coord_flip(ylim = c(0, 1.7)) +
  scale_fill_brewer(
    palette = "Dark2",
    direction = -1,
  ) +
  theme_gaa() +
  labs(
    title = "Expected Wins by Team (2022)",
    x = "Team",
    y = "Expected Wins",
    fill = "Week"
  ) +
  guides(fill = guide_legend(nrow = 1, reverse = TRUE))

ggsave(
  filename = sprintf("plots/wins-week_%s.png", Sys.Date()),
  height = 5,
  width = 10,
  dpi = 300
)

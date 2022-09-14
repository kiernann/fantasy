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
    across(scoringPeriodId, as.factor),
    lbl = as.character(round(points))
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
      label = lbl
    ),
    nudge_y = -3
  ) +
  coord_flip() +
  scale_fill_brewer(
    palette = "Dark2"
  ) +
  theme_gaa() +
  labs(
    title = "Points Scored by Team (2022)",
    x = "Team",
    y = "Points",
    fill = "Week"
  )

ggsave(
  filename = sprintf("plots/scores-week_%s.png", Sys.Date()),
  height = 5,
  width = 10,
  dpi = 300
)


# wins --------------------------------------------------------------------


x %>% 
  mutate(
    across(scoringPeriodId, as.factor),
    lbl = ifelse(
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
      label = lbl
    ),
    nudge_y = -0.03
  ) +
  coord_flip(ylim = c(0, 1)) +
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
  )

ggsave(
  filename = sprintf("plots/wins-week_%s.png", Sys.Date()),
  height = 5,
  width = 10,
  dpi = 300
)

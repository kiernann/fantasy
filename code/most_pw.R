library(tidyverse)
library(fflr)
lid <- "252353"
old_data <- ff_matchup(lid, current = FALSE)

old_scores <- rep(list(NA), length(old_data))
for (i in seq_along(old_data)) {
  old_scores[[i]] <- map_df(old_data[[i]]$schedule, form_matchup)
  old_scores[[i]] <- mutate(old_scores[[i]], year = old_data[[i]]$seasonId)
  old_scores[[i]] <- select(old_scores[[i]], year, everything())
}

old_scores <- bind_rows(old_scores)
teams <- form_teams(data = ff_members(lid))
teams <- add_row(teams, id = c(2, 7, 9), abbrev = c("ROWN", "GRIZ", "COLN"))
teams <- select(teams, id, abbrev)
teams <- arrange(teams, id)
teams <- distinct(teams)
teams$abbrev[teams$id == 8] <- "CHAR"
old_scores <- left_join(old_scores, teams, by = "id")

new_data <- ff_matchup(lid, current = TRUE)
new_scores <- map_df(new_data$schedule, form_matchup)
new_scores <- mutate(new_scores, year = new_data$seasonId)
new_scores <- select(new_scores, year, everything())
teams$abbrev[teams$id == 8] <- "CORY"
new_scores <- left_join(new_scores, teams, by = "id")

all_scores <- bind_rows(old_scores, new_scores)

all_scores <- all_scores %>% 
  group_by(year, week) %>% 
  mutate(
    power = match(score, sort(score)) - 1,
    power = power/max(power)
  )

all_scores %>% 
  ggplot(aes(x = reorder(abbrev, power), y = power)) +
  geom_col() +
  facet_wrap(~year, ncol = 1) +
  theme(
    legend.position = "none",
    axis.text = element_text(angle = 90)
  ) +
  labs(
    title = "Power Wins",
    x = "Team",
    y = "Wins"
  )

ggsave(
  filename = "~/Pictures/pwins.png",
  dpi = "retina",
  width = 
)

all_scores %>%
  filter(as.integer(week) <= 12) %>% 
  group_by(year, abbrev) %>% 
  summarise(power = sum(power)) %>% 
  arrange(desc(power))

library(tidyverse)
library(httr)

fantasy_data <- function(lid, ...) {
  api <- stringr::str_c("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/", lid)
  data <- httr::content(httr::GET(url = api, query = list(...)))
  return(data)
}

records <- tibble()
for (i in 2015:2018) {
  data <- fantasy_data(252353, view = "roster", seasonId = i)[[1]]
  transpose(transpose(data$teams)$record)$overall %>% 
    map_df(as_tibble) %>% 
    mutate(
      year = as.character(data$seasonId),
      diff = pointsFor - pointsAgainst,
      id = as_vector(transpose(data$teams)$primaryOwner),
      rank = as_vector(transpose(data$teams)$rankCalculatedFinal)
    ) %>% 
    group_by(id) %>% 
    arrange(desc(wins)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(select(map_dfr(data$members, as_tibble), id, firstName)) %>% 
    select(
      year,
      firstName,
      rank,
      wins, 
      losses, 
      percentage, 
      pointsFor, 
      pointsAgainst,
      diff
    ) %>% 
    arrange(rank) %>% 
    bind_rows(records) -> records
}

records$year <- fct_rev(as_factor(records$year))

write_csv(records, "data/past_records.csv")

records %>% 
  drop_na(firstName) %>% 
  ggplot(aes(x = firstName, y = diff)) +
  geom_col(aes(fill = year), position = position_dodge(0.75), width = 0.75) +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
  theme(legend.position = "bottom") +
  labs(
    title = "2015-2018 GAA FFL Seasonal Points Above Against",
    y = "(Points For) - (Points Against)",
    x = "Manager", 
    fill = "Year"
  )

ggsave(
  filename = "plots/record_score_diff.png",
  plot = last_plot(),
  dpi = "retina",
  height = 5,
  width = 9
)

records %>% 
  drop_na(firstName) %>% 
  group_by(year) %>% 
  mutate(
    avg = median(pointsFor),
    avg_diff = pointsFor - avg
  ) %>% 
  ggplot(aes(x = firstName, y = avg_diff)) +
  geom_col(aes(fill = year), position = position_dodge(0.75), width = 0.75) +
  geom_hline(yintercept = 0) +
  scale_fill_brewer(type = "qual", palette = "Dark2", direction = -1) +
  theme(legend.position = "bottom") +
  labs(
    title = "2015-2018 GAA FFL Seasonal Points Above Median",
    y = "(Points For) - (Median Points)",
    x = "Manager", 
    fill = "Year"
  )

ggsave(
  filename = "plots/record_score_diff2.png",
  plot = last_plot(),
  dpi = "retina",
  height = 5,
  width = 9
)

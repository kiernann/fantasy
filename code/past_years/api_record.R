library(tidyverse)
library(httr)

fantasy_data <- function(lid, ...) {
  api <- stringr::str_c("https://fantasy.espn.com/apis/v3/games/ffl/leagueHistory/", lid)
  data <- httr::content(httr::GET(url = api, query = list(...)))
  return(data)
}

y <- tibble()
for (year in 2015:2018) {
  x <- fantasy_data(252353, view = "roster", seasonId = year)[[1]]
  transpose(transpose(x$teams)$record)$overall %>% 
    map_df(as_tibble) %>% 
    mutate(
      year = x$seasonId,
      diff = pointsFor - pointsAgainst,
      id = as_vector(transpose(x$teams)$primaryOwner),
      rank = as_vector(transpose(x$teams)$rankCalculatedFinal)
    ) %>% 
    left_join(select(map_dfr(x$members, as_tibble), id, firstName)) %>% 
    select(
      year,
      manager = firstName,
      rank,
      wins, 
      losses, 
      percentage, 
      pointsFor, 
      pointsAgainst,
      diff
    ) %>% 
    arrange(rank) %>% 
    bind_rows(y) -> y
}

players <- html_text(html_nodes(file, css = "td.playertablePlayerName"))
espn_url_gen <- function(league_id,
                         team_id,
                         scoring_period,
                         season_id,
                         view = "scoringperiod",
                         version = "quick") {
  url <- paste0("http://games.espn.com/ffl/boxscorequick?",
         "leagueId=",         as.character(league_id),
         "&teamId=",          as.character(team_id), 
         "&scoringPeriodId=", as.character(scoring_period), 
         "&seasonId=",        as.character(season_id), 
         "&view=",            as.character(view),
         "&version=",         as.character(version))
  return(url)
}
espn_url <- espn_url_gen(252353, 1, 1, 2018)
test <- 
  read_html(espn_url) %>% 
  html_nodes(css = "#playertable_0") %>% 
  html_table(fill = TRUE) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  select(X1, X2, X5) %>% 
  rename(pos = X1,
         player = X2, 
         points = X5)
test <- test[-c(1:3), ]
test$player <- str_replace(test$player, ",.*", "")
test$player <- str_replace(test$player, "D/ST D/ST^", "Defense")
test

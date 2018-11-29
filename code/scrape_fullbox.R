library(tidyverse)
library(rvest)
fullbox_url <- function(league, team, period, season) {
  paste0("http://games.espn.com/ffl/boxscorefull?",
         "leagueId=",         league,
         "&teamId=",          team,
         "&scoringPeriodId=", period,
         "&seasonId=",        season,
         "&view=",            "scoringperiod",
         "&version=",         "full")
}


scrape_fullbox <- function(espn_url) {
  offense <-
    read_html(espn_url) %>%
    html_nodes(css  = "#playertable_0") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot = X1,
           player = X2,
           pass_made = X6,
           pass_yds = X7,
           pass_td = X8,
           pass_int = X9,
           rush_atempt = X11,
           rush_yds = X12,
           rush_td = X13,
           recv_made = X15,
           recv_yds = X16,
           recv_td = X17,
           recv_target = X18,
           fumble = X21,
           misc_td = X22,
           points = X24) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             read_html(espn_url) %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col = player,
             into = c("player", "team"),
             sep = ", ") %>%
    separate(col = team,
             into = c("team", "role"),
             sep = "\\s")
  kicking <-
    read_html(espn_url) %>%
    html_node("#playertable_1") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot = X1,
           player = X2,
           kick_long = X8,
           kick_total = X9,
           kick_extra = X10,
           points = X12) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             read_html(espn_url) %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col = player,
             into = c("player", "team"),
             sep = ", ") %>%
    separate(col = team,
             into = c("team", "role"),
             sep = "\\s")

  defense <-
    read_html(espn_url) %>%
    html_node("#playertable_2") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot = X1,
           player = X2,
           dst_td = X6,
           dst_int = X7,
           dst_recov = X8,
           dst_sack = X9,
           dst_safe = X10,
           dst_block = X11,
           dst_allow = X12,
           points = X14) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             read_html(espn_url) %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col = player,
             into = c("player", "role"),
             sep = "\\s")

  bench <-
    read_html(espn_url) %>%
    html_node("#playertable_3") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot = X1,
           player = X2,
           points = X24) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             read_html(espn_url) %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points),
           slot = "BE") %>%
    separate(col = player,
             into = c("player", "team"),
             sep = ", ") %>%
    separate(col = team,
             into = c("team", "role"),
             sep = "\\s")
  return(bind_rows(offense, kicking, defense, bench) %>%
           select(slot, player, team, role, points))
}

scrape_fullbox(espn_url = fullbox_url(league = 252353,
                                      team   = 1,
                                      period = 1,
                                      season = 2018))

empty <- rep(list(rep(list(NA), 10)), 12)

for (p in 1:12) {
  for (t in c(1:11)[-7]) {
    empty[[p]][[t]] <-
      scrape_fullbox(espn_url = fullbox_url(league = 252353,
                                            team   = t,
                                            period = p,
                                            season = 2018)) %>%
      mutate(season = as.character(2018),
             period = as.character(p))
  }
}

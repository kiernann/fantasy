library(tidyverse)
library(rvest)
start_time <- Sys.time()

# URL function ------------------------------------------------------------

# Function to define URL
fullbox_url <- function(league, team, period, season) {
  paste0("http://games.espn.com/ffl/boxscorefull?",
         "leagueId=",         league,
         "&teamId=",          team,
         "&scoringPeriodId=", period,
         "&seasonId=",        season,
         "&view=",            "scoringperiod",
         "&version=",         "full")
}

# Scrape 2018 fullbox -----------------------------------------------------

# Function to scrape ESPN full box score page
scrape_fullbox18 <- function(espn_url) {
  
  # Read webpage as XML document
  espn_html <- read_html(espn_url)
  
  # Scrape offensive starters
  # Store in list
  offense18 <-
    espn_html %>%
    html_nodes(css  = "#playertable_0") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot        = X1,
           player      = X2,
           pass_made   = X6,
           pass_yds    = X7,
           pass_td     = X8,
           pass_int    = X9,
           rush_atempt = X11,
           rush_yds    = X12,
           rush_td     = X13,
           recv_made   = X15,
           recv_yds    = X16,
           recv_td     = X17,
           recv_target = X18,
           fumble      = X21,
           misc_td     = X22,
           points      = X24) %>%
    select(-starts_with("X")) %>%
    
    # Add variable for player owner
    mutate(owner  =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    
    # Seperate player string into name and team
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    
    # Seperate team string into NFL team and field position
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  
  # Scrape starting kicker
  kicker18 <-
    espn_html %>%
    html_node("#playertable_1") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot       = X1,
           player     = X2,
           kick_long  = X8,
           kick_total = X9,
           kick_extra = X10,
           points = X12) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  
  # Scrape starting defense
  defense18 <-
    espn_html %>%
    html_node("#playertable_2") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot      = X1,
           player    = X2,
           dst_td    = X6,
           dst_int   = X7,
           dst_sack  = X9,
           dst_safe  = X10,
           dst_block = X11,
           dst_allow = X12,
           points    = X14) %>%
    select(-starts_with("X")) %>%
    mutate(owner =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col = player,
             into = c("player", "role"),
             sep = "\\s")
  
  # Scrape benched players of all positions
  bench18 <-
    espn_html %>%
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
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points),
           slot   = "BE") %>%
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  
  # Bind each list into single rectangle
  return(bind_rows(offense18,
                   kicker18,
                   defense18,
                   bench18))
}

# Loop 2018 box scores ----------------------------------------------------

# Create empty list to store lists
# Each element contains 1 team from 1 week
empty18 <- rep(list(rep(list(NA), 10)), 12)

# For every week...
for (p in 1:12) {
  
  # For every team...
  for (t in c(1:11)[-7]) {
    empty18[[p]][[t]] <-
      
      # Create URL with week and team numbers
      scrape_fullbox18(espn_url = fullbox_url(league = 252353,
                                              team   = t,
                                              period = p,
                                              season = 2018)) %>% 
      
      # Add those values as variables
      mutate(season = as.character(2018),
             period = as.character(p))
  }
}

# Combine 2018 box scores -------------------------------------------------

# Create empty list for team rectangles
bind_list18 <- rep(list(NA), 11)

# Remove the 7th list for old team
for (i in c(1:11)[-7]) {
  bind_list18[[i]] <- lapply(empty18, "[[", i) %>% bind_rows
}

# 
bind_list18 <- bind_list18[-7]

# Turn lists of lists of lists into rectangle
all18 <- bind_rows(bind_list18)

# Scrape 2017 fullbox -----------------------------------------------------

# Do the same for 2017 full box scores, which have slightly different format
scrape_fullbox17 <- function(espn_url) {
  espn_html <- read_html(espn_url)
  offense17 <- 
    espn_html %>%
    html_nodes(css  = "#playertable_0") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    
    # Only difference is the order of these variables
    rename(slot        = X1,
           player      = X2,
           pass_made   = X4,
           pass_yds    = X5,
           pass_td     = X6,
           pass_int    = X7,
           rush_atempt = X9,
           rush_yds    = X10,
           rush_td     = X11,
           recv_made   = X13,
           recv_yds    = X14,
           recv_td     = X15,
           recv_target = X16,
           fumble      = X19,
           misc_td     = X20,
           points      = X22) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  kicker17 <-
    espn_html %>%
    html_node("#playertable_1") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot       = X1,
           player     = X2,
           kick_long  = X6,
           kick_total = X7,
           kick_extra = X8,
           points     = X10) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  
  defense17 <-
    espn_html %>%
    html_node("#playertable_2") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot      = X1,
           player    = X2,
           dst_td    = X4,
           dst_int   = X5,
           dst_sack  = X7,
           dst_safe  = X8,
           dst_block = X9,
           dst_allow = X10,
           points    = X12) %>%
    select(-starts_with("X")) %>%
    mutate(owner =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points)) %>%
    separate(col  = player,
             into = c("player", "role"),
             sep  = "\\s")
  
  bench17 <-
    espn_html %>%
    html_node("#playertable_3") %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    slice(4:n()) %>%
    rename(slot   = X1,
           player = X2,
           points = X22) %>%
    select(-starts_with("X")) %>%
    mutate(owner  =
             espn_html %>%
             html_node(css = "div.teamInfoOwnerData") %>%
             html_text() %>%
             word(1) %>%
             tolower(),
           points = as.numeric(points),
           slot   = "BE") %>%
    separate(col  = player,
             into = c("player", "team"),
             sep  = ", ") %>%
    separate(col  = team,
             into = c("team", "role"),
             sep  = "\\s")
  
  return(bind_rows(offense17,
                   kicker17,
                   defense17,
                   bench17))
  
}

# Loop 2017 box scores ----------------------------------------------------

empty17 <- rep(list(rep(list(NA), 10)), 12)

for (p in 1:12) {
  for (t in c(1:11)[-7]) {
    empty17[[p]][[t]] <-
      scrape_fullbox17(espn_url = fullbox_url(league = 252353,
                                              team   = t,
                                              period = p,
                                              season = 2017)) %>%
      mutate(season = as.character(2017),
             period = as.character(p))
  }
}

# Combine 2017 box scores -------------------------------------------------

bind_list17 <- rep(list(NA), 11)
for (i in c(1:11)[-7]) {
  bind_list17[[i]] <- lapply(empty17, "[[", i) %>% bind_rows
}
bind_list17 <- bind_list17[-7]
all17 <- bind_rows(bind_list17)

# Combine all scores ------------------------------------------------------

# Combine 2017 and 2018 rectangles
all <- 
  bind_rows(all17, all18) %>% 
  select(season, period, owner, everything())

# Seperate fractional values into two variables
all <- all %>% 
  separate(col  = pass_made, 
           into = c("pass_comp", "pass_attempt"),
           sep  = "/") %>% 
  separate(col  = kick_long, 
           into = c("kick_long_made", "kick_long_attempt"),
           sep  = "/") %>% 
  separate(col  = kick_total, 
           into = c("kick_made", "kick_attempt"),
           sep  = "/") %>% 
  separate(col  = kick_extra, 
           into = c("kick_pat", "kick_pat_attempt"),
           sep  = "/")

print(Sys.time() - start_time)

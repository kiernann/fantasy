library(tidyverse)
draft15 <- 
  read_csv("./data/draft_2015.csv") %>% 
  mutate(year = parse_datetime("2015-08-15 17:00"),
         type = "snake")

draft16 <- 
  read_csv("./data/draft_2016.csv") %>% 
  mutate(year = parse_datetime("2016-09-05 14:30"),
         type = "snake")

draft17 <- 
  read_csv("./data/draft_2017.csv") %>% 
  mutate(year = parse_datetime("2017-08-31 21:00"),
         type = "auction")

draft18 <- 
  read_csv("./data/draft_2018.csv") %>% 
  mutate(year = parse_datetime("2018-09-05 21:00"),
         type = "auction")

draft_history <- bind_rows(draft15, draft16, draft17, draft18)
write_csv(draft_history, "./data/draft_history.csv")

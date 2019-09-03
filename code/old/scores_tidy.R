library(googledrive)
drive_get("~/home/fantasy/gaa.18")$drive_resource
drive_download(file = "gaa.18",
               path = "./data/gaa",
               type = "csv",
               overwrite = TRUE)
scores <- 
  read_csv("./data/gaa.csv",
           col_types = cols(date = col_date(format = "%m/%d/%Y"))) %>%
  select(-year, -week, -game) %>% 
  gather(-date,
         key = team,
         value = score) %>% 
  arrange(date)

write_csv(scores, "./data/scores_tidy.csv")

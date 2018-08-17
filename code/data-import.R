options(stringsAsFactors = FALSE)
library(tidyverse)
library(RCurl)
source('code/functions.R')

# Import raw data
datafolder <- '../../Data/tennis_wta-master'

file_list <- list.files(datafolder)[str_detect(list.files(datafolder), 
                                               "wta_matches_[:digit:]{4}.csv")]

# Function to import one match file based on list above
import_data <- function(x) {
  indata <- fread(file.path(datafolder, x)) %>% 
    as.tibble() %>% 
    filter(!is.na(score), 
           score != "",
           str_detect(score, '[:digit:]'), 
           str_detect(score, '-')) %>% 
    mutate(match_id = paste(sep = "-", tourney_id, match_num))
  
  tourney_info <- masterdata %>% 
    select(starts_with("tourney"), surface, draw_size, best_of, round) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  match_info <- masterdata %>% 
    select(ends_with("id")) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  # player id
  win <- masterdata %>% select(starts_with("winner")) 
  names(win) <- str_replace(string = names(win), pattern = "winner|loser", replacement = "player")
  
  lose <- masterdata %>% select(starts_with("loser"))
  names(lose) <- str_replace(string = names(lose), pattern = "winner|loser", replacement = "player")
  
  player_info <- bind_rows(win, lose) %>% 
    select(player_id, player_name, player_hand, player_ht, player_ioc) %>% 
    unique()
}

  
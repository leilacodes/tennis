options(stringsAsFactors = FALSE)
library(tidyverse)
library(RCurl)
library(data.table)
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
  
  # Don't include 'best of' bc some rows say 5
  tourney_info <- indata %>% 
    select(starts_with("tourney"), surface, draw_size) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  match_info <- indata %>% 
    select(ends_with("id"), round) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  # player id
  win <- indata %>% select(starts_with("winner")) 
  names(win) <- str_replace(string = names(win), pattern = "winner|loser", replacement = "player")
  
  lose <- indata %>% select(starts_with("loser"))
  names(lose) <- str_replace(string = names(lose), pattern = "winner|loser", replacement = "player")
  
  player_info <- bind_rows(win, lose) %>% 
    select(player_id, player_name, player_hand, player_ht, player_ioc) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  return(list(tourney_info = tourney_info,
              match_info = match_info, 
              player_info = player_info))
}

masterlists <- lapply(file_list, import_data)

tournament_list <- map_dfr(masterlists, function(x) x[["tourney_info"]])  %>% 
  mutate(tourney_date = as.Date.character(as.character(tourney_date), "%Y%m%d"))

match_list <- map_dfr(masterlists, function(x) x[["match_info"]]) 

player_info <- map_dfr(masterlists, function(x) x[["player_info"]]) %>% unique()

# QA and clean the tables --------------------------
match_list %>% group_by(match_id) %>% filter(n() > 1)
tournament_list %>% group_by(tourney_id)

lapply(player_info, anyDuplicated)

# Some dupes for changing country, some for knowing hand
hand_code <- data.frame(player_hand = c("R", "L", "U", "", NA), priority = c(1, 1, 0, 0, 0))

# Favor known hand, break ties with latest record
# Need to hard code Ankita Raina's hand
player_info <- player_info %>% 
  mutate(player_hand = ifelse(player_id == 206102, "R", player_hand)) %>%  
  group_by(player_id) %>% 
  left_join(hand_code) %>% 
  filter(priority == max(priority), 
         sourcefile == max(sourcefile)) %>% 
  unique() %>% 
  arrange(player_id)

freq(player_info$player_hand)

# Most unknown hand is pre-2000

# Aggregate win-loss
winloss <- match_list %>% 
  left_join(tournament_list %>% select(tourney_id, tourney_date)) %>% 
  arrange(tourney_date) %>% 
  group_by(winner_id, loser_id) %>% 
  mutate(matchcount = row_number()) %>% 
  arrange(winner_id, loser_id, tourney_id)

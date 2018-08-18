options(stringsAsFactors = FALSE)
library(tidyverse)
library(RCurl)
source('code/functions.R')

# Import raw data
datafolder <- '../../Data/tennis_wta-master'

list.files(datafolder)[str_detect(list.files(datafolder), "wta_matches_[:digit:]{4}.csv")]

# Function to import one match file
rawdata <- read.csv(file.path(datafolder, 'wta_matches_2014.csv'))

# firstlook(rawdata)

# graph_freqs(dset = rawdata, 
            # varlist = c("surface", "draw_size", "tourney_level", "winner_hand", "winner_ioc"))

# Clean data and make match ID
# Remove null scores, matches with games played
masterdata <- rawdata %>% 
  filter(!is.na(score), 
         score != "",
         str_detect(score, '[:digit:]'), 
         str_detect(score, '-')) %>% 
  mutate(match_id = paste(sep = "-", tourney_id, match_num))

# varinfo(masterdata)

# Check for duplicates
# masterdata %>% group_by(match_id) %>% 
  # filter(n() > 1)

# freq(rawdata$surface)


# Separate out lookup tables
tourney_info <- masterdata %>% 
  select(starts_with("tourney"), surface, draw_size, best_of, round) %>% 
  unique()

# # dupe check
# tourney_id %>% group_by(tourney_id) %>% filter(n() > 1)
# length(tourney_id$tourney_id) - length(unique(tourney_id$tourney_id))

varinfo(tourney_info)

match_info <- masterdata %>% 
  select(ends_with("id")) %>% 
  unique()

# dupe check
varinfo(match_info)

# player id
win <- masterdata %>% select(starts_with("winner")) 
names(win) <- str_replace(string = names(win), pattern = "winner|loser", replacement = "player")
win %>% head()

lose <- masterdata %>% select(starts_with("loser"))
names(lose) <- str_replace(string = names(lose), pattern = "winner|loser", replacement = "player")

player_info <- bind_rows(win, lose) %>% 
  select(player_id, player_name, player_hand, player_ht, player_ioc) %>% 
  unique()
  
freq(player_info$player_hand)

player_info %>% filter(!(player_hand %in% c("L", "R")))

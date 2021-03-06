source('functions/setup.R')

# Import raw data
datafolder <- '../tennis_wta'

file_list <- list.files(datafolder)[str_detect(list.files(datafolder), 
                                               "wta_matches_[:digit:]{4}.csv")]

# Function to import one match file based on list above
import_data <- function(x) {
  indata <- import_match_file(x) 
  
  # Don't include 'best of' bc some rows say 5
  tourney_info <- indata %>% 
    select(starts_with("tourney"), surface, draw_size) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  match_info <- indata %>% 
    select(ends_with("id"), round) %>% 
    unique() %>% 
    mutate(sourcefile = x)
  
  entry_info <- indata %>% 
    select(-best_of, -ends_with("hand"), 
           -ends_with("_ht"),
           -ends_with("_ioc"),
           -starts_with("w_"),
           -starts_with("l_")) %>% 
    select(tourney_id, match_id, round, draw_size, tourney_date,
           starts_with("winner"), starts_with("loser"),
           everything()) %>% 
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
              player_info = player_info, 
              entry_info = entry_info))
}

tic()
masterlists <- lapply(file_list, import_data)
toc()

tournament_list <- map_dfr(masterlists, function(x) x[["tourney_info"]])  %>% 
  mutate(tourney_date = as.Date.character(as.character(tourney_date), "%Y-%m-%d"))

match_list <- map_dfr(masterlists, function(x) x[["match_info"]]) 

player_info <- map_dfr(masterlists, function(x) x[["player_info"]]) %>% unique()

entry_info <- map_dfr(masterlists, 
                      function(x) {
                        x[["entry_info"]] %>% 
                          mutate_at(.vars = c("winner_seed", "loser_seed"), 
                                    .funs = as.numeric) %>% 
                          unique()
                      })

# QA and clean the tables --------------------------
firstlook(tournament_list)
firstlook(match_list)
firstlook(player_info)
firstlook(entry_info)

# Check out missing values
entry_info %>% group_by(sourcefile) %>% 
  summarise(sum(is.na(winner_seed)))

# Seeding info changes after 1980 

# Check for dupes
match_list %>% group_by(match_id) %>% filter(n() > 1)
tournament_list %>% group_by(tourney_id) %>% filter(n() > 1)

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

player_info %>% group_by(player_id) %>% filter(n() > 1)

# Most unknown hand is pre-2000

# Aggregate win-loss
# Subtract 1 to count only previous
match_list <- match_list %>% 
  left_join(tournament_list %>% select(tourney_id, tourney_date)) %>% 
  arrange(tourney_date) %>% 
  group_by(winner_id, loser_id) %>% 
  arrange(winner_id, loser_id, tourney_date)  %>% ungroup()

career_wins_vs <- match_list %>% 
  arrange(winner_id, loser_id, tourney_date) %>% 
  group_by(winner_id, loser_id) %>% 
  mutate(career_wins = row_number())  %>% ungroup()

career_losses_vs <- match_list %>% 
  arrange(loser_id, winner_id, tourney_date) %>% 
  group_by(loser_id, winner_id) %>% 
  mutate(career_losses = row_number())  %>% ungroup()

# career_wins_surface <- match_list %>% 
#   left_join(tournament_list %>% select(tourney_id, surface)) %>% 
#   arrange(winner_id, surface, tourney_date) %>% 
#   group_by(winner_id, surface) %>% 
#   mutate(career_wins_surface = row_number()) %>% ungroup()
# 
# career_losses_surface <- match_list %>% 
#   left_join(tournament_list %>% select(tourney_id, surface)) %>% 
#   arrange(loser_id, surface, tourney_date) %>% 
#   group_by(loser_id, surface) %>% 
#   mutate(career_wins_surface = row_number()) %>% ungroup()

rm(masterlists)

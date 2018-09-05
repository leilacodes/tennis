# recode_entry <- function(x) {
#   recode(x,
#          A = "",
#          ALT = "",
#          IP = "",
#          LL = "Lucky Loser",
#          Q = "Qualifier",
#          SE = "Special Exempt",
#          SR = "",
#          WC = "Wild Card"
#          )
# }


import_match_file <- function(filename) {
  rawdata <- fread(file.path(datafolder, filename),
                   fill = TRUE)
  
  cleandata <- rawdata %>%  
    drop_na(score) %>% 
    filter(score != "",
           !str_detect(score, "[:alpha:]"), 
           str_detect(score, '[:digit:]-[:digit:]'),
           best_of != 5)  %>% 
    mutate(tourney_date = as.Date.character(as.character(tourney_date), "%Y%m%d"),
           match_id = paste(sep = "-", tourney_id, round, match_num))
  
  return(cleandata)
}

prep_dset <- function(dset, prefix) {
  
  outcome <- dset$win %>% unique()
  
  if(length(outcome) > 1) {
    stop("Data Error: Only one outcome allowed")
  } else if(outcome == TRUE) {
    names(dset) <- str_replace_all(names(dset),
                                   pattern = "winner_|^w_", 
                                   replacement = "player_")
    names(dset) <- str_replace(names(dset),
                               pattern = "loser_|^l_", 
                               replacement = "opponent_")
  } else if (outcome == FALSE) {
    names(dset) <- str_replace_all(names(dset),
                                   pattern = "loser_|^l_", 
                                   replacement = "player_")
    
    names(dset) <- str_replace_all(names(dset),
                                   pattern = "winner_|^w_", 
                                   replacement = "opponent_")
  } else {
    stop("Data error: need outcome")
  }
  
  return(dset)
}

getlosses <- function(x) {
  print(x[1,]$sourcefile)
  
  players <- x %>% select(player_id, opponent_id) %>% unique()
  
  loss_subset <- career_losses_vs %>%
    select(loser_id, winner_id, tourney_date, career_losses) %>%
    inner_join(players, by = c("loser_id" = "player_id",
                               "winner_id" = "opponent_id")) %>%
    filter(tourney_date < max(x$tourney_date))
  
  output <- x %>%
    fuzzy_left_join(loss_subset,
                    by = c("player_id" = "loser_id",
                           "opponent_id" = "winner_id",
                           "tourney_date" = "tourney_date"),
                    match_fun = c(`==`, `==`, `>`)) %>%
    replace_na(replace = list(career_losses = 0)) %>%
    group_by(match_id) %>%
    filter(career_losses == max(career_losses)) %>%
    select(-ends_with('.y'))
  
  return(output)
}

getwins <- function(x) {

  print(x[1,]$sourcefile)

  players <- x %>% select(player_id, opponent_id) %>% unique()

  win_subset <- career_wins_vs %>%
    select(loser_id, winner_id, tourney_date, career_wins) %>%
    inner_join(players, by = c("winner_id" = "player_id",
                               "loser_id" = "opponent_id")) %>%
    filter(tourney_date < max(x$tourney_date))

  output <- x %>%
    fuzzy_left_join(win_subset,
                    by = c("player_id" = "winner_id",
                           "opponent_id" = "loser_id",
                           "tourney_date" = "tourney_date"),
                    match_fun = c(`==`, `==`, `>`)) %>%
    replace_na(replace = list(career_wins = 0)) %>%
    group_by(match_id) %>%
    filter(career_wins == max(career_wins)) %>%
    select(-ends_with('.y'))

  return(output)
}

# Check that it worked
# match_list %>% filter((winner_id == "202469" & loser_id == "201347") |
#                         (winner_id == "201347" & loser_id == "202469")) %>%
#   select(winner_id, loser_id, tourney_date) %>% arrange(tourney_date)
# 
# withwins %>% filter(player_id == "201347", opponent_id == "202469") %>% 
#   select(player_name, opponent_name, tourney_date.x, career_wins)
# 
# withlosses %>% filter(player_id == "201347", opponent_id == "202469")%>%
#   select(player_name, opponent_name, tourney_date.x, career_losses)
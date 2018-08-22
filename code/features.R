source('functions/setup.R')
source('code/data_import.R')

# Subset to modeling data

aggdata <- entry_info %>% filter(year(tourney_date) > 1987)

splitdata <- split(aggdata, aggdata$sourcefile)

getlosses <- function(x) {
  print(x[1,]$sourcefile)
  
  players <- x %>% select(winner_id, loser_id) %>% unique()
  
  loss_subset <- career_losses_vs %>%
    select(loser_id, winner_id, tourney_date, career_losses) %>% 
    inner_join(players) %>% 
    filter(tourney_date < max(x$tourney_date))
    
  output <- x %>% 
    fuzzy_left_join(loss_subset,
                    by = c("winner_id" = "loser_id",
                           "loser_id" = "winner_id", 
                           "tourney_date" = "tourney_date"), 
                    match_fun = c(`==`, `==`, `>`)) %>%
    replace_na(replace = list(career_losses = 0)) %>% 
    group_by(match_id) %>% 
    filter(career_losses == max(career_losses)) %>% 
    select(-ends_with('.y'))
  
  return(output)
}

tic()
withlosses <- map_dfr(splitdata , getlosses)
toc()

# Get wins
getwins <- function(x) {
  print(x[1,]$sourcefile)
  
  players <- x %>% select(winner_id, loser_id) %>% unique()
  
  win_subset <- career_wins_vs %>%
    select(loser_id, winner_id, tourney_date, career_wins) %>% 
    inner_join(players) %>% 
    filter(tourney_date < max(x$tourney_date))
  
  output <- x %>% 
    fuzzy_left_join(win_subset,
                    by = c("winner_id" = "winner_id",
                           "loser_id" = "loser_id", 
                           "tourney_date" = "tourney_date"), 
                    match_fun = c(`==`, `==`, `>`)) %>%
    replace_na(replace = list(career_wins = 0)) %>% 
    group_by(match_id) %>% 
    filter(career_wins == max(career_wins)) %>% 
    select(-ends_with('.y'))
  
  return(output)
}

tic()
withwins <- map_dfr(splitdata, getwins)
toc()

# Whew save the data!
saveRDS(withlosses, file = "data/withlosses.RDS")
saveRDS(withwins, filie = "data/withwins.RDS")

# Check that it worked

#QA
graph_freqs(entry_info)

# Designate each match as a win or loss
set.seed <- 11271989

outcomedata <- entry_info %>% 
  bind_cols(., win = rbernoulli(nrow(entry_info)))

splitdata <- split(outcomedata, outcomedata$win)

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

# Define features (Including pulling in match records)
# The first join 
prepped_dset <- map_dfr(splitdata, prep_dset) %>%
  mutate(rank_point_diff = player_rank_points - opponent_rank_points,
         age_diff = player_age - opponent_age) 

# Career losses
tic()
agg_losses <- map_dfr(split_dset, function(x) {
  x %>% ste, match_id) %>% 
    fuzzy_left_join(career_losses_vs %>% select(loser_id, winner_id, tourney_date, career_losses, match_id),  
                                                            by = c("player_id" = "loser_id",
                                                                   "opponent_id" = "winner_id",
                                                                   "tourney_date" = "tourney_date"),
                    match_fun = list(`==`, `==`, `>`)) %>% 
    group_by(match_id.x) %>% 
    replace_na(replace = list(career_losses = 0)) %>% 
    filter(career_losses == max(career_losses)) %>% 
    select(-ends_with(".y"), -loser_id, -winner_id)
})
toc()

tic()
agg_wins <- map_dfr(split_dset, function(x) {
  x %>% select(player_id, opponent_id, tourney_date, match_id) %>% 
    fuzzy_left_join(career_wins_vs %>% select(loser_id, winner_id, tourney_date, career_wins, match_id),  
                    by = c("player_id" = "loser_id",
                           "opponent_id" = "winner_id",
                           "tourney_date" = "tourney_date"),
                    match_fun = list(`==`, `==`, `>`)) %>% 
    group_by(match_id.x) %>% 
    replace_na(replace = list(career_wins = 0)) %>% 
    filter(career_wins == max(career_wins)) %>% 
    select(-ends_with(".y"), -loser_id, -winner_id)
})
toc()

  # fuzzy_left_join(x = .,
  #                 y = career_losses_vs,
                  # by = c("player_id" = "loser_id",
                  #        "opponent_id" = "winner_id",
                  #        "tourney_date" = "tourney_date"),
                  # match_fun = list(`==`, `==`, `>`)) %>%
  # fuzzy_anti_join(career_losses_vs %>% select(loser_id, winner_id, tourney_date),
  #                 by = c("loser_id" = "loser_id",
  #                        "winner_id" = "winner_id",
  #                        "tourney_date" = "tourney_date"),
  #                 match_fun = list(`==`, `==`, `>`))

  

firstlook(prepped_dset)

# Add features

# Commented out if no relationship visible
plotlift <- function(x) {
  plot_ntile_lift(prepped_dset, quo(win), x)
}

plotlift(quo(player_rank_points))
plotlift(quo(opponent_rank_points))
plotlift(quo(rank_point_diff))
plotlift(quo(player_age))
plotlift(quo(opponent_age))

# plot_ntile_lift(prepped_dset, quo(age_diff))

# Multicollinearity
cor(prepped_dset %>%
      select(player_rank_points, opponent_rank_points, rank_point_diff) %>% 
      na.exclude)


# Weight by how not-close the score was?

# Momentum
# Wins vs this player
# Recent wins
# Recent ranking gains
# Recent wins vs top x players
# Recent wins vs players of this type

# History
# Last result vs this player
# Historical success on this surface
# Historical success vs this playing style

# Defending champion
# Opponent's momentum
# Number of titles
# Number of finals

# Trends over time?
# Weight by draw size (do people perform differently under different stakes?)
# First grand slam?

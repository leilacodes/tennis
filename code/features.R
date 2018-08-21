source('functions/setup.R')
source('code/data_import.R')

# Import files
# Filter out null scores and withdrawals/retirements
# Remove columns that had to be cleaned
dset <- import_match_file('wta_matches_2018.csv') %>%  
  select(-best_of, -ends_with("hand"), 
         -ends_with("_ht"),
         -ends_with("_ioc"))

firstlook(dset)

dset %>% filter(is.na(winner_rank_points) | is.na(loser_rank_points)) %>% View()

#QA
graph_freqs(dset)

# Designate each match as a win or loss
set.seed <- 11271989

outcomedata <- dset %>% 
  bind_cols(., win = rbernoulli(nrow(dset)))

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
prepped_dset <- map_dfr(splitdata, prep_dset) %>% 
  mutate(rank_point_diff = player_rank_points - opponent_rank_points,
         age_diff = player_age - opponent_age) %>% 
  fuzzy_left_join(x = ., 
                  y = career_losses_vs,
                  by = c("player_id" = "loser_id", 
                         "opponent_id" = "winner_id",
                         "tourney_date" = "tourney_date")
                  # match_fun = list(`==`, `==`, `>=`))
                  # Left off: don't run that! only need the max date not all of them
  

firstlook(prepped_dset)

# Add features

plot_ntile_lift <- function(dset, yvar, xvar) {
  plotdata <- dset %>% 
    mutate(ntile_var = ntile(!!xvar, n = 40)) %>% 
  group_by(ntile_var) %>% 
  summarise(reponse_pct = mean(!!yvar), 
            n = n())
  
  print(plotdata)
  
  ggplot(data = plotdata, 
         aes(x = ntile_var, y = reponse_pct, size = n)) +
    geom_point() + 
    labs(title = xvar,
         subtitle = "20 bins")
}

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

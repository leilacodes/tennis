source('functions/setup.R')
source('code/data_import.R')

# Subset to modeling data

aggdata <- entry_info %>% filter(year(tourney_date) > 1987)

#QA
graph_freqs(aggdata)

freq(aggdata$surface)

aggdata %>% filter(surface == "") %>% group_by(sourcefile) %>% View()

firstlook(aggdata)

# Designate each match as a win or loss
set.seed <- 11271989

outcomedata <- aggdata %>% 
  bind_cols(., win = rbernoulli(nrow(aggdata)))

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

# Apply randomization and recombine dataset
# Add a few features
modeldset <- map_dfr(splitdata, prep_dset) %>%
  mutate(rank_point_diff = player_rank_points - opponent_rank_points,
         age_diff = player_age - opponent_age) 

splitdata <- split(modeldset, modeldset$sourcefile)

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

tic()
withlosses <- map_dfr(splitdata , getlosses) %>% rename(tourney_date = tourney_date.x)
toc()

# Get wins
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

tic()
withwins <- map_dfr(splitdata, getwins) %>% rename(tourney_date = tourney_date.x)
toc()

# Whew save the data!
saveRDS(withlosses, file = "data/withlosses.RDS")
saveRDS(withwins, file = "data/withwins.RDS")

# Check that it worked
match_list %>% filter((winner_id == "202469" & loser_id == "201347") |
                        (winner_id == "201347" & loser_id == "202469")) %>%
  select(winner_id, loser_id, tourney_date) %>% arrange(tourney_date)

withwins %>% filter(player_id == "201347", opponent_id == "202469") %>% 
  select(player_name, opponent_name, tourney_date.x, career_wins)

withlosses %>% filter(player_id == "201347", opponent_id == "202469")%>%
  select(player_name, opponent_name, tourney_date.x, career_losses)

# Add features
# Fed Cup website says youngest player was 12
prepped_dset <- modeldset %>% 
  left_join(withwins) %>% 
  left_join(withlosses) %>% 
  filter(player_age >= 12, opponent_age >= 12) 
  
# Ranking points changed in 2016 not to count the Olympics
# Fill in ranking points that are missing?
# Remove temp dsets
firstlook(prepped_dset)
varinfo(prepped_dset) %>% arrange(desc(ndistinct))

# Cannot find Laura Hernandez online -- delete? Tweeted Jeff Sackman
# Delete age under 12 since Fed Cup website says youngest player was 12
prepped_dset %>% filter(player_age < 13 | opponent_age < 13)
prepped_dset %>% filter(player_name == "Denise")


# Commented out if no relationship visible
plotlift <- function(x) {
  plot_ntile_lift(prepped_dset, quo(win), x)
}

ggplot(data = prepped_dset, aes(x = player_rank_points)) + geom_density()

plotlift(quo(player_rank_points))
plotlift(quo(opponent_rank_points))
plotlift(quo(rank_point_diff))
plotlift(quo(age_diff))
plotlift(quo(career_wins))
plotlift(quo(career_losses))

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

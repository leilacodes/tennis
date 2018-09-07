source('functions/setup.R')
source('code/data_import.R')

# Subset to modeling data
# aggdata <- entry_info %>% filter(year(tourney_date) > 1987)

# Include only as far back as to include the players on tour in 2016+
# playerpool <- match_list %>%
#   filter(year(tourney_date) >= 2016) %>% select(winner_id, loser_id)
# 
# aggdata <- entry_info %>% filter(year(tourney_date) > 1987,
#                                  (winner_id %in% unique(c(playerpool$winner_id,
#                                                           playerpool$loser_id)) |
#                                    loser_id %in% unique(c(playerpool$winner_id,
#                                                           playerpool$loser_id))))
# ggplot(data = aggdata %>% mutate(age_diff = winner_age - loser_age)) +
#   geom_histogram(aes(x = age_diff))
# aggdata %>% group_by(tourney_date) %>% tally()
# 
# #QA
# graph_freqs(aggdata)
# freq(aggdata$surface)
# 
# # Do we want to do something about null surfaces? all fed cup
# aggdata %>% filter(surface == "") %>% group_by(sourcefile) %>% View()
# 
# firstlook(aggdata)
# 
# # Designate each match as a win or loss
# set.seed <- 11271989
# 
# outcomedata <- aggdata %>% 
#   bind_cols(., win = rbernoulli(nrow(aggdata)))
# 
# splitdata <- split(outcomedata, outcomedata$win)
# 
# # Apply randomization and recombine dataset
# # Add a few features
# modeldset <- map_dfr(splitdata, prep_dset) %>%
#   mutate(rank_point_diff = player_rank_points - opponent_rank_points,
#          age_diff = player_age - opponent_age) 
# 
# # splitdata <- split(modeldset, modeldset$sourcefile)
# 
# # # Get losses
# # tic()
# # withlosses <- map_dfr(splitdata , getlosses) %>% rename(tourney_date = tourney_date.x)
# # toc()
# # 
# # # Get wins
# #
# # tic()
# # withwins <- map_dfr(splitdata, getwins) %>% rename(tourney_date = tourney_date.x)
# # toc()
# 
# # Whew save the data!
# # saveRDS(withlosses, file = "data/withlosses.RDS")
# # saveRDS(withwins, file = "data/withwins.RDS")
# 
# withlosses <- readRDS('data/withlosses.RDS') %>% select(tourney_id, match_id, round, opponent_id, player_id, career_losses)
# withwins <- readRDS('data/withwins.RDS') %>% select(tourney_id, match_id, round, opponent_id, player_id, career_wins)
# 
# # Add features
# # Fed Cup website says youngest player was 12
# # Cannot find Laura Hernandez online -- delete? Tweeted Jeff Sackman
# # Delete age under 12 since Fed Cup website says youngest player was 12
# prepped_dset %>% filter(player_age < 13 | opponent_age < 13)
# prepped_dset %>% filter(player_name == "Denise")
# 
# prepped_dset <- modeldset %>% 
#   left_join(withwins) %>% 
#   left_join(withlosses) %>% 
#   filter(player_age >= 12, opponent_age >= 12) %>% 
#   replace_na(replace = list(opponent_rank_points = 0,
#                             player_rank_points = 0,
#                             rank_point_diff = 0, 
#                             career_wins = 0,
#                             career_losses = 0)) %>% 
#   mutate(opp_unseeded = is.na(opponent_seed),
#          player_unseeded = is.na(player_seed),
#          opp_unranked = is.na(opponent_rank),
#          player_unranked = is.na(player_rank),
#          higher_rank_points = case_when(player_rank_points > opponent_rank_points ~ 1,
#                                         !is.na(player_rank_points) & is.na(opponent_rank_points) ~ 1,
#                                         is.na(player_rank_points) & !is.na(opponent_rank_points) ~ 0,
#                                         TRUE ~ 0),
#          higher_seed = case_when(player_seed > opponent_seed ~ 1,
#                                  !is.na(player_seed) & is.na(opponent_seed) ~ 1,
#                                  is.na(player_seed) & !is.na(opponent_seed) ~ 0,
#                                  TRUE ~ 0)) 
# 
# freq(modeldset$player_entry)
# freq(modeldset$opponent_entry)
# 
# saveRDS(prepped_dset, 'data/prepped_dset.RDS')

prepped_dset <- readRDS('data/prepped_dset.RDS')

prepped_dset <- prepped_dset %>% 
  mutate(nsets = str_count(score, pattern = "-"),
         ntiebreaks = str_count(score, pattern = "7-6") + str_count(score, pattern = "6-7"),
         player_qualifier = ifelse(player_entry == "Q", 1, 0),
         opponent_qualifier = ifelse(opponent_entry == "Q", 1, 0),
         player_wc = ifelse(player_entry == "WC", 1, 0),
         opponent_wc = ifelse(opponent_entry == "WC", 1, 0),
         winloss = career_wins - career_losses,
         first_match = ifelse(career_wins == 0 & career_losses == 0, 1, 0)) %>% 
  select(-ends_with("entry"))


# Calculate dominance in a match
# testscores <- prepped_dset %>% filter(ntiebreaks > 2) %>% select(score)

setscores <- prepped_dset %>% separate(col = score, into = c("set1", "set2", "set3"),
           sep = " ") %>% select(starts_with("set")) %>% 
  map_dfc(function(x) map(x, convertscore) %>% unlist())%>% 
  mutate(dominance = set1 + set2 + set3)

model_dset <- prepped_dset %>% bind_cols(setscores)  %>% 
  mutate(set1 = ifelse(win == FALSE, -set1, set1),
         set2 = ifelse(win == FALSE, -set2, set2),
         set3 = ifelse(win == FALSE, -set3, set3),
         dominance = ifelse(win == FALSE, -dominance, dominance)) %>% 
  select(-ends_with("rank"), -match_num, -sourcefile, -draw_size, -starts_with("set"), -dominance) 

saveRDS(model_dset, 'data/model_dset.RDS')

# Ranking points changed in 2016 not to count the Olympics
# Fill in ranking points that are missing?
# Remove temp dsets
firstlook(prepped_dset)
varinfo(prepped_dset) %>% arrange(desc(ndistinct))



# Commented out if no relationship visible
plotlift <- function(x) {
  plot_lift_ntile(prepped_dset, quo(win), x, nbreaks = 10)
}

ggplot(data = prepped_dset, aes(x = player_rank_points)) + geom_density()

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

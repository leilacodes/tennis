source('functions/setup.R')

# Include only as far back as to include the players on tour in 2016+
rawdata <- readRDS('data/model_dset.RDS') 
# %>% 
#   mutate(never_played = ifelse(career_wins == 0 & career_losses == 0, 1, 0),
#          winloss = career_wins - career_losses,
#          played_before = ifelse(never_played == 1, 0, 1))

# saveRDS(rawdata, 'data/model_dset.RDS')

train <- rawdata %>% filter(year(tourney_date) < 2018)
test <- rawdata %>% anti_join(train)

# Baseline model: higher rank
test %>% 
  mutate(pred = ifelse(higher_rank_points == 1, 1, 0),
         err = ifelse(pred == win, 1, 0)) %>% 
  summarise(mean(err))


# Baseline model: higher seed
test %>% 
  mutate(pred = ifelse(higher_seed == 1, 1, 0),
         err = ifelse(pred == win, 1, 0)) %>% 
  summarise(mean(err))

# Different by round?

# More predictable in earlier rounds
test %>% 
  mutate(pred = ifelse(higher_rank_points == 1, 1, 0),
         err = ifelse(pred == win, 1, 0)) %>% 
  group_by(round) %>% 
  summarise(mean(err))

test %>% 
  mutate(pred = ifelse(higher_seed == 1, 1, 0),
         err = ifelse(pred == win, 1, 0)) %>% 
  group_by(round) %>% 
  summarise(mean(err))

# Why is the career wins lift so weird?



# Logistic regression -----------------------------------------------------

names(rawdata)

varlist <- c("player_rank_points",
             "opponent_rank_points", 
             "career_wins",
             "career_losses",
             "higher_seed",
             "played_before",
             "player_qualifier"
             "opponent_unranked",
             "opponent_qualifier",
             "opponent_wc",
             )

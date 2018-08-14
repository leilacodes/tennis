library(tidyverse)
library(ggplot2)
options(stringsAsFactors = FALSE)

# Explore Jeff Sackman's match charting dataset
# Volunteers chart matches to collect data (charted_by column)
# Characterize who gets charted / volunteer preferences
# Can this dataset be used for exploring internal consistency of measurements? (decided no)

# Note: this file can be downloaded from 'https://github.com/JeffSackmann/tennis_MatchChartingProject/blob/master/charting-w-matches.csv'

data_folder <- 'C:/Users/Banhlam/Documents/Data/tennis_MatchChartingProject-master'
match_ids_raw <- read_csv(file = file.path(data_folder, 'charting-w-matches.csv'))

match_ids_clean <- janitor::clean_names(match_ids_raw) %>% 
  filter(!is.na(charted_by))

# Clearly some dominant charters
ggplot(match_ids_clean, aes(x = charted_by)) + geom_bar()

# Who has charted >=10?
ggplot(match_ids_clean %>% group_by(charted_by) %>%
         filter(n() > 9), aes(x = charted_by)) + geom_bar()

# How many players?
allplayers <- data.frame(player = c(match_ids_clean$player_1, 
                                    match_ids_clean$player_2))
length(unique(allplayers$player))

# Distribution of # matches charted?
matchcounts <- allplayers %>% group_by(player) %>% tally() %>% arrange(desc(n))

ggplot(matchcounts, aes(x = n)) + geom_density()

# Many with less than a handful

# Player-charter dataset
charter <- bind_rows(match_ids_clean %>% select(player = player_1, charted_by),
                     match_ids_clean %>% select(player = player_2, charted_by))

# Volunteers' favorite players by # matches charted
favor <- charter %>% group_by(charted_by, player) %>% 
  summarise(n_charted_by_charter = n()) %>% 
  mutate(total_charted = sum(n_charted_by_charter)/2,
         pct_of_charted = n_charted_by_charter / total_charted) %>% 
  # arrange(desc(total_charted), desc(pct_of_charted)) %>% 
  ungroup() %>% group_by(charted_by) %>% 
  mutate(sd_favor = sd(n_charted_by_charter)) %>% 
  filter(total_charted > 10) %>% 
  arrange(charted_by, desc(pct_of_charted))


# Number of players charted overall by each volunteer
favor %>% select(charted_by, total_charted) %>% unique() %>% View()

# Future: Find way to separate out "accidental" favorites: use number of unique opponents?

# # # #

# Crowd favorites: # distinct charters by player
# Doesn't account for number of matches played/tournament success
charter %>% group_by(player) %>% 
  summarise(n_charters = n_distinct(charted_by)) %>% 
  arrange(desc(n_charters))
  

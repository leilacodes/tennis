library(tidyverse)
options(stringsAsFactors = FALSE)

data_folder <- 'C:/Users/Banhlam/Documents/Data/tennis_MatchChartingProject-master'

match_ids_raw <- read_csv(file = file.path(data_folder, 'charting-m-matches.csv'))

match_ids_clean <- janitor::clean_names(match_ids_raw) %>% 
  filter(!is.na(charted_by))

point_stats_raw <- read_csv(file = file.path(data_folder,
                                             'charting-m-points.csv')) %>% 
  janitor::clean_names()

summary(point_stats_raw)
lapply(point_stats_raw, function(x) sum(is.na(x)))

# How many players?
length(unique(allplayers))

# Most matches?
allplayers <- data.frame(player = c(match_ids_clean$player_1, match_ids_clean$player_2))
matchcounts <- allplayers %>% group_by(player) %>% tally() %>% arrange(desc(n))

hist(matchcounts$n)

over20 <- matchcounts %>% filter(n >= 20) %>% pull(player)

match_ids_clean %>% names()

match_ids_clean %>% filter(player_1 == "Alexander Zverev" | player_2 == "Alexander Zverev") %>% 
  group_by(charted_by) %>% 
  tally()

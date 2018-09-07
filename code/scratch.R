library(tidyverse)
options(stringsAsFactors = FALSE)
rawdata <- read_csv(file = 'C:/Users/Banhlam/Documents/Data/tennis_MatchChartingProject-master/charting-m-matches.csv')

summary(rawdata)
rawdata %>% filter(is.na(`Best of`)) %>% View()
lapply(rawdata, function(x) sum(is.na(x)))

cleandata <- janitor::clean_names(rawdata) %>% 
  filter(!is.na(charted_by))
lapply(cleandata, function(x) sum(is.na(x)))

View(head(cleandata))

cleandata %>% group_by(match_id) %>% tally() %>% summary()

# No matches charted by multiple people
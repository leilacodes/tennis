options(stringsAsFactors = FALSE)
library(tidyverse)
library(RCurl)
source('code/functions.R')
source('code/data-import.R')

# Designate each match as a win or loss
set.seed <- 11271989

outcomedata <- masterdata %>% 
  bind_cols(., win = rbernoulli(nrow(masterdata)))

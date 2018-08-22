options(stringsAsFactors = FALSE)
library(tidyverse)
library(data.table)
library(glue)
library(fuzzyjoin)
library(tictoc)
source('functions/utility_functions.R')
source('functions/analysis_functions.R')

datafolder <- '../../Data/tennis_wta-master'

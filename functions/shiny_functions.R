options(stringsAsFactors = FALSE)
library(tidyverse)
library(data.table)
library(glue)
library(fuzzyjoin)
library(tictoc)
source('../functions/utility_functions.R')
source('../functions/analysis_functions.R')

square <- function(x) x^2

transformations <- c(`Log` = "log",
                     `Sqrt` = "sqrt",
                     `Sq` = "square")

plot_lift_range <- function(dset, xvar, nbreaks = 20) {
  
  rangegroup <- cut(dset[,xvar], breaks = nbreaks)
  
  plotdata <- dset %>% 
    bind_cols(rangegroup = rangegroup) %>% 
    group_by(rangegroup) %>% 
    summarise(lift = mean(win) / (n() / nrow(dset)),
              n = n())
  
  ggplot(data = plotdata,
         aes(x = rangegroup, y = lift, size = n)) +
    geom_point() +
    labs(title = glue("Lift of {xvar} by range group"),
         subtitle = glue("{nbreaks} bins by range"))
}

plot_lift <- function(dset, xvar) {
  
  if(length(unique(dset[,xvar])) > 10) {
    stop("x must be categorical with under 10 unique values")
  }
  
  plotdata <- dset %>% 
    group_by_at(.vars = xvar) %>% 
    summarise(win_rate = mean(win),
              n = n()) %>% 
    mutate(lift = win_rate / (n / nrow(dset)))  
    # filter(lift == max(abs(lift)))
  
  ggplot(data = plotdata, aes_string(x = xvar, y = "lift")) + geom_bar(stat = "identity")
  
}
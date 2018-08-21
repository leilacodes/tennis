import_match_file <- function(filename) {
  rawdata <- fread(file.path(datafolder, filename),
                   fill = TRUE) %>% as.tibble()
  
  cleandata <- rawdata %>%  
    drop_na(score) %>% 
    filter(score != "",
           !str_detect(score, "[:alpha:]"), 
           str_detect(score, '[:digit:]-[:digit:]'),
           best_of != 5)  %>% 
    mutate(tourney_date = as.Date.character(as.character(tourney_date), "%Y%m%d"),
           match_id = paste(sep = "-", tourney_id, round, match_num))
  
  return(cleandata)
}

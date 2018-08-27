firstlook <- function(dset) {
  glimpse(dset)
  
  print(summary(dset))
  
  varinfo(dset)
}

freq <- function(vector) {
  counts <- as.data.frame(table(vector, useNA = "always"))
  names(counts) <- c("value", "count")
  pct <- counts$count / sum(counts$count)
  
  return(data.frame(counts, pct))
}

varinfo <- function(dset) {
  require(tibble)
  ndistinct <- lapply(dset, function(x) length(unique(x))) %>% 
    unlist()
  class <- lapply(dset, function(x) class(x)) %>% unlist()
  nnull <- lapply(dset, function(x) sum(is.na(x))) %>% unlist()
  nblank <- dset %>% summarise_if(.predicate = is.character,
                                  .funs = function(x) mean(trimws(x) == "") %>%
                                    round(digits = 3)) %>% 
    t() %>% as.data.frame() %>% rownames_to_column(var = "colname") %>% 
    rename(pctblank = V1)
  pct0 <- dset %>% summarise_if(.predicate = is.numeric,
                                .funs = function(x) mean(x == 0) %>% 
                                  round(digits = 3)) %>% 
    t() %>% as.data.frame() %>% rownames_to_column(var = "colname") %>% 
    rename(pct0 = V1)
  
  return(data.frame(class, ndistinct, nnull) %>% 
           rownames_to_column(var = "colname") %>% 
           left_join(nblank) %>% left_join(pct0))
}

# Freqs
graph_freqs <- function(dset, 
                        varlist,
                        cutoff = 10,
                        nrows = 3, 
                        ncols = 3) {
  require(glue)
  require(gridExtra)
  # Use all variables by default
  
  if (missing(varlist)) {
    
    varlist <- varinfo(dset) %>% 
      filter(ndistinct <= cutoff) %>% 
      pull(colname)
    
  } else {
    
    varlist <- dset %>% select_at(.vars = varlist) %>% 
      varinfo() %>% 
      filter(ndistinct <= cutoff) %>% 
      pull(colname)
    
  }
  
  message("Vars to graph:")
  print(varlist)
  
  # Create a graph object for each variable
  graphlist <- map(varlist, 
                   function(x) assign(x,
                                      ggplot(data = dset,
                                             aes_string(x = x)) + geom_bar(),
                                      envir = .GlobalEnv
                   ))
  
  vargroups <- split(varlist, ceiling(seq_along(varlist)/(nrows*ncols)))
  
  varstrings <- vargroups %>% 
    map(.f = function(x) paste0("grid.arrange(",
                                paste(x, collapse = ","),
                                glue(", ncol = {ncols})")))
  
  map(varstrings, .f = function(x) eval(parse(text = x)))
}

# Sort columns alphabetically
alphabetize_cols <- function(dset) {
  colnames <- sort(names(dset))
  
  dset %>% select_at(.vars = colnames)
}

# Lift for continuous vars
plot_ntile_lift <- function(dset, yvar, xvar) {
  plotdata <- dset %>% 
    mutate(ntile_var = ntile(!!xvar, n = 40)) %>% 
    group_by(ntile_var) %>% 
    summarise(reponse_pct = mean(!!yvar), 
              n = n())
  
  print(plotdata)
  
  ggplot(data = plotdata, 
         aes(x = ntile_var, y = reponse_pct, size = n)) +
    geom_point() + 
    labs(title = xvar,
         subtitle = "20 bins")
}

# Lift for continuous vars
plot_lift_ntile <- function(dset, yvar, xvar, nbreaks = 20) {
  plotdata <- dset %>% 
    mutate(ntile_var = ntile(!!xvar, n = nbreaks)) %>% 
    group_by(ntile_var) %>% 
    summarise(reponse_pct = mean(!!yvar), 
              n = n())
  
  ggplot(data = plotdata, 
         aes(x = ntile_var, y = reponse_pct, size = n)) +
    geom_point() + 
    labs(title = xvar,
         subtitle = glue("{nbreaks} bins by percentile"))

}


plot_lift_range <- function(dset, yvar, xvar, nbreaks) {
  plotdata <- dset %>% 
    mutate(cut_var = cut(!!xvar, breaks = nbreaks)) %>% 
  group_by(cut_var) %>% 
    summarise(response_pct = mean(!!yvar),
              n = n())
  
  ggplot(data = plotdata,
         aes(x = cut_var, y = response_pct, size = n)) +
    geom_point() +
    labs(title = xvar,
         subtitle = glue("{nbreaks} bins by range"))
}

plot_lift <- function(dset, yvar, xvar) {
  if(length(unique(dset %>% select(!!xvar))) > 2) stop("x must be binary")
  
  dset %>% 
    group_by(!!xvar) %>% 
    summarise(response_pct = mean(!!yvar),
              n = n()) %>% 
    mutate(lift = response_pct / (n / nrow(dset))) %>% 
    filter(lift == max(abs(lift)))
}

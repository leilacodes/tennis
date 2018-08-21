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
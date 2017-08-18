library(tidyverse)
library(stringr)

# User defined functions ------------------------------------------------------

# Read in 09/10 through 14/15 ILI from EPI website
read_past_ILI <- function(path) {

  season <- paste0(substr(path, 10, 13), "/", substr(path, 15, 18))
 
  these_data <- read_csv(path) %>%
    set_names(tolower(names(.))) %>%
    select(release_week, region, week, wili = perc_weighted_ili) %>%
    mutate(week = as.numeric(substr(week, 5, 6)),
           region = if_else(region == "National", "US National",
                            paste("HHS", region)),
           release_week = as.numeric(str_extract(release_week, "[0-9]{2}$")),
           final_week = ifelse(last(week) > 28, 28, last(week)),
           indicator = ifelse(release_week == final_week, "final", "initial")) %>%
    rowwise() %>%
    filter(release_week %in% c(week, final_week)) %>%
    select(-release_week, -final_week)
  
  past_ILI[[season]] <<- these_data

}

# Read in 15/16 and 16/17 ILI from weekly CSVs
read_recent_ILI <- function(directory, year) {
  
  if (!(year %in% c(2015, 2016))) stop("Year must be one of 2015 or 2016")
  
  # List to store output in
  temp_list <- list()
  
  files <- list.files(directory, pattern = "*.csv") 
  season <- paste0(year, "/", year + 1)
  
  
  for (i in seq_along(files)){
  
    these_data <- read_csv(paste0(directory, "/", files[i]))
    location <- str_extract(files[i], "_([^_]*)_")

    if (year == 2015) {
      these_data <- these_data %>%
        select(region = REGION, week = WEEK, 
               wili = `X..WEIGHTED.ILI`)
    } else {
      these_data <- these_data %>%
        select(region = REGION, week = WEEK,
               wili = `% WEIGHTED ILI`)
    }
    
    these_data <- these_data %>%
      mutate(release_week = as.numeric(substr(str_extract(files[i], "wk[0-9]{2}"), 3, 4)),
             season = season,
             region = if_else(region == "X", "US National", 
                              paste("HHS", region)))
    
    temp_list[[i]] <- these_data
  }
  
  season_data <- bind_rows(temp_list) %>%
    mutate(final_week = 28) %>%
    rowwise() %>%
    filter(release_week %in% c(week, final_week))
  
  past_ILI[[season]] <<- season_data
  
}

# Read in past data -----------------------------------------------------------

# 2009/2010 to 2013/2014 data
files <- list.files("Data", pattern = "*.csv")
path <- paste0("Data/", files[1])

past_ILI <- list()

for (i in seq_along(files)) {
  read_past_ILI(path = paste0("Data/", files[i]))
}

# 2015/2016 and 2016/2017
read_recent_ILI("../Challenge_2015_16/ILINet", 2015)
read_recent_ILI("../Challenge_2016_17/ILINet", 2016)




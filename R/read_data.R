library(tidyverse)
library(MMWRweek)

source("R/EpiDataAPI.R")

# Helper functions ----------------------------------------
# Function to deal with missing fields in Epidata objects
null_to_na <- function(x) ifelse(is.null(x), NA, x)

# Pull initial ILINet values between a range
initial_ili <- function(start, stop) {
  Epidata$fluview(regions = list("nat", "hhs1", "hhs2", "hhs3",
                                 "hhs4", "hhs5", "hhs6", "hhs7",
                                 "hhs8", "hhs9", "hhs10"),
                  epiweeks = Epidata$range(start, stop),
                  lag = 0)$epidata %>%
    modify_depth(2, null_to_na) %>%
    bind_rows() %>%
    mutate(season = paste0(substr(start, 1, 4), "-", 
                           substr(stop, 1, 4)))
}

# Pull in current ILINet values between a range
current_ili <- function(start, stop) {
  Epidata$fluview(regions = list("nat", "hhs1", "hhs2", "hhs3",
                                 "hhs4", "hhs5", "hhs6", "hhs7",
                                 "hhs8", "hhs9", "hhs10"),
                  epiweeks = Epidata$range(start, stop))$epidata %>%
    modify_depth(2, null_to_na) %>%
    bind_rows() %>%
    mutate(season = paste0(substr(start, 1, 4), "-", 
                           substr(stop, 1, 4)))
}

# Pull in initial ILINet values --------------------
initial_1011 <- initial_ili(201040, 201139)
initial_1112 <- initial_ili(201140, 201239)
initial_1213 <- initial_ili(201240, 201339) 
initial_1314 <- initial_ili(201340, 201439) 
initial_1415 <- initial_ili(201440, 201539) 
initial_1516 <- initial_ili(201540, 201639)
initial_1617 <- initial_ili(201640, 201739) 
initial_1718 <- initial_ili(201740, 201839) 

all_initial_ili <- bind_rows(initial_1011, initial_1112, initial_1213,
                             initial_1314, initial_1415, initial_1516,
                             initial_1617, initial_1718) %>%
  select(season, init_issue = issue, epiweek, region, initial = wili)

# Pull in initial ILINet values --------------------
current_1011 <- current_ili(201040, 201139)
current_1112 <- current_ili(201140, 201239)
current_1213 <- current_ili(201240, 201339) 
current_1314 <- current_ili(201340, 201439) 
current_1415 <- current_ili(201440, 201539) 
current_1516 <- current_ili(201540, 201639)
current_1617 <- current_ili(201640, 201739) 
current_1718 <- current_ili(201740, 201839) 

all_current_ili <- bind_rows(current_1011, current_1112, current_1213,
                             current_1314, current_1415, current_1516,
                             current_1617, current_1718) %>%
  select(season, curr_issue = issue, epiweek, region, current = wili)

ili_compare <- inner_join(all_initial_ili, all_current_ili,
                         by = c("season", "epiweek", "region")) %>%
  mutate(diff = current - initial)

save(all_initial_ili, all_current_ili, ili_compare,
     file = "Data/Backfill_files.Rdata")

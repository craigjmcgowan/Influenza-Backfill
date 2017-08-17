library(tidyverse)
library(stringr)

files <- list.files("Data", pattern = "*.csv")
path <- paste0("Data/", files[1])


read_past_ILI <- function(path) {

  season <- paste0(substr(path, 10, 13), "/", substr(path, 15, 18))
 
  these_data <- read_csv(path) %>%
    set_names(tolower(names(.))) %>%
    select(release_week, region, week, wili = perc_weighted_ili) %>%
    mutate(week = as.numeric(substr(week, 5, 6)),
           region = if_else(region == "National", "US National",
                            paste("HHS", region)),
           release_week = as.numeric(str_extract(release_week, "[0-9]{2}$")),
           season = season,
           final_week = ifelse(last(week) > 28, 28, last(week))) %>%
    rowwise() %>%
    filter(release_week %in% c(week, final_week))
  
  past_ILI[[season]] <<- these_data

}


for (i in seq_along(files)) {
  read_past_ILI(path = paste0("Data/", files[i]))
}

tst <- these_data %>%
  group_by(week, region, season) %>%
  summarize(diff = last(wili) - first(wili)) 

these_data %>%
  group_by(week, region, season) %>%
  filter(week == 1, region == "HHS Region 1") %>%
  summarize(diff = last(wili) - first(wili))
?`<<-`
?rev


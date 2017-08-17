library(tidyverse)

files <- list.files("Data", pattern = "*.csv")
path <- paste0("Data/", files[1])

past_ILI <- list()
read_past_ILI <- function(path, out_list) {

  these_data <- read_csv(path) %>%
    set_names(tolower(names(.))) %>%
    select(release_week, region, week, wili = perc_weighted_ili) %>%
    separate(week, c("year", "week"), 4, convert = TRUE) %>%
    mutate(region = if_else(region == "National", "US National",
                            paste("HHS", region)))
  
}

?`<<-`
?rev


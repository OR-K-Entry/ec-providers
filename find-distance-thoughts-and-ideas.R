library(tidyverse)

nces <- read_csv(here::here("data", "nces.csv"))

# nces %>% 
#   count(lea_name, sch_name) 

providers <- read_csv(here::here("data", "providers.csv"))

# nces$lat
# nces$lon
# 
# providers$lattitude
# providers$longitude

one_school <- nces[1, ]

find_distance_of_providers_for_one_school <- function(school_data, providers_data) {
  
  ### PSEUDOCODE
  school_sf <- create_sf_object(school_data)
  providers_sf <- create_sf_object(providers_data)
  
  distance_of_every_provider_to_school <- map(providers_sf) %>%
    st_distance(school_sf)
  
  # something else
}

find_distance_of_providers_for_one_school(one_school, providers)
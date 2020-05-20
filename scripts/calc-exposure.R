library(tidyverse)
library(here)

# Load data

distances_f <- here("data", "haversine-100mi.csv")
nces_f <- here("data", "nces.csv")

distances <- read_csv(f)
nces <- read_csv(nces_f)

# Prepare NCES data by selecting data from two years (just for initial analyses)

nces_1415 <- nces %>% 
  filter(school_year == "2014-2015") %>% 
  set_names(c("ncessch", str_c(names(.)[-1], "_1415")))

nces_1718 <- nces %>% 
  filter(school_year == "2017-2018") %>% 
  set_names(c("ncessch", str_c(names(.)[-1], "_1718")))

# Join prepepared NCES data

data_to_model <- distances %>% 
  left_join(nces_1415) %>% 
  left_join(nces_1718)

data_to_model

# Calculate schools exposure to different providers

exposure_of_schools <- data_to_model %>% 
  group_by(ncessch) %>% 
  summarize(number_of_providers = n(),
            mean_distance_to_providers = mean(miles))

exposure_of_schools %>% 
  gather(key, val, -ncessch) %>% 
  ggplot(aes(x = val)) +
  geom_histogram() +
  facet_wrap(~key)
  
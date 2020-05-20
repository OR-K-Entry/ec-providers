library(tidyverse)
library(here)

distances_f <- here("data", "haversine-100mi.csv")
nces_f <- here("data", "nces.csv")

distances <- read_csv(f)
nces <- read_csv(nces_f)

nces_1415 <- nces %>% 
  filter(school_year == "2014-2015") %>% 
  set_names(c("ncessch", str_c(names(.)[-1], "_1415")))

nces_1718 <- nces %>% 
  filter(school_year == "2017-2018") %>% 
  set_names(c("ncessch", str_c(names(.)[-1], "_1718")))

data_to_model <- distances %>% 
  left_join(nces_1415) %>% 
  left_join(nces_1718)

data_to_model

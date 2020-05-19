library(tidyverse)
library(geosphere)
theme_set(theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

nces <- read_csv(here::here("data", "nces.csv"))
providers <- read_csv(here::here("data", "providers.csv")) %>% 
  rownames_to_column("internal_provider_id") %>% 
  select(internal_provider_id, 
         provider_lat = lattitude, 
         provider_lon = longitude) %>% 
  distinct()

elsi <- read_csv(fs::dir_ls(here::here("data"), regexp = "ELSI"),
                 skip = 5) 
names(elsi) <- c("school_name", "state", "ncessch", 
                 "sch_level_17", "sch_level_16", "sch_level_15")

elsi <- select(elsi, -state)

nces <- left_join(nces, elsi) %>% 
  filter(sch_level_17 == "1-Primary",
         school_year == "2016-2017") %>% 
  select(ncessch, 
         nces_lat = lat, nces_lon = lon) %>% 
  distinct()

full <- expand.grid(ncessch = nces$ncessch, 
                    internal_provider_id = providers$internal_provider_id,
                    stringsAsFactors = FALSE) %>% 
  as_tibble()

full <- left_join(full, nces) %>% 
  left_join(providers)

# output is in meters
# with(full,
#      distm(c(nces_lon[1], nces_lat[1]),
#            c(provider_lon[1], provider_lat[1]),
#            fun = distHaversine)
# )
# in miles
# 441025.3/1609.344 

# Distance function
compute_distance <- function(lon1, lat1, lon2, lat2) {
  l <- list(lon1, lat1, lon2, lat2)
  
  pmap_dbl(l, ~as.numeric(
    distm(c(..1, ..2), c(..3, ..4), 
          fun = distHaversine))
  ) / 1609.344
}

full <- full %>% 
  mutate(miles = compute_distance(nces_lon, nces_lat,
                                  provider_lon, provider_lat))

write_csv(full, here::here("data", "all-haversine-distances.csv"))

full %>% 
  filter(miles < 100) %>% 
  write_csv(here::here("data", "haversine-100mi.csv"))


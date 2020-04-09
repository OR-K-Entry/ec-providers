library(tidyverse)
theme_set(theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  legend.position = "bottom"))

nces <- read_csv(here::here("data", "nces.csv"))
providers <- read_csv(here::here("data", "providers.csv"))

districts <- tigris::school_districts("OR") %>% 
  sf::st_as_sf()

ggplot(districts) +
  geom_sf(fill = "#EEEDDB",
          color = "#e2e0c1") +
  geom_point(aes(lon, lat, color = "Schools"), nces,
             alpha = 0.7) +
  geom_point(aes(longitude, lattitude, color = "Pre-K Providers"), providers,
             size = 0.3,
             alpha = 0.7) +
  scale_color_manual(name = "", values = c("#77caa1", "#7a77ca"))

#dir.create(here::here("plots"))
ggsave(here::here("plots", "prelim-map.png"),
       dpi = 600,
       width = 8.5,
       height = 6)

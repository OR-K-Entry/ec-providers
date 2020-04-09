# html files downloaded from https://orm.naccrraware.net/orm/ormSearch
library(rvest)
library(tidyverse)

############## list all html files
files <- fs::dir_ls(here::here("data","providers"))

############## Functions to pull the data
extract_table <- function(filepath) {
  html_file <- read_html(filepath)
  html_file %>% 
    html_node(xpath = '//*[@id="ormDoAdvSearchPdr"]/table') %>% 
    html_table(fill = TRUE) %>% 
    select(-1, -10) 
}

format_table <- function(tbl) {
  tbl <- setNames(tbl[-1, ], tbl[1, ])
  map_df(tbl, ~ifelse(.x == "", NA_character_, .x)) %>% 
    janitor::clean_names()
}

############## pull data
ec_providers <- map_df(files, ~
                         extract_table(.x) %>% 
                         format_table(),
                       .id = "county") %>% 
  mutate(county = gsub(".+providers/(.+)\\.html", "\\1", county),
         county = gsub("\\d", "", county)) %>% 
  select(-first_name) %>% 
  drop_na(business_name) %>% 
  distinct()


############## Function to extract location data
pull_loc_data <- function(html) {
  scrpt <- html %>% 
    html_nodes(xpath = "//*/script") %>% 
    pluck(19) %>% 
    html_text()
  
  scrpt <- stringr::str_extract(scrpt, 'markers = \\[\\{(.+)\\}\\];')
  strsplit(scrpt, ",")[[1]]
}

extract_name <- function(splt_loc_data) {
  pulled <- splt_loc_data[grepl("name", splt_loc_data)]
  gsub("name:\"(.+)\"", "\\1", pulled)
}

extract_phone <- function(splt_loc_data) {
  pulled <- splt_loc_data[grepl("Phone:", splt_loc_data)]
  gsub("Phone:\"(.+)\"", "\\1", pulled)
}

extract_city <- function(splt_loc_data) {
  pulled <- splt_loc_data[grepl("address:", splt_loc_data)]
  gsub("^address:\"(.+)$", "\\1", pulled)
}

extract_lat <- function(splt_loc_data) {
  pulled <- splt_loc_data[grepl("lat:", splt_loc_data)]
  pulled %>% 
    parse_number()
}

extract_lon <- function(splt_loc_data) {
  pulled <- splt_loc_data[grepl("lng:", splt_loc_data)]
  pulled %>% 
    parse_number()
}

# combine above functions to a single function
get_lat_lon <- function(html) {
  d <- pull_loc_data(html)
  tibble(business_name = extract_name(d),
         lattitude = extract_lat(d),
         longitude = extract_lon(d),
         area_code = gsub("\\((.+)\\).+", "\\1", extract_phone(d)),
         phone = gsub(".+\\)(.+)", "\\1", extract_phone(d)),
         city = extract_city(d)
  )
}

############## Extract location data
ec_locs <- map_df(files, ~
                    read_html(.x) %>% 
                      get_lat_lon(), 
                  .id = "county") %>% 
  mutate(county = gsub(".+providers/(.+)\\.html", "\\1", county),
         county = gsub("\\d", "", county)) %>% 
  distinct() %>% 
  mutate(business_name = gsub("name:\"", "", business_name)) %>% 
  filter(business_name != "(ECC)")

############## Join files and finalize data
ec_locs %>% 
  count(county, business_name, area_code, phone, city) %>% 
  filter(n > 1) %>% 
  semi_join(ec_locs, .)

# One provider has two locations (as seen above). I did some research and went 
# with the one that appears closest to the physical address they have listed
# on their website

ec_locs <- ec_locs %>% 
  filter(round(lattitude, 5) != 45.55036 & 
         round(longitude, 5) != -122.9064)

ec_providers %>% 
  count(county, business_name, area_code, phone, city) %>% 
  filter(n > 1) %>% 
  semi_join(ec_providers, .) 
# The above is still somewhat problematic & I'm not sure what to do it about it 
# at the moment. Just moving on for now but will probs need to come back to it

left_join(ec_providers, ec_locs) %>% 
  write_csv(here::here("data", "providers.csv"))

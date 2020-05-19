library(tidyverse)
# line 84 has mac-specific code ... figure out PC version

geo <- c(
  geo_15 = "https://nces.ed.gov/ccd/Data/zip/EDGE_GEOIDS_201415_PUBLIC_SCHOOL_csv.zip",
  geo_16 = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_1516.zip",
  geo_17 = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_1617.zip",
  geo_18 = "https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_1718.zip"
)

ccd_dir <- c(
  dir_15 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1415_w_0216601a_txt.zip",
  dir_16 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1516_w_2a_011717_csv.zip",
  dir_17 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1617_w_1a_11212017_csv.zip",
  dir_18 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1718_w_1a_083118.zip",
  dir_19 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_029_1819_w_0a_04082019_csv.zip"
)

ccd_mem <- c(
  mem_15 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_052_1415_w_0216161a_txt.zip",
  mem_16 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_052_1516_w_2a_011717_csv.zip",
  mem_17 = "https://nces.ed.gov/ccd/Data/zip/ccd_SCH_052_1617_l_2a_11212017_csv.zip",
  mem_18 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_052_1718_l_1a_083118.zip"
)

ccd_staff <- c(
  staff_15 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_059_1415_w_0216161a_txt.zip",
  staff_16 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_059_1516_w_2a_011717_csv.zip",
  staff_17 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_059_1617_l_2a_11212017_csv.zip",
  staff_18 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_059_1718_l_1a_083118.zip"
)

# Check for title status in other files. If it's not in there, it will be in 
# the school characteristics file, which has not been loaded here.

ccd_lunch <- c(
  lunch_15 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_033_1415_w_0216161a_txt.zip",
  lunch_16 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_033_1516_w_2a_011717_csv.zip",
  lunch_17 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_033_1617_l_2a_11212017_csv.zip",
  lunch_18 = "https://nces.ed.gov/ccd/Data/zip/ccd_sch_033_1718_l_1a_083118.zip"
)

nces_files <- list(geo = geo, 
	                 ccd_dir = ccd_dir, 
	                 ccd_mem = ccd_mem, 
	                 ccd_staff = ccd_staff, 
	                 ccd_lunch = ccd_lunch)

tmp <- tempdir()

dl_files <- map(nces_files, ~paste0(names(.x), ".zip")) 

map2(nces_files, dl_files, ~
	map2(.x, .y, ~
		download.file(.x, file.path(tmp, .y))
		)
	)

files <- map(dl_files, ~map(.x, ~
	unzip(file.path(tmp, .x), list = TRUE)) %>%
		map(~.x$Name[grepl("*.xlsx|*.csv|*.txt|CSV\\.zip", .x$Name)]) %>%
		map_chr(~ifelse(length(.x) > 0, .x, NA)) %>%
		setNames(names(.x))
)

dir.create(here::here("data", "nces"))
dir.create(here::here("data", "nces", "working"))

map2(dl_files, files, ~
	map2(.x, .y, ~{
			unzip(file.path(tmp, .x), 
			      files = .y, 
			      exdir = here::here("data", "nces", "working"))
		}
	)
)

zip_files <- grep(
	"\\.zip", 
	list.files(here::here("data", "nces", "working"), full.names = TRUE),
	value = TRUE
)

system(paste0("unzip -o '", zip_files,"'", " -d ", here::here("data", "nces", "working")))

files <- map(files, ~gsub(" CSV.zip", "\\.csv", .x))

read_files <- function(file) {
  if(grepl("\\.xlsx", file)) {
    d <- readxl::read_xlsx(file)
  } else if(grepl("\\.csv", file)) {
    d <- readr::read_csv(file, col_types = cols(.default = "c"))
  } else if(grepl("\\.txt", file)) {
    d <- readr::read_delim(file, 
                           delim = "\t", 
                           col_types = cols(.default = "c"))
  } else {
  	stop("File type not found for: ", file)
  }
  d
}

l <- map(files, ~map(.x, ~read_files(here::here("data", "nces", "working", .x))))

################################ Geo ###########################################
l$geo <- l$geo %>%
	map2(names(geo), ~
    select(.x, 
    	     NCESSCH, 
           contains("STATE"), 
           matches("^LAT|^LON|^CBSA"), 
           contains("CNTY")) %>%
      mutate(school_year = paste0("20", parse_number(.y) - 1, "-",
      	                          "20", parse_number(.y)))
    )

names(l$geo[[1]]) <- c("NCESSCH", "STATE", "LAT", "LON", "CBSA", "school_year")
names(l$geo[[2]]) <- c("NCESSCH", "STATE", "LAT", "LON", "CBSA", 
                       "CBSATYPE", "CNTY", "NMCNTY", "school_year")
l$geo <- l$geo %>%
	map(~mutate_at(.x, vars(LAT, LON), as.double)) %>%
	reduce(bind_rows) %>%
	filter(STATE == "OR") %>%
	janitor::clean_names() %>%
  group_by(ncessch) %>% 
  fill(lat, lon, cbsa, cbsatype, cnty, nmcnty, .direction = "updown")

dir.create(here::here("data", "nces", "intermediary"))
write_csv(l$geo, here::here("data", "nces", "intermediary", "schools_geo.csv"))

################################ Directory #####################################
l$ccd_dir[1:2] <- l$ccd_dir[1:2] %>%
	map(select, 
    SCHOOL_YEAR = SURVYEAR, FIPST, STATENAME, SCH_NAME, LEA_NAME, ST_LEAID, 
    LEAID, ST_SCHID, NCESSCH, SY_STATUS_TEXT, SCH_TYPE_TEXT, RECON_STATUS, 
    CHARTER_TEXT, G_PK_OFFERED = PKOFFERED
)

l$ccd_dir[3:5] <- l$ccd_dir[3:5] %>%
	map(select,
    SCHOOL_YEAR, FIPST, STATENAME, SCH_NAME, LEA_NAME, ST_LEAID, LEAID, ST_SCHID, 
    NCESSCH, SY_STATUS_TEXT, SCH_TYPE_TEXT, RECON_STATUS, CHARTER_TEXT,
    G_PK_OFFERED
)

l$ccd_dir <- reduce(l$ccd_dir, bind_rows) %>% 
  janitor::clean_names() %>% 
  filter(statename == "OREGON")

write_csv(l$ccd_dir, here::here("data", "nces", "intermediary", "schools_dir.csv"))

################################ Membership ####################################
l$ccd_mem[1:2] <- l$ccd_mem[1:2] %>% 
	map(~
		select(.x, SCHOOL_YEAR = SURVYEAR, FIPST, STATENAME, SCH_NAME, 
					 ST_LEAID, LEAID, ST_SCHID, NCESSCH, TOTAL, AMPKM:TRAEF) %>%
		gather(var, n, AMPKM:TRAEF) %>%
		mutate(race_eth = substr(var, 1, 2),
			     grade = substr(var, 3, 4),
			     gender = substr(var, 5, 5),
			     TOTAL = parse_number(TOTAL)) %>%
		select(-var) %>%
		filter(as.numeric(n) > 0 & STATENAME == "OREGON")
	)

#l$ccd_mem[3:4] <- 
  l$ccd_mem[3:4] %>%
	map(~
		select(.x, 
			SCHOOL_YEAR, FIPST, STATENAME, SCH_NAME, 
			ST_LEAID, LEAID, ST_SCHID, NCESSCH,
			TOTAL = TOTAL_INDICATOR, race_eth = RACE_ETHNICITY, 
			grade = GRADE, gender = SEX) %>%
		drop_na(TOTAL) %>%
		filter(STATENAME == "OREGON") %>%
		group_by(NCESSCH) %>%
		mutate(TOTAL = sum(parse_number(TOTAL)))
	)

l$ccd_mem <- reduce(l$ccd_mem, bind_rows) %>%
	janitor::clean_names() 

l$ccd_mem <- l$ccd_mem %>%
	mutate(race_eth = case_when(
		race_eth == "AM" ~ "American Indian or Alaska Native",
		race_eth == "AS" ~ "Asian",
		race_eth == "BL" ~ "Black or African American",
		race_eth == "HI" ~ "Hispanic/Latino",
		race_eth == "HP" ~ "Native Hawaiian or Other Pacific Islander",
		race_eth == "TR" ~ "Two or more races",
		race_eth == "WH" ~ "White",
		TRUE ~ race_eth
		)
	) 

l$ccd_mem <- l$ccd_mem %>%
	mutate(grade = case_when(grepl("^K", grade) ~ "Kinder",
		                        grepl("^P", grade) ~ "Pre-Kinder",
		                        grepl("^N", grade) ~ "Not specified",
		                        TRUE ~ as.character(parse_number(grade))))

l$ccd_mem <- l$ccd_mem %>%
	mutate(gender = case_when(grepl("^F", gender) ~ "Female",
		                    grepl("^M", gender) ~ "Male",
		                    TRUE ~ "Not specified"
		)
	)

write_csv(l$ccd_mem, here::here("data", "nces", "intermediary", "schools_mem.csv"))

################################## Staff #######################################
l$ccd_staff[1:2] <- l$ccd_staff[1:2] %>%
	map(~
		select(.x, SCHOOL_YEAR = SURVYEAR, FIPST, STATENAME, SCH_NAME, 
					 ST_LEAID, LEAID, ST_SCHID, NCESSCH, TEACHERS = FTE)
	)
l$ccd_staff[3:4] <- l$ccd_staff[3:4] %>%
	map(~
		select(.x, SCHOOL_YEAR, FIPST, STATENAME, SCH_NAME, 
					 ST_LEAID, LEAID, ST_SCHID, NCESSCH, TEACHERS)
	)

l$ccd_staff <- reduce(l$ccd_staff, bind_rows) %>%
	filter(STATENAME == "OREGON" & as.numeric(TEACHERS) > 0) %>%
	janitor::clean_names()

write_csv(l$ccd_staff, here::here("data", "nces", "intermediary", "schools_staff.csv"))

################################## Lunch #######################################
l$ccd_lunch[1:2] <- l$ccd_lunch[1:2] %>%
	map(~
		select(.x, SCHOOL_YEAR = SURVYEAR, FIPST, STATENAME, SCH_NAME, 
					 ST_LEAID, LEAID, ST_SCHID, NCESSCH, frl = TOTFRL) %>%
		mutate(frl = as.numeric(frl))
	)

l$ccd_lunch[3:4] <- l$ccd_lunch[3:4] %>%
	map(~
		select(.x, SCHOOL_YEAR, FIPST, STATENAME, SCH_NAME, 
					 ST_LEAID, LEAID, ST_SCHID, NCESSCH, LUNCH_PROGRAM, 
					 STUDENT_COUNT) %>%
		filter(LUNCH_PROGRAM == "Free lunch qualified" |
			     LUNCH_PROGRAM == "Reduced-price lunch qualified") %>%
		group_by(SCHOOL_YEAR, FIPST, STATENAME, SCH_NAME, 
					   ST_LEAID, LEAID, ST_SCHID, NCESSCH) %>%
		summarize(frl = sum(as.numeric(STUDENT_COUNT))) 
	)

l$ccd_lunch <- reduce(l$ccd_lunch, bind_rows) %>%
	filter(STATENAME == "OREGON") %>%
	janitor::clean_names()

write_csv(l$ccd_lunch, here::here("data", "nces", "intermediary", "schools_lunch.csv"))


################################ Final NCES ####################################
nces <- reduce(l, left_join) %>%
  group_by(ncessch, state, lat, lon, cbsa, school_year, cbsatype, cnty, nmcnty,
           fipst, statename, sch_name, lea_name, st_leaid, leaid, st_schid,
           sy_status_text, sch_type_text, recon_status, charter_text,
           g_pk_offered, total, race_eth, gender, teachers, frl) %>%
  summarize(n = sum(as.numeric(n))) %>%
  ungroup()

new_labs <- nces %>%
  count(race_eth) %>%
  mutate(new_col_names = c("re_amind_sch", "re_asian_sch", "re_black_sch", "re_hisp_sch", 
                           "re_nathi_sch", "sch_re_missing_sch", "re_missing_sch", "re_multi_sch", 
                           "re_white_sch", "re_missing_sch")) %>%
  select(-n)

race_eth <- nces %>%
  left_join(new_labs) %>%
  select(ncessch, school_year, new_col_names, gender, n, total) %>%
  group_by(ncessch, school_year, new_col_names, total) %>%
  summarize(tot_race_eth = sum(n)) %>%
  mutate(prop = tot_race_eth / total) %>%
  select(-tot_race_eth) %>%
  spread(new_col_names, prop, fill = 0) %>%
  rename(tot_enrollment_sch = total)

gender <- nces %>%
  select(ncessch, school_year, race_eth, gender, n, total) %>%
  group_by(ncessch, school_year, gender, total) %>%
  summarize(tot_gen = sum(n)) %>%
  ungroup() %>%
  mutate(prop = tot_gen / total, 
         gender = ifelse(gender == "Not specified" | 
                           is.na(gender), 
                         "prop_gen_miss_sch",
                         gender)) %>%
  select(-tot_gen) %>%
  spread(gender, prop) %>%
  select(ncessch:Female, prop_gen_miss_sch) %>%
  rename(prop_gen_female_sch = Female,
         tot_enrollment_sch = total)

nces <- nces %>%
  distinct(ncessch, state, lat, lon, cbsa, school_year, cbsatype, cnty, nmcnty,
           fipst, statename, sch_name, lea_name, st_leaid, leaid, st_schid,
           sy_status_text, sch_type_text, recon_status, charter_text,
           g_pk_offered, tot_enrollment_sch = total, frl, 
           teacher_fte = teachers) %>%
  mutate(prop_frl_sch = frl / tot_enrollment_sch) %>%
  select(-frl) %>%
  drop_na(st_schid)

nces <- nces %>%
  left_join(race_eth) %>%
  left_join(gender)

write_csv(nces, here::here("data", "nces", "nces.csv"))


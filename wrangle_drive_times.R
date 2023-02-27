library(tidyverse)

### Create df_acute with acute travel times
replacement_name_for_brisbane <- "Brisbane (PAH/RBWH)"

df_acute_addons <- data.frame(
  # add brisbane as 8 minutes for acute care
  # add rbwh as 0 minutes
  town_name = c("Brisbane", "RBWH"),
  acute_time = c(8, 0),
  acute_care_transit_location=rep("Brisbane (PAH/RBWH)", 2),
  acute_care_centre = c(replacement_name_for_brisbane, replacement_name_for_brisbane)
)

clean_acute_centre <- function(x) {
  x <- tolower(x)
  brisbane_centres <- c(
    "pah", 
    "princess alexandra hospital",
    "rbwh",
    "royal brisbane and women's hospital"
  )
  case_when(
    x %in% brisbane_centres ~ replacement_name_for_brisbane,
    x %in% c("gcuh", "gold coast university hospital") ~ "Gold Coast University Hospital",
    x == "townsville hospital" ~ "Townsville University Hospital",
    x == "rockhampton" ~ "Rockhampton Hospital",
    x == "camooweal primary health clinic" ~ "Camooweal Primary Health Centre",
    x == "taroom" ~ "Taroom Hospital",
    x == "mareeba" ~ "Mareeba Hospital",
    x == "weipa" ~ "Weipa Hospital",
    x == "bamaga" ~ "Bamaga Hospital",
    x == "thurday island" ~ "Thursday Island Hospital",
    TRUE ~ str_replace(str_to_title(x), "Phc", "Primary Health Centre")
  )
}


df_acute <- readxl::read_excel("input/drive_times/Qld_towns_RSQ pathways V3.xlsx", skip=2) %>%
  select(town_name=TOWN_NAME, acute_time=Total_transport_time_min, 
         acute_care_transit_location=Destination1, acute_care_centre=Destination2) %>%
  filter(!is.na(acute_care_centre)) %>%
  mutate(acute_care_centre = clean_acute_centre(acute_care_centre),
         acute_care_transit_location = clean_acute_centre(acute_care_transit_location)) %>%
  rbind(., df_acute_addons) %>%
  distinct() # removes the duplicated Killarney

write.csv(df_acute, "input/df_acute.csv", row.names=FALSE)


############# CHECKS
drive_times_dir <- "input/drive_times/NewRehabTimes"


# df <- read.csv(file.path(drive_times_dir, "LongreachRDT.csv"))
# 
# df_ids <- read.csv(file.path(drive_times_dir, "QLDLocations3422.csv"))
# 
# (inner_join(df, df_ids, by=c("From_ID"="ID")) %>% nrow()) == nrow(df)
# 
# df_rsq <- read("input/drive_times/Qld_towns_RSQ pathways V2.xlsx")
# 
# df_rsq <- readxl::read_excel("input/drive_times/Qld_towns_RSQ pathways V2.xlsx", skip=2) %>%
#   select(town_name=TOWN_NAME, acute_time=Total_transport_time_min, acute_care_centre=Destination2) %>%
#   filter(!is.na(acute_care_centre)) %>%
#   mutate(acute_care_centre=ifelse(acute_care_centre=="Townsville Hospital","Townsville University Hospital","Brain Injury Rehabilitation Unit"))
# 
# inner_join(df_rsq, df_ids, by=c("town_name"="Location"))
# 
# 
# # check whether there is a drive time and acute time for every location
# left_join(df_rsq, select(df, From_Location, drive_time=Total_Minutes), by=c("town_name"="From_Location")) %>%
#   (function(x) nrow(x) == nrow(na.omit(x))) 

# add the additional ferry time for island locations
df_ids <- read.csv(file.path(drive_times_dir, "QLDLocations3422.csv"))
df_tsv <- read.csv(file.path(drive_times_dir, "TownsvilleRDT.csv"))

df_ids %>% filter(!Location %in% df_tsv$From_Location)


# torres straight islands have boat speed of 30km/hr (1 hour and 10 mins for ~35km journey from seisia to thursday island)
# https://peddellsferry.com.au/ferry-timetable/


island_list <- list(
  list(
    island_location = "Boigu Island",
    closest_town = "Seisia",
    travel_time = (200/30)*60 # 200kms @ 30km/hr
  ),
  list(
    island_location = "Saibai Island",
    closest_town = "Seisia",
    travel_time = (165/30)*60
  ),
  list(
    island_location = "Erub (Darnley) Island",
    closest_town = "Seisia",
    travel_time = (210/30)*60
  ),
  list(
    island_location = "Yorke Island",
    closest_town = "Seisia",
    travel_time = (170/30)*60
  ),
  list(
    island_location = "Iama (Yam) Island",
    closest_town = "Seisia",
    travel_time = (115/30)*60
  ),
  list(
    island_location = "Mer (Murray) Island",
    closest_town = "Seisia",
    travel_time = (215/30)*60
  ),
  list(
    island_location = "Mabuiag Island",
    closest_town = "Seisia",
    travel_time = (105/30)*60
  ),
  list(
    island_location = "Badu Island",
    closest_town = "Seisia",
    travel_time = (80/30)*60
  ),
  list(
    island_location = "St Pauls", # called moa island on google maps
    closest_town = "Seisia",
    travel_time = (75/30)*60
  ),
  list(
    island_location = "Warraber Island",
    closest_town = "Seisia",
    travel_time = (85/30)*60
  ),
  list(
    island_location = "Hammond Island",
    closest_town = "Seisia",
    travel_time = (35/30)*60
  ),
  list(
    island_location = "Gununa",
    closest_town = "Doomadgee",
    travel_time = (35/30)*60 + 94 # 35min boat (assume same speed as torres strait) to Gangalidda, then 94 min drive to Doomadgee
  ),
  list(
    island_location = "Hamilton Island",
    closest_town = "Airlie Beach",
    travel_time = 35 + 15 # 35 min ferry + 15 min drive (https://www.directferries.com.au/shute_harbour_hamilton_island_marina_ferry.htm)
  )
)


df_islands <- 
  do.call(rbind, island_list) %>% 
  as.data.frame() %>%
  mutate(across(everything(), unlist)) %>%
  left_join(., rename(df_ids, id=ID), by=c("island_location" = "Location"))


############# wrangle times

silver_locs <- c(
  "Cairns Hospital",
  "Townsville University Hospital",
  "Central West Sub-Acute Service",
  "Rockhampton Hospital",
  "Roma Hospital",
  "Gympie Hospital",
  "Sunshine Coast University Hospital",
  "Brain Injury Rehabilitation Unit",
  "Gold Coast University Hospital",
  "Sarina Hospital",
  "RBWH",
  "Logan Hospital",
  "Redcliffe Hospital",
  "Maleny Hospital",
  "Prince Charles Hospital",
  "Geriatric Assessment Rehabilitation Unit",
  "Brighton Bain Injury Service"
)

future_gold_locs <- c(
  "Townsville University Hospital",
  "Sunshine Coast University Hospital",
  "Brain Injury Rehabilitation Unit",
  "RBWH",
  "Gold Coast University Hospital"
)

future_gold_and_cairns_locs <- c(
  future_gold_locs,
  "Cairns Hospital"
)

gold_locs <- c(
  "Townsville University Hospital",
  "Brain Injury Rehabilitation Unit"
)

platinum_locs <- c(
  "Brain Injury Rehabilitation Unit"
)

files <- c(
  "CairnsRDT",
  "TownsvilleRDT",
  "LongreachRDT",
  "RockhamptonRDT",
  "RomaRDT",
  "GympieRDT",
  "SCRDT",
  "PARDT",
  "GCRDT",
  "RHDTtoSarina",
  "RHDTtoRBWHSilver",
  "RHDTtoLogan",
  "RHDTtoRedcliffe",
  "RHDTtoMaleny",
  "DTtoPCHospital",
  "DTtoGARU",
  "DTtoBBIS"
)


centre_renaming <- function(x) {
  case_when(
    x == "Brain Injury Rehabilitation Unit" ~ "Princess Alexandra Hospital (PAH)",
    x == "RBWH" ~ "Royal Brisbane and Women's Hospital (RBWH)",
    x == "Maleny Hospital" ~ "Maleny Soldiers Memorial Hospital",
    x == "Brighton Bain Injury Service" ~ "Brighton Brain Injury Service",
    TRUE ~ x
  )
}


df_combined <- 
  plyr::ldply(file.path(drive_times_dir, paste0(files, ".csv")), read.csv) %>%
  select(id=From_ID, town_name=From_Location, x=From_x, y=From_y, centre=To_Title, minutes=Total_Minutes) %>%
  mutate(centre=str_trim(centre))


get_df_times <- function(data, centres, islands_data=df_islands, save_file=NULL){
  df_times <- 
    data %>%
    filter(centre %in% centres) %>%
    group_by(id) %>%
    arrange(minutes) %>%
    slice(1) %>%
    ungroup()
  
  islands_times <-
    left_join(
      islands_data, 
      select(df_times, -x, -y, -id), 
      by=c("closest_town"="town_name")
    ) %>%
    mutate(minutes = minutes + travel_time) %>%
    rename(town_name = island_location) %>%
    select(names(df_times))
  
  df_times <- rbind(df_times, islands_times)
  df_times$centre = centre_renaming(df_times$centre)
  
  if(!is.null(save_file)){
    write.csv(df_times, file=save_file, row.names=FALSE)
  }
  df_times
}

rehab_times_dir <- "input/rehab_times"

get_df_times(data=df_combined, centres=silver_locs, save_file=file.path(rehab_times_dir, "silver_rehab.csv"))
get_df_times(data=df_combined, centres=future_gold_locs, save_file=file.path(rehab_times_dir, "future_gold_rehab.csv"))
get_df_times(data=df_combined, centres=gold_locs, save_file=file.path(rehab_times_dir, "gold_rehab.csv"))
get_df_times(data=df_combined, centres=platinum_locs, save_file=file.path(rehab_times_dir, "platinum_rehab.csv"))
get_df_times(data=df_combined, centres=future_gold_and_cairns_locs, save_file=file.path(rehab_times_dir, "future_gold_and_cairns_rehab.csv"))

silver_times <- get_df_times(data=df_combined, centres=silver_locs)
gold_times <- get_df_times(data=df_combined, centres=gold_locs)
future_gold_times <- get_df_times(data=df_combined, centres=future_gold_locs)
platinum_times <- get_df_times(data=df_combined, centres=platinum_locs)
future_gold_and_cairns_times <- get_df_times(data=df_combined, centres=future_gold_and_cairns_locs)

acute_drive_times <- 
  df_acute %>%
  mutate(acute_care_centre = ifelse(acute_care_centre == "Brisbane (PAH/RBWH)", "Brain Injury Rehabilitation Unit", acute_care_centre)) %>%
  select(-acute_time, -acute_care_transit_location) %>%
  inner_join(., df_combined, by = c("town_name", "acute_care_centre"="centre"))


df_island_acute_drive_times <-
  acute_drive_times %>%
  filter(town_name %in% df_islands$closest_town) %>%
  rename(mainland_to_acute_centre = minutes) %>%
  select(-id, -x, -y) %>%
  left_join(df_islands, ., by=c("closest_town"="town_name")) %>%
  mutate(minutes = mainland_to_acute_centre + travel_time) %>%
  rename(town_name = island_location) %>%
  select(names(acute_drive_times))

acute_drive_times_all <- rbind(acute_drive_times, df_island_acute_drive_times)

weighted_rehab_times <-
  inner_join(
    rename(future_gold_and_cairns_times, silver_rehab_centre=centre, silver_time=minutes),
    select(acute_drive_times_all, id, gold_rehab_centre=acute_care_centre, gold_time=minutes),
    by="id"
  ) %>%
  mutate(minutes=(silver_time + gold_time)/2) %>%
  select(-silver_time, -gold_time)

all_rehab_times <- 
  rename(silver_times, silver_rehab_centre=centre, silver_time=minutes) %>%
  inner_join(
    .,
    select(gold_times, id, gold_rehab_centre=centre, gold_time=minutes),
    by="id"
  ) %>%
  inner_join(
    .,
    select(future_gold_times, id, future_gold_rehab_centre=centre, future_gold_time=minutes),
    by="id"
  ) %>%
  inner_join(
    .,
    select(platinum_times, id, platinum_rehab_centre=centre, platinum_time=minutes),
    by="id"
  ) %>%
  inner_join(
    .,
    select(platinum_times, id, future_gold_and_cairns_rehab_centre=centre, future_gold_and_cairns_time=minutes),
    by="id"
  ) 

write.csv(all_rehab_times, file=file.path(rehab_times_dir, "all_rehab_time.csv"), row.names=FALSE)
write.csv(weighted_rehab_times, file=file.path(rehab_times_dir, "weighted_rehab_time.csv"), row.names=FALSE)

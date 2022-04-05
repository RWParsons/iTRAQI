library(tidyverse)



############# CHECKS
drive_times_dir <- "input/drive_times/NewRehabTimes"


df <- read.csv(file.path(drive_times_dir, "LongreachRDT.csv"))

df_ids <- read.csv(file.path(drive_times_dir, "QLDLocations3422.csv"))

(inner_join(df, df_ids, by=c("From_ID"="ID")) %>% nrow()) == nrow(df)

df_rsq <- read("input/drive_times/Qld_towns_RSQ pathways V2.xlsx")

df_rsq <- readxl::read_excel("input/drive_times/Qld_towns_RSQ pathways V2.xlsx", skip=2) %>%
  select(town_name=TOWN_NAME, acute_time=Total_transport_time_min, acute_care_centre=Destination2) %>%
  filter(!is.na(acute_care_centre)) %>%
  mutate(acute_care_centre=ifelse(acute_care_centre=="Townsville Hospital","Townsville University Hospital","Brain Injury Rehabilitation Unit"))

inner_join(df_rsq, df_ids, by=c("town_name"="Location"))


# check whether there is a drive time and acute time for every location
left_join(df_rsq, select(df, From_Location, drive_time=Total_Minutes), by=c("town_name"="From_Location")) %>%
  (function(x) nrow(x) == nrow(na.omit(x))) 


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
  "Gold Coast University Hospital"
)

future_gold_locs <- c(
  "Townsville University Hospital",
  "Sunshine Coast University Hospital",
  "Brain Injury Rehabilitation Unit",
  "Gold Coast University Hospital"
)

gold_locs <- c(
  "Townsville University Hospital",
  "Brain Injury Rehabilitation Unit"
)

platinum_locs <- c(
  "Brain Injury Rehabilitation Unit"
)

df_combined <- 
  plyr::ldply(file.path(drive_times_dir, paste0(silver_locs, ".csv")), read.csv) %>%
  select(id=From_ID, town_name=From_Location, x=From_x, y=From_y, centre=To_Title, minutes=Total_Minutes) %>%
  mutate(centre=str_trim(centre))


get_df_times <- function(data, centres, save_file=NULL){
  df_times <- 
    data %>%
    filter(centre %in% centres) %>%
    group_by(id) %>%
    arrange(minutes) %>%
    slice(1) %>%
    ungroup()
  
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

silver_times <- get_df_times(data=df_combined, centres=silver_locs)
gold_times <- get_df_times(data=df_combined, centres=gold_locs)

weighted_rehab_times <- 
  inner_join(
    rename(silver_times, rehab_centre=centre, silver_time=minutes),
    select(gold_times, id, gold_time=minutes),
    by="id"
  )

write.csv(weighted_rehab_times, file=file.path(rehab_times_dir, "weighted_rehab_time.csv"), row.names=FALSE)

# subset polygons
set.seed(42)
library(sf)
library(sp)
library(tidyverse)

# dissolve polygon
aggregate_by_SA <- function(qld_sf, SA_number, SA_year){
  # sa_main <- glue::glue('SA{SA_number}_{ifelse(SA_number %in% c(3,4), "CODE", "MAIN")}16')
  sa_main <- glue::glue('SA{SA_number}_CODE{SA_year}')
  if(!sa_main %in% names(qld_sf)) return(message(sa_main, " was not found in polygon layer"))
  message(glue::glue('----- grouping polygons within SA{SA_number} -----'))
  rmapshaper::ms_dissolve(qld_sf, sa_main)
}

qld_SAs2016 <-
  st_read("input/qld_sa_zones_2016/MB_2016_QLD.shp") %>%
  rename(SA1_CODE16=SA1_MAIN16, SA2_CODE16=SA2_MAIN16)
qld_SAs2011 <- st_read("input/qld_sa_zones_2011/MB_2011_QLD.shp") %>%
  rename(SA1_CODE11=SA1_MAIN11, SA2_CODE11=SA2_MAIN11)
saveRDS(
  aggregate_by_SA(qld_SAs2016, 2, SA_year=16),
  file="output/sa_polygons/QLD_SA2_2016.rds"
)
saveRDS(
  aggregate_by_SA(qld_SAs2016, 1, SA_year=16),
  file="output/sa_polygons/QLD_SA1_2016.rds"
)
saveRDS(
  aggregate_by_SA(qld_SAs2011, 2, SA_year=11),
  file="output/sa_polygons/QLD_SA2_2011.rds"
)
saveRDS(
  aggregate_by_SA(qld_SAs2011, 1, SA_year=11),
  file="output/sa_polygons/QLD_SA1_2011.rds"
)
aus_SA1_2021 <- st_read("input/aus_sa1_2021/SA1_2021_AUST_GDA94.shp")
aus_SA2_2021 <- st_read("input/aus_sa2_2021/SA2_2021_AUST_GDA94.shp")
saveRDS(
  aus_SA1_2021[aus_SA1_2021$STE_NAME21=="Queensland", ],
  file="output/sa_polygons/QLD_SA1_2021.rds"
)
saveRDS(
  aus_SA2_2021[aus_SA2_2021$STE_NAME21=="Queensland", ], 
  file="output/sa_polygons/QLD_SA2_2021.rds"
)
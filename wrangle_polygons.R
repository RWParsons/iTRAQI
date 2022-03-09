# wrangle polygons for downloadable aggregate data and map
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
aus_SA1_2021 <- st_read("input/aus_sa1_2021/SA1_2021_AUST_GDA2020.shp")
aus_SA2_2021 <- st_read("input/aus_sa2_2021/SA2_2021_AUST_GDA2020.shp")
saveRDS(
  aus_SA1_2021[aus_SA1_2021$STE_NAME21=="Queensland", ],
  file="output/sa_polygons/QLD_SA1_2021.rds"
)
saveRDS(
  aus_SA2_2021[aus_SA2_2021$STE_NAME21=="Queensland", ], 
  file="output/sa_polygons/QLD_SA2_2021.rds"
)


# join interpolated values and aggregate within zones
get_SA_agged_times <- function(lzn_kriged_df, SA_number, SA_year, simplify_keep=1, save_path=NULL){
  coordinates(lzn_kriged_df) <- ~ X + Y
  lzn_kriged_sf <- st_as_sf(lzn_kriged_df)
  lzn_kriged_sf <- st_set_crs(lzn_kriged_sf, 4326)
  lzn_kriged_sf <- st_transform(lzn_kriged_sf, crs = 4326)
  
  qld_SAs <- readRDS(glue::glue("output/sa_polygons/QLD_SA{SA_number}_20{SA_year}.rds"))
  qld_SAs <- st_transform(qld_SAs, crs = 4326)
  qld_SAs_with_int_times <- st_join(qld_SAs, lzn_kriged_sf)
  
  # https://ryanpeek.org/2019-04-29-spatial-joins-in-r/
  SA_id <- glue::glue('SA{SA_number}_CODE{SA_year}')
  SAs_agg_times <- 
    qld_SAs_with_int_times %>%
    na.omit() %>%
    as.data.frame() %>%
    group_by(across(all_of(SA_id))) %>%
    summarize(value=median(var1.pred, na.omit=TRUE))
  
  SAs_agg_times <- merge(qld_SAs, SAs_agg_times, all.x=TRUE)
  
  if(simplify_keep<1){
    SAs_agg_times <- rmapshaper::ms_simplify(SAs_agg_times, keep=simplify_keep)
  }
  
  if(!is.null(save_path)){
    saveRDS(SAs_agg_times, save_path)
  }
  return(SAs_agg_times)
}


grid <- expand.grid(
  data=list.files("output/kriging_data"),
  SA_polygons=list.files("output/sa_polygons"),
  stringsAsFactors=FALSE
)

sf_use_s2(FALSE)

for(i in 1:nrow(grid)){
  data_file <- grid$data[i]
  sa_file <- grid$SA_polygons[i]
  kriged_df <- readRDS(file.path("output/kriging_data", grid$data[i]))
  sa_polygons <- readRDS(file.path("output/sa_polygons", grid$SA_polygons[i]))
  care_type <- str_extract(data_file, "^[a-z]*(?=_)")
  SA_level <- str_extract(sa_file, "(?<=SA)[0-9]")
  SA_year <- str_extract(sa_file, "(?<=20)[0-9]{2}")
  
  get_SA_agged_times(
    lzn_kriged_df=kriged_df,
    SA_number=SA_level,
    SA_year=SA_year,
    save_path=glue::glue("output/layers/{care_type}_polygons_SA{SA_level}_year20{SA_year}.rds")
  ) %>% as.data.frame() %>%
    dplyr::select(1, value) %>% 
    write.csv(
      file=glue::glue("output/download_data/{care_type}_data_SA{SA_level}_year20{SA_year}.csv"),
      row.names=FALSE
    )
}


# make 2016 layers with simplify_keep=0.1 for faster load time and smaller file size
grid_2016 <- filter(grid, str_detect(SA_polygons, "2016"))

for(i in 1:nrow(grid_2016)){
  data_file <- grid_2016$data[i]
  sa_file <- grid_2016$SA_polygons[i]
  kriged_df <- readRDS(file.path("output/kriging_data", grid_2016$data[i]))
  sa_polygons <- readRDS(file.path("output/sa_polygons", grid_2016$SA_polygons[i]))
  care_type <- str_extract(data_file, "^[a-z]*(?=_)")
  SA_level <- str_extract(sa_file, "(?<=SA)[0-9]")
  SA_year <- str_extract(sa_file, "(?<=20)[0-9]{2}")
  
  get_SA_agged_times(
    lzn_kriged_df=kriged_df,
    SA_number=SA_level,
    SA_year=SA_year,
    simplify_keep=0.1,
    save_path=glue::glue("output/layers/{care_type}_polygons_SA{SA_level}_year20{SA_year}_simplified.rds")
  )
}




library(rmapshaper)
library(leaflet)
library(leaflet.extras)
bins <- c(0, 30, 60, 120, 180, 240, 300, 360, Inf)
pal <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
l <- get_SA_agged_times(
  lzn_kriged_df=kriged_df,
  SA_number=SA_level,
  SA_year=SA_year
)


l_0.9 <- ms_simplify(l, keep=0.9)
l_0.6 <- ms_simplify(l, keep=0.6)
l_0.3 <- ms_simplify(l, keep=0.3)
l_0.1 <- ms_simplify(l, keep=0.1)


t0 <- Sys.time()
leaflet(options=leafletOptions(minZoom=5)) %>%
  setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addPolygons(
    data=l,
    fillColor=~pal(value),
    color="black",
    fillOpacity=1,
    weight=1
  )
t_full <- Sys.time() - t0

t0 <- Sys.time()
leaflet(options=leafletOptions(minZoom=5)) %>%
  setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addPolygons(
    data=l_0.9,
    fillColor=~pal(value),
    color="black",
    fillOpacity=1,
    weight=1
  )
t_0.9 <- Sys.time() - t0

t0 <- Sys.time()
leaflet(options=leafletOptions(minZoom=5)) %>%
  setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addPolygons(
    data=l_0.6,
    fillColor=~pal(value),
    color="black",
    fillOpacity=1,
    weight=1
  )
t_0.6 <- Sys.time() - t0

t0 <- Sys.time()
leaflet(options=leafletOptions(minZoom=5)) %>%
  setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addPolygons(
    data=l_0.3,
    fillColor=~pal(value),
    color="black",
    fillOpacity=1,
    weight=1
  )
t_0.3 <- Sys.time() - t0

t0 <- Sys.time()
leaflet(options=leafletOptions(minZoom=5)) %>%
  setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addPolygons(
    data=l_0.1,
    fillColor=~pal(value),
    color="black",
    fillOpacity=1,
    weight=1
  )
t_0.1 <- Sys.time() - t0


c(t_full, t_0.9, t_0.6, t_0.3, t_0.1)



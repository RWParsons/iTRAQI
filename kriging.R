# kriging
set.seed(42)
library(sp)
library(gstat)
library(tidyverse)
library(sf)

sf_use_s2(FALSE)

qld_bounary <- read_sf("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
qld_SAs2021 <- readRDS("output/sa_polygons/QLD_SA1_2021.rds")
qld_SAs2016 <- readRDS("output/sa_polygons/QLD_SA1_2016.rds")
qld_SAs2011 <- readRDS("output/sa_polygons/QLD_SA1_2011.rds")

qld_SAs_all <- 
  rbind(
    rename(qld_SAs2011, code=1),
    rename(qld_SAs2016, code=1),
    select(rename(qld_SAs2021, code=1), code)
  ) 

aus <- raster::getData('GADM', country = 'AUS', level = 1)

CELL_SIZE_METERS <- 1000
CELL_SIZE <- CELL_SIZE_METERS / 111320
VGM_MODEL = "Sph"

replacement_name_for_brisbane <- "Brisbane (PAH/RBWH)"

df_acute_addons <- data.frame(
  # add brisbane as 8 minutes for acute care
  # add rbwh as 0 minutes
  town_name = c("Brisbane", "RBWH"),
  acute_time = c(8, 0),
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
    TRUE ~ "bad match"
  )
}

df_acute <- readxl::read_excel("input/drive_times/Qld_towns_RSQ pathways V2.xlsx", skip=2) %>%
  select(town_name=TOWN_NAME, acute_time=Total_transport_time_min, acute_care_centre=Destination2) %>%
  filter(!is.na(acute_care_centre)) %>%
  mutate(acute_care_centre = clean_acute_centre(acute_care_centre)) %>%
  rbind(., df_acute_addons) %>%
  distinct() # removes the duplicated Killarney

df_rehab <- read.csv("input/rehab_times/weighted_rehab_time.csv")

df_times <- inner_join(df_acute, rename(df_rehab, rehab_time=minutes), by="town_name") %>%
  rename(location=town_name)

write.csv(df_times, "input/QLD_locations_with_RSQ_times_20220518.csv", row.names = F)

df_times <- 
  df_times %>%
  rename(rehab_centre = silver_rehab_centre) %>%
  select(-gold_rehab_centre)
# df_times_rsq <- readxl::read_xlsx(
#   "input/drive_times/qld_towns_RSQ pathways V2.xlsx",
#   skip=2
# ) %>%
#   filter(!(TOWN_NAME=="Killarney" & round(xcoord) == 144 ))
# df_times <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv") 
# df_times <- df_times %>%
#   select(-acute_time) %>%
#   inner_join(
#     ., 
#     select(df_times_rsq, location=TOWN_NAME, acute_time=Total_transport_time_min),
#     by="location"
#   )%>%
#   select(names(df_times)) %>%
#   mutate(acute_time=as.integer(acute_time))
coordinates(df_times) <- ~ x + y

get_kriging_grid <- function(cellsize, add_centroids=FALSE, centroids_polygon_sf=NULL) {
  grid <- makegrid(aus[aus$NAME_1 == "Queensland",], cellsize = cellsize)
  pnts_sf <- st_as_sf(grid, coords = c('x1', 'x2'), crs = st_crs(qld_bounary))
  pnts <- pnts_sf %>% 
    mutate(
      # https://gis.stackexchange.com/a/343479
      intersection = as.integer(st_intersects(geometry, qld_bounary))
    ) %>%
    filter(!is.na(intersection)) %>%
    st_coordinates() %>% 
    as.data.frame()
  
  if(add_centroids) {
    # add centroids of all polygons to pnts to ensure there's at least one interpolated value within
    centroids <- 
      centroids_polygon_sf %>%
      st_centroid() %>%
      st_coordinates() %>%
      na.omit() %>% 
      as.data.frame()
    pnts <- rbind(pnts, centroids)
  }
  
  coordinates(pnts) <- ~ X + Y
  pnts
}

do_kriging <- function(pnts, vgm_model, data, formula, return_raster=FALSE, save_path=NULL) {
  lzn_vgm <- variogram(formula, data)
  # return(lzn_vgm)
  lzn_fit <- fit.variogram(lzn_vgm, model=vgm(vgm_model))
  kriged_layer <-
    krige(
      formula, data, pnts,
      model=lzn_fit
    ) %>%
    as.data.frame()
  if(return_raster) {
    kriged_layer <- raster::rasterFromXYZ(kriged_layer, crs=4326)
  }
  if(!is.null(save_path)){
    saveRDS(kriged_layer, file=save_path)
  }else{
    return(kriged_layer)
  }
}

# get grid for interpolations
pnts_for_agg <- get_kriging_grid(
  cellsize = CELL_SIZE, add_centroids = TRUE, centroids_polygon_sf = qld_SAs_all
)
pnts_for_raster <- get_kriging_grid(cellsize = CELL_SIZE, add_centroids = FALSE)

# interpolate and write layers to disk
do_kriging(
  pnts=pnts_for_agg, vgm_model=VGM_MODEL, data=df_times,
  formula=rehab_time~1, save_path="output/kriging_data/rehab_for_agg.rds"
)
do_kriging(
  pnts=pnts_for_agg, vgm_model=VGM_MODEL, data=df_times,
  formula=acute_time~1, save_path="output/kriging_data/acute_for_agg.rds"
)
do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_times, return_raster=TRUE,
  formula=rehab_time~1, save_path="output/layers/rehab_raster.rds"
)
do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_times, return_raster=TRUE,
  formula=acute_time~1, save_path="output/layers/acute_raster.rds"
)


do_rehab_layer_kriging <- function(file_path, save_path=NULL){
  df_rehab <- read.csv(file_path)
  coordinates(df_rehab) <- ~ x + y
  do_kriging(
    pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_rehab, return_raster=TRUE,
    formula=minutes~1, save_path=save_path
  )
  return()
}

rehab_times_dir <- "input/rehab_times"
data_files <- list.files(rehab_times_dir, full.names = T)
raster_out_paths <- list.files(rehab_times_dir, full.names = F)
raster_out_paths <- str_replace(raster_out_paths, "csv", "rds")
raster_out_paths <- file.path("output/layers", raster_out_paths)

idx_ignore <- grep("weighted|all", data_files) # ignore the weighted and combined times rehab files
map2(data_files[-idx_ignore], raster_out_paths[-idx_ignore], do_rehab_layer_kriging)

# sanity check plots
library(leaflet)
library(leaflet.extras)
bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900, 1200, 1800)
palBin <- colorBin("YlOrRd", domain = 0:1800, bins=bins, na.color="transparent")
r <- readRDS("output/layers/acute_raster.rds")
# r <- readRDS("output/layers/rehab_raster.rds")
leaflet() %>%
  addMapPane(name = "layers", zIndex = 200) %>%
  addMapPane(name = "maplabels", zIndex = 400) %>%
  addMapPane(name = "markers", zIndex = 405) %>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addProviderTiles("CartoDB.VoyagerOnlyLabels",
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addRasterImage(
    data=r,
    x=raster::raster(r, layer=1),
    colors=palBin
  ) %>%
  addCircleMarkers(
    lng=df_times$x,
    lat=df_times$y,
    popup=paste0(df_times$location, ", acute time:", df_times$acute_time, ", rehab time:", df_times$rehab_time),
    radius=2, fillOpacity=0,
    options = leafletOptions(pane = "markers")
  )


# sanity check - visualise the custom rehab rasters 
# library(leaflet)
# library(leaflet.extras)
# kriging_raster <- do_kriging(
#   pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_gold, return_raster=TRUE,
#   formula=rehab_time~1
# )
# bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
# palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
# 
# palNum1 <- colorNumeric(c("#FFFFCC", "#FFEDA0"), domain=0:30, na.color="transparent")
# palNum2 <- colorNumeric(c("#FFEDA0", "#FED976"), domain=30:60, na.color="transparent")
# palNum3 <- colorNumeric(c("#FED976", "#FEB24C"), domain=60:120, na.color="transparent")
# palNum4 <- colorNumeric(c("#FEB24C", "#FD8D3C"), domain=120:180, na.color="transparent")
# palNum5 <- colorNumeric(c("#FD8D3C", "#FC4E2A"), domain=180:240, na.color="transparent")
# palNum6 <- colorNumeric(c("#FC4E2A", "#E31A1C"), domain=240:300, na.color="transparent")
# palNum7 <- colorNumeric(c("#E31A1C", "#B10026"), domain=300:360, na.color="transparent")
# palNum8 <- colorNumeric(c("#B10026", "#000000"), domain=360:900, na.color="transparent")
# 
# palNum <- function(x){
#   case_when(
#     x < 30 ~ palNum1(x),
#     x < 60 ~ palNum2(x),
#     x < 120 ~ palNum3(x),
#     x < 180 ~ palNum4(x),
#     x < 240 ~ palNum5(x),
#     x < 300 ~ palNum6(x),
#     x < 360 ~ palNum7(x),
#     x < 900 ~ palNum8(x),
#     x >= 900 ~ "#000000",
#     TRUE ~ "transparent"
#   )
# }
# leaflet() %>% 
#   # addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
#   addMapPane(name = "layers", zIndex = 200) %>%
#   addMapPane(name = "maplabels", zIndex = 400) %>%
#   addMapPane(name = "markers", zIndex = 405) %>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addProviderTiles("CartoDB.VoyagerOnlyLabels",
#                    options = leafletOptions(pane = "maplabels"),
#                    group = "map labels") %>%
#   addRasterImage(
#     data=kriging_raster,
#     x=raster::raster(kriging_raster, layer=1),
#     options=leafletOptions(pane="layers"),
#     colors=palNum
#   ) %>%
#   addCircleMarkers(
#     lng=as.data.frame(df_platinum)$x, lat=as.data.frame(df_platinum)$y,
#     radius=2, fillOpacity=0,
#     # popup=glue::glue("TWP:{df_gold$town}, name:{df_gold$location}, time:{df_gold$rehab_time} "),
#     options=leafletOptions(pane="markers")
#   )




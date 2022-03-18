# kriging
set.seed(42)
library(sp)
library(gstat)
library(tidyverse)
library(sf)

qld_bounary <- read_sf("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
qld_SAs2021 <- st_read("input/qld_sa_zones_2021/MB_2021_AUST_GDA2020.shp") %>%
  filter(STE_NAME21=="Queensland")
aus <- raster::getData('GADM', country = 'AUS', level = 1)
CELL_SIZE = 0.03
VGM_MODEL = "Sph"

df_times_rsq <- readxl::read_xlsx(
  "input/drive_times/qld_towns_RSQ pathways V2.xlsx",
  skip=2
) %>%
  filter(!(TOWN_NAME=="Killarney" & round(xcoord) == 144 ))
df_times <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv") 
df_times <- df_times %>%
  select(-acute_time) %>%
  inner_join(
    ., 
    select(df_times_rsq, location=TOWN_NAME, acute_time=Total_transport_time_min),
    by="location"
  )%>%
  select(names(df_times)) %>%
  mutate(acute_time=as.integer(acute_time))
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
  cellsize = CELL_SIZE, add_centroids = TRUE, centroids_polygon_sf = qld_SAs2021
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

# sanity check plots
# library(leaflet)
# library(leaflet.extras)
# bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
# palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
# r <- readRDS("output/layers/acute_raster.rds")
# leaflet() %>% 
#   addMapPane(name = "layers", zIndex = 200) %>%
#   addMapPane(name = "maplabels", zIndex = 400) %>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addProviderTiles("CartoDB.VoyagerOnlyLabels",
#                    options = leafletOptions(pane = "maplabels"),
#                    group = "map labels") %>%
#   addRasterImage(
#     data=r,
#     x=raster::raster(r, layer=1),
#     options=leafletOptions(pane="layers"),
#     colors=palBin
#   )

# get rehab raster when considering different sets of possible centres
files <- file.path("input/drive_times", list.files("input/drive_times/"))
df_combined <- plyr::ldply(files[!str_detect(files, "qld_town")], read.csv)

df_towns_details <- read.csv("input/drive_times/qld_town_names_points.csv") %>%
  select(town=Town_Point, x=Xcoord, y=Ycoord)

get_df_times <- function(data, centres){
  df_combined %>% 
    select(town=From_TOWN_POINT, centre=To_Title, rehab_time=Total_Minutes) %>%
    mutate(centre=str_trim(centre)) %>%
    filter(centre %in% centres) %>%
    group_by(town) %>%
    arrange(rehab_time) %>%
    slice(1) %>%
    ungroup() %>%
    inner_join(., df_towns_details, by="town")
}

df_platinum <- get_df_times(
  df_combined, 
  c("Brain Injury Rehabilitation Unit")
)
coordinates(df_platinum) <- ~ x + y

df_gold <- get_df_times(
  df_combined, 
  c("Brain Injury Rehabilitation Unit", "Townsville University Hospital")
)
coordinates(df_gold) <- ~ x + y

df_future_gold <- get_df_times(
  df_combined, 
  c("Sunshine Coast University Hospital",
    "Gold Coast University Hospital",
    "Townsville University Hospital",
    "Brain Injury Rehabilitation Unit")
)
coordinates(df_future_gold) <- ~ x + y


do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_platinum, return_raster=TRUE,
  formula=rehab_time~1, save_path="output/layers/rehab_raster_platinum.rds"
)
do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_gold, return_raster=TRUE,
  formula=rehab_time~1, save_path="output/layers/rehab_raster_gold.rds"
)
do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_future_gold, return_raster=TRUE,
  formula=rehab_time~1, save_path="output/layers/rehab_raster_future_gold.rds"
)


# sanity check - visualise the custom rehab rasters 
library(leaflet)
library(leaflet.extras)
kriging_raster <- do_kriging(
  pnts=pnts_for_raster, vgm_model=VGM_MODEL, data=df_future_gold, return_raster=TRUE,
  formula=rehab_time~1
)
bins <- c(0, 30, 60, 120, 180, 240, 300, 360, Inf)
pal <- colorBin("YlOrRd", domain = 0:900, bins = bins, na.color="transparent")
leaflet() %>% 
  addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
  addMapPane(name = "layers", zIndex = 200) %>%
  addMapPane(name = "maplabels", zIndex = 400) %>%
  addMapPane(name = "markers", zIndex = 205) %>%
  addProviderTiles("CartoDB.VoyagerNoLabels") %>%
  addProviderTiles("CartoDB.VoyagerOnlyLabels",
                   options = leafletOptions(pane = "maplabels"),
                   group = "map labels") %>%
  addRasterImage(
    data=kriging_raster,
    x=raster::raster(kriging_raster, layer=1),
    options=leafletOptions(pane="layers"),
    colors=pal
  )


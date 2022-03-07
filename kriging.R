# kriging
set.seed(42)

library(sp)
library(gstat)
library(tidyverse)
library(sf)
# library(rnaturalearth)
# library(ggspatial)

qld_bounary <- read_sf("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
qld_SAs2021 <- st_read("input/qld_sa_zones_2021/MB_2021_AUST_GDA2020.shp") %>%
  filter(STE_NAME21=="Queensland")
df_times <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv")
coordinates(df_times) <- ~ x + y
aus <- raster::getData('GADM', country = 'AUS', level = 1)
CELL_SIZE = 0.03
VGM_MODEL = "Sph"

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
    kriged_layer <- rasterFromXYZ(kriged_layer, crs=4326)
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


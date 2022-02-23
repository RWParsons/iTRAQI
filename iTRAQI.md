iTRAQI
================
23 February, 2022

#### resources used:

-   kriging (<https://rpubs.com/nabilabd/118172>)

# load inputs

``` r
df_times <- read.csv("input/QLD_locations_with_RSQ_times_20220210.csv")
coordinates(df_times) <- ~ x + y

qld_bounary <- st_read("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
```

    ## Reading layer `QLD_STATE_POLYGON_shp' from data source 
    ##   `C:\Users\n10891277\OneDrive - Queensland University of Technology\Documents\R_projects\SC_TRAUMA\iTRAQI\input\qld_state_polygon_shp\QLD_STATE_POLYGON_shp.shp' 
    ##   using driver `ESRI Shapefile'
    ## Simple feature collection with 2031 features and 6 fields
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 137.996 ymin: -29.1779 xmax: 153.5552 ymax: -9.141203
    ## Geodetic CRS:  GDA94

``` r
qld_SAs <- st_read("input/qld_sa_zones/MB_2016_QLD.shp")
```

    ## Reading layer `MB_2016_QLD' from data source 
    ##   `C:\Users\n10891277\OneDrive - Queensland University of Technology\Documents\R_projects\SC_TRAUMA\iTRAQI\input\qld_sa_zones\MB_2016_QLD.shp' 
    ##   using driver `ESRI Shapefile'
    ## replacing null geometries with empty geometries
    ## Simple feature collection with 69764 features and 16 fields (with 25 geometries empty)
    ## Geometry type: POLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: 137.9943 ymin: -29.1779 xmax: 153.5522 ymax: -9.142176
    ## Geodetic CRS:  GDA94

``` r
# ggplot(data = world) +
#   geom_sf() +
#   geom_sf(data=qld_SAs, aes(color=SA2_MAIN16)) +
#   coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = T) 
```

# base map and points

``` r
world <- ne_countries(scale = "medium", returnclass = "sf")
ggplot(data = world) +
  geom_sf() +
  geom_sf(data=qld_bounary, color="blue") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = T) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data=as.data.frame(df_times), aes(x,y))
```

    ## Scale on map varies by more than 10%, scale bar may be inaccurate

![](iTRAQI_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

# kriging - select variogram model (Gaussian looks best)

``` r
lzn_vgm <- variogram(acute_time~1, df_times)
lzn_fit <- fit.variogram(lzn_vgm, model=vgm("Gau"))

plot(lzn_vgm, lzn_fit)
```

![](iTRAQI_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# show.vgms()
```

# kriging - create spatial domain to interpolate over

``` r
map <- read_sf("input/qld_state_polygon_shp/QLD_STATE_POLYGON_shp.shp")
aus <- raster::getData('GADM', country = 'AUS', level = 1)
grid <- makegrid(aus[aus$NAME_1 == "Queensland",], cellsize = 0.1)
pnts_sf <- st_as_sf(grid, coords = c('x1', 'x2'), crs = st_crs(map))

pnts <- pnts_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, map))
) %>%
  filter(!is.na(intersection)) %>%
  st_coordinates() %>% 
  as.data.frame()
coordinates(pnts) <- ~ X + Y
```

# kriging - generate interpolations

``` r
lzn_kriged <- krige(acute_time ~ 1, df_times, pnts, model=lzn_fit)%>%
  as.data.frame
```

    ## [using ordinary kriging]

``` r
lzn_kriged  %>%
  ggplot(aes(X, Y)) + 
  geom_tile(aes(fill=var1.pred)) + 
  coord_equal()+
  scale_fill_gradient(low = "yellow", high="red")
```

![](iTRAQI_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# add interpolation layer to map

``` r
ggplot(data = world) +
  geom_sf() +
  geom_sf(data=qld_bounary, color="blue") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = T) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  geom_tile(data=lzn_kriged, aes(X, Y, fill=var1.pred)) +
  geom_point(data=as.data.frame(df_times), aes(x,y)) +
  scale_fill_gradient(low = "yellow", high="red")
```

    ## Scale on map varies by more than 10%, scale bar may be inaccurate

![](iTRAQI_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# spatial join from SA1 and SA2 polygons to interpolated values

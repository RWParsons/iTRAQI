# wrangle polygons for downloadable aggregate data and map
set.seed(42)
library(sf)
library(sp)
library(tidyverse)

qld_SAs2016 <-
  st_read("input/qld_sa_zones_2016/MB_2016_QLD.shp") %>%
  rename(SA1_CODE16=SA1_MAIN16, SA2_CODE16=SA2_MAIN16)

seifa_scale_to_text <- function(x){
  case_when(
    x==1 ~ "Most disadvantaged",
    x==2 ~ "Disadvantaged",
    x==3 ~ "Middle socio-economic status",
    x==4 ~ "Advantaged",
    x==5 ~ "Most advantaged",
    TRUE ~ "NA"
  )
}

make_seifas_df <- function(){
  # SEIFA 2016 source: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2033.0.55.0012016?OpenDocument
  # SEIFA 2011 source: https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/2033.0.55.0012011?OpenDocument
  quintile_from_decile <- function(x) (x + x %% 2)/2
  
  seifa_2016_sa1 <- readxl::read_xls("input/remoteness_and_seifa_data/2016_sa1_seifa.xls", sheet="Table 3", skip=6, col_names=FALSE)
  seifa_2016_sa1 <- seifa_2016_sa1[, c(2, (ncol(seifa_2016_sa1)-3): ncol(seifa_2016_sa1))]
  names(seifa_2016_sa1) <- c("SA1_CODE16", "state", "rank", "decile", "percentile")
  seifa_2016_sa1$quintile <- quintile_from_decile(seifa_2016_sa1$decile)
  
  seifa_2016_sa2 <- readxl::read_xls("input/remoteness_and_seifa_data/2016_sa2_seifa.xls", sheet="Table 3", skip=6, col_names=FALSE)
  seifa_2016_sa2 <- seifa_2016_sa2[, c(1, (ncol(seifa_2016_sa2)-6): (ncol(seifa_2016_sa2)-3))]
  names(seifa_2016_sa2) <- c("SA2_CODE16", "state", "rank", "decile", "percentile")
  seifa_2016_sa2$quintile <- quintile_from_decile(seifa_2016_sa2$decile)
  
  seifa_2011_sa1 <- readxl::read_xls("input/remoteness_and_seifa_data/2011_sa1_seifa.xls", sheet="Table 3", skip=6, col_names=FALSE)
  seifa_2011_sa1 <- seifa_2011_sa1[, c(1, (ncol(seifa_2011_sa1)-3): ncol(seifa_2011_sa1))]
  names(seifa_2011_sa1) <- c("code7", "state", "rank", "decile", "percentile")
  sa1_digits <- readxl::read_xls("input/remoteness_and_seifa_data/2011_sa1_seifa.xls", sheet="Table 7", skip=6, col_names=FALSE) %>%
    select(code7=1, SA1_CODE11=2)
  seifa_2011_sa1 <- inner_join(sa1_digits, seifa_2011_sa1, by="code7") %>% select(-code7)
  seifa_2011_sa1$quintile <- quintile_from_decile(seifa_2011_sa1$decile)
  
  seifa_2011_sa2 <- readxl::read_xls("input/remoteness_and_seifa_data/2011_sa2_seifa.xls", sheet="Table 3", skip=6, col_names=FALSE)
  seifa_2011_sa2 <- seifa_2011_sa2[, c(1, (ncol(seifa_2011_sa2)-6): (ncol(seifa_2011_sa2)-3))]
  names(seifa_2011_sa2) <- c("SA2_CODE11", "state", "rank", "decile", "percentile")
  seifa_2011_sa2$decile <- as.numeric(seifa_2011_sa2$decile)
  seifa_2011_sa2$percentile <- as.numeric(seifa_2011_sa2$percentile)
  seifa_2011_sa2$quintile <- quintile_from_decile(seifa_2011_sa2$decile)
  
  list(
    seifa_2016_sa1=seifa_2016_sa1,
    seifa_2016_sa2=seifa_2016_sa2,
    seifa_2011_sa1=seifa_2011_sa1,
    seifa_2011_sa2=seifa_2011_sa2
  )
}

seifa_list <- make_seifas_df()

make_asgs_df <- function(){
  # source: Susanna's method
  asgs_2016_sa1 <- haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2016_sa1_detls.dta") %>%
    select(SA1_CODE16=sa1_maincode, ra, ra_name)
  asgs_2016_sa2 <- haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2016_sa2_detls.dta") %>%
    select(SA2_CODE16=sa2_maincode, ra, ra_name)
  list(
    asgs_2011_sa1=haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2011_sa1_detls.dta") %>%
      select(SA1_CODE11=sa1_maincode, ra, ra_name),
    asgs_2011_sa2=haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2011_sa2_detls.dta") %>%
      select(SA2_CODE11=sa2_maincode, ra, ra_name),
    
    asgs_2016_sa1=haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2016_sa1_detls.dta") %>%
      select(SA1_CODE16=sa1_maincode, ra, ra_name),
    asgs_2016_sa2=haven::read_dta("input/remoteness_and_seifa_data/qld_ASGS2016_sa2_detls.dta") %>%
      select(SA2_CODE16=sa2_maincode, ra, ra_name)
  )
}

asgs_list <- make_asgs_df()

# join interpolated values and aggregate within zones
get_SA_agged_times <- function(lzn_kriged_df, SA_number, SA_year, simplify_keep=1, add_seifa_and_asgs=FALSE, save_path=NULL){
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
    summarize(
      value=median(var1.pred, na.rm=TRUE),
      min=min(var1.pred, na.rm=TRUE),
      max=max(var1.pred, na.rm=TRUE)
    )
  
  SAs_agg_times <- merge(qld_SAs, SAs_agg_times, all.x=TRUE)
  
  if(simplify_keep<1){
    SAs_agg_times <- rmapshaper::ms_simplify(SAs_agg_times, keep=simplify_keep)
  }
  
  if(add_seifa_and_asgs){
    seifa_df <- seifa_list[[glue::glue("seifa_20{SA_year}_sa{SA_number}")]]
    seifa_df <- select(seifa_df, 1, seifa_quintile=quintile)
    SAs_agg_times <- merge(SAs_agg_times, seifa_df, all.x=TRUE)
    
    asgs_df <- asgs_list[[glue::glue("asgs_20{SA_year}_sa{SA_number}")]]
    SAs_agg_times <- merge(SAs_agg_times, asgs_df, all.x=TRUE)
  }
  
  if(!is.null(save_path)){
    saveRDS(SAs_agg_times, save_path)
  }
  return(SAs_agg_times)
}


sf_use_s2(FALSE)

grid <- expand.grid(
  data=list.files("output/kriging_data"),
  SA_polygons=list.files("output/sa_polygons"),
  stringsAsFactors=FALSE
)

if(FALSE) {
  for(i in 1:nrow(grid)){
    data_file <- grid$data[i]
    sa_file <- grid$SA_polygons[i]
    kriged_df <- readRDS(file.path("output/kriging_data", grid$data[i]))
    sa_polygons <- readRDS(file.path("output/sa_polygons", grid$SA_polygons[i]))
    care_type <- str_extract(data_file, "^[a-z]*(?=_)")
    SA_level <- str_extract(sa_file, "(?<=SA)[0-9]")
    SA_year <- str_extract(sa_file, "(?<=20)[0-9]{2}")
    # if(care_type=="rehab") next
    # print(care_type)
    get_SA_agged_times(
      lzn_kriged_df=kriged_df,
      SA_number=SA_level,
      SA_year=SA_year,
      # no need to save these as they're no longer being used
      # save_path=glue::glue("output/layers/{care_type}_polygons_SA{SA_level}_year20{SA_year}.rds")
    ) %>% as.data.frame() %>%
      dplyr::select(1, value, min, max) %>% 
      write.csv(
        file=glue::glue("output/download_data/{care_type}_data_SA{SA_level}_year20{SA_year}.csv"),
        row.names=FALSE
      )
  }
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
  # if(care_type=="rehab") next
  layer <- get_SA_agged_times(
    lzn_kriged_df=kriged_df,
    SA_number=SA_level,
    SA_year=SA_year,
    simplify_keep=0.1,
    add_seifa_and_asgs=TRUE
  )
  if(SA_level=="1"){
    layer <- merge(layer, asgs_list$asgs_2016_sa1, all.x=TRUE)
  } else if(SA_level=="2"){
    layer <- merge(layer, asgs_list$asgs_2016_sa2, all.x=TRUE)
  }
  saveRDS(layer, glue::glue("output/layers/{care_type}_polygons_SA{SA_level}_year20{SA_year}_simplified.rds"))
}


# combine SA agg layers into a single layer with column for SA and care type so that shiny can filter on them
##########

make_combined_SA2_layers <- function(save=TRUE){
  region_lookup <- distinct(as.data.frame(qld_SAs2016)[, c("SA2_CODE16", "SA2_NAME16")])
  
  SA2_acute <- readRDS("output/layers/acute_polygons_SA2_year2016_simplified.rds")
  SA2_acute$care_type <- "acute"
  SA2_acute <- left_join(SA2_acute, region_lookup) %>%
    mutate(popup_acute = paste0(
      paste0(
        "<b>SA2 Region: </b>", .[["SA2_NAME16"]], "<br>",
        "<b>SA", 2, " ID: </b>", .[[1]], "<br>",
        "<b>Remoteness: </b>", .[["ra_name"]], "<br>",
        "<b>SEIFA: </b>", seifa_scale_to_text(.[["seifa_quintile"]]), "<br>",
        "<b>Time to ", care_type, " care in minutes (estimate [min - max]): </b>", "<br>", 
        "&nbsp;&nbsp;&nbsp;&nbsp; ", round(.[["value"]]), " [", round(.[["min"]]), " - ", round(.[["max"]]), "]<br>"
      )
    ))
  
  SA2_rehab <- readRDS("output/layers/rehab_polygons_SA2_year2016_simplified.rds")
  SA2_rehab$care_type <- "rehab"
  SA2_rehab <- left_join(SA2_rehab, region_lookup) %>%
    mutate(popup_rehab = paste0(
      paste0(
        "<b>SA2 Region: </b>", .[["SA2_NAME16"]], "<br>",
        "<b>SA", 2, " ID: </b>", .[[1]], "<br>",
        "<b>Remoteness: </b>", .[["ra_name"]], "<br>",
        "<b>SEIFA: </b>", seifa_scale_to_text(.[["seifa_quintile"]]), "<br>",
        "<b>Time to ", care_type, " care in minutes (estimate [min - max]): </b>", "<br>", 
        "&nbsp;&nbsp;&nbsp;&nbsp; ", round(.[["value"]]), " [", round(.[["min"]]), " - ", round(.[["max"]]), "]<br>"
      )
    ))
  
  combined_SA2 <- select(SA2_acute, 1, ra, seifa_quintile, popup_acute, value_acute=value) %>% 
    left_join(., select(as.data.frame(SA2_rehab), 1, popup_rehab, value_rehab=value))
  
  if(save) saveRDS(combined_SA2, "output/layers/combined_polygons_SA2_year2016_simplified.rds")
  combined_SA2
}

make_combined_SA1_layers <- function(save=TRUE){
  region_lookup <- distinct(as.data.frame(qld_SAs2016)[, c("SA1_CODE16", "SA2_NAME16")])
  
  SA1_acute <- readRDS("output/layers/acute_polygons_SA1_year2016_simplified.rds")
  SA1_acute$care_type <- "acute"
  SA1_acute <- left_join(SA1_acute, region_lookup) %>%
    mutate(popup_acute = paste0(
      paste0(
        "<b>SA2 Region: </b>", .[["SA2_NAME16"]], "<br>",
        "<b>SA", 1, " ID: </b>", .[[1]], "<br>",
        "<b>Remoteness: </b>", .[["ra_name"]], "<br>",
        "<b>SEIFA: </b>", seifa_scale_to_text(.[["seifa_quintile"]]), "<br>",
        "<b>Time to ", care_type, " care in minutes (estimate [min - max]): </b>", "<br>", 
        "&nbsp;&nbsp;&nbsp;&nbsp; ", round(.[["value"]]), " [", round(.[["min"]]), " - ", round(.[["max"]]), "]<br>"
      )
    ))
  
  SA1_rehab <- readRDS("output/layers/rehab_polygons_SA1_year2016_simplified.rds")
  SA1_rehab$care_type <- "rehab"
  SA1_rehab <- left_join(SA1_rehab, region_lookup) %>%
    mutate(popup_rehab = paste0(
      paste0(
        "<b>SA2 Region: </b>", .[["SA2_NAME16"]], "<br>",
        "<b>SA", 1, " ID: </b>", .[[1]], "<br>",
        "<b>Remoteness: </b>", .[["ra_name"]], "<br>",
        "<b>SEIFA: </b>", seifa_scale_to_text(.[["seifa_quintile"]]), "<br>",
        "<b>Time to ", care_type, " care in minutes (estimate [min - max]): </b>", "<br>", 
        "&nbsp;&nbsp;&nbsp;&nbsp; ", round(.[["value"]]), " [", round(.[["min"]]), " - ", round(.[["max"]]), "]<br>"
      )
    ))
  
  combined_SA1 <- select(SA1_acute, 1, ra, seifa_quintile, popup_acute, value_acute=value) %>% 
    left_join(., select(as.data.frame(SA1_rehab), 1, popup_rehab, value_rehab=value))
  
  if(save) saveRDS(combined_SA1, "output/layers/combined_polygons_SA1_year2016_simplified.rds")
  combined_SA1
}

# make_combined_SA2_layers()
# make_combined_SA1_layers()

stack_SA1_and_SA2_layers <- function(save=TRUE){
  SA1 <- make_combined_SA1_layers(save=FALSE)
  SA2 <- make_combined_SA2_layers(save=FALSE)
  
  SA1$SA_level <- 1
  SA2$SA_level <- 2
  
  names(SA1)[1] <- "CODE"
  names(SA2)[1] <- "CODE"
  
  stacked <- rbind(SA1, SA2)
  if(save) saveRDS(stacked, "output/layers/stacked_SA1_and_SA2_polygons_year2016_simplified.rds")
  stacked
}

stack <- stack_SA1_and_SA2_layers(save=FALSE)

stack_rehab <- select(stack, -popup_acute, -value_acute) %>% rename(popup=popup_rehab, value=value_rehab) %>% mutate(care_type="rehab")
stack_acute <- select(stack, -popup_rehab, -value_rehab) %>% rename(popup=popup_acute, value=value_acute) %>% mutate(care_type="acute")
vstack <- rbind(stack_acute, stack_rehab)
saveRDS(vstack, "output/layers/vertical_stacked_SA1_and_SA2_polygons_year2016_simplified.rds")
#############

# combine downloadable data sheets so that acute and rehab times are on same sheet
combine_data <- function(SA_year, SA_level){
  df_acute <- read.csv(glue::glue("output/download_data/acute_data_SA{SA_level}_year20{SA_year}.csv"))
  df_rehab <- read.csv(glue::glue("output/download_data/rehab_data_SA{SA_level}_year20{SA_year}.csv"))
  get_names <- function(x) {
    c(
      glue::glue("median_time_to_{x}_care"),
      glue::glue("min_time_to_{x}_care"),
      glue::glue("max_time_to_{x}_care")
    )
  }
  names(df_acute)[-1] <- get_names("acute")
  names(df_rehab)[-1] <- get_names("rehab")
  
  df_combined <- inner_join(df_acute, df_rehab, by = glue::glue("SA{SA_level}_CODE{SA_year}"))
  
  write.csv(
    df_combined,
    glue::glue("output/download_data/combined_data_SA{SA_level}_year20{SA_year}.csv"),
    row.names=FALSE
  )
  if(SA_year != 21){
    seifa_df <- seifa_list[[glue::glue("seifa_20{SA_year}_sa{SA_level}")]] %>%
      select(-state, -rank)
    names(seifa_df)[-1] <- paste0("seifa_", names(seifa_df)[-1])
    
    asgs_df <- asgs_list[[glue::glue("asgs_20{SA_year}_sa{SA_level}")]]
    
    df_seifa_and_asgs <- 
      df_combined %>% 
      select(1) %>%
      left_join(., seifa_df, by=glue::glue("SA{SA_level}_CODE{SA_year}")) %>%
      left_join(., asgs_df, by=glue::glue("SA{SA_level}_CODE{SA_year}"))
    
    write.csv(
      df_seifa_and_asgs,
      glue::glue("output/download_data/seifa_and_israd_SA{SA_level}_year20{SA_year}.csv"),
      row.names=FALSE
    )
  }
  
  return(df_combined)
}

grid_combine <- expand.grid(year=c(11, 16, 21), sa=c(1, 2))
map2(.x=grid_combine$year, .y=grid_combine$sa, combine_data)


write_data_to_xlsx <- function(template_dir, 
                               output_dir,
                               seifa=seifa_list, 
                               asgs=asgs_list, 
                               data_dir="output/download_data/"){
  unlink(paste0(output_dir, "*")) # delete old files in output_dir
  templates <- dir(template_dir)
  for(f in templates){
    gc()
    out_file <- str_remove(f, "front page ")
    # copy template files to output_dir
    file.copy(file.path(template_dir, f), file.path(output_dir, out_file))
    
    SA_year <- str_extract(f, "(?<=20)[0-9]{2}")
    SA_level <- str_extract(f, "(?<=SA)[0-9]")
    
    # write drive times, seifa, and israd data to separate sheets in the new xlsx doc (from template)
    df_drive_times <- read.csv(file.path(data_dir, glue::glue("combined_data_SA{SA_level}_year20{SA_year}.csv")))
    xlsx::write.xlsx(
      x=df_drive_times,
      file=file.path(output_dir, out_file),
      sheetName="Travel time",
      row.names=FALSE,
      append=TRUE
    )
    
    drive_times_idx <- select(df_drive_times, 1)
    gc()
    seifa_name <- glue::glue("seifa_20{SA_year}_sa{SA_level}")
    if(!is.null(seifa[[seifa_name]])){
      df_seifa <- left_join(drive_times_idx, as.data.frame(seifa[[seifa_name]]))
      df_seifa$state <- NULL
      df_seifa$quintile_label <- seifa_scale_to_text(df_seifa$quintile)
      xlsx::write.xlsx(
        x=df_seifa,
        file=file.path(output_dir, out_file),
        sheetName="SEIFA",
        row.names=FALSE,
        append=TRUE
      )
    }
    gc()
    asgs_name <- glue::glue("asgs_20{SA_year}_sa{SA_level}")
    if(!is.null(asgs[[asgs_name]])){
      df_israd <- left_join(drive_times_idx, as.data.frame(asgs[[asgs_name]]))
      xlsx::write.xlsx(
        x=df_israd,
        file=file.path(output_dir, out_file),
        sheetName="Remoteness",
        row.names=FALSE,
        append=TRUE
      )
    }
  }
}

write_data_to_xlsx(
  template_dir="input/downloadable_data_templates/",
  output_dir="output/compiled_download_data/"
)

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


# l_0.9 <- ms_simplify(l, keep=0.9)
# l_0.6 <- ms_simplify(l, keep=0.6)
# l_0.3 <- ms_simplify(l, keep=0.3)
# l_0.1 <- ms_simplify(l, keep=0.1)
# 
# 
# t0 <- Sys.time()
# leaflet(options=leafletOptions(minZoom=5)) %>%
#   setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addPolygons(
#     data=l,
#     fillColor=~pal(value),
#     color="black",
#     fillOpacity=1,
#     weight=1
#   )
# t_full <- Sys.time() - t0
# 
# t0 <- Sys.time()
# leaflet(options=leafletOptions(minZoom=5)) %>%
#   setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addPolygons(
#     data=l_0.9,
#     fillColor=~pal(value),
#     color="black",
#     fillOpacity=1,
#     weight=1
#   )
# t_0.9 <- Sys.time() - t0
# 
# t0 <- Sys.time()
# leaflet(options=leafletOptions(minZoom=5)) %>%
#   setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addPolygons(
#     data=l_0.6,
#     fillColor=~pal(value),
#     color="black",
#     fillOpacity=1,
#     weight=1
#   )
# t_0.6 <- Sys.time() - t0
# 
# t0 <- Sys.time()
# leaflet(options=leafletOptions(minZoom=5)) %>%
#   setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addPolygons(
#     data=l_0.3,
#     fillColor=~pal(value),
#     color="black",
#     fillOpacity=1,
#     weight=1
#   )
# t_0.3 <- Sys.time() - t0
# 
# t0 <- Sys.time()
# leaflet(options=leafletOptions(minZoom=5)) %>%
#   setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5)%>%
#   addProviderTiles("CartoDB.VoyagerNoLabels") %>%
#   addPolygons(
#     data=l_0.1,
#     fillColor=~pal(value),
#     color="black",
#     fillOpacity=1,
#     weight=1
#   )
# t_0.1 <- Sys.time() - t0
# 
# 
# c(t_full, t_0.9, t_0.6, t_0.3, t_0.1)



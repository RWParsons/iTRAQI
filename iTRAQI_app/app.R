library(shiny)
library(leaflet)
# To get the pane options in raster layers to work, install leaflet from 
# https://github.com/rstudio/leaflet/tree/joe/feature/raster-options
# see associated PR here: https://github.com/rstudio/leaflet/pull/692
library(leaflet.extras)
library(RColorBrewer)
library(tidyverse)
library(raster)


layer_input <- c(
  "SA1 acute time" = "SA1s_acute",
  "SA2 acute time" = "SA2s_acute",
  "SA1 rehab time" = "SA1s_rehab",
  "SA2 rehab time" = "SA2s_rehab",
  "Acute time" = "kriged_acute",
  "Rehab time" = "kriged_rehab"
)

group_display <- "SA1 acute time"

layers_dir <- "../input/layers"
SAs_sf <- readRDS(file.path(layers_dir, "SA1s_acute.rds"))

df_locations <- read.csv("../input/QLD_locations_with_RSQ_times_20220210.csv") %>%
  mutate(popup=paste0(
  "<b>Location: </b>", location, "<br>",
  "<b>Acute care destination: </b>", acute_care_centre, "<br>",
  "<b>Time to acute care (minutes): </b>", acute_time, "<br>",
  "<b>Rehab care destination: </b>", rehab_centre, "<br>",
  "<b>Time to rehab care (minutes): </b>", rehab_time, "<br>"
))

rehab_centres <- c(
  "Sunshine Coast University Hospital",
  "Central West Sub-Acute Service",
  "Gympie Hospital",
  "Rockhampton Hospital",
  "Roma Hospital"
)
acute_centres <- c(
  "Brain Injury Rehabilitation Unit",
  "Gold Coast University Hospital",
  "Townsville University Hospital"
)

df_centres <- read.csv("../input/centres.csv") %>% 
  janitor::clean_names() %>%
  mutate(centre_name = str_trim(centre_name))%>%
  filter(centre_name %in% c(rehab_centres, acute_centres)) %>%
  mutate(
    care_type=ifelse(centre_name %in% acute_centres, "acute", "rehab"),
    popup=paste0(
      "<b>Centre name: </b>", centre_name, "<br>",
      "<b>Care type: </b>", ifelse(care_type=="acute", "Acute care", "Rehabilitation care"), "<br>",
      "<b>Address: </b>", address, "<br>"
    )
  )

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map_async", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, right = 0,
    checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session){
  # https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77
  rvs <- reactiveValues(to_load=0, map=NULL)
  
  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, Inf)
  pal <- colorBin("YlOrRd", domain = SAs_sf$mean, bins=bins, na.color="transparent")
  centre_icons <- iconList(
    acute=makeIcon(iconUrl = "../input/imgs/acute_care2.png", iconWidth = 783/18, iconHeight = 900/18),
    rehab=makeIcon(iconUrl = "../input/imgs/rehab_care.png", iconWidth = 783/18, iconHeight = 783/18)
  )
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map_async", data = SAs_sf)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      proxy %>% addLegend(
        position = "bottomright",
        pal = pal, values = ~mean
      )
    }
  })
  
  output$map_async <- renderLeaflet({
    rvs$to_load <- isolate(rvs$to_load) + 1 # change the value to trigger observeEvent
    rvs$map <- 
      leaflet() %>%
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 205) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addLayersControl(
        position = "topright",
        baseGroups = names(layer_input),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      addCircleMarkers(
        lng=df_locations$x, lat=df_locations$y, 
        radius=2, fillOpacity=0,
        popup=df_locations$popup,
        options=leafletOptions(pane="markers")
      ) %>%
      addMarkers(
        lng=df_centres$x, lat=df_centres$y, 
        icon=centre_icons[df_centres$care_type],
        popup=df_centres$popup,
        options=leafletOptions(pane="markers")
      )

    rvs$map
  })
  
  observeEvent(rvs$to_load,{
    req(rvs$map) # if it's not null or false
    
    group_names_to_load <- names(layer_input)
    raster_layers <- grep("kriged", layer_input)
    polygon_layers <- group_names_to_load[-raster_layers]
    raster_layers <- group_names_to_load[raster_layers]
    
    for(group_name in polygon_layers){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      leafletProxy("map_async") %>%
        addPolygons(
          data=new_layer,
          fillColor=~pal(mean),
          color="black",
          fillOpacity=1,
          weight=1,
          group=group_name,
          options=leafletOptions(pane="layers")
        )
    }
    
    for(group_name in raster_layers){
      new_layer <- readRDS(file.path(layers_dir, glue::glue("{layer_input[group_name]}.rds")))
      leafletProxy("map_async") %>%
        addRasterImage(
          data=new_layer,
          x=raster(new_layer, layer=1),
          group=group_name,
          options=leafletOptions(pane="layers"),
          colors=pal
        )
    }
  })
  
}

shinyApp(ui, server)

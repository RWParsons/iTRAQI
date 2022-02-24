library(shiny)
library(leaflet)
library(RColorBrewer)


layer_input <- c(
  "SA1 acute time" = "SA1s_acute",
  "SA2 acute time" = "SA2s_acute"
)

group_display <- "SA1 acute time"

layers_dir <- "../input/layers"
SAs_sf <- readRDS(file.path(layers_dir, "SA1s_acute.rds"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map_async", width = "100%", height = "100%"),
  # leafletOutput("map2", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, right = 0,
    checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session){
  # https://medium.com/ibm-data-ai/asynchronous-loading-of-leaflet-layer-groups-afc073999e77
  rvs <- reactiveValues(to_load=0, map=NULL)
  

  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, Inf)
  pal <- colorBin("YlOrRd", domain = SAs_sf$mean, bins = bins)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "maplabels", zIndex = 420) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addPolygons(
        data=SAs_sf,
        fillColor = ~pal(mean),
        color="black",
        fillOpacity=1,
        weight=1,
        group="SAs",
        options = leafletOptions(pane = "polygons")
      )%>%
      addLayersControl(
        baseGroups = "CartoDB.PositronNoLabels",
        overlayGroups = c("map labels","SAs")
      )
  })
  # # Use a separate observer to recreate the legend as needed.
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
      addMapPane(name = "polygons", zIndex = 410) %>%
      addMapPane(name = "maplabels", zIndex = 420) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addPolygons(
        data=SAs_sf,
        fillColor = ~pal(mean),
        color="black",
        fillOpacity=1,
        weight=1,
        group=group_display,
        options = leafletOptions(pane = "polygons")
      ) %>%
      addLayersControl(
        position = "topright",
        baseGroups = names(layer_input),
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup(names(layer_input)) %>%
      showGroup(group_display) 

    rvs$map
  })
  
  observeEvent(rvs$to_load,{
    req(rvs$map) # if it's not null or false
    
    group_names_to_load <- names(layer_input)
    group_names_to_load <- group_names_to_load[group_names_to_load != group_display] # take out the default layer already added
    
    for (group_name in group_names_to_load){
      new_layer <- readRDS(file.path(layers_dir, "SA2s_acute.rds"))
      leafletProxy("map_async") %>%
        addPolygons(
                data=new_layer,
                fillColor = ~pal(mean),
                color="black",
                fillOpacity=1,
                weight=1,
                group=group_name,
                options = leafletOptions(pane = "polygons")
              )
    }
  })
  
}

shinyApp(ui, server)

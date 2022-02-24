library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  # leafletOutput("map2", width = "100%", height = "100%"),
  absolutePanel(
    top = 0, left = 50,
    checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session){
  layers_dir <- "../input/layers"
  SA2s_sf <- readRDS(file.path(layers_dir, "SA2s_acute.rds"))

  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, Inf)
  pal <- colorBin("YlOrRd", domain = SA2s_sf$mean, bins = bins)
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addPolygons(
        data=SA2s_sf, 
        fillColor = ~pal(mean), 
        color="black", 
        fillOpacity=1,
        weight=1
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = SA2s_sf)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      # pal <- colorpal()
      proxy %>% addLegend(
        position = "bottomright",
        pal = pal, values = ~mean
      )
    }
  })

}

shinyApp(ui, server)

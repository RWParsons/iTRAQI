library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
# To get the pane options in raster layers to work, install leaflet from 
# https://github.com/rstudio/leaflet/tree/joe/feature/raster-options
# see associated PR here: https://github.com/rstudio/leaflet/pull/692
# can install directly with `remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")`
library(leaflet.extras)
library(leaflegend)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(sp)
library(raster)
library(rgdal)


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

seifa_text_to_value <- function(x){
  case_when(
    x=="Most disadvantaged" ~ 1,
    x=="Disadvantaged" ~ 2,
    x=="Middle socio-economic status" ~ 3,
    x=="Advantaged" ~ 4,
    x=="Most advantaged" ~ 5,
  )
}


ui <- navbarPage(
  "iTRAQI",
  tabPanel(
    title="Main Map",
    div(
      tags$style(type = "text/css", "#main_map {height: calc(100vh - 80px) !important;}"),
      withSpinner(leafletOutput("main_map")),
      absolutePanel(
        id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        h3("control panel"),
        sliderTextInput("seifa", "SEIFA", seifa_scale_to_text(1:5), selected = c(seifa_scale_to_text(1), seifa_scale_to_text(5))),
        verbatimTextOutput("value")
      )
    )
  )
)

server <- function(input, output, session){
  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
  palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
  
  output$value <- renderPrint({seifa_text_to_value(input$seifa)})
  
  output$main_map <- renderLeaflet({
    leaflet(options=leafletOptions(minZoom=5)) %>%
      setMaxBounds(lng1 = 115, lat1 = -45.00, lng2 = 170, lat2 = -5) %>%
      addSearchOSM(options=searchOptions(moveToLocation=FALSE, zoom=NULL)) %>%
      addMapPane(name = "layers", zIndex = 200) %>%
      addMapPane(name = "maplabels", zIndex = 400) %>%
      addMapPane(name = "markers", zIndex = 205) %>%
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      addProviderTiles("CartoDB.VoyagerOnlyLabels",
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      addLegendBin(
        opacity=1,
        position="topleft",
        pal=palBin,
        values=0:900,
        title=htmltools::tagList(tags$div("Time to care (minutes)"), tags$br())
      ) 
  })
  
  polygons <- readRDS("../output/layers/stacked_SA1_and_SA2_polygons_year2016_simplified.rds") %>%
    filter(SA_level==2)
  
  observe({
    seifa_filter <- input$seifa
    
    seifa_lower <- seifa_text_to_value(seifa_filter[1])
    seifa_upper <- seifa_text_to_value(seifa_filter[2])
    
    update_polygons <- polygons %>%
      filter(seifa_quintile >= seifa_lower,
             seifa_quintile <= seifa_upper)
    
    
    # if (sizeBy == "superzip") {
    #   # Radius is treated specially in the "superzip" case.
    #   radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    # } else {
    #   radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    # }
    
    leafletProxy("main_map", data = update_polygons) %>%
      addPolygons(
        fillColor=~palBin(value_acute),
        color="black",
        fillOpacity=1,
        weight=1,
        popup=update_polygons$popup_acute,
        options=leafletOptions(pane="layers")
        # layerId="updatePoly"
      )
  })
  
  
  
  
  
}

shinyApp(ui, server)
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

dropdownButton2 <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  # https://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
}


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

ra_scale_to_text <- function(x){
  case_when(
    x==0 ~ "Major Cities of Australia",
    x==1 ~ "Inner Regional Australia",
    x==2 ~ "Outer Regional Australia",
    x==3 ~ "Remote Australia",
    x==4 ~ "Very Remote Australia",
    TRUE ~ "NA"
  )
}

ra_text_to_value <- function(x){
  case_when(
    x=="Major Cities of Australia" ~ 0,
    x=="Inner Regional Australia" ~ 1,
    x=="Outer Regional Australia" ~ 2,
    x=="Remote Australia" ~ 3,
    x=="Very Remote Australia" ~ 4,
  )
}

dropdown_width <- "100%"

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
        selectInput(
          inputId="sa_level",
          label="Statistical area level",
          choice=c("SA1", "SA2"),
          selected="SA1",
          width=dropdown_width
        ),
        dropdownButton2(
          label="Socioeconomic status", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="seifa", label="SEIFA", width=dropdown_width,
            choices=c(seifa_scale_to_text(1:5), NA),
            selected=c(seifa_scale_to_text(1:5), NA)
          )
        ),
        dropdownButton2(
          label="Remoteness index", status="default", width=dropdown_width,
          checkboxGroupInput(
            inputId="remoteness", label="Remoteness", width=dropdown_width,
            choices=c(ra_scale_to_text(0:4)),
            selected=c(ra_scale_to_text(0:4))
          )
        )
      )
    )
  )
)

server <- function(input, output, session){
  bins <- c(0, 30, 60, 120, 180, 240, 300, 360, 900)
  palBin <- colorBin("YlOrRd", domain = 0:900, bins=bins, na.color="transparent")
  
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
  
  polygons <- readRDS("../output/layers/stacked_SA1_and_SA2_polygons_year2016_simplified.rds")
  
  first_load <- TRUE
  proxy <- leafletProxy("main_map")
  
  observeEvent(list(input$seifa, input$remoteness, input$sa_level), {
    # resources:
      # https://stackoverflow.com/questions/58014620/remove-specific-layers-in-r-leaflet
      # https://stackoverflow.com/questions/62700258/leaflet-in-another-tab-not-updated-with-leafletproxy-before-visiting-tab
    
    desired_codes <- 
      polygons %>%
      filter(seifa_quintile %in% seifa_text_to_value(input$seifa),
             ra %in% ra_text_to_value(input$remoteness),
             SA_level == as.numeric(str_extract(input$sa_level, "[0-9]{1}"))) %>%
      pull(CODE)
    
    if(first_load){
      codes_to_remove <- c()
      codes_to_add <- desired_codes
      first_load <<- FALSE
    }else{
      codes_to_remove <- current_codes[!current_codes %in% desired_codes]
      codes_to_add <- desired_codes[!desired_codes %in% current_codes]
    }
    
    current_codes <<- desired_codes
    
    update_polygons <- 
      polygons %>%
      filter(CODE %in% codes_to_add)
    
    proxy %>%
      addPolygons(
        data=update_polygons,
        fillColor=~palBin(update_polygons$value_acute),
        color="black",
        fillOpacity=1,
        weight=1,
        popup=update_polygons$popup_acute,
        options=leafletOptions(pane="layers"),
        layerId=~CODE
      ) %>%
      removeShape(codes_to_remove)
  })
}

shinyApp(ui, server)

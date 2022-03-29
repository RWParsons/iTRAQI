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
        radioButtons(inputId="layer_selection", label="Layer", choices=c("None", "SA1 Acute", "SA2 Acute"), selected="None"),
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
  rvs <- reactiveValues(map=NULL)
  
  output$main_map <- renderLeaflet({
    map <- leaflet(options=leafletOptions(minZoom=5)) %>%
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
      ) %>% hideGroup(groupings$group_id)
    
    for(i in groupings$group_id){
      polygons_df <- polygons[polygons$group_id==i,]
      map <- map %>%
        addPolygons(
          data=polygons_df,
          fillColor=~palBin(polygons_df$value_acute),
          color="black",
          fillOpacity=1,
          weight=1,
          group=i,
          popup=polygons_df$popup_acute,
          options=leafletOptions(pane="layers")
        )
    }
    map
  })
  
  
  groupings <- expand.grid(
    seifa=c(1:5, NA),
    ra=0:4,
    sa=1:2
  )
  groupings$group_id <- as.character(1:nrow(groupings))
  
  polygons <- 
    readRDS("../output/layers/stacked_SA1_and_SA2_polygons_year2016_simplified.rds") %>% 
    left_join(., groupings, by=c("ra", "seifa_quintile"="seifa", "SA_level"="sa"))
  
  first_load <- TRUE
  proxy <- leafletProxy("main_map")
  
  observeEvent(list(input$seifa, input$remoteness, input$layer_selection), {
    sa_selected <- as.numeric(str_extract(input$layer_selection, "[0-9]{1}"))
    ra_selected <- ra_text_to_value(input$remoteness)
    seifa_selected <- seifa_text_to_value(input$seifa)
    
    if (input$layer_selection == "None") {
      proxy %>% hideGroup(groupings$group_id)
    } else {
      show_ids <- groupings %>%
        filter(sa==sa_selected,
               ra%in%ra_selected,
               seifa%in%seifa_selected) %>%
        pull(group_id)
      hide_ids <- groupings$group_id[!groupings$group_id %in% show_ids]
      proxy %>% 
        showGroup(show_ids) %>%
        hideGroup(hide_ids)
    }
  })
  
}

shinyApp(ui, server)

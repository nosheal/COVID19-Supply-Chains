#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(RJSONIO)
library(readxl)
library(widgetframe)
library(lattice)
library(DT)

#Load the Data

df_for_maps <- readRDS("Data/df_for_maps.rds")


# define colors

sc_color <- colorFactor(c("purple", "darkred"), domain = df_for_maps$purpose)


## Set icons and colors (one for each layer) and prepare for maps
df_for_maps <- df_for_maps %>% 
  mutate(icon = case_when(
    purpose == "Testing" ~ "fa-syringe", 
    purpose == "Surgical Masks" ~ "fa-head-side-mask"),
    map_col = case_when(
      purpose == "Testing" ~ "darkred", 
      purpose == "Surgical Masks" ~ "purple")
  ) 

### LOAD MAPPING PREREQS

#font pluging
fontawesomePlugin <- htmlDependency("fontawesome", "5.13.0",
                                    src = c(href = "https://use.fontawesome.com/releases/v5.13.0"),
                                    stylesheet = "css/all.css"
)
#add dependencies
addDependencies <- function(map) {
  map$dependencies <- c(map$dependencies, leafletDependencies$easyButton(),
                        leafletDependencies$fontawesome())
  
  map
}



# Function for creating circle markers given a data set
createCircleMarkersWithData <- function(map, dataSet, clusterId, group, fillColor, icon) {
  jsFuncStr = str_replace_all(str_interp("
    function(cluster) {
      var childCount = cluster.getChildCount();
      return new L.DivIcon({
                                               html: '<div style=\"background-color:${fillColor}; color: white\"><span>' + cluster.getChildCount() + '</div><span>',
                                               className: 'marker-cluster', iconSize: new L.Point(40, 40)
                                             });
    }", list(fillColor = fillColor)), "[\r\n]" , "")
  addAwesomeMarkers(map,
                    data = dataSet,
                    popup = dataSet$popup,
                    icon = awesomeIcons(
                      icon = icon,
                      library = "fa",
                      markerColor = fillColor,
                      iconColor = "#ffffff"
                    ),
                    clusterOptions = markerClusterOptions(
                      iconCreateFunction = JS(jsFuncStr)
                    ),
                    clusterId = clusterId,
                    layerId = dataSet$layerId)
}


registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

########

# Define UI for application that draws the mapp
ui <- fluidPage(

    navbarPage("COVID-19 Supply Chains", id="nav",
               
               tabPanel("Interactive map",
                        div(class="outer",
                            
                            tags$head(
                                # Include our custom CSS
                                includeCSS("https://raw.githubusercontent.com/rstudio/shiny-examples/master/063-superzip-example/styles.css"),
                                includeScript("https://raw.githubusercontent.com/rstudio/shiny-examples/master/063-superzip-example/gomap.js")
                            ),
                            
                            #leafletOutput
                            leafletOutput("mymap"),
                            #layercontrol
                            absolutePanel(bottom = 40, left = 10,
                                          checkboxGroupInput("purpose_select", "Select Purposes to Display", 
                                                             choices = unique(df_for_maps$purpose), selected = "All"
                                          )
                            )
                        )
               ),
               
               tabPanel("Data explorer",
                        hr(),
                        DT::dataTableOutput("ziptable")
               )
    )
    
    
)


# Define server logic required to draw the map
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    df_for_maps %>% filter(purpose %in% input$purpose_select)
  })
  

  output$mymap <- renderLeaflet({
      m <- leaflet() %>% 
        # add layers of maps (decided to provide three options)
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        registerPlugin(fontawesomePlugin) %>%
        # set the boundary of the map so that the user cannot zoom out of one world 
        # view of the map
        setMaxBounds(lng1 = 210,
                     lat1 = 89.45016124669523,
                     lng2 = -210,
                     lat2 = -87.71179927260242) %>%
        
        # add mini map on bottom right corner with collapse option
        addMiniMap(
          tiles = providers$Stamen.TonerLite,
          toggleDisplay = T
        ) %>%
        
        # add button that zooms out to zoom level 1 of the map (showing the entire world map)
        addEasyButton(easyButton(
          icon = "fa-globe",
          title = "Zoom to Level 1",
          onClick = JS("
                 function(btn, map) {
                 map.setZoom(1);
                 }")
        )
        ) %>% 
        
        #add layer control option for clusters and map type 
        #addLayersControl(
        #baseGroups = c("OSM(Default)", "Google", "Toner Lite", "NatGeoWorldMap"),
        #overlayGroups = c("Swabs", "Nonwoven Fabrics"),
        # collapsable table
        # options = layersControlOptions(collapsed = TRUE)
        #) %>% 
        # add legend (table) that shows which color represents which country of origin (color key) - bottom left (due to mini map)
        addLegend(position = c("bottomleft"),
                  values = filteredData()$purpose,
                  pal = sc_color,
                  title = "Purpose"
        ) 
      
      marker_list <- as.vector(unique(filteredData()$purpose)) ## define a list of individual purposes
      
      for (y in unique(filteredData()$purpose))
      {
        map_lay <- filteredData() %>% 
          filter(purpose == y)
        
        m <- m %>% createCircleMarkersWithData(dataSet = map_lay,
                                               fillColor = map_lay$map_col[1],
                                               icon = map_lay$icon,
                                               clusterId = y) 
        
      }
      
      m
      
      
    })
    
    output$ziptable <- DT::renderDataTable({
        df <- df_for_maps %>%
          mutate(Action = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
        
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

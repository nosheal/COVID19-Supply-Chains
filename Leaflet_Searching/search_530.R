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
library(shinyWidgets)
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(htmltools)
library(htmlwidgets)
library(RJSONIO)
library(readxl)
library(widgetframe)
library(lattice)
library(DT)
library(tidytext)
library(lubridate)
library(here)

#Load the Data

df_for_maps <- readRDS(here("Data/df_for_maps.rds"))

measure <- c("None", "Confirmed Cases", "Deaths")
demand <- as.data.frame(measure)
demand$demand_id <- c(0,1,2)

df_for_maps_words <- readRDS(here("Data/words.RDS"))

sc_color <- colorFactor(c("cadetblue", "darkblue", "blue", "purple", "green", "darkgreen"), domain = df_for_maps$specific_product)


## Set icons and colors (one for each layer) and prepare for maps
## Set icons and colors (one for each layer) and prepare for maps
df_for_maps <- df_for_maps %>% 
  mutate(icon = case_when(
    specific_product == "Face Masks Only" ~ "fa-head-side-mask", 
    specific_product == "Respirators Only" ~ "fa-head-side-mask",
    specific_product == "Respirators and Face Masks" ~ "fa-head-side-mask",
    specific_product == "Cloth Masks" ~ "fa-theater-masks",
    specific_product == "Non-Woven Fabric" ~ "fa-mitten",
    specific_product == "No Latex Elastic" ~ "fa-ring"),
    map_col = case_when(
      specific_product == "Face Masks Only" ~ "darkblue", 
      specific_product == "Respirators Only" ~ "darkgreen",
      specific_product == "Respirators and Face Masks" ~ "green",
      specific_product == "Cloth Masks" ~ "cadetblue",
      specific_product == "Non-Woven Fabric" ~ "purple",
      specific_product == "No Latex Elastic" ~ "blue")
  )

df_for_maps <- df_for_maps %>%
  mutate(useful_map = case_when(
    useful == "Affirmatively" ~ "Producing Standard Product for FDA Approved Hospital Grade Masks", 
    useful == "Possibly" ~ "Producing Products that Meet Technical Requirements of Hospital Grade Masks", 
    useful == "Unknown" ~ "Unknown", 
    is.na(useful) ~ "Unknown"
  )) %>% 
  filter(company != "Wurzburg", company != "Wurzburg, Inc.",
         company != "Cinder & Sky, Inc.") %>% 
  mutate(group = if_else(is.na(group), "Missing", group)) %>% 
  mutate(useful = if_else(is.na(useful), "Unknown", useful)) %>% 
  mutate(broad_loc = if_else(is.na(broad_loc), "Unknown", broad_loc))

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
                    layerId = dataSet$layerId, 
                    group = dataSet$group[1])
}


registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

#### Demand layers

# URL to access the JHU ESRI ArcGIS Online Layer
covid_esri_url = "https://services9.arcgis.com/6Hv9AANartyT7fJW/arcgis/rest/services/USCounties_cases_V1/FeatureServer/0"

# The popup content for all maps
the_popup = paste0("function(feature) {",
                   "x = feature.properties;",
                   "temp = {Countyname: x.Countyname, ST_Abbr: x.ST_Abbr, \ 
                                Confirmed: x.Confirmed, Deaths: x.Deaths, \
                                FatalityRa: x.FatalityRa.toFixed(1), url: x.url, \ 
                                DateChecke: x.DateChecke} ;",
                   "  return L.Util.template(",
                   "    \"<p><b>{Countyname} County, {ST_Abbr}</b></br>",
                   "     <small><em>as of {DateChecke}</em></small></p>",
                   "      <p>Confirmed cases: {Confirmed} ",
                   "       <br>Deaths: {Deaths}",
                   "       <br>Fatality Rate: {FatalityRa}",
                   "      </p>",
                   "    \",",
                   "    temp",
                   "  );",
                   "}"
)

########

# Define UI for application that draws the mapp
ui <- fluidPage(

    navbarPage("COVID-19 Supply Chains", id="nav",
               
               tabPanel("US Face Masks and Respirators - Thomasnet Suppliers",
                        div(class="outer",
                            
                            
                            #leafletOutput
                            leafletOutput("mymap"),
                            #layercontrol
                            fluidRow(
                              column(3, 
                                          checkboxGroupInput("end_select", "End Product", 
                                                             choices = unique(df_for_maps$specific_product[df_for_maps$broad_product == "End Product"]), selected = c("Respirators Only", "Respirators and Face Masks", "Face Masks Only")
                                          )
                              ),
                              column(3, 
                                      checkboxGroupInput("int_select", "Intermediary Product", 
                                                         choices = unique(df_for_maps$specific_product[df_for_maps$broad_product == "Intermediary Product"])
                                      )
                              ),
                              column(3, 
                                     checkboxGroupInput("sup_select", "Supplier Type", 
                                                        choices = unique(df_for_maps$broad_loc), selected = c("Primary Manufacturer", "Distributor", "Secondary Manufacturer")
                                     )
                                     
                              )
                            ),
                            fluidRow(
                              column(3,
                                          selectInput("demand_select", "Demand Layer", 
                                                             choices = unique(as.character(demand$measure))
                                          )
                              ),
                              column(3, 
                                     checkboxGroupInput("med_select", "Affirmatively Serving the Medical Market?", 
                                                        choices = unique(df_for_maps$medical_market), selected = c("Yes", "Unknown")
                                     )
                              ),
                              column(3, 
                                     checkboxGroupInput("fda_select", "Technical Specs", 
                                                        choices = c("Producing Standard Product for FDA Approved Hospital Grade Masks", "Producing Products that Meet Technical Requirements of Hospital Grade Masks", "Unknown"), selected = c("Producing Standard Product for FDA Approved Hospital Grade Masks", "Producing Products that Meet Technical Requirements of Hospital Grade Masks")
                                     )
                              )
                            ),
                            fluidRow(
                              column(3,
                                     sliderTextInput("date_select", "Date", 
                                                     choices = unique(df_for_maps$date), selected = ymd("2020-05-30")
                                     )
                              ),
                              column(3, 
                                     selectizeInput("product_select", "Search (not case sensitive)", 
                                                    choices = unique(df_for_maps_words$word), selected = NULL, multiple = TRUE),
                                     radioButtons("search_comb", "Combine Search Terms With...",
                                                  choices = c("and", "or"), selected = "or", inline = TRUE)
                                     ), 
                              column(3, 
                                     "Data from Thomasnet (5/29/20 - 07/13/20). Not all supplier locations are shown.")
                            )
                        )
               ),

               tabPanel("Testing",
                        div(class="outer",
                            
                            #leafletOutput
                            leafletOutput("elinamap"),
                            absolutePanel(bottom = 10, right = 10,
                                          "Map in progress."
                            )
                        )
               ),
               tabPanel("Confirmed Domestic Manufacturing Entities",
                        div(class="outer",
                            
                            #leafletOutput
                            leafletOutput("afonsomap"),
                            absolutePanel(bottom = 10, right = 10,
                                          "Map in progress."
                            )
                        )
               )
    )
    
    
)



# Define server logic required to draw the map
server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  # We deal with each supply "group" seperately and merge it back together into our final dataframe
  # There is likely a more efficient way to do this, and this section should be revised. 

  filteredDate <- reactive({
    df_for_maps %>% 
      filter(date == ymd(input$date_select))
  })
  
  filteredFDA <- reactive({
    filteredDate() %>% 
      filter(useful_map %in% input$fda_select)
  })
  
  filteredCovid <- reactive({
    filteredFDA() %>% 
      filter(medical_market %in% input$med_select)
  })
  
  filteredProduct <- reactive({
    
    end_data <- filteredCovid() %>% 
      filter(specific_product %in% input$end_select)
    int_data <- filteredCovid() %>% 
      filter(specific_product %in% input$int_select)
    
    bind_rows(end_data, int_data)
    
  })
  
  filteredSupply <- reactive({
    filteredProduct() %>% 
      filter(broad_loc %in% input$sup_select)
  })

  
  filteredDemand <- reactive({
    demand %>%  filter(measure %in% input$demand_select)
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
                  values = search_tab()$specific_product,
                  pal = sc_color,
                  title = "Supply Chain Layer"
        ) 
      
      for (y in unique(search_tab()$specific_product))
      {
        map_lay <- search_tab() %>% 
          filter(specific_product == y)
        

          m <- m %>% createCircleMarkersWithData(dataSet = map_lay,
                                                 fillColor = map_lay$map_col[1],
                                                 icon = map_lay$icon,
                                                 clusterId = y) 
          
        
        
      }
      
      
      g <- if (filteredDemand()$demand_id == 1){
        m %>%
          
          setView(lng=-100.34004, lat=37.2855, zoom=3) %>%
          
          addEsriFeatureLayer(covid_esri_url,
                              color = '#000000', 
                              weight = 0.25 , # thickness of the county outines
                              opacity = 0.65, # opacity of the county outlines
                              fillOpacity = 0.65,  # opacity of the county fill
                              highlightOptions = highlightOptions(weight=2, 
                                                                  fillOpacity=0.8, 
                                                                  bringToFront=TRUE,
                                                                  sendToBack=TRUE),
                              options = featureLayerOptions(
                                simplifyFactor = 0.5,
                                precision = 5,
                                style = JS("function(feature){
                          var frate = feature.properties.Confirmed;
                          if(frate <= 0){
                            return {fillColor: '#f0f9e8'};
                          } else if(frate < 100){
                            return {fillColor: '#bae4bc'};
                          } else if(frate < 500){
                            return {fillColor: '#7bccc4'};
                          } else if(frate < 1000){
                            return {fillColor: '#43a2ca'};
                          } else {
                            return {fillColor: '#0868ac'};
                          }
                        }")),
                              
                              popupProperty = JS(the_popup)) %>%
          
          addLegend(title="Confirmed Cases", 
                    colors=c('#0868ac','#43a2ca','#7bccc4','#bae4bc','#f0f9e8'),
                    labels=c("1000+","500-1000", "500-100", "1-100", "0"), 
                    opacity=0.8, position="bottomleft")
      } else if (filteredDemand()$demand_id == 2){
        m %>%
          
          setView(lng=-100.34004, lat=37.2855, zoom=4) %>%
          
          addEsriFeatureLayer(covid_esri_url,
                              color = '#000000', 
                              weight = 0.25 , # thickness of the county outines
                              opacity = 0.65, # opacity of the county outlines
                              fillOpacity = 0.65,  # opacity of the county fill
                              highlightOptions = highlightOptions(weight=2, 
                                                                  fillOpacity=0.8, 
                                                                  bringToFront=TRUE,
                                                                  sendToBack=TRUE),
                              options = featureLayerOptions(
                                simplifyFactor = 0.5,
                                precision = 5,
                                style = JS("function(feature){
                          var frate = feature.properties.Deaths;
                          if(frate <= 0){
                            return {fillColor: '#f7f7f7'};
                          } else if(frate < 100){
                            return {fillColor: '#cccccc'};
                          } else if(frate < 500){
                            return {fillColor: '#969696'};
                          } else if(frate < 1000){
                            return {fillColor: '#636363'};
                          } else {
                            return {fillColor: '#252525'};
                          }
                        }")),
                              
                              popupProperty = JS(the_popup)) %>%
          
          addLegend(title="Deaths", 
                    colors=c('#252525','#636363','#969696','#cccccc','#f7f7f7'),
                    labels=c("1000+","500-1000", "500-100", "1-100", "0"), 
                    opacity=0.8, position="bottomleft")
        
      } else {
        m
      }
      
      g
      
      
    })
  
  output$elinamap <- renderLeaflet({
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
                values = filteredSupply()$specific_product,
                pal = sc_color,
                title = "Supply Chain Layer"
      )
  })
  
  output$afonsomap <- renderLeaflet({
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
                values = filteredSupply()$specific_product,
                pal = sc_color,
                title = "Supply Chain Layer"
      )
  })
  
  updateSelectizeInput(session, 'product_select',
                       choices = unique(df_for_maps_words$word),
                       server = TRUE
  )
  
    
    search_tab <- reactive({
     
      search_results <- as.vector(input$product_select)
      
      if (length(search_results > 0)){
        if(input$search_comb == "or"){
          filteredSupply() %>% 
            mutate(desc = tolower(desc)) %>% 
            filter(str_detect(desc, paste(search_results, sep = "|")))
        } else if (input$search_comb == "and") {
          filteredSupply() %>% 
            mutate(desc = tolower(desc)) %>% 
            filter(apply(sapply(search_results, grepl, desc), 1, all))
        }
        }
      else {
        filteredSupply()
        }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

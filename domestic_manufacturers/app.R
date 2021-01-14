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

df_for_maps <- readRDS(here("conf_maps_geocoded.RDS"))

df_for_maps_tests <- readRDS(here("tests_geocoded.RDS")) %>% 
  filter(!specific_product %in% c("Vials", "Pipettes", "Pipette Tips", "Vortex Mixers", "Microcentrifuges", "Glass Vials")) %>% 
  filter(!company %in% c("AJ Organica Pvt. Ltd.", "CellGenix USA", "ProImmune Ltd.", "Oxford Gene Technology"))

df_for_maps_other<- readRDS(here("tests_geocoded.RDS")) %>% 
  filter(specific_product %in% c("Vials", "Pipettes", "Pipette Tips", "Vortex Mixers", "Microcentrifuges", "Glass Vials")) %>%
  filter(!company %in% c("SGD Pharma", "ExtraGene,Inc."))

df_for_maps_sd <- readRDS(here("sd_geocoded.RDS")) %>% 
  filter(!company %in% c("AJ Organica Pvt. Ltd.", "CellGenix USA", "ProImmune Ltd.", "Oxford Gene Technology"))

measure <- c("None", "Confirmed Cases", "Deaths")
demand <- as.data.frame(measure)
demand$demand_id <- c(0,1,2)


df_for_maps <- df_for_maps %>% 
  mutate(pos = case_when(
    specific_product == "Respirators and Face Masks" ~ 1, 
    specific_product == "Face Masks Only" ~ 2, 
    specific_product == "Respirators Only" ~ 3, 
    specific_product == "Non-Woven Fabric" ~ 5, 
    specific_product == "No Latex Elastic" ~ 6,
    specific_product == "Cloth Masks" ~ 4
  ), 
  specific_product = reorder(specific_product, pos))

df_for_maps_tests <- df_for_maps_tests %>%
  filter(specific_product != "Plastics Contract Manufacturing") %>% 
  mutate(pos = case_when(
    specific_product == "Covid-19 Test Kits" ~ 1, 
    specific_product == "Other Test Kits" ~ 2, 
    specific_product == "Oropharyngeal Swabs" ~ 4, 
    specific_product == "Nasopharyngeal Swabs" ~ 5, 
    specific_product == "RNA Extraction Machine" ~ 3,
    specific_product == "Nucleotides" ~ 6,
    specific_product == "Reagents (All)" ~ 8, 
    specific_product == "Viral Transport Medium (VTM)" ~ 7 
  ),
  specific_product = reorder(specific_product, pos))

df_for_maps_other <- df_for_maps_other %>% 
  mutate(pos = case_when(
    specific_product == "Vials" ~ 1, 
    specific_product == "Pipettes" ~ 2, 
    specific_product == "Pipette Tips" ~ 3, 
    specific_product == "Vortex Mixers" ~ 4, 
    specific_product == "Microcentrifuges" ~ 5, 
    specific_product == "Glass Vials" ~ 6
  )) %>% 
  mutate(specific_product = fct_reorder(specific_product, pos))

df_for_maps_sd <- df_for_maps_sd %>% 
  mutate(pos = case_when(
    product == "Saliva Direct Lab" ~ 1, 
    product == "Detection" ~ 2, 
    product == "Probes" ~ 3, 
    product == "Primers" ~ 4, 
    product == "Master Mix" ~ 5
    )) %>% 
  mutate(product = fct_reorder(product, pos))

# define colors

sc_color <- colorFactor(c("cadetblue", "red", "darkblue", "green", "orange"), domain = df_for_maps$specific_product)

sc_color_test <- colorFactor(c("green", "orange", "blue", "darkred", "purple", "red", "cadetblue", "darkgreen"), domain = df_for_maps_tests$specific_product)

sc_color_other <- colorFactor(c("orange", "green", "purple", "blue", "darkred", "red", "cadetblue"), domain = df_for_maps_other$specific_product)

sc_color_sd <- colorFactor(c("green", "cadetblue", "purple", "darkblue", "orange"), domain = df_for_maps_sd$product)

## Set icons and colors (one for each layer) and prepare for maps

df_for_maps_sd <- df_for_maps_sd %>% 
  mutate(icon = case_when(
    product == "Saliva Direct Lab" ~ "fa-poll", 
    product == "Detection" ~ "fa-flask", 
    product == "Probes" ~ "fa-syringe", 
    product == "Primers" ~ "fa_vials", 
    product == "Master Mix" ~ "fa_tape"
  ), 
  map_col = case_when(
    product == "Saliva Direct Lab" ~ "green", 
    product == "Detection" ~ "cadetblue", 
    product == "Probes" ~ "purple", 
    product == "Primers" ~ "darkblue", 
    product == "Master Mix" ~ "orange"
  ))

df_for_maps_tests <- df_for_maps_tests %>% 
  mutate(icon = case_when(
    specific_product == "Covid-19 Test Kits" ~ "fa-vials", 
    specific_product == "Other Test Kits" ~ "fa-poll",
    specific_product == "Oropharyngeal Swabs" ~ "fa-tape",
    specific_product == "Nasopharyngeal Swabs" ~ "fa-tape",
    specific_product == "RNA Extraction Machine" ~ "fa-syringe",
    specific_product == "Nucleotides" ~ "fa-dna",
    specific_product == "Reagents (All)" ~ "fa-capsules",
    specific_product == "Viral Transport Medium (VTM)" ~ "fa-vial"),
    map_col = case_when(
      specific_product == "Covid-19 Test Kits" ~ "green", 
      specific_product == "Other Test Kits" ~ "orange",
      specific_product == "Oropharyngeal Swabs" ~ "darkred",
      specific_product == "Nasopharyngeal Swabs" ~ "purple",
      specific_product == "RNA Extraction Machine" ~ "blue",
      specific_product == "Nucleotides" ~ "red",
      specific_product == "Reagents (All)" ~ "darkgreen",
      specific_product == "Viral Transport Medium (VTM)" ~ "cadetblue")
  )

df_for_maps_other <- df_for_maps_other %>% 
  mutate(icon = case_when(
    specific_product == "Vials" ~ "fa-vials", 
    specific_product == "Pipettes" ~ "fa-syringe",
    specific_product == "Pipette Tips" ~ "fa-syringe",
    specific_product == "Vortex Mixers" ~ "fa-tape",
    specific_product == "Microcentrifuges" ~ "fa-tape",
    specific_product == "Glass Vials" ~ "fa-vials"),
    map_col = case_when(
      specific_product == "Vials" ~ "orange", 
      specific_product == "Pipettes" ~ "green",
      specific_product == "Pipette Tips" ~ "purple",
      specific_product == "Vortex Mixers" ~ "blue",
      specific_product == "Microcentrifuges" ~ "darkred",
      specific_product == "Glass Vials" ~ "cadetblue")
  )

df_for_maps <- df_for_maps %>% 
  mutate(icon = case_when(
    specific_product == "Face Masks Only" ~ "fa-head-side-mask", 
    specific_product == "Respirators Only" ~ "fa-head-side-mask",
    specific_product == "Respirators and Face Masks" ~ "fa-head-side-mask",
    specific_product == "Cloth Masks" ~ "fa-theater-masks",
    specific_product == "Non-Woven Fabric" ~ "fa-mitten",
    specific_product == "No Latex Elastic" ~ "fa-ring"),
    map_col = case_when(
      specific_product == "Respirators and Face Masks" ~ "cadetblue", 
      specific_product == "Respirators Only" ~ "darkblue",
      specific_product == "Face Masks Only" ~ "red",
      specific_product == "Non-Woven Fabric" ~ "green",
      specific_product == "No Latex Elastic" ~ "orange")
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

# A function that takes a plugin htmlDependency object and adds
# it to the map. This ensures that however or whenever the map
# gets rendered, the plugin will be loaded into the browser.
registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
}

# Function for creating circle markers given a data set
createCircleMarkersWithData <- function(map, dataSet, clusterId, fillColor, icon) {
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

########

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    navbarPage("US Covid-19 Medical Supplies - Thomasnet Suppliers", id="nav",
               
               tabPanel("Confirmed Domestic Manufacturers of Masks/Respirators",
                        div(class="outer",
                            
                            
                            #leafletOutput
                            leafletOutput("mymap"),
                            #layercontrol
                            fluidRow(
                                column(3, 
                                       checkboxGroupInput("end_select", "End Product", 
                                                          choices = c("Respirators and Face Masks", "Face Masks Only", "Respirators Only"), selected = c("Respirators Only", "Respirators and Face Masks", "Face Masks Only")
                                       )
                                ),
                                column(3, 
                                       checkboxGroupInput("int_select", "Intermediary Product", 
                                                          choices = c("Non-Woven Fabric", "No Latex Elastic"), selected = c("Non-Woven Fabric", "No Latex Elastic"))
                                       ),
                                column(3, 
                                       checkboxGroupInput("sup_select", "Plant or HQ", 
                                                          choices = c("Manufacturing Location and HQ", "Manufacturing Branch", "Manufacturing Location (UC)", "HQ Only", "Branch or Subsidiary", "Unknown"), selected = c("Manufacturing Location and HQ", "Manufacturing Branch", "Manufacturing Location (UC)"))
                                       )
                                       
                                ),
                            fluidRow(
                                column(3,
                                       selectInput("demand_select", "Demand Layer", 
                                                   choices = unique(as.character(demand$measure))
                                       )
                                ),
                                column(3, 
                                       selectizeInput("company_select", "Search", 
                                                      choices = unique(df_for_maps$company), selected = NULL, multiple = TRUE)
                                ), 
                                column(3, 
                                       "Products are standard FDA approved Masks/Respirators. Data from Thomasnet (5/29/20 - 01/11/2021). ")
                            )
                        )
                            
               ),
               tabPanel("Potential Manufacturing Locations for Testing Supply Chain",
                        div(class="outer",
                            
                            
                            #leafletOutput
                            leafletOutput("mymap_tests"),
                            #layercontrol
                            fluidRow(
                              column(3, 
                                     checkboxGroupInput("end_select_tests", "End Product", 
                                                        choices = c("Covid-19 Test Kits", "Other Test Kits"), selected = c("Covid-19 Test Kits", "Other Test Kits")
                                     )
                              ),
                              column(3, 
                                     checkboxGroupInput("int_select_tests", "Intermediary Product", 
                                                        choices = c("RNA Extraction Machine", "Oropharyngeal Swabs", "Nasopharyngeal Swabs", "Nucleotides", "Viral Transport Medium (VTM)", "Reagents (All)"), selected = c("RNA Extraction Machine", "Oropharyngeal Swabs", "Nasopharyngeal Swabs", "Nucleotides", "Viral Transport Medium (VTM)"))
                              ),
                              column(3, 
                                     checkboxGroupInput("sd_select_tests", "Potential Product for Saliva Direct Test",
                                                        choices = c("All", "Primers", "Probes", "Master Mixer", "Detection"), selected = "All")
                                     )
                            ),
                            fluidRow(
                              column(3,
                                     selectInput("demand_select", "Demand Layer", 
                                                 choices = unique(as.character(demand$measure))
                                     )
                              ),
                              column(3, 
                                     "Data from Thomasnet (01/07/2021 - 01/11/2021). Note: Not all locaitons are mapped, and the accuracy of mapped locations and their product offerings has not been checked.")
                            )
                        )
                        
               ),
               tabPanel("Potential Manufacturing Locations for Testing Supply Chain - Other Products",
                        div(class="outer",
                            
                            
                            #leafletOutput
                            leafletOutput("mymap_other"),
                            #layercontrol
                            fluidRow(
                              column(3, 
                                     checkboxGroupInput("int_select_other", "Intermediary Product", 
                                                        choices = c("Vials", "Pipettes", "Pipette Tips", "Vortex Mixers", "Microcentrifuges", "Glass Vials"), selected = c("Vials", "Pipettes", "Pipette Tips", "Vortex Mixers", "Microcentrifuges", "Glass Vials"))
                              )
                            ),
                            fluidRow(
                              column(3,
                                     selectInput("demand_select", "Demand Layer", 
                                                 choices = unique(as.character(demand$measure))
                                     )
                              ),
                              column(3, 
                                     "Data from Thomasnet (01/07/2021 - 01/11/2021). Note: Not all locaitons are mapped, and the accuracy of mapped locations and their product offerings has not been checked.")
                            )
                        )
                        
               ),
               tabPanel("Saliva Direct Lab Locations and Potential Manufacturing Locations of Intermediary Inputs",
                        div(class="outer",
                            
                            
                            #leafletOutput
                            leafletOutput("mymap_sd"),
                            #layercontrol
                            fluidRow(
                              column(3, 
                                     checkboxGroupInput("sd_select", "Saliva Direct Labs and Potential Intermediary Products", 
                                                        choices = c("Saliva Direct Lab", "Detection", "Primers", "Probes", "Master Mix"), selected = c("Saliva Direct Lab", "Detection", "Primers", "Probes", "Master Mix")
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
                                     "Data from Thomasnet (01/07/2021 - 01/11/2021). Note: Not all locations are mapped, and the accuracy of mapped locations of intermediary products and the relevancy of those products has not been checked.")
                            )
                        )
                        
               )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
   filteredSD <- reactive({
     df_for_maps_sd %>% 
       filter(product %in% input$sd_select)
   })

   filteredOther <- reactive({
    
      df_for_maps_other %>% 
        filter(specific_product %in% input$int_select_other) 
    
   })
  
    filteredTests <- reactive({
      
      end_data <- df_for_maps_tests %>% 
        filter(specific_product %in% input$end_select_tests)
      int_data <- df_for_maps_tests %>% 
        filter(specific_product %in% input$int_select_tests)
      
      bind_rows(end_data, int_data)
      
    })
    
    filteredManfHq <- reactive({
        df_for_maps %>% 
            filter(manf_hq %in% input$sup_select)
    })
    
    filteredSupply <- reactive({
        
        end_data <- filteredManfHq() %>% 
            filter(specific_product %in% input$end_select)
        int_data <- filteredManfHq() %>% 
            filter(specific_product %in% input$int_select)
        
        bind_rows(end_data, int_data)
        
    })
    
    filteredDemand <- reactive({
      demand %>%  filter(measure %in% input$demand_select)
    })
    
    updateSelectizeInput(session, 'company_select',
                         choices = unique(df_for_maps$company),
                         server = TRUE
    )
    
    
    search_tab <- reactive({
      
      search_results <- as.vector(input$company_select)
      
      if (length(search_results > 0)){
        filteredSupply() %>% 
          filter(apply(sapply(search_results, grepl, company), 1, all))
      }
      else {
        filteredSupply()
      }
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
                      values = filteredSupply()$specific_product,
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
    
    output$mymap_tests <- renderLeaflet({
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
                  values = filteredTests()$specific_product,
                  pal = sc_color_test,
                  title = "Supply Chain Layer"
        ) 
      
      
      for (y in unique(filteredTests()$specific_product))
      {
        map_lay <- filteredTests() %>% 
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
    
    output$mymap_sd <- renderLeaflet({
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
                  values = filteredSD()$product,
                  pal = sc_color_sd,
                  title = "Supply Chain Layer"
        ) 
      
      
      for (y in unique(filteredSD()$product))
      {
        map_lay <- filteredSD() %>% 
          filter(product == y)
        
        
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
        
    output$mymap_other <- renderLeaflet({
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
                  values = filteredOther()$specific_product,
                  pal = sc_color_other,
                  title = "Supply Chain Layer"
        ) 
      
      
      for (y in unique(filteredOther()$specific_product))
      {
        map_lay <- filteredOther() %>% 
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
    
   
}

# Run the application 
shinyApp(ui = ui, server = server)

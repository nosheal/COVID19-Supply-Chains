
library(tidyverse)
library(ggmap)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(RJSONIO)
library(readxl)
library(widgetframe)
library(leaflet.esri)
library(lattice)
library(RColorBrewer)
library(here)

#Load the Data

df_for_maps <- readRDS(here("Data/Cleaned/df_for_maps.rds"))

df_for_maps_words <- df_for_maps %>% 
  unnest_tokens(word, desc)

stop_words <- stop_words %>% 
  filter(!word %in% c("beyond", "changes",  "contains", "containing", "contain", "nearly", "only", "novel", "plus", "possible", "particular", "particularly", "provides", "probably")) %>% 
  filter(lexicon == "SMART")

tidy_maps <- df_for_maps_words %>% 
  anti_join(stop_words) %>% 
  filter(word != "19") %>% 
  mutate(word = str_replace(word, "covid", "covid-19"))

# define colors

sc_color <- colorFactor(c("red", "orange", "green", "cadetblue", "darkblue"), domain = df_for_maps$specific_product)


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
      specific_product == "Respirators and Face Masks" ~ "cadetblue", 
      specific_product == "Respirators Only" ~ "darkblue",
      specific_product == "Face Masks Only" ~ "red",
      specific_product == "Non-Woven Fabric" ~ "green",
      specific_product == "No Latex Elastic" ~ "orange")
  )

df_for_maps <- df_for_maps %>% 
  filter(company != "Wurzburg", company != "Wurzburg, Inc.") %>% 
  mutate(group = if_else(is.na(group), "Missing", group))

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
            values = df_for_maps$specific_product,
            pal = sc_color,
            title = "Purpose"
  ) %>% 
  addLayersControl(
    overlayGroups = as.vector(unique(df_for_maps$group)),
    options = layersControlOptions(collapsed = FALSE)
  )




######################### Now iterate over all unique purposes

marker_list <- as.vector(unique(df_for_maps$specific_product)) ## define a list of individual purposes

for (y in unique(df_for_maps$specific_product))
{4
  map_lay <- df_for_maps %>% 
    filter(specific_product == y)
  
  for (z in unique(df_for_maps$group)){
    m <- m %>% createCircleMarkersWithData(dataSet = map_lay,
                                           fillColor = map_lay$map_col[1],
                                           icon = map_lay$icon,
                                           clusterId = y) 
    
  }
  

  
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

g <- if (demand$demand_id == 1){
  m %>%
  
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
            opacity=0.8, position="bottomright")
} else if (demand$demand_id == 2){
  m %>%
    
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
              opacity=0.8, position="bottomright")
  
} else {
  m
}

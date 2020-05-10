
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

#Load the Data
df_for_maps <- readRDS("Data//df_for_maps.rds")
us_swabs <- readRDS("Data//us_swabs.rds")
global_nonwoven <- readRDS("Data//global_nonwoven.rds")

### LOAD MAPPING PREREQS

#grouped layer control plugin
groupedLayerControlPlugin <- htmlDependency("leaflet-groupedlayercontrol", "0.61",
                                            src = c(href = "http://ismyrnow.github.io/leaflet-groupedlayercontrol/src/"),
                                            script = "leaflet.groupedlayercontrol.js",
                                            stylesheet = "leaflet.groupedlayercontrol.css"
)

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
                    group = group)
}

# define colors

sc_color <- colorFactor(c("purple", "darkred"), domain = df_for_maps$purpose)

########

m <- leaflet(data = df_for_maps) %>% 
  # add layers of maps (decided to provide three options)
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  registerPlugin(groupedLayerControlPlugin) %>%
  registerPlugin(fontawesomePlugin) %>%
  
  onRender("function(el, x, data) {
        var baseLayers = {
            'Toner Lite': this.layerManager.getLayerGroup('Toner Lite'),
        };

        var groupedOverlays = {
            'Testing': {
                'Swabs': this.layerManager.getLayerGroup('Swabs'),
            },
            'Masks': {
                'Nonwoven Fabrics': this.layerManager.getLayerGroup('Nonwoven Fabric'),
            }
        };
        
        var Options = {
          groupCheckboxes: true
        };

        console.log(L.control.groupedLayers);
        L.control.groupedLayers(baseLayers, groupedOverlays, Options).addTo(this);
    }", data = df_for_maps) %>%
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
  
  #1. Swabs
  createCircleMarkersWithData(dataSet = us_swabs,
                              fillColor = "darkred",
                              icon = "fa-syringe",
                              clusterId = "domSwabs",
                              group = "Swabs") %>%
  #2. Nonwoven Fabrics
  createCircleMarkersWithData(dataSet = global_nonwoven,
                              fillColor = "purple",
                              icon = "fa-head-side-mask",
                              clusterId = "glblNonWoven",
                              group = "Nonwoven Fabric") %>%
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
            values = df_for_maps$purpose,
            pal = sc_color,
            title = "Purpose"
  ) 
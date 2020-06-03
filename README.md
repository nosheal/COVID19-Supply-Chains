# COVID19-Supply-Chains
Mapping COVID-19 Supply Chains

This repository is designed to use R leaflet and shiny functionalities to build an interactive mapping tool to visualize different layers of the COVID-19 supply chain for three key products: 1) face masks and respirators; 2) ventilators; 3) tests 

There are a few key files: 

* Cleaning and preliminary analysis of the data (which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/tidy_thomas.Rmd))
* Preparing data for mapping, which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/prepare_for_maps.Rmd)
* Mapping independent of shiny integration (which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/Demand.R))
* Shiny integration and the live app (the code can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/Leaflet_Searching/search_530.R) and the live version can be found [here](https://nosheal.shinyapps.io/COVID19_SUPPLY_CHAINS/))

  
Demand layers are drawn from previously exisitng ESRI integrations. Two resources to bring in ESRI layers from can be found at [here](https://coronavirus-disasterresponse.hub.arcgis.com/#get-data) and [here](https://dlab.berkeley.edu/blog/data-and-tools-mapping-covid-19)
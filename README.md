# COVID19-Supply-Chains
Mapping COVID-19 Supply Chains

This repository is designed to use R leaflet and shiny functionalities to build an interactive mapping tool to visualize different layers of the COVID-19 supply chain. 

There are a few key files: 

* Cleaning and preprocessing of the data (which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/clean_process.Rmd))
* Mapping independent of shiny integration (which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/Mapping.R))
* Mapping for shiny integration, but outside of the shiny environment (which can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/Mapping_for_shiny.R)) 
  * Note the difference between mapping for shiny integration and mapping independently is that the independent maps rely on the javascript grouped layer control plug in. Mapping for shiny integration solves the layering issue by calling layers from one dataframe that is dynamically filtered based on checkboxgroupinputs. 
* Shiny integration and the live app (the code can be found [here](https://github.com/nosheal/COVID19-Supply-Chains/blob/master/Leaflet_Searching/search_v.R) and the live version can be found [here](https://nosheal.shinyapps.io/COVID19_SUPPLY_CHAINS/))
  * Shiny integration allows for a map tab as well as a data explorer tab, and will be modified to also include graphing and other analytical outputs. 
  
Demand layers are drawn from previously exisitng ESRI integrations. Two resources to bring in ESRI layers from can be found at [here](https://coronavirus-disasterresponse.hub.arcgis.com/#get-data) and [here](https://dlab.berkeley.edu/blog/data-and-tools-mapping-covid-19)
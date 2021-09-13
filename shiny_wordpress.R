require(rgdal)
require(ggplot2)
library(devtools)
# install_github("edzer/sfr")
library(sf)
library(tidyverse)
library(shiny)
require(leaflet)
require(ggplot2)
require(data.table)
library(rgdal)
library(dplyr)
require(plotly)
require(shinythemes)
library(viridis)
library(hrbrthemes)
library(RColorBrewer)


unzip(zipfile = "camp20grounds20and20parks.zip", 
      exdir = 'Camp Grounds and Parks')

shp <- readOGR(dsn = "Camp Grounds and Parks/Camp Grounds and Parks", layer = "Campgrounds")

#Converting to data frame
shapefile_df <- data.frame(shp)

# keeping only national parks
national_df = shapefile_df%>%
  filter(str_detect(PARK_NAME, "National"))


# removing NA
national_df = national_df %>% drop_na()

# national_df

library(leaflet)
library(maps)



newdat <- national_df[,c(3,10,11)]


mm <- leaflet(data = newdat) %>% 
  #using a provider tile terrain from stamen maps
  addProviderTiles("Stamen.Terrain") %>%
  setView(144.2794, -36.7570, zoom = 6) %>%   
  #hovering on popups shows site names
  addMarkers(~coords.x1, ~coords.x2, label = ~as.character(SITE_NAME),
             clusterOptions = markerClusterOptions(),
             labelOptions = labelOptions(noHide = FALSE, direction = "left",textsize='8px',
                                         style=list(
                                           'color'= "black",
                                           'font-family'= 'serif',
                                           'font-style'= 'italic',
                                           'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                           'font-size' = '9px',
                                           'border-color' = 'rgba(0,0,0,0.5)'
                                         ))
  )





mm

require(rgdal)
require(ggplot2)
library(devtools)
# install_github("edzer/sfr")
library(sf)
library(tidyverse)
library(leaflet)
library(maps)
library(dplyr)
library(data.table)


unzip(zipfile = "Vicmap Features of Interest.zip", 
      exdir = 'Vicmap Features of Interest')

shp <- readOGR(dsn = "Vicmap Features of Interest/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer", layer = "foi_index_centroid")

#Converting to data frame
shapefile_df <- data.frame(shp)



esite<-shapefile_df %>% filter((FTYPE == "emergency facility") & (PARENTNAME %like% "NATIONAL PARK"))



anglerIcon3 <- makeIcon(
  iconUrl = "https://www.flaticon.com/svg/vstatic/svg/1061/1061536.svg?token=exp=1618179936~hmac=1e9b8a9af053682b7f4e074fc36434c4",
  iconWidth = 20, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 94,
  
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)

newdat <- esite


mm3 <- leaflet(data = newdat) %>% 
  #using a provider tile terrain from stamen maps
  addProviderTiles("Esri.WorldImagery") %>%
  #hovering on popups shows site names
  addMarkers(~X_COORD, ~Y_COORD, label = ~NAME_LABEL,icon = anglerIcon3,
             popup = paste("<b>Facilty type:</b>", newdat$FEATSUBTYP, "<br>",
                           "<b>Name:</b>", newdat$NAME_LABEL, "<br>",
                           "<b>Located at:</b>", tolower(newdat$PARENTNAME),"<br>"),
             labelOptions = labelOptions(noHide = FALSE, direction = "left",textsize='14px',
                                         style=list(
                                           'color'= "black",
                                           'font-family'= 'serif',
                                           'font-style'= 'italic',
                                           'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                           'font-size' = '14px',
                                           'border-color' = 'rgba(0,0,0,0.5)'
                                         ))
             )

mm3
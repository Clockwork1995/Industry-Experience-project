require(rgdal)
require(ggplot2)
library(devtools)
# install_github("edzer/sfr")
library(sf)
library(tidyverse)
library(readxl)
library(sf)
library(fs)
library(magrittr)
library(leaflet)
library(leaflet.extras)

unzip(zipfile = "SDM835846.zip", 
      exdir = 'PARKRES')
parkres_shp <- readOGR(dsn = "PARKRES/ll_gda94/sde_shape/whole/VIC/CROWNLAND/layer", layer = "parkres")


# keeping only national parks

national_shp = subset(parkres_shp, str_detect(NAME, "National"))





## toilet data
toilets <- read.csv(file = 'toiletmapexport_210301_074428.csv')
VIC_toilets = filter(toilets, State == "VIC")
VIC_toilets_parks_reserve = filter(VIC_toilets, FacilityType == "Park or reserve")
VIC_toilets_parks_reserve$Name = as.character(VIC_toilets_parks_reserve$Name)
VIC_toilets_parks_reserve$Address1 = as.character(VIC_toilets_parks_reserve$Address1)
VIC_toilets_parks_reserve$Ambulant = as.character(VIC_toilets_parks_reserve$Ambulant)
VIC_toilets_parks_reserve$Accessible = as.character(VIC_toilets_parks_reserve$Accessible)
VIC_toilets_parks_reserve$ToiletNote = as.character(VIC_toilets_parks_reserve$ToiletNote)

#hospital data
hospital = readOGR(dsn = "hospital", layer = "doc-point")
hospital = data.frame(hospital)
hospital$Name = as.character(hospital$Name)
hospital$Address = as.character(hospital$Address)

anglerIcon1 <- makeIcon(
  iconUrl = "https://www.flaticon.com/svg/vstatic/svg/4433/4433047.svg?token=exp=1617229155~hmac=c5d16a1f20784de52d4d096619d2a681",
  iconWidth = 12, iconHeight = 12,
  iconAnchorX = 22, iconAnchorY = 94,
  
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)



anglerIcon2 <- makeIcon(
  iconUrl = "https://as2.ftcdn.net/jpg/00/96/48/11/500_F_96481179_ANEpnLLHZZxtIezAh5k3tTKHO3VaFqjF.jpg",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 22, iconAnchorY = 94,
  
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)




pal <- colorFactor(
  palette = palette(rainbow(63)),
  domain = national_shp$NAME
)

leaflet() %>%
  addProviderTiles("Stamen.Toner") %>%
  setView(144.2794, -36.7570, zoom = 7) %>%
  addPolygons(data = national_shp,color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(NAME),
              highlightOptions = highlightOptions(color = "#666", weight = 2,
                                                  bringToFront = TRUE),
              label = national_shp$NAME,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))%>%
  addMarkers(data = VIC_toilets_parks_reserve,~Longitude, ~Latitude, label = ~Name,
             icon=anglerIcon1,
             popup = paste("<b>Toilet name:</b>", VIC_toilets_parks_reserve$Name, "<br>",
                           "<b>Address:</b>", VIC_toilets_parks_reserve$Address1, "<br>",
                           "<b>Ambulant:</b>", VIC_toilets_parks_reserve$Ambulant, "<br>",
                           "<b>Accessible:</b>", VIC_toilets_parks_reserve$Accessible,"<br>",
                           "<b>Other info:</b>", VIC_toilets_parks_reserve$ToiletNote),
             labelOptions = labelOptions(noHide = FALSE, direction = "left",textsize='8px',
                                         style=list(
                                           'color'= "black",
                                           'font-family'= 'serif',
                                           'font-style'= 'italic',
                                           'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                           'font-size' = '9px',
                                           'border-color' = 'rgba(0,0,0,0.5)'
                                         ))
  )%>%
  
  addMarkers(data = hospital,~coords.x1, ~coords.x2, label = ~Name,icon = anglerIcon2,
             popup = paste("<b>Hospital name:</b>", hospital$Name, "<br>",
                           "<b>Hospital address:</b>", hospital$Address),
             #              clusterOptions = markerClusterOptions(),
             labelOptions = labelOptions(noHide = FALSE, direction = "left",textsize='8px',
                                         style=list(
                                           'color'= "black",
                                           'font-family'= 'serif',
                                           'font-style'= 'italic',
                                           'box-shadow' = '3px 3px rgba(0,0,0,0.25)',
                                           'font-size' = '15px',
                                           'border-color' = 'rgba(0,0,0,0.5)'
                                         ))
             
  )




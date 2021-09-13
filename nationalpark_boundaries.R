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

##parkres shafile

unzip(zipfile = "SDM835846.zip", 
      exdir = 'PARKRES')

parkres_shp <- readOGR(dsn = "PARKRES/ll_gda94/sde_shape/whole/VIC/CROWNLAND/layer", layer = "parkres")

parkres_shapefile_df <- data.frame(parkres_shp)

# keeping only national parks
parkres_national_df = parkres_shapefile_df%>%
  filter(str_detect(NAME, "National"))
parkres_national_df

# keeping only national parks

national_shp = subset(parkres_shp, str_detect(NAME, "National"))

head(national_shp)


pal <- colorFactor(
  palette = palette(rainbow(63)),
  domain = national_shp$NAME
)

labels <- national_shp$NAME


leaflet(national_shp) %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~pal(NAME),
              highlightOptions = highlightOptions(color = "#666", weight = 2,
                                                  bringToFront = TRUE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) 

# required libraries are loaded
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



#Emergency facility data
unzip(zipfile = "Vicmap Features of Interest.zip", 
      exdir = 'Vicmap Features of Interest')

shp1 <- readOGR(dsn = "Vicmap Features of Interest/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer", layer = "foi_index_centroid")

#Converting to data frame
shapefile1_df <- data.frame(shp1)



esite<-shapefile1_df %>% filter((FTYPE == "emergency facility") & (PARENTNAME %like% "NATIONAL PARK"))

newdat1 <- esite


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

anglerIcon3 <- makeIcon(
  iconUrl = "https://www.flaticon.com/svg/vstatic/svg/2991/2991244.svg?token=exp=1618183017~hmac=e26835a1f920a3dc3b2f6295b28b0e75",
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 22, iconAnchorY = 94,
  
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)





pal <- colorFactor(
    palette = palette(rainbow(63)),
    domain = national_shp$NAME
)



xl_df<- read_excel("Campgrounds_site_v3.xlsx")



newdat <- xl_df[,c(1,3,10,11,13,14,15)]

# palette = palette(rainbow(28))





anglerIcon <- makeIcon(
    iconUrl = "https://www.flaticon.com/premium-icon/icons/svg/8/8193.svg",
    iconWidth = 40, iconHeight = 40,
    iconAnchorX = 22, iconAnchorY = 94,
    
    shadowWidth = 50, shadowHeight = 64,
    shadowAnchorX = 4, shadowAnchorY = 62
)








# Define UI 
shinyUI <- (fluidPage( 
    
    navbarPage(theme = shinythemes::shinytheme("cyborg"),""),
    # Application title
    headerPanel("Campgrounds and facilities in National parks"),
    
    # Sidebar with 2 selection inputs 

        
        
        # Show the caption and plot of the bar charts, bubble charts and also plot the leaflet map
        mainPanel(
            
            
            
            tabsetPanel(
              tabPanel("Campgrounds", leafletOutput("mymap1")),
              tabPanel("Hospitals", leafletOutput("mymap2")),
              tabPanel("Emergency Facility", leafletOutput("mymap3"))
              
            )
            
            
            
        )
      
    
    
))

# server.R

# Define server logic required 
shinyServer <- (function(input, output) {
    
    # Return the formula text for printing as a caption
    output$caption <- reactiveText(function() {
        paste("Variations based on:", input$variable)
    })
    
    # Generate a plot of variations by socio-economic factors 
    
    # ggplot version
    

    
    
    # interactive leaflet map 
    output$mymap1 <- renderLeaflet({ # create leaflet map
        mm <- leaflet(data = newdat) %>% 
            #using a provider tile terrain from stamen maps
            addTiles() %>%
            setView(144.2794, -36.7570, zoom = 6.5) %>%   
            #hovering on popups shows site names
            addMarkers(~xcoord, ~ycoord, label = ~as.character(SITE_NAME), icon = anglerIcon,
                       popup = paste("<b>National park name:</b>", newdat$PARK_NAME, "<br>",
                                     "<b>Campground name:</b>", newdat$SITE_NAME, "<br>",
                                     "<b>General info:</b>", newdat$NOTE, "<br>",
                                     "<b>Animals to be aware of:</b>", newdat$ANIMALS_WARN,"<br>",
                                     "<b>Other safety warnings:</b>", newdat$OTHER_WARN),
                       clusterOptions = markerClusterOptions(),
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
        
        
        
        
        
        mm
    })
    
    output$mymap2 <- renderLeaflet({ # create leaflet map
      mm1<- leaflet() %>%
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
      
      mm1
    })
    
    output$mymap3 <- renderLeaflet({ # create leaflet map
      mm3 <- leaflet(data = newdat1) %>% 
        #using a provider tile terrain from stamen maps
        addProviderTiles("Stamen.Terrain") %>%
        #hovering on popups shows site names
        addMarkers(~X_COORD, ~Y_COORD, label = ~NAME_LABEL,
                   popup = paste("<b>Facilty type:</b>", newdat1$FEATSUBTYP, "<br>",
                                 "<b>Name:</b>", newdat1$NAME_LABEL, "<br>",
                                 "<b>Located at:</b>", tolower(newdat1$PARENTNAME),"<br>"),
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
    })
    

    

    

    
    
    
    
})



# Run the interactive shiny application 
shinyApp(ui = shinyUI, server = shinyServer)





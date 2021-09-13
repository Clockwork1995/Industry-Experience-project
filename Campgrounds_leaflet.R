xl_df<- read_excel("Campgrounds_site_v4.xlsx")



newdat <- xl_df[,c(1,3,10,11,13,14,15)]

# palette = palette(rainbow(28))





anglerIcon <- makeIcon(
  iconUrl = "https://www.flaticon.com/premium-icon/icons/svg/8/8193.svg",
  iconWidth = 40, iconHeight = 40,
  iconAnchorX = 22, iconAnchorY = 94,
  
  shadowWidth = 50, shadowHeight = 64,
  shadowAnchorX = 4, shadowAnchorY = 62
)


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

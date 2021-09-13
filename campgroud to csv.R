## Xl to csv

xl_df<- read_excel("Campgrounds_site_v4.xlsx")



# write.csv(xl_df, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\Campgrounds_mapbox.csv", row.names = FALSE)

library(tidyverse)

#renaming some columns

xl_df = xl_df %>% 
  rename(
    Longitude = xcoord,
    Latitude = ycoord,
    Park = PARK_NAME,
    Description = ASSET_DESC,
    Animal_warning = ANIMALS_WARN,
    Other_warning = OTHER_WARN,
    Site_Name = SITE_NAME
  )



xl_df$NOTE <- as.character(xl_df$NOTE)
xl_df$NOTE[is.na(xl_df$NOTE)] <- " "

xl_df$Animal_warning <- as.character(xl_df$Animal_warning)
xl_df$Animal_warning[is.na(xl_df$Animal_warning)] <- " "

xl_df$Other_warning <- as.character(xl_df$Other_warning)
xl_df$Other_warning[is.na(xl_df$Other_warning)] <- " "


write.csv(xl_df, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\Campgrounds_mapbox.csv", row.names = FALSE)



#emergency facility conversion to csv and some other changes

unzip(zipfile = "Vicmap Features of Interest.zip", 
      exdir = 'Vicmap Features of Interest')

shp <- readOGR(dsn = "Vicmap Features of Interest/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer", layer = "foi_index_centroid")

#Converting to data frame
shapefile_df <- data.frame(shp)



esite<-shapefile_df %>% filter((FTYPE == "emergency facility") & (PARENTNAME %like% "NATIONAL PARK"))



esite = esite %>% 
  rename(
    Longitude = X_COORD,
    Latitude = Y_COORD
  )




write.csv(esite, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\emergencyfacility_mapbox.csv", row.names = FALSE)

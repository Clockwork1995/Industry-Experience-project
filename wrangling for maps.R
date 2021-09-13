library(tidyverse)
require(rgdal)
require(ggplot2)
library(devtools)
library(sf)
library(tidyverse)
library(leaflet)
library(maps)
library(dplyr)
library(data.table)
require(rgdal)
require(ggplot2)
library(devtools)
library(sf)
library(tidyverse)
library(readxl)
library(sf)
library(fs)
library(magrittr)
library(leaflet)
library(leaflet.extras)

## campgrounds data

#reading the full dataset
camp_df<- read_excel("Campgrounds_site_v4.xlsx")


#selecting required columns
newdat <- national_df[,c(1,3,4,10,11,13,14,15)]


#renaming some columns

newdat = newdat %>% 
  rename(
    Longitude = xcoord,
    Latitude = ycoord,
    Park = PARK_NAME,
    Description = ASSET_DESC,
    Animal_warning = ANIMALS_WARN,
    Other_warning = OTHER_WARN,
    Site_Name = SITE_NAME
  )


#replacing nulls with NONE

newdat$NOTE <- as.character(newdat$NOTE)
newdat$NOTE[is.na(newdat$NOTE)] <- "NONE"

newdat$Animal_warning <- as.character(newdat$Animal_warning)
newdat$Animal_warning[is.na(newdat$Animal_warning)] <- "NONE"

newdat$Other_warning <- as.character(newdat$Other_warning)
newdat$Other_warning[is.na(newdat$Other_warning)] <- "NONE"


write.csv(newdat, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\Campgrounds_final.csv", row.names = FALSE)






## hospital data


# original file name from source - vic_hospitals.kmz
# rename kmz  file by adding .zip then extract it to get doc.kml file

unzip(zipfile = "vic_hospitals.kmz.zip", 
      exdir = 'hospital_doc_file')



## Convert kml file to csv using mygeodata.cloud website


unzip(zipfile = "mygeodata.zip") #doc.csv file

hospital_df <- read.csv(file = 'doc.csv')

#selecting required columns
hospital_df <- hospital_df[,c(1,2,3,7,8,11,13)]


#renaming some columns

hospital_df = hospital_df %>% 
  rename(
    Longitude = X,
    Latitude = Y
  )


write.csv(hospital_df, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\hospital_final.csv", row.names = FALSE)





## Emergency facility data

#extracting the zip file

unzip(zipfile = "Vicmap Features of Interest.zip", 
      exdir = 'Vicmap Features of Interest')

#reading shapefile
shp <- readOGR(dsn = "Vicmap Features of Interest/ll_gda94/sde_shape/whole/VIC/VMFEAT/layer", layer = "foi_index_centroid")

#Converting to data frame
shapefile_df <- data.frame(shp)


#filtering and keeping only natinal park data
esite<-shapefile_df %>% filter((FTYPE == "emergency facility") & (PARENTNAME %like% "NATIONAL PARK"))


#keeping required columns
esite <- esite[,c(6,8,10,21,22)]

#renaming some columns

esite = esite %>% 
  rename(
    Longitude = X_COORD,
    Latitude = Y_COORD,
    Park = PARENTNAME,
    Facility_type = FEATSUBTYP

  )

#writing into a csv file with appropriate name

write.csv(esite, file = "c:\\Users\\ClockworK\\Desktop\\IE project\\emergency_facility_final.csv", row.names = FALSE)

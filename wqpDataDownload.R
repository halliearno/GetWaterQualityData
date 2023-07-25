#Hallie Arno 
#NPS GLKN/ SIP Ecology Assistant 
#Updated 15 June, 2023
#Downloads and cleans Great Lakes water quality data from WQP


#Load Packages
library(lubridate)
library(tidyverse)
library(dataRetrieval)
library(data.table)


#Filepaths for site names and site metadata
locations <- read.csv("C:\\Users\\harno\\Documents\\RCode\\GLKNStations.csv")# %>% # Site names and info for all parks 
# mutate(Station_ID = str_extract(MonitoringLocationIdentifier,"[^\\-]+$")) 

#Extract sites from lake parks spreadsheet
sites_to_get <- locations$MonitoringLocationIdentifier


#Read WQP Data 
df <- readWQPqw(c(siteNumbers = sites_to_get,""),'','') 


#delete any "non-data" results, such as weather obs, comments, air temp, etc. Do this by using "grepl" with "!" (not) to delete any records with "ActivityMediaName"="Air" or "Other". Note: the "|" is "or" and allows selection of multiple items to delete. Deletes whole row.  
#delete other non-data (subjective) results that have "ActivityMediaName" = "Water". Delete in "CharacteristicName" =  "water apperance (text)" and "wave height" using "!grepl". Deletes whole row.
dt <- data.table(df) %>% 
  filter(!grepl("Quality Control", ActivityTypeCode)) %>% 
  filter(!grepl("Air|Other", ActivityMediaName)) %>% 
  filter(!grepl("Wave height|Water appearance", CharacteristicName))

#When charactersistic name is Phosphorus and is not from the surface, replace w/ "Bottom Phosphorus"
dt[,CharacteristicName:= fcase(ActivityDepthHeightMeasure.MeasureValue !=0 & CharacteristicName == "Total Phosphorus, mixed forms", "Bottom Phosphorus",
                               CharacteristicName != "Bottom Phosphorus", CharacteristicName)] 


#Select and rename relevant columns
glknwqp <- dt %>% select(c("ActivityIdentifier", 
                           "CharacteristicName", 
                           "ActivityDepthHeightMeasure.MeasureValue", 
                           "ResultMeasureValue", "ActivityEndDateTime", 
                           "MonitoringLocationIdentifier", 
                           "ResultMeasure.MeasureUnitCode")) %>% 
  
  mutate(Site = str_extract(MonitoringLocationIdentifier,"[^\\-]+$")) %>% 
  mutate(Park = substr(Site, 1, 4)) %>% 
  
  rename(c("Activity_ID" = "ActivityIdentifier", 
           "Depth" = "ActivityDepthHeightMeasure.MeasureValue", 
           "Value" = "ResultMeasureValue",   
           "DateTime" = "ActivityEndDateTime",
           "Variable" = "CharacteristicName",
           "Units" = "ResultMeasure.MeasureUnitCode")) %>%
  
  mutate_at(vars("Depth"),~replace_na(.,0))




# Add lat long
locations <- locations %>% select(StationID, LatitudeMeasure, LongitudeMeasure, MonitoringLocationName)

colnames(locations) <- c("glknid", "Latitude", "Longitude", "Name")


df <- merge(glknwqp, locations, by.x = "Site", by.y = "glknid", all.x = TRUE)

df <- df %>% 
  mutate(Date = date(DateTime)) %>% 
  mutate(Month = month(DateTime)) %>% 
  mutate(Year = year(DateTime))

#Check
print(dim(df)-dim(glknwqp))
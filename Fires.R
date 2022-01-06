knitr::opts_chunk$set(echo = TRUE, warning = FALSE)
library(ggfortify)
library(kableExtra)
library(xml2)
library(utils)
library(tidyverse)
library(stringr)
library(knitr)
library(magrittr)
library(tidyr)
library(sf)
library(leaflet)
library(viridis)
library(rjson)
library(jsonlite)
library(RCurl)
library(geojsonio)
library(rgdal)
library(maps)
library(mapview)



data0 <-read.csv("https://wildfire.alberta.ca/resources/historical-data/documents/fires_2006to2018.csv",header = TRUE , sep = ",")




data1 <-read.csv("https://wildfire.alberta.ca/resources/historical-data/documents/fires_1996to2005.csv",header = TRUE , sep = ",")




data2 <-read.csv("https://wildfire.alberta.ca/resources/historical-data/documents/fires_1983to1995.csv",header = TRUE , sep = ",")




data3 <-read.csv("https://wildfire.alberta.ca/resources/historical-data/documents/fires_1961to1982.csv",header = TRUE , sep = ",")



# Commom data frame column names: (data0 and data1)


D <- list(data0, data1)
Reduce(intersect, lapply(D, names))




data <- rbind(data0,data1)



head(data,20000)




str(data)




data$forest[substring(data$fire_number,1,1) == 'C'] <- "Calgary"
data$forest[substring(data$fire_number,1,1) == 'E'] <- "Edson"
data$forest[substring(data$fire_number,1,1) == 'H'] <- "High Level"
data$forest[substring(data$fire_number,1,1) == 'G'] <- "Grande Prairie"
data$forest[substring(data$fire_number,1,1) == 'L'] <- "Lac La Biche"
data$forest[substring(data$fire_number,1,1) == 'M'] <- "Fort McMurray"
data$forest[substring(data$fire_number,1,1) == 'P'] <- "Peace River"
data$forest[substring(data$fire_number,1,1) == 'R'] <- "Rocky"
data$forest[substring(data$fire_number,1,1) == 'S'] <- "Slave Lake"
data$forest[substring(data$fire_number,1,1) == 'W'] <- "Whitecourt"


# Commom data frame column names: (data and data2)


D <- list(data, data2)
Reduce(intersect, lapply(D, names))


head(data,300)

str(data2)

data2$activity_class[data2$activity == 1] <- "windrows"
data2$activity_class[data2$activity == 2] <- "piles"
data2$activity_class[data2$activity == 3] <- "re-piles"
data2$activity_class[data2$activity == 4] <- "brush"
data2$activity_class[data2$activity == 5] <- "grass"
data2$activity_class[data2$activity == 6] <- "stubble"
data2$activity_class[data2$activity == 7] <- "slash"
data2$activity_class[data2$activity == 8] <- "refuse"
data2$activity_class[data2$activity == 9] <- "flaring gas"
data2$activity_class[data2$activity == 10] <- "heavy equipment"
data2$activity_class[data2$activity == 11] <- "rail transportation"
data2$activity_class[data2$activity == 12] <- "vehicle transportation"
data2$activity_class[data2$activity == 13] <- "all-terrain vehicle"
data2$activity_class[data2$activity == 14] <- "welders or associated equipment"
data2$activity_class[data2$activity == 15] <- "power saws"
data2$activity_class[data2$activity == 16] <- "power lines"
data2$activity_class[data2$activity == 17] <- "engines"
data2$activity_class[data2$activity == 18] <- "stoves, lamps"
data2$activity_class[data2$activity == 19] <- "smoking"
data2$activity_class[data2$activity == 20] <- "fireworks"
data2$activity_class[data2$activity == 21] <- "cooking"
data2$activity_class[data2$activity == 22] <- "pipelines"
data2$activity_class[data2$activity == 23] <- "unclassified"
data2$activity_class[data2$activity == 24] <- "ammunition, fireworks, etc."
data2$activity_class[data2$activity == 25] <- "children playing with matches"
data2$activity_class[data2$activity == 26] <- "arson"
data2$activity_class[data2$activity == 27] <- "air transportation"
data2$activity_class[data2$activity == 28] <- "power saw"
data2$activity_class[data2$activity == 29] <- "refuelling"
data2$activity_class[data2$activity == 30] <- "lighting fires"
data2$activity_class[data2$activity == 31] <- "traditional burning"
data2$activity_class[data2$activity == 99] <- "all other activities"

data2$true_cause[data2$truecause == 1] <- "abandoned fire"
data2$true_cause[data2$truecause == 2] <- "unsafe fire"
data2$true_cause[data2$truecause == 3] <- "insufficient resources(for size of fire)"
data2$true_cause[data2$truecause == 4] <- "unattended fire"
data2$true_cause[data2$truecause == 5] <- "insufficient buffer"
data2$true_cause[data2$truecause == 6] <- "flammable fluids"
data2$true_cause[data2$truecause == 7] <- "burning substance"
data2$true_cause[data2$truecause == 8] <- "hot exhaust or exhaust sparks"
data2$true_cause[data2$truecause == 9] <- "PG Personal Gain"
data2$true_cause[data2$truecause == 10] <- "UG Unresolved Grievance"
data2$true_cause[data2$truecause == 11] <- "LS Lack of Supervision"
data2$true_cause[data2$truecause == 12] <- "IO Imitating Others"
data2$true_cause[data2$truecause == 13] <- "unpredictable happening"
data2$true_cause[data2$truecause == 14] <- "arson known"
data2$true_cause[data2$truecause == 15] <- "high hazard occurrence"
data2$true_cause[data2$truecause == 16] <- "arson suspected"

data2$fire_origin[data2$origin == 1] <- "provincial land"
data2$fire_origin[data2$origin == 2] <- "private land"
data2$fire_origin[data2$oridin == 3] <- "Indian reserve"
data2$fire_origin[data2$origin == 4] <- "Metis settlement"
data2$fire_origin[data2$oridin == 5] <- "Province of British Columbia"
data2$fire_origin[data2$origin == 6] <- "Province of Saskatchewan"
data2$fire_origin[data2$oridin == 7] <- " North West Territories"
data2$fire_origin[data2$origin == 8] <- "provincial park"
data2$fire_origin[data2$oridin == 9] <- "national park"
data2$fire_origin[data2$origin == 10] <- "USA"

data2$size_class[data2$sizeclass == 'A'] <- "0.01 to 0.1 ha"
data2$size_class[data2$sizeclass == 'B'] <- "0.11 to 4.0 ha"
data2$size_class[data2$sizeclass == 'C'] <- "4.1 to 40.0 ha"
data2$size_class[data2$sizeclass == 'D'] <- "40.1 to 200.0 ha"
data2$size_class[data2$sizeclass == 'E'] <- "200.1 + ha"

data2$forest[data2$forest == 0] <- "Athabasca"
data2$forest[data2$forest == 1] <- "Bow Crow"
data2$forest[data2$forest == 2] <- "Edson"
data2$forest[data2$forest == 3] <- "Footner"
data2$forest[data2$forest == 4] <- "Grande Prairie"
data2$forest[data2$forest == 5] <- "Lac La Biche"
data2$forest[data2$forest == 6] <- "Peace River"
data2$forest[data2$forest == 7] <- "Rocky/Clearwater"
data2$forest[data2$forest == 8] <- "Slave Lake"
data2$forest[data2$forest == 9] <- "Whitecourt"

head(data2,200)

dat2<-data.frame("fire_number" = data2$firenumber , "fire_location_latitude" = data2$lat,"fire_location_longitude" = data2$long ,"fire_year" = as.character(data2$fire_year)  , "start_for_fire_date" = data2$startdate  , "discovered_date" = data2$discovdate,"fire_origin" = data2$fire_origin, "true_cause" = data2$true_cause, "activity_class" = data2$activity_class,"uc_hectares" = data2$grandarea, "size_class" = data2$size_class, "forest" = data2$forest)

dat2<-data.frame(dat2)

head(dat2,20000)

D <- list(dat2)
Reduce(intersect, lapply(D, str))



# Commom data frame column names: (data2 and data)

D <- list(data2, data)
Reduce(intersect, lapply(D, names))



# Commom data frame column names: (data2 and data3)


D <- list(data2, data3)
Reduce(intersect, lapply(D, names))

D <- list(data3)
Reduce(intersect, lapply(D, str))

data3$true_cause[data3$SPECAUSE == 1] <- "Lightning"
data3$true_cause[data3$SPECAUSE == 2] <- "Burning Building"
data3$true_cause[data3$SPECAUSE == 3] <- "Burning Vehicle"
data3$true_cause[data3$SPECAUSE == 4] <- "Refuse Burning"
data3$true_cause[data3$SPECAUSE == 5] <- "Land Clearing"
data3$true_cause[data3$SPECAUSE == 6] <- "Range Improvement"
data3$true_cause[data3$SPECAUSE == 7] <- "Smoking"
data3$true_cause[data3$SPECAUSE == 8] <- "Blasting"
data3$true_cause[data3$SPECAUSE == 9] <- "Exhaust"
data3$true_cause[data3$SPECAUSE == 10] <- "Engine Sparks"
data3$true_cause[data3$SPECAUSE == 11] <- "Camp Fire"
data3$true_cause[data3$SPECAUSE == 12] <- "Slash Disposal"
data3$true_cause[data3$SPECAUSE == 13] <- "Spontaneous Combustion"
data3$true_cause[data3$SPECAUSE == 14] <- "Brush and Debris Disposal"
data3$true_cause[data3$SPECAUSE == 15] <- "Hazard Reduction Burn"
data3$true_cause[data3$SPECAUSE == 16] <- "Other Prescribed Burn"
data3$true_cause[data3$SPECAUSE == 17] <- "Oil and Gas Well"
data3$true_cause[data3$SPECAUSE == 18] <- "Grass Burning on Railroad Right-ofWay"
data3$true_cause[data3$SPECAUSE == 19] <- "Railroad Locomotive Exhaust"
data3$true_cause[data3$SPECAUSE == 20] <- "Grudge Fire"
data3$true_cause[data3$SPECAUSE == 21] <- "Job Fire"
data3$true_cause[data3$SPECAUSE == 22] <- "Playing with Matches"
data3$true_cause[data3$SPECAUSE == 23] <- "Glass"
data3$true_cause[data3$SPECAUSE == 24] <- "Smudge Fire"

data3$fire_origin[data3$OWNSHIP == 1] <- "Provincial Crown Land"
data3$fire_origin[data3$OWNSHIP == 2] <- "Indian Reserve"
data3$fire_origin[data3$OWNSHIP == 3] <- "Federal Lands"
data3$fire_origin[data3$OWNSHIP == 4] <- "Private land"
data3$fire_origin[data3$OWNSHIP == 5] <- "British Columbia"
data3$fire_origin[data3$OWNSHIP == 6] <- "Saskatchewan"
data3$fire_origin[data3$OWNSHIP == 7] <- "Other"
data3$fire_origin[data3$OWNSHIP == 8] <- "Metis Colony"
data3$fire_origin[data3$OWNSHIP == 9] <- "Northwest Territories"

data3$activity_class[data3$CAUSECLA == 1] <- "Owner (Private)"
data3$activity_class[data3$CAUSECLA == 2] <- "Public Employees"
data3$activity_class[data3$CAUSECLA == 3] <- "Local Resident"
data3$activity_class[data3$CAUSECLA == 4] <- "Seasonal Worker"
data3$activity_class[data3$CAUSECLA == 5] <- "Transient"
data3$activity_class[data3$CAUSECLA == 6] <- "Children"
data3$activity_class[data3$CAUSECLA == 7] <- "Others - Known"
data3$activity_class[data3$CAUSECLA == 8] <- "Unknown"

data3$size_class[data3$SIZECLAS == 1] <- "Up to 0.25 Acres"
data3$size_class[data3$SIZECLAS == 2] <- "0.26 - 10.0 Acres"
data3$size_class[data3$SIZECLAS == 3] <- "10.1 - 100.0 Acres"
data3$size_class[data3$SIZECLAS == 4] <- "100.1 - 500.0 Acres"
data3$size_class[data3$SIZECLAS == 5] <- "500.1 Acres +"

data3$forest[data3$FOREST == 'A'] <- "Athabasca"
data3$forest[data3$FOREST == 'B'] <- "Bow"
data3$forest[data3$FOREST == 'C'] <- "Crow"
data3$forest[data3$FOREST == 'E'] <- "Edson"
data3$forest[data3$FOREST == 'F'] <- "Footner Lake"
data3$forest[data3$FOREST == 'G'] <- "Grande Prairie"
data3$forest[data3$FOREST == 'L'] <- "Lac La Biche"
data3$forest[data3$FOREST == 'P'] <- "Peace River"
data3$forest[data3$FOREST == 'R'] <- "Rocky/Clearwater"
data3$forest[data3$FOREST == 'S'] <- "Slave Lake"
data3$forest[data3$FOREST == 'W'] <- "Whitecourt"

head(data3,15000)


# Data3

D <- list(data3)
Reduce(intersect, lapply(D, names))

dat3 <-data.frame( "fire_number" = data3$FIRENUMBER ,"fire_location_latitude" = data3$LAT  , "fire_location_longitude" = data3$LONG , "fire_year" = str_c(19,data3$YEAR)  , "start_for_fire_date" = str_c(19,data3$YEAR,'-',data3$MON,'-',data3$DAY) ,"discovered_date" = str_c(19,data3$YEAR,'-',data3$DMON,'-',data3$DDAY),"true_cause" = data3$true_cause, "fire_origin" = data3$fire_origin, "activity_class" = data3$activity_class,"uc_hectares" = data3$TOTAL,"size_class" = data3$size_class, "forest" = data3$forest)

D <- list(dat3)
Reduce(intersect, lapply(D, str))

D <- list(dat2,dat3)
Reduce(intersect, lapply(D, names))

dat3<-data.frame(dat3)

df <-rbind(dat2,dat3)

head(df,28000)

head(data,30000)

D <- list(data , df)
Reduce(intersect, lapply(D, names))

D <- list(data , df)
Reduce(intersect, lapply(D, str))

dat1<- data.frame("fire_number" = data$fire_number  ,           
                  "fire_year"  = as.character(data$fire_year)  ,           
                  "size_class"  = data$size_class,            
                  "fire_location_latitude"  = data$fire_location_latitude, 
                  "fire_location_longitude"  = data$fire_location_longitude,
                  "fire_origin"  = data$fire_origin ,           
                  "activity_class" = data$activity_class  ,        
                  "true_cause"  = data$true_cause    ,         
                  "discovered_date"  = data$discovered_date   ,    
                  "start_for_fire_date"  = data$start_for_fire_date ,   
                  "uc_hectares" = data$uc_hectares,  
                  "forest" = data$forest)

data4 <- rbind(df , dat1)

D <- list(data4)
Reduce(intersect, lapply(D, str))

head(data4, 40000)

write.csv(data4,"fires.csv",row.names=FALSE)

data4 <- data4[data4$true_cause == "Vehicle Fire",]

map = leaflet(data4)%>%
  addTiles() %>%
  addCircles(lng = ~fire_location_longitude, lat = ~fire_location_latitude, popup = ~as.character(activity_class), label = ~as.character(forest))%>%
  
  addMarkers(lng = ~fire_location_longitude, lat = ~fire_location_latitude, popup = ~as.character(activity_class), label = ~as.character(forest))

map

mapshot(map, file = paste0(getwd(), "/burnmap.png"))



ggplot(data4,aes(x = fire_year , y = uc_hectares , color = forest))+geom_point() + theme(axis.text.x = element_text(angle = 45))+
  labs(x = "Year", y = "Qty",
       title = "Hectares burnt by year",
       caption = "Data: Forest Fires") +
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))

ggsave("forest burnt hectares.png", width = 6, height = 6)



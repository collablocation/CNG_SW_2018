################################################################################
########################  DATA PREPROCESSING  ##################################
################################################################################
library(sp)
library(mapview)
library(rgdal) # for importing Shapefiles to R as Spatial data frame
library(maptools) # snapPointsToLines function
library(dplyr)
library(data.table)
library(dtplyr) # a combination of dplyr + data.table
library(foreign)
library(classInt)
library(RColorBrewer)
library(DT) # popup alert window using JS functionality
library(htmltools) # for point popups
library(stringr)
library(reshape2) # suppporting function colsplit

library(colourpicker)
library(rmarkdown)
library(htmlwidgets)
library(webshot)

# packages used for simplifying spatial lines dataframes (ref: https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d#.v6nxnf8m1)
library(spdplyr) # for manipulating the attribute data inside the spatial data frame
library(geojsonio) # for converting the spatial data frame to GeoJSON and saving to file systems
library(rmapshaper) # for manipulating the geometry (polygon, line, marker) part of the GeoJSON data

# The stable version of leaflet doesn't support functions like mouseover popups. Using the development version by devtools::install_github("rstudio/leaflet")
library(devtools)
devtools::install_github("rstudio/leaflet")

## Importing data for existing CNG stations, truck stops and AADT 
CNG_Stations <- read.dbf("C:/Users/.../CNG_update.dbf") 
# focusing on public stations for heavy duty trucks
CNG_Stations <- CNG_Stations %>% filter(grepl('public|Public',Groups_Wit) & NG_Vehicle=='HD') # 360 existing CNG stations for heavy-duty trucking in the Southwest (10 states)
CNG_Stations <- CNG_Stations %>% select(ID,Fuel_Type,Station_Na,Street_Add,Intersecti,City,State,ZIP,Status_Cod,Groups_Wit,Expected_D,
                                        Access_Day,Cards_Acce,NG_Fill_Ty,NG_PSI,Latitude,Longitude,Date_Last,Updated_At,Owner_Type,
                                        Federal_Ag,Federal__1,Open_Date,NG_Vehicle)
CNG_Stations <- CNG_Stations %>% mutate(ID_2 = 1:n())
CNG_Stations$ID_2 <- as.factor(CNG_Stations$ID_2)

Truck_Stops <- read.dbf("C:/Users/.../Truck_Stops_update_fleet.dbf") 
Truck_Stops <- Truck_Stops %>% select(F_Longitud,F_Latitude,F_POI_Name,F_Address_,FID_1,AADTT,FLEET2) # update_2018
colnames(Truck_Stops) <- c("LONGITUDE","LATITUDE","NAME","ADDRESS","ID","AADTT","FLEET")
Truck_Stops$ID <- as.factor(Truck_Stops$ID)

Truck_Stops <- Truck_Stops %>% mutate(ID_2 = 361:(n()+360)) # update_2018
Truck_Stops$ID_2 <- as.factor(Truck_Stops$ID_2)

# combining CNG_Stations and Truck_Stops into one simple dataframe for distance calculation
locations <- CNG_Stations %>% select(ID_2,Latitude,Longitude)
colnames(locations) <- c("ID_2","LATITUDE","LONGITUDE")
locations <- rbind(locations,select(Truck_Stops,ID_2,LATITUDE,LONGITUDE))

# importing metro population centroids of 323 OD pairs
Metro_centroids <- read.dbf("C:/Users/.../Metro_centroids_update_near.dbf")

## Fixing up the missing names of road segments issue
## Focusing on NHS 1 and 3: Interstate and Non-Interstate Strategic Highway Network
## IMPORTANT: POLYLINE SHAPEFILE HAS TO BE SAVED AS THE ORIGINAL ORDER EXACTLY!
NHS_1and3 <- read.dbf("C:/Users/.../AADT_600mi_NHS1&3.dbf")
NHS_1and3 <- NHS_1and3 %>% mutate(FID=seq.int(nrow(NHS_1and3)))
NHS_1and3_v2 <- NHS_1and3 %>% filter(!is.na(SIGN1))
missing <- NHS_1and3 %>% filter(is.na(SIGN1))
missing2 <- missing %>% filter(USLRSID!="6_")
# arranging missing2 dataframe in terms of USLRSID and BEGMP
# and then do a loop to merge missing2 rows one by one based on the USLRSID and ENDMP/BEGMP with existing SIGN1

# There are two steps
# First, based on a segment with known SIGN1 and its ENDMP, finding the missing SIGN1 next to the segment: BEGMP is the ENDMP of segment with known SIGN1
missing3 <- missing2 %>% arrange(USLRSID,BEGMP)
missing3 <- missing3 %>% mutate(LABEL = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_1and3_v3 <- NHS_1and3_v2 %>% mutate(LABEL = paste0(USLRSID,"-",ENDMP,"-",STATE))
NHS_1and3_v4 <- NHS_1and3_v3 %>% select(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(missing3)) {
  row <- merge(missing3[i, ],NHS_1and3_v4,'LABEL',all.x=TRUE)
  row2 <- row %>% select(LABEL,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL <- paste0(row3$USLRSID,"-",row3$ENDMP,"-",row3$STATE)
  NHS_1and3_v4 <- rbind(NHS_1and3_v4,row3)
  df <- rbind(df,row3)
}

df2 <- df %>% group_by(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df2_fixed <- df2 %>% filter(!is.na(SIGN1))

# Second, based on a segment with known SIGN1 and its BEGMP, finding the missing SIGN1 next to the segment: ENDMP is the BEGMP of segment with known SIGN1
df2_missing <- df2 %>% filter(is.na(SIGN1))
df2_missing <- df2_missing %>% mutate(LABEL2 = paste0(USLRSID,"-",ENDMP,"-",STATE))
df2_missing <- df2_missing %>% arrange(USLRSID,desc(ENDMP))
NHS_1and3_v3 <- NHS_1and3_v2 %>% mutate(LABEL2 = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_1and3_v4 <- NHS_1and3_v3 %>% select(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(df2_missing)) {
  row <- merge(df2_missing[i, ],NHS_1and3_v4,'LABEL2',all.x=TRUE)
  row2 <- row %>% select(LABEL2,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL2,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL2","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL2 <- paste0(row3$USLRSID,"-",row3$BEGMP,"-",row3$STATE)
  NHS_1and3_v4 <- rbind(NHS_1and3_v4,row3)
  df <- rbind(df,row3)
}

df3 <- df %>% group_by(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df3_fixed <- df3 %>% filter(!is.na(SIGN1))

# finally, 62 of 96 segments have been assigned to 'SIGN1". 
# other 34 segments are assigned by using google maps and street map in ArcGIS
colnames(df3_fixed) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE","count")
fixed <- rbind(df2_fixed,df3_fixed)
missing_fixed <- merge(missing,fixed,'ID',all.x=TRUE)
missing_fixed <- missing_fixed %>% arrange(ID)
missing <- missing %>% arrange(ID)
missing <- missing %>% mutate(NAME=missing_fixed$SIGN1.y)
missing$SIGN1 <- missing$NAME
missing <- missing[,1:72]
NHS_1and3_fixed <- rbind(NHS_1and3_v2,missing)

NHS_1and3_fixed$SIGN1 <- as.character(NHS_1and3_fixed$SIGN1)
NHS_1and3_fixed[54866:54868,13] <- 'U60' 
NHS_1and3_fixed[54920,13] <- 'PORT CHICAGO HWY' 
NHS_1and3_fixed[54921:54922,13] <- 'U217' 
NHS_1and3_fixed[54925:54931,13] <- 'U480' 
NHS_1and3_fixed[54932,13] <- 'SAN EDUARDO AVE' 
NHS_1and3_fixed[54933:54949,13] <- 'U130' 
NHS_1and3_fixed[54956:54958,13] <- 'CENTRAL E FWY' 

NHS_1and3_fixed <- NHS_1and3_fixed %>% arrange(FID)
NHS_1and3_fixed <- NHS_1and3_fixed[,1:71]
write.dbf(NHS_1and3_fixed,"C:/Users/Fangwu Wei/Dropbox/ABOR RIF Alt Fuels/Data/geodesign_app/AADT_600mi_NHS1&3.dbf")

## IMPORTANT: POLYLINE SHAPEFILE HAS TO BE SAVED AS THE ORIGINAL ORDER EXACTLY!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## LAST DATA UPDATE before Making OPEN-SOURCE
# NHS 7&0; find missing road names
NHS_7 <- read.dbf("C:/Users/Fangwu Wei/Dropbox/ABOR RIF Alt Fuels/Data/geodesign_app/AADT_600mi_NHS7.dbf")
NHS_7 <- NHS_7 %>% mutate(FID=seq.int(nrow(NHS_7)))
NHS_7_v2 <- NHS_7 %>% filter(!is.na(SIGN1))
missing <- NHS_7 %>% filter(is.na(SIGN1))
missing2 <- missing %>% filter(USLRSID!="6_")
# arranging missing2 dataframe in terms of USLRSID and BEGMP
# and then do a loop to merge missing2 rows one by one based on the USLRSID and ENDMP/BEGMP with existing SIGN1

# There are two steps
# First, based on a segment with known SIGN1 and its ENDMP, finding the missing SIGN1 next to the segment: BEGMP is the ENDMP of segment with known SIGN1
missing3 <- missing2 %>% arrange(USLRSID,BEGMP)
missing3 <- missing3 %>% mutate(LABEL = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_7_v3 <- NHS_7_v2 %>% mutate(LABEL = paste0(USLRSID,"-",ENDMP,"-",STATE))
NHS_7_v4 <- NHS_7_v3 %>% select(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(missing3)) {
  row <- merge(missing3[i, ],NHS_7_v4,'LABEL',all.x=TRUE)
  row2 <- row %>% select(LABEL,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL <- paste0(row3$USLRSID,"-",row3$ENDMP,"-",row3$STATE)
  NHS_7_v4 <- rbind(NHS_7_v4,row3)
  df <- rbind(df,row3)
}

df2 <- df %>% group_by(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df2_fixed <- df2 %>% filter(!is.na(SIGN1))

# Second, based on a segment with known SIGN1 and its BEGMP, finding the missing SIGN1 next to the segment: ENDMP is the BEGMP of segment with known SIGN1
df2_missing <- df2 %>% filter(is.na(SIGN1))
df2_missing <- df2_missing %>% mutate(LABEL2 = paste0(USLRSID,"-",ENDMP,"-",STATE))
df2_missing <- df2_missing %>% arrange(USLRSID,desc(ENDMP))
NHS_7_v3 <- NHS_7_v2 %>% mutate(LABEL2 = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_7_v4 <- NHS_7_v3 %>% select(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(df2_missing)) {
  row <- merge(df2_missing[i, ],NHS_7_v4,'LABEL2',all.x=TRUE)
  row2 <- row %>% select(LABEL2,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL2,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL2","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL2 <- paste0(row3$USLRSID,"-",row3$BEGMP,"-",row3$STATE)
  NHS_7_v4 <- rbind(NHS_7_v4,row3)
  df <- rbind(df,row3)
}

df3 <- df %>% group_by(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df3_fixed <- df3 %>% filter(!is.na(SIGN1))

colnames(df3_fixed) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE","count")
fixed <- rbind(df2_fixed,df3_fixed)
missing_fixed <- merge(missing,fixed,'ID',all.x=TRUE,all.y=TRUE)
missing_fixed <- missing_fixed %>% arrange(ID)

write.csv(missing_fixed,"C:/Users/.../NHS7_missing.csv")

missing_fixed_v2 <- read.csv("C:/Users/.../NHS7_missing.csv")

write.csv(NHS_7_v2,"C:/Users/.../NHS7_v2.csv")

NHS7_complete <- read.csv("C:/Users/.../NHS7_complete.csv")
NHS7_complete <- NHS7_complete %>% arrange(FID)
write.dbf(NHS7_complete, "C:/Users/.../AADT_600mi_NHS7.dbf")


## NHS_0
NHS_0 <- read.dbf("C:/Users/.../AADT_600mi_NHS0.dbf")
NHS_0 <- NHS_0 %>% mutate(FID=seq.int(nrow(NHS_0)))
NHS_0_v2 <- NHS_0 %>% filter(!is.na(SIGN1))
missing <- NHS_0 %>% filter(is.na(SIGN1))
missing2 <- missing %>% filter(USLRSID!="6_")
# arranging missing2 dataframe in terms of USLRSID and BEGMP
# and then do a loop to merge missing2 rows one by one based on the USLRSID and ENDMP/BEGMP with existing SIGN1

# There are two steps
# First, based on a segment with known SIGN1 and its ENDMP, finding the missing SIGN1 next to the segment: BEGMP is the ENDMP of segment with known SIGN1
missing3 <- missing2 %>% arrange(USLRSID,BEGMP)
missing3 <- missing3 %>% mutate(LABEL = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_0_v3 <- NHS_0_v2 %>% mutate(LABEL = paste0(USLRSID,"-",ENDMP,"-",STATE))
NHS_0_v4 <- NHS_0_v3 %>% select(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(missing3)) {
  row <- merge(missing3[i, ],NHS_0_v4,'LABEL',all.x=TRUE)
  row2 <- row %>% select(LABEL,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL <- paste0(row3$USLRSID,"-",row3$ENDMP,"-",row3$STATE)
  NHS_0_v4 <- rbind(NHS_0_v4,row3)
  df <- rbind(df,row3)
}

df2 <- df %>% group_by(LABEL,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df2_fixed <- df2 %>% filter(!is.na(SIGN1))

# Second, based on a segment with known SIGN1 and its BEGMP, finding the missing SIGN1 next to the segment: ENDMP is the BEGMP of segment with known SIGN1
df2_missing <- df2 %>% filter(is.na(SIGN1))
df2_missing <- df2_missing %>% mutate(LABEL2 = paste0(USLRSID,"-",ENDMP,"-",STATE))
df2_missing <- df2_missing %>% arrange(USLRSID,desc(ENDMP))
NHS_0_v3 <- NHS_0_v2 %>% mutate(LABEL2 = paste0(USLRSID,"-",BEGMP,"-",STATE))
NHS_0_v4 <- NHS_0_v3 %>% select(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE)
df <- data.frame()
for (i in 1:nrow(df2_missing)) {
  row <- merge(df2_missing[i, ],NHS_0_v4,'LABEL2',all.x=TRUE)
  row2 <- row %>% select(LABEL2,ID.x,ID.y,SIGN1.x,SIGN1.y,USLRSID.x,USLRSID.y,BEGMP.x,ENDMP.x,BEGMP.y,ENDMP.y,STATE.x,STATE.y)
  row3 <- row2 %>% select(LABEL2,ID.x,SIGN1.y,USLRSID.x,BEGMP.x,ENDMP.x,STATE.x)
  colnames(row3) <- c("LABEL2","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE")
  row3$LABEL2 <- paste0(row3$USLRSID,"-",row3$BEGMP,"-",row3$STATE)
  NHS_0_v4 <- rbind(NHS_0_v4,row3)
  df <- rbind(df,row3)
}

df3 <- df %>% group_by(LABEL2,ID,SIGN1,USLRSID,BEGMP,ENDMP,STATE) %>% summarise(count=n())
df3_fixed <- df3 %>% filter(!is.na(SIGN1))

colnames(df3_fixed) <- c("LABEL","ID","SIGN1","USLRSID","BEGMP","ENDMP","STATE","count")
fixed <- rbind(df2_fixed,df3_fixed)
missing_fixed <- merge(missing,fixed,'ID',all.x=TRUE,all.y=TRUE)
missing_fixed <- missing_fixed %>% arrange(ID)

write.csv(missing_fixed,"C:/Users/.../NHS0_missing.csv")

missing_fixed_v2 <- read.csv("C:/Users/.../NHS0_missing.csv")

write.csv(NHS_0_v2,"C:/Users/.../NHS0_v2.csv")

NHS0_complete <- read.csv("C:/Users/.../NHS0_complete.csv")
NHS0_complete <- NHS0_complete %>% arrange(FID)
write.dbf(NHS0_complete, "C:/Users/.../AADT_600mi_NHS0.dbf")


#### REMEMBER: WHEN WRITE POLYGON OR POLYLINE DBF, HAS TO BE ORIGINAL ORDER ####

## simply adding a binary variable to zip-based fleet data to take existing CNG stations into account
# if a zip code area is covered by existing CNG stations, it's yes so fleet value will be divided by 2
# if a zip code area is not covered by existing CNG stations, it's no so fleet value won't be divided.
# identify available public CNG stations

## Fleet data is not free.

stationIcon <- makeIcon(iconUrl = "C:/Users/...(shiny app directory)/www/marker.png",iconWidth = 35,iconHeight = 40,iconAnchorX = 18, iconAnchorY = 39)

# large number of road segments makes visualization very slow and browser crashes very often
# using 'unsplit lines' function in ArcGIS to aggregate road segments to reduce the number of segments 
# still, it's slow and unstable to load a layer including over 60,000 road segments (rows), that's why the AADTT data have been splitted to six smaller datasets (layers) in terms of NHS codes.
# NOtes: step 1: unsplit road segments based on AADT12, reduced but still a lot
# Step 2: categorize AADTT12 of each segment and unsplit based on AADT categories, significantly reduced but loading was still slow because the geometry that each spatiallines dataframe depends on doesn't change regardless of significant reduction of total number of road segments
# Step 3: excluding road segments in states of AL,GA,MS,TN,FL,MO,NE,IL,WY,ID,OR to further make the entire spatiallines dataframe smaller
# Step 4: a way to simplify spatiallines dataframe while keeping the integrity of its geometry intact
Annual_Average_Daily_Traffic_NHS0 <- readOGR("C:/Users/...",
                                             layer="AADTT_600mi_unsplit_NHS0",verbose=FALSE)
Annual_Average_Daily_Traffic_NHS1 <- readOGR("C:/Users/...",
                                             layer="AADTT_600mi_unsplit_NHS1",verbose=FALSE)
Annual_Average_Daily_Traffic_NHS3 <- readOGR("C:/Users/...",
                                             layer="AADTT_600mi_unsplit_NHS3",verbose=FALSE)
Annual_Average_Daily_Traffic_NHS7 <- readOGR("C:/Users/...",
                                             layer="AADTT_600mi_unsplit_NHS7",verbose=FALSE)
Annual_Average_Daily_Traffic_NHS10 <- readOGR("C:/Users/...",
                                              layer="AADTT_600mi_unsplit_NHS10",verbose=FALSE)
Annual_Average_Daily_Traffic_NHSothers <- readOGR("C:/Users/...",
                                                  layer="AADTT_600mi_unsplit_NHSothers",verbose=FALSE)

Annual_Average_Daily_Traffic_Truck <- readOGR("C:/Users/...",
                                              layer="AADT_600mi_v2_3",verbose=FALSE)
I10 <- readOGR("C:/Users/...",
               layer="I10 corridor",verbose=FALSE)

ZIP_Fleet <- readOGR("C:/Users/...",
                     layer="ZIP_Fleet_projected",verbose=FALSE)

Pipeline <- readOGR("C:/Users/...",
                    layer="pipelines_SW",verbose=FALSE)

###### OD_PATH LAYER UPDATE_2018 ##########
OD_paths <- readOGR("C:/Users/...",
                    layer="OD_shortest_paths_unsplit_v2",verbose=FALSE)
OD_paths <- OD_paths %>% filter(OD_ID!=235) # replace shortest path between Loredo-Corpus Christi
road_network_spatial_specialcase <- readOGR("C:/Users/...",
                                            layer="OD_shortest_paths_specialcase_unsplit",verbose=FALSE)
road_network_spatial_specialcase$OD_ID <- 235
OD_paths <- rbind(OD_paths,road_network_spatial_specialcase)

Dallas_O <- OD_paths %>% filter(NAME_O=="Dallas")
Dallas_D <- OD_paths %>% filter(NAME_D=="Dallas")
Dallas_O$NAME_O <- "Dallas-Fort Worth"
Dallas_D$NAME_D <- "Dallas-Fort Worth"
Dallas <- rbind(Dallas_O,Dallas_D)

OD_paths_others <- OD_paths %>% filter(NAME_O!="Dallas"&NAME_D!="Dallas")
OD_paths <- rbind(OD_paths_others,Dallas)

OD_paths <- OD_paths %>% mutate(PAIR = paste0(NAME_O,"-",NAME_D))
OD_paths <- merge(OD_paths,od_table_coverage_final,by.x='PAIR',by.y='OD')

OD_paths <- OD_paths %>% filter(OD_PATH %in% path_number)
OD_paths <- OD_paths %>% arrange(PAIR)

# converting to latlon
# leaflet expects coordinates to be in longlat. 
# Use example 'mylines_ll <- sp::spTransform(mylines, CRS("+init=epsg:4326"))' and mylines_ll should be rendered correctly. 
# mapview does this for you automatically
# So the solution in this case was to just not reproject the data to the leaflet projection beforehand, and provide it in lat-lon coordinates (EPSG:4326)
Annual_Average_Daily_Traffic_NHS0 <- sp::spTransform(Annual_Average_Daily_Traffic_NHS0, CRS("+init=epsg:4326"))
Annual_Average_Daily_Traffic_NHS1 <- sp::spTransform(Annual_Average_Daily_Traffic_NHS1, CRS("+init=epsg:4326"))
Annual_Average_Daily_Traffic_NHS3 <- sp::spTransform(Annual_Average_Daily_Traffic_NHS3, CRS("+init=epsg:4326"))
Annual_Average_Daily_Traffic_NHS7 <- sp::spTransform(Annual_Average_Daily_Traffic_NHS7, CRS("+init=epsg:4326"))

Annual_Average_Daily_Traffic_Truck <- sp::spTransform(Annual_Average_Daily_Traffic_Truck, CRS("+init=epsg:4326"))
I10 <- sp::spTransform(I10, CRS("+init=epsg:4326"))

ZIP_Fleet <- sp::spTransform(ZIP_Fleet, CRS("+init=epsg:4326"))

Pipeline <- sp::spTransform(Pipeline, CRS("+init=epsg:4326"))

OD_paths <- sp::spTransform(OD_paths, CRS("+init=epsg:4326"))

# MUST convert to characters first to keep index and level information and then convert to numbers accordingly in order to keep accuracy

# Using AADTT instead of AADT, focusing on AADT for Truck
Annual_Average_Daily_Traffic_NHS0$AADT_Categ_v2 <- as.numeric(as.character(Annual_Average_Daily_Traffic_NHS0$AADTT_Cate))
Annual_Average_Daily_Traffic_NHS1$AADT_Categ_v2 <- as.numeric(as.character(Annual_Average_Daily_Traffic_NHS1$AADTT_Cate))
Annual_Average_Daily_Traffic_NHS3$AADT_Categ_v2 <- as.numeric(as.character(Annual_Average_Daily_Traffic_NHS3$AADTT_Cate))
Annual_Average_Daily_Traffic_NHS7$AADT_Categ_v2 <- as.numeric(as.character(Annual_Average_Daily_Traffic_NHS7$AADTT_Cate))

Annual_Average_Daily_Traffic_Truck$AADT_Categ_v2 <- as.numeric(as.character(Annual_Average_Daily_Traffic_Truck$AADTT_Cate))

Annual_Average_Daily_Traffic_NHS0 <- Annual_Average_Daily_Traffic_NHS0 %>% 
  mutate(Jenks = ifelse(AADT_Categ_v2 == 1, "0-2,000",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(AADT_Categ_v2 == 2, "2,001-6,000",
                               ifelse(AADT_Categ_v2 == 3, "6,001-12,000",
                                      ifelse(AADT_Categ_v2 == 4, "12,001-22,000", "22,001-62,538")))))
Annual_Average_Daily_Traffic_NHS0$Jenks <- as.factor(Annual_Average_Daily_Traffic_NHS0$Jenks)
# reorder the levels of factor to make it correct
Annual_Average_Daily_Traffic_NHS0$Jenks <- factor(Annual_Average_Daily_Traffic_NHS0$Jenks,
                                                  levels=c("0-2,000","2,001-6,000","6,001-12,000","12,001-22,000", "22,001-62,538"))

Annual_Average_Daily_Traffic_NHS1 <- Annual_Average_Daily_Traffic_NHS1 %>% 
  mutate(Jenks = ifelse(AADT_Categ_v2 == 1, "0-2,000",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(AADT_Categ_v2 == 2, "2,001-6,000",
                               ifelse(AADT_Categ_v2 == 3, "6,001-12,000",
                                      ifelse(AADT_Categ_v2 == 4, "12,001-22,000", "22,001-62,538")))))
Annual_Average_Daily_Traffic_NHS1$Jenks <- as.factor(Annual_Average_Daily_Traffic_NHS1$Jenks)
# reorder the levels of factor to make it correct
Annual_Average_Daily_Traffic_NHS1$Jenks <- factor(Annual_Average_Daily_Traffic_NHS1$Jenks,
                                                  levels=c("0-2,000","2,001-6,000","6,001-12,000","12,001-22,000", "22,001-62,538"))

Annual_Average_Daily_Traffic_NHS3 <- Annual_Average_Daily_Traffic_NHS3 %>% 
  mutate(Jenks = ifelse(AADT_Categ_v2 == 1, "0-2,000",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(AADT_Categ_v2 == 2, "2,001-6,000",
                               ifelse(AADT_Categ_v2 == 3, "6,001-12,000",
                                      ifelse(AADT_Categ_v2 == 4, "12,001-22,000", "22,001-62,538")))))
Annual_Average_Daily_Traffic_NHS3$Jenks <- as.factor(Annual_Average_Daily_Traffic_NHS3$Jenks)
# reorder the levels of factor to make it correct
Annual_Average_Daily_Traffic_NHS3$Jenks <- factor(Annual_Average_Daily_Traffic_NHS3$Jenks,
                                                  levels=c("0-2,000","2,001-6,000","6,001-12,000","12,001-22,000", "22,001-62,538"))

Annual_Average_Daily_Traffic_NHS7 <- Annual_Average_Daily_Traffic_NHS7 %>% 
  mutate(Jenks = ifelse(AADT_Categ_v2 == 1, "0-2,000",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(AADT_Categ_v2 == 2, "2,001-6,000",
                               ifelse(AADT_Categ_v2 == 3, "6,001-12,000",
                                      ifelse(AADT_Categ_v2 == 4, "12,001-22,000", "22,001-62,538")))))
Annual_Average_Daily_Traffic_NHS7$Jenks <- as.factor(Annual_Average_Daily_Traffic_NHS7$Jenks)
# reorder the levels of factor to make it correct
Annual_Average_Daily_Traffic_NHS7$Jenks <- factor(Annual_Average_Daily_Traffic_NHS7$Jenks,
                                                  levels=c("0-2,000","2,001-6,000","6,001-12,000","12,001-22,000", "22,001-62,538"))

Annual_Average_Daily_Traffic_Truck <- Annual_Average_Daily_Traffic_Truck %>% 
  mutate(Jenks = ifelse(AADT_Categ_v2 == 1, "0-2,000",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(AADT_Categ_v2 == 2, "2,001-6,000",
                               ifelse(AADT_Categ_v2 == 3, "6,001-12,000",
                                      ifelse(AADT_Categ_v2 == 4, "12,001-22,000", "22,001-62,538")))))
Annual_Average_Daily_Traffic_Truck$Jenks <- as.factor(Annual_Average_Daily_Traffic_Truck$Jenks)
# reorder the levels of factor to make it correct
Annual_Average_Daily_Traffic_Truck$Jenks <- factor(Annual_Average_Daily_Traffic_Truck$Jenks,
                                                   levels=c("0-2,000","2,001-6,000","6,001-12,000","12,001-22,000", "22,001-62,538"))

ZIP_Fleet <- ZIP_Fleet %>% 
  mutate(Jenks = ifelse(FLEET == 0, "0",
                        ifelse(FLEET %in% 1:150, "1-150",
                               ifelse(FLEET %in% 151:450, "151-450",                                
                                      ifelse(FLEET %in% 451:1000, "451-1,000",
                                             ifelse(FLEET %in% 1001:2300, "1,001-2,300", "2,301-6,530"))))))
ZIP_Fleet$Jenks <- as.factor(ZIP_Fleet$Jenks)
# reorder the levels of factor to make it correct
ZIP_Fleet$Jenks <- factor(ZIP_Fleet$Jenks,levels=c("0","1-150","151-450","451-1,000","1,001-2,300", "2,301-6,530"))

# NHS0, NHS1, NHS3, NHS7
for(i in 1:5) {
  NHS <- Annual_Average_Daily_Traffic_NHS0 %>% filter(AADT_Categ_v2==i)
  NHS <- geojson_json(NHS)
  NHS <- ms_simplify(NHS)
  assign(paste("NHS0_",i,sep=""),NHS)
  print(i)
}

for(i in 1:5) {
  NHS <- Annual_Average_Daily_Traffic_Truck %>% filter(AADT_Categ_v2==i)
  NHS <- geojson_json(NHS)
  NHS <- ms_simplify(NHS)
  assign(paste("ALLNHS_",i,sep=""),NHS)
  print(i)
}
Annual_Average_Daily_Traffic_Truck <- geojson_json(Annual_Average_Daily_Traffic_Truck)
Annual_Average_Daily_Traffic_Truck <- ms_simplify(Annual_Average_Daily_Traffic_Truck)

I10 <- geojson_json(I10)
I10 <- ms_simplify(I10)

NHS0_1<-Annual_Average_Daily_Traffic_NHS0%>%filter(AADT_Categ_v2==1)
NHS0_2<-Annual_Average_Daily_Traffic_NHS0%>%filter(AADT_Categ_v2==2)
NHS0_3<-Annual_Average_Daily_Traffic_NHS0%>%filter(AADT_Categ_v2==3)
NHS0_4<-Annual_Average_Daily_Traffic_NHS0%>%filter(AADT_Categ_v2==4)
NHS0_5<-Annual_Average_Daily_Traffic_NHS0%>%filter(AADT_Categ_v2==5)

NHS0_1 <- geojson_json(NHS0_1)
NHS0_1 <- ms_simplify(NHS0_1)
NHS0_2 <- geojson_json(NHS0_2)
NHS0_2 <- ms_simplify(NHS0_2)
NHS0_3 <- geojson_json(NHS0_3)
NHS0_3 <- ms_simplify(NHS0_3)
NHS0_4 <- geojson_json(NHS0_4)
NHS0_4 <- ms_simplify(NHS0_4)
NHS0_5 <- geojson_json(NHS0_5)
NHS0_5 <- ms_simplify(NHS0_5)

# categorize fleet data
ZIP_Fleet_0 <- ZIP_Fleet %>% filter(FLEET==0)
ZIP_Fleet_1 <- ZIP_Fleet %>% filter(FLEET>0 & FLEET<=150)
ZIP_Fleet_2 <- ZIP_Fleet %>% filter(FLEET>150 & FLEET<=450)
ZIP_Fleet_3 <- ZIP_Fleet %>% filter(FLEET>450 & FLEET<=1000)
ZIP_Fleet_4 <- ZIP_Fleet %>% filter(FLEET>1000 & FLEET<=2300)
ZIP_Fleet_5 <- ZIP_Fleet %>% filter(FLEET>2300 & FLEET<=6530)

# simplify sub-fleet files
ZIP_Fleet_1 <- geojson_json(ZIP_Fleet_1)
ZIP_Fleet_2 <- geojson_json(ZIP_Fleet_2)
ZIP_Fleet_3 <- geojson_json(ZIP_Fleet_3)
ZIP_Fleet_4 <- geojson_json(ZIP_Fleet_4)
ZIP_Fleet_5 <- geojson_json(ZIP_Fleet_5)

ZIP_Fleet <- ms_simplify(ZIP_Fleet)
ZIP_Fleet_0 <- ms_simplify(ZIP_Fleet_0)
ZIP_Fleet_1 <- ms_simplify(ZIP_Fleet_1)
ZIP_Fleet_2 <- ms_simplify(ZIP_Fleet_2)
ZIP_Fleet_3 <- ms_simplify(ZIP_Fleet_3)
ZIP_Fleet_4 <- ms_simplify(ZIP_Fleet_4)
ZIP_Fleet_5 <- ms_simplify(ZIP_Fleet_5)

Pipeline <- geojson_json(Pipeline)
Pipeline <- ms_simplify(Pipeline)

OD_paths <- ms_simplify(OD_paths)

# Categorize 323 od paths into five groups in terms of ktons by using natural breaks (Jenks) classification method
OD_paths_edit <- merge(OD_paths,od_table_coverage_final,by.x="PAIR",by.y="OD")
OD_paths_edit <- OD_paths_edit %>% select(1:10,21:22,14)
# write the final version of shapefile with kton and tonmile attributes
writeOGR(OD_paths_edit,"C:/Users/Fangwu Wei/Dropbox/ABOR RIF Alt Fuels/Data/Update_2018",layer="OD_shortest_paths_final_v3_edit",
         verbose=FALSE,driver="ESRI Shapefile")
OD_paths2 <- OD_paths_edit %>% 
  mutate(WIDTH = ifelse(TOTAL_KTONS_2015.y <= 600, "1",   # not using AADT12_v2 %in% 17:16386, "17-16386"
                        ifelse(TOTAL_KTONS_2015.y > 600 & TOTAL_KTONS_2015.y <= 2000, "2",
                               ifelse(TOTAL_KTONS_2015.y > 2000 & TOTAL_KTONS_2015.y <= 4500, "3",
                                      ifelse(TOTAL_KTONS_2015.y > 4500 & TOTAL_KTONS_2015.y <= 9000, "4", "5")))))
OD_paths2$WIDTH <- as.numeric(OD_paths2$WIDTH)

rm(Annual_Average_Daily_Traffic_NHS0,
   Annual_Average_Daily_Traffic_NHS3,
   Annual_Average_Daily_Traffic_NHS7,
   Annual_Average_Daily_Traffic_NHS10,
   Annual_Average_Daily_Traffic_NHSothers)


####################################################################
############################# OD DATA ##############################
library(dplyr)
library(igraph)
library(foreign)
library(reshape2)
###################################################################
########################## Prepare ################################
#### OD DATA - UPDATE 2018 ####
####
# step 1: use near function in arcgis to find the nearest point of each od centroid on road network (NHS 1&3, nad1983) and the distance to the nearest point
# step 2: read the generated shapefile from step 1 and save as a csv file
# step 3: import the csv file to arcgis by using near-x and near-y
# step 4: use split function to split road network by using search radius, 0.001 mile (projected)

faf_points_original <- read.dbf("C:/Users/.../XYMetro_centroids_update.dbf")
write.csv(faf_points_original,"C:/Users/.../XYMetro_centroids_update.csv")

road_network <- read.dbf("C:/Users/.../Update_2018/AADT_600mi_v2_split.dbf")
road_network <- road_network %>% mutate(label=seq(1,247801,by=1),start=paste0(start_y,",",start_x),end=paste0(end_y,",",end_x)) # original order may be used later

# identify the centroids 
road_network_centroids <- road_network %>% group_by(FAF4_ID) %>% summarise(count=n())
road_network_centroids2 <- road_network_centroids %>% filter(count>1)
road_network_part1 <- road_network %>% filter(FAF4_ID %in% road_network_centroids2$FAF4_ID)
road_network_part2 <- road_network %>% filter(!(FAF4_ID %in% road_network_centroids2$FAF4_ID))

centroids_raw <- intersect(road_network_part1$start,road_network_part1$end) # find common elements

centroids_raw_1 <- as.data.frame(centroids_raw)
colnames(centroids_raw_1) <- "point"
centroids_raw_v1 <- as.character(centroids_raw_1$point)

centroids_raw_2 <- colsplit(centroids_raw_1$point,",",c("lat","lon"))
write.csv(centroids_raw_2,"C:/Users/.../Update_2018/nodes.csv") # and then use NEAR function to find the nearest faf_cfs_near points

points_southwest <- read.dbf("C:/Users/.../XYMetro_centroids_update_near.dbf") # shapefile of near-x and near-y
points_southwest <- points_southwest %>% select(1, 5:18)
# use near function in arcgis with XYMetro_centroids_update_near.shp before read the dbf file to find the FID of the nearest point 
# which will be used for merging
nodes_southwest <- read.dbf("C:/Users/.../XYnodes.dbf") # shapefile of common points on road network
colnames(nodes_southwest) <- c("Field1_2","lat","lon","NEAR_FID_2","NEAR_DIST_2","NEAR_X_2","NEAR_Y_2")
points_southwest <- points_southwest %>% mutate(FID=seq(0,25,by=1))
nodes_southwest <- merge(nodes_southwest,points_southwest,by.x="NEAR_FID_2",by.y="FID")
nodes_southwest <- nodes_southwest %>% mutate(point=paste0(lat,",",lon))
nodes_southwest <- nodes_southwest %>% arrange(Field1)
write.csv(nodes_southwest,"C:/Users/.../nodes_v2.csv")


#######################################################################
##################  NETWORK ANALYSIS - UPDATE 2018  ###################
#######################################################################
road_network_part2_1 <- road_network_part2 %>% filter(NHS==1 | NHS==3)
road_network_part2_2 <- road_network_part2 %>% filter(NHS!=1 & NHS!=3)
road_network_part2_1 <- road_network_part2_1 %>% mutate(weight = MILES/SPD_LIMIT)
road_network_part2_2 <- road_network_part2_2 %>% mutate(weight = MILES/(SPD_LIMIT*0.5)) # 50% penalty
road_network_part2 <- rbind(road_network_part2_1,road_network_part2_2)
road_network_part2 <- road_network_part2 %>% arrange(label)

road_network_unique <- road_network_part2
road_network_duplicate <- road_network %>% filter(!(FAF4_ID %in% road_network_unique$FAF4_ID)) %>% mutate(weight = SHAPE_LEN/SPD_LIMIT)
road_network_unique_v2 <- road_network_unique %>% select(start,end,weight,label,STATE,STFIPS,CTFIPS,FAF4_ID,MILES)
road_network_duplicate_v2 <- road_network_duplicate %>% select(start,end,weight,label,STATE,STFIPS,CTFIPS,FAF4_ID,SHAPE_LEN)
colnames(road_network_unique_v2)[9] <- "length"
colnames(road_network_duplicate_v2)[9] <- "length"
road_network_v2 <- rbind(road_network_unique_v2,road_network_duplicate_v2)
road_network_v2 <- road_network_v2 %>% arrange(label) # original order is very important to make sure further network analysis is correct

graph_road_network <- graph.data.frame(road_network_v2, directed=FALSE, vertices=NULL)

# calculate shortest path for each OD pair
route_number <- 1
pair_original <- data.frame(label=integer(),id=numeric(),start=character(),end=character(),weight=numeric(),STATE=factor(),
                            STFIPS=integer(),CTFIPS=integer(),FAF4_ID=integer(),length=numeric(),FAF_O=integer(),FAF_D=integer(),
                            NAME_O=factor(),NAME_D=factor(),LOC_O=character(),LOC_D=character(),OD_ID=integer()) 
road_network_spatial <- readOGR("C:/Users/...",
                                layer="AADT_600mi_v2_split",verbose=FALSE)
road_network_spatial <- road_network_spatial %>% mutate(label=seq(1,247801,by=1))
road_network_spatial_v2 <- road_network_spatial_test[0, ]

# There 26 metro O-D centroids in the Southwest.
for(i in 1:25) {
  shortest_paths_details <- get.shortest.paths(graph_road_network,from=centroids_raw_v1[i],to=centroids_raw_v1[(i+1):26],output="both")
  for(j in 1:(26-i)) {
    pair <- as.data.frame(unlist(shortest_paths_details$epath[j]))
    colnames(pair) <- "label"
    pair <- pair %>% mutate(id=seq(1,NROW(pair),by=1))
    pair2 <- merge(pair,road_network_v2,"label",all.x=TRUE)
    origin <- centroids_raw_v1[i]
    dest <- centroids_raw_v1[i+j]
    origin_v2 <- nodes_southwest %>% filter(point==origin) %>% select(FAF,SHORTNAME,point)
    dest_v2 <- nodes_southwest %>% filter(point==dest) %>% select(FAF,SHORTNAME,point)
    pair2 <- pair2 %>% mutate(FAF_O=origin_v2$FAF,FAF_D=dest_v2$FAF,NAME_O=origin_v2$SHORTNAME,NAME_D=dest_v2$SHORTNAME,
                              LOC_O=origin_v2$point,LOC_D=dest_v2$point,OD_ID=route_number)
    road_network_spatial_test <- merge(road_network_spatial,pair2,"label")
    road_network_spatial_test <- road_network_spatial_test %>% filter(OD_ID==route_number)
    road_network_spatial_v2 <- rbind(road_network_spatial_v2,road_network_spatial_test)
    print(route_number)
    route_number <- route_number+1
  }
}

## Special case: Laredo - Corpus Christi #############################################
# 50% speed penalty is not given in terms of NHS 1&3 in order to obtain a more realistic shortest path.
road_network_part2_1 <- road_network_part2 %>% filter(NHS==1 | NHS==3)
road_network_part2_2 <- road_network_part2 %>% filter(NHS!=1 & NHS!=3)
road_network_part2_1 <- road_network_part2_1 %>% mutate(weight = MILES/SPD_LIMIT)
road_network_part2_2 <- road_network_part2_2 %>% mutate(weight = MILES/SPD_LIMIT)
road_network_part2 <- rbind(road_network_part2_1,road_network_part2_2)
road_network_part2 <- road_network_part2 %>% arrange(label)

road_network_unique <- road_network_part2
road_network_duplicate <- road_network %>% filter(!(FAF4_ID %in% road_network_unique$FAF4_ID)) %>% mutate(weight = SHAPE_LEN/SPD_LIMIT)
road_network_unique_v2 <- road_network_unique %>% select(start,end,weight,label,STATE,STFIPS,CTFIPS,FAF4_ID,MILES)
road_network_duplicate_v2 <- road_network_duplicate %>% select(start,end,weight,label,STATE,STFIPS,CTFIPS,FAF4_ID,SHAPE_LEN)
colnames(road_network_unique_v2)[9] <- "length"
colnames(road_network_duplicate_v2)[9] <- "length"
road_network_v2 <- rbind(road_network_unique_v2,road_network_duplicate_v2)
road_network_v2 <- road_network_v2 %>% arrange(label) # original order is very important to make sure further network analysis is correct

graph_road_network <- graph.data.frame(road_network_v2, directed=FALSE, vertices=NULL)

# calculate shortest path for each OD pair
route_number <- 1
pair_original <- data.frame(label=integer(),id=numeric(),start=character(),end=character(),weight=numeric(),STATE=factor(),
                            STFIPS=integer(),CTFIPS=integer(),FAF4_ID=integer(),length=numeric(),FAF_O=integer(),FAF_D=integer(),
                            NAME_O=factor(),NAME_D=factor(),LOC_O=character(),LOC_D=character(),OD_ID=integer()) 
road_network_spatial <- readOGR("C:/Users/...",
                                layer="AADT_600mi_v2_split",verbose=FALSE)
road_network_spatial <- road_network_spatial %>% mutate(label=seq(1,247801,by=1))
road_network_spatial_v2 <- road_network_spatial_test[0, ]

for(i in 13:13) {
  shortest_paths_details <- get.shortest.paths(graph_road_network,from=centroids_raw_v1[i],to=centroids_raw_v1[(i+1):26],output="both")
  for(j in 1:(26-i)) {
    pair <- as.data.frame(unlist(shortest_paths_details$epath[j]))
    colnames(pair) <- "label"
    pair <- pair %>% mutate(id=seq(1,NROW(pair),by=1))
    pair2 <- merge(pair,road_network_v2,"label",all.x=TRUE)
    origin <- centroids_raw_v1[i]
    dest <- centroids_raw_v1[i+j]
    origin_v2 <- nodes_southwest %>% filter(point==origin) %>% select(FAF,SHORTNAME,point)
    dest_v2 <- nodes_southwest %>% filter(point==dest) %>% select(FAF,SHORTNAME,point)
    pair2 <- pair2 %>% mutate(FAF_O=origin_v2$FAF,FAF_D=dest_v2$FAF,NAME_O=origin_v2$SHORTNAME,NAME_D=dest_v2$SHORTNAME,
                              LOC_O=origin_v2$point,LOC_D=dest_v2$point,OD_ID=route_number)
    road_network_spatial_test <- merge(road_network_spatial,pair2,"label")
    road_network_spatial_test <- road_network_spatial_test %>% filter(OD_ID==route_number)
    road_network_spatial_v2 <- rbind(road_network_spatial_v2,road_network_spatial_test)
    print(route_number)
    route_number <- route_number+1
  }
}

road_network_spatial_v3 <- road_network_spatial_v2 %>% select(label,ID,FAF4_ID.y,length,NAME_O,NAME_D,OD_ID)
road_network_spatial_specialcase <- road_network_spatial_v3 %>% filter(NAME_O=="Laredo" & NAME_D=="Corpus Christi")
writeOGR(road_network_spatial_specialcase,"C:/Users/...",
         layer="OD_shortest_paths_specialcase",verbose=FALSE,driver="ESRI Shapefile")

# snap existing CNG stations and truck stops within 3-mile buffer to the nearest shortest path
OD_shortest_paths_unsplit <- readOGR("C:/Users/...",
                                     layer="OD_shortest_paths_specialcase_unsplit",verbose=FALSE)
OD_sites_within3mi_projected <- readOGR("C:/Users/...",
                                        layer="OD_site_original_within3mi_specialcase",verbose=FALSE)

snap_v0 <- snap_i[0,] # generate a single spatial dataframe snap_i and then extract the top row for snap_v0; update_2018

i<-1
snap_i <- snapPointsToLines(OD_sites_within3mi_projected,OD_shortest_paths_unsplit,maxDist = 3,withAttrs = TRUE) # maptools library 
snap_v0 <- rbind(snap_v0,snap_i)
print(i)

writeOGR(snap_v0,"C:/Users/...",
         layer="OD_site_original_within3mi_snapped_specialcase",verbose=FALSE,driver="ESRI Shapefile")

###########################################################


#################################################################


road_network_spatial <- readOGR("C:/Users/...",
                                layer="FAF4_lower48_projected_split",verbose=FALSE)

# correct one
road_network_spatial_v3 <- road_network_spatial_v2 %>% select(label,ID,FAF4_ID.y,length,NAME_O,NAME_D,OD_ID)
writeOGR(road_network_spatial_v3,"C:/Users/...",
         layer="OD_shortest_paths",verbose=FALSE,driver="ESRI Shapefile")

# create a dataframe including existing stations and candidates
existing_stations <- CNG_Stations %>% select(Latitude,Longitude,ID_2) %>% mutate(selection=1)
candidate_sites <- Truck_Stops %>% select(LATITUDE,LONGITUDE,ID_2) %>% mutate(selection=0)
colnames(candidate_sites)[1:2] <- c("Latitude","Longitude")
OD_sites <- rbind(existing_stations,candidate_sites)
write.csv(OD_sites,"C:/Users/.../OD_site_original.csv")

# snap existing CNG stations and truck stops within 3-mile buffer to the nearest shortest path
OD_shortest_paths_unsplit <- readOGR("C:/Users/...",
                                     layer="OD_shortest_paths_unsplit",verbose=FALSE)
OD_sites_within3mi_projected <- readOGR("C:/Users/...",
                                        layer="OD_site_original_within3mi",verbose=FALSE)

# snap OD paths one by one to avoid that one point 
snap_v0 <- snap_i[0,] # generate a single spatial dataframe snap_i and then extract the top row for snap_v0; update_2018
for (i in 1:325) {
  OD_shortest_paths_unsplit_v2 <- OD_shortest_paths_unsplit %>% filter(OD_ID==i)
  snap_i <- snapPointsToLines(OD_sites_within3mi_projected,OD_shortest_paths_unsplit_v2,maxDist = 3,withAttrs = TRUE) # maptools library 
  snap_v0 <- rbind(snap_v0,snap_i)
  print(i)
}
writeOGR(snap_v0,"C:/Users/...",
         layer="OD_site_original_within3mi_snapped",verbose=FALSE,driver="ESRI Shapefile")

# create 325 OD tables for the shortest paths
ori_dest <- read.dbf("C:/Users/.../Metro_centroids_update_near.dbf")
# the snapped file only has coordinates of original points so one more step is needed
# which is to use calculate function to find the actual coordinates (near-x and near-y) of the snapping points on road network
points <- read.dbf("C:/Users/.../OD_site_original_within3mi_snapped.dbf")
segments <- read.dbf("C:/Users/.../OD_shortest_paths_unsplit_split_v2.dbf")
segments <- segments %>% mutate(start=paste0(start_y,",",start_x),end=paste0(end_y,",",end_x),
                                start2=paste0(round(start_y,digits=6),",",round(start_x,digits=6)),
                                end2=paste0(round(end_y,digits=6),",",round(end_x,digits=6)))
# reorder the list of segments
segments <- segments %>% mutate(OD_ID_v2=0)
row <- 1
order_count <- segments %>% group_by(NAME_O,NAME_D) %>% summarise(count=n())
for(i in 1:325) {
  n <- order_count[i,3]
  for(j in 1:n$count) {
    segments[row,13] <- i
    row <- row + 1
  }
}

ori_dest <- ori_dest %>% mutate(point=paste0(round(NEAR_Y,digits=6),",",round(NEAR_X,digits=6)))
points <- points %>% mutate(point=paste0(snap_y,",",snap_x),point2=paste0(round(snap_y,digits=6),",",round(snap_x,digits=6)))
points$nrst_l_ <- as.numeric(as.character(points$nrst_l_))
points <- points %>% mutate(nrst_l = nrst_l_ + 1)
od_table <- data.frame(label=integer(),id=numeric(),start2=character(),end2=character(),NAME_O=factor(),NAME_D=factor(),SHAPE_LEN=numeric(),
                       ID_2=character(),selectn=character(),od_path=numeric())
for (i in 1:325) {
  points_test <- points %>% filter(nrst_l==i) # i=1, Albuquerque - Austin
  segments_test <- segments %>% filter(OD_ID_v2==i) 
  segments_test <- segments_test %>% mutate(LABEL=seq(1,nrow(segments_test),by=1))
  segments_test_ori <- as.character(unique(segments_test$NAME_O)) 
  segments_test_dest <- as.character(unique(segments_test$NAME_D)) 
  ori <- ori_dest %>% filter(SHORTNAME==segments_test_ori)
  dest <- ori_dest %>% filter(SHORTNAME==segments_test_dest)
  segments_test2 <- merge(ori_dest,segments_test,by.x="point",by.y="end2")
  segments_test2 <- segments_test2 %>% filter(SHORTNAME==ori$SHORTNAME | SHORTNAME==dest$SHORTNAME)
  segments_test_remaining <- segments_test %>% filter(LABEL!=segments_test2$LABEL)
  segments_test_remaining <- merge(segments_test_remaining,points_test,by.x='end2',by.y='point2',all.x=TRUE)
  segments_test2_v2 <- segments_test2 %>% select(start2,point,NAME_O,NAME_D,SHAPE_LEN)
  segments_test2_v2 <- segments_test2_v2 %>% mutate(ID_2='0',selectn=1)
  segments_test_remaining_v2 <- segments_test_remaining %>% select(start2,end2,NAME_O,NAME_D,SHAPE_LEN,ID_2,selectn)
  colnames(segments_test2_v2)[2] <- "end2" 
  segments_test_v2 <- rbind(segments_test2_v2,segments_test_remaining_v2)
  segments_test_v2 <- segments_test_v2 %>% mutate(label=seq(1,nrow(segments_test_v2),by=1))
  graph_segments_test <- graph.data.frame(segments_test_v2, directed=FALSE, vertices=NULL)
  shortest_paths_details <- get.shortest.paths(graph_segments_test,from=ori$point,to=dest$point,output="both")
  pair <- as.data.frame(unlist(shortest_paths_details$epath[1]))
  colnames(pair) <- "label"
  pair <- pair %>% mutate(id=seq(1,nrow(pair),by=1))
  pair2 <- merge(pair,segments_test_v2,"label",all.x=TRUE)
  pair2 <- pair2 %>% arrange(id) %>% mutate(od_path=i)
  od_table <- rbind(od_table,pair2)
  write.csv(pair2,paste0("C:/Users/.../OD_table_",i,".csv"))
  print(i)
}
write.csv(od_table,"C:/Users/.../od_table.csv")

od_table_na <- od_table %>% filter(is.na(ID_2))
od_table_na_2 <- od_table_na %>% group_by(od_path) %>% summarise(count=n())

# fix missing values (NA rows)
# scenario 1: some stations/truck stops are not included because of the slight difference of geographic coordinates
# secnario 2: some NA rows are duplicate stations/truck stops because of function split at points in arcgis
pair_na_final <- data.frame(id=numeric(),SHAPE_LEN=numeric(),od_path=numeric(),ID_2.y=factor(),selectn.y=factor())
for(i in 1:325) {
  points_test <- points %>% filter(nrst_l==i)
  pair2 <- od_table %>% filter(od_path==i)
  pair_na <- pair2 %>% filter(is.na(ID_2))
  coordinates <- colsplit(pair_na$end2,",",c("end_y","end_x"))
  pair_na <- cbind(pair_na, coordinates)
  pair_na <- pair_na %>% mutate(end3=paste0(round(end_y,digits=5),",",round(end_x,digits=5)))
  coordinates_points <- colsplit(points_test$point2,",",c("point_y","point_x"))
  points_test_v2 <- cbind(points_test,coordinates_points)
  points_test_v2 <- points_test_v2 %>% mutate(point3=paste0(round(point_y,digits=5),",",round(point_x,digits=5)))
  pair_na_v2 <- merge(pair_na,points_test_v2,by.x='end3',by.y='point3')
  pair_na_v3 <- pair_na_v2 %>% select(id,SHAPE_LEN,od_path,ID_2.y,selectn.y)
  pair_na_final <- rbind(pair_na_final,pair_na_v3)
  print(i)
}

od_table_v2 <- od_table %>% mutate(identifier=paste0(od_path,"-",id))
#od_table_v2_2 <- od_table_v2 %>% filter(od_path!=8)
od_table_v2_2 <- od_table_v2
pair_na_final_v2 <- pair_na_final %>% mutate(identifier=paste0(od_path,"-",id))
od_table_v2_2 <- merge(od_table_v2_2,pair_na_final_v2,"identifier",all.x=TRUE)
write.csv(od_table_v2_2,"C:/Users/.../od_table_v2.csv")

# correct na values and then read the updated file
# ID_2=2 means the last segment of an od path, the destination is not a truck stop or a CNG station but it has to be selected every time
# so selection=1 always
# ID_2=99999 means the segments with NAs but these segments won't affect the driving range feature (around 1896 segments)
# For example, segments A-B, B-C and C-D only B and D are actual truck stops or CNG stations and C is just a segment point
# because of 'split line at points' function with 3-mile search radius in ArcGIS
# a nearby station or a truck stop is snapped to an OD path twice
# It will be fixed if it does matter when considering arc optimization (FRLM)
# It means the length of segment B-C would be added to C-D
od_table_final <- read.csv("C:/Users/.../od_table_v2.csv")
coordinates_start <- colsplit(od_table_final$start2,",",c("lat_start","lon_start"))
coordinates_end <- colsplit(od_table_final$end2,",",c("lat_end","lon_end"))
od_table_final <- cbind(od_table_final,coordinates_start,coordinates_end)
od_table_final <- od_table_final %>% select(label,NAME_O,NAME_D,SHAPE_LEN,ID_2,selectn,od_path,lat_start,lon_start,lat_end,lon_end)
colnames(od_table_final) <- c("LABEL","ORIGIN","DESTINATION","LENGTH","ID_2","SELECTION","OD_PATH","LAT_START","LON_START","LAT_END","LON_END")
od_table_final$ID_2 <- as.factor(od_table_final$ID_2)
write.csv(od_table_final,"C:/Users/.../od_table_final.csv")

# changes: LA->Port of LA/LB, Hou->Port of Hou, New Orleans->Port of New Orleans
FAF_OD_PAIR <- read.csv("C:/Users/.../FAF OD pair data_v4.csv")
FAF_OD_PAIR <- FAF_OD_PAIR %>% mutate(OD=paste0(ORIGIN,"-",DESTINATION),OD_reverse=paste0(DESTINATION,"-",ORIGIN))

# only 323 matching od pairs from 325 od pairs
od_table_final <- read.csv("C:/Users/.../od_table_final.csv")
od_table_final <- od_table_final %>% select(2:12)
od_table_final$ID_2 <- as.factor(od_table_final$ID_2)
od_table_coverage <- od_table_final %>% group_by(ORIGIN,DESTINATION,OD_PATH) %>% summarise(COUNT=n()) %>% mutate(OD=paste0(ORIGIN,"-",DESTINATION))
od_table_coverage <- od_table_coverage %>% select(1:3,5)
od_table_coverage_v2 <- od_table_coverage %>% filter((OD %in% FAF_OD_PAIR$OD) | (OD %in% FAF_OD_PAIR$OD_reverse))
od_table_coverage_v2_1 <- merge(od_table_coverage_v2,select(FAF_OD_PAIR,1:9),"OD")
od_table_coverage_v2_2 <- merge(od_table_coverage_v2,select(FAF_OD_PAIR,1:8,10),by.x="OD",by.y="OD_reverse")
od_table_coverage_final <- rbind(od_table_coverage_v2_1,od_table_coverage_v2_2)
od_table_coverage_final <- od_table_coverage_final %>% select(1:5,7,9:12)
colnames(od_table_coverage_final) <- c("OD","ORIGIN","DESTINATION","OD_PATH","DMS_ORIG","DMS_DEST","DMS_MODE","TOTAL_KTONS_2015","TOTAL_TON_MILE_2015","LENGTH")
write.csv(od_table_coverage_final,"C:/Users/.../FAF OD pair data_v3_final.csv")
path_number <- sort(od_table_coverage_final$OD_PATH)
od_table_coverage_segments <- od_table_final %>% filter(OD_PATH %in% path_number)

# quartile by tons
quartile <- quantile(od_table_coverage_final$TOTAL_KTONS_2015)
od_table_coverage_final_top_quartile <- od_table_coverage_final %>% filter(TOTAL_KTONS_2015>=quartile[4])
od_table_coverage_final_2nd_quartile <- od_table_coverage_final %>% filter(TOTAL_KTONS_2015>=quartile[3] & TOTAL_KTONS_2015<quartile[4])
od_table_coverage_final_3rd_quartile <- od_table_coverage_final %>% filter(TOTAL_KTONS_2015>=quartile[2] & TOTAL_KTONS_2015<quartile[3])
od_table_coverage_final_4th_quartile <- od_table_coverage_final %>% filter(TOTAL_KTONS_2015<quartile[2])

###############################################################################################################

library("tidyverse")
library('lwgeom')
library('sf')

rm(list=ls())

setwd("C:/Users/timot/Documents/Albacore_OLE")

###Prepare shapefiles to use in clipping and plotting

Map<-read_sf('shapefiles/Pacific_Landmasses.shp')
IATTC<-read_sf("shapefiles/RFMO_Boundaries/iattc.shp")
WCPFC<-read_sf('shapefiles/RFMO_Boundaries/WCPFC.shp')

Pacific_IATTC<-st_shift_longitude(IATTC)
Pacific_WCPFC<-st_shift_longitude(WCPFC)

Convention_Areas<-rbind(IATTC, WCPFC)
Convention_Areas$area <- st_area(Convention_Areas)
Combined_Convention_Bounds <- Convention_Areas %>% summarise(area = sum(area))

Pacific_Convention_Areas<-rbind(Pacific_IATTC, Pacific_WCPFC)
Pacific_Convention_Areas$area <- st_area(Pacific_Convention_Areas)
Pacific_Combined_Convention_Bounds <- Pacific_Convention_Areas %>% summarise(area = sum(area))

ggplot(Convention_Areas) + geom_sf()
ggplot(Combined_Convention_Bounds) + geom_sf()
ggplot(Pacific_Combined_Convention_Bounds) + geom_sf()


### Read in data
###Input is GFW global data aggregated at either 1.0 or 0.25 resolution

Fishing_Data<-read.csv("GFW_Data/processed_data/global/Daily_2012-2016_fishingeffortmmsi_1.0.csv")
myvars <- c("date", "year", "month", "lon_bin", "lat_bin", "flag", "geartype", "vessel_hours", "fishing_hours", "mmsi_present")
myvars <- c("date", "year", "month", "lon_bin", "lat_bin", "mmsi", "fishing_hours")
Fishing_Data<-Fishing_Data[myvars]

###Clip data to study area extent and re-center on the Pacific
###METHOD 1
Fishing_Data <- st_as_sf(Fishing_Data, coords = c('lon_bin', 'lat_bin'), crs = "+init=epsg:4326")
Fishing_Data<-st_intersection(Fishing_Data, Combined_Convention_Bounds)
Fishing_Data<-st_shift_longitude(Fishing_Data) 
Lon_Lat_Coords<-as.data.frame(st_coordinates(Fishing_Data))
names(Lon_Lat_Coords)<-c("Lon","Lat")
Fishing_Data<-as.data.frame(Fishing_Data)
Fishing_Data<-cbind(Fishing_Data, Lon_Lat_Coords)
Fishing_Data<- Fishing_Data[c(-9,-10)]  ### Change to -6,-7 for mmsi

write.csv(Fishing_Data, "XXX.csv")

###METHOD 2
###Helps with memory limited computers

Unique_Month<-as.data.frame(unique(Fishing_Data$month))
names(Unique_Month)<-c("month")
List_Month<-as.list(as.character(Unique_Month$month))
Complete_Clip<-NULL


for (i in 1:length(List_Month)){
  month = List_Month[i]
  Fishing_Data_Subset <- Fishing_Data[which(Fishing_Data$month == month),]
  Fishing_Data_Subset <- st_as_sf(Fishing_Data_Subset, coords = c('lon_bin', 'lat_bin'), crs = "+init=epsg:4326")
  Fishing_Data_Subset <-st_intersection(Fishing_Data_Subset, Combined_Convention_Bounds)
  Fishing_Data_Subset <-st_shift_longitude(Fishing_Data_Subset) 
  Lon_Lat_Coords<-as.data.frame(st_coordinates(Fishing_Data_Subset))
  names(Lon_Lat_Coords)<-c("Lon","Lat")
  Fishing_Data_Subset <-as.data.frame(Fishing_Data_Subset)
  Fishing_Data_Subset <-cbind(Fishing_Data_Subset, Lon_Lat_Coords)
  Fishing_Data_Subset <-Fishing_Data_Subset [c(-6,-7)]  ### Change to -6,-7 for mmsi, -9,-10 for byflag
  Complete_Clip<-rbind(Complete_Clip, Fishing_Data_Subset)
  print(Sys.time())
}

write.csv(Complete_Clip, "GFW_Data/processed_data/pacific/Daily_Pacific_2012_2016_fishingeffortmmsi_1.0.csv")

###Aggregate By Grid Cell

effort_all <- Complete_Clip %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>%
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 10, 10, log_fishing_hours)) %>% 
  filter(fishing_hours >= 24)


effort_pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'), 
                               interpolate = 'linear')

p1 <- effort_all %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = log_fishing_hours))+
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral") +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3") +
  geom_sf(data = Map, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) + 
  scale_fill_gradientn(
    "Fishing Hours",
    na.value = NA,
    limits = c(1, 5),
    colours = effort_pal(5), # Linear Green
    labels = c("10", "100", "1,000", "10,000", "100,000+"),
    values = scales::rescale(c(0, 1)))+
  labs(fill  = 'Fishing hours (log scale)',
       title = 'Global fishing effort in 2016') +
  guides(fill = guide_colourbar(barwidth = 10))

p1

##### Use this to get landmass shapefiles centered on the Pacific

library(raster)
library(maptools)

L<-shapefile('C:/Users/timot/Documents/Albacore_OLE/shapefiles/ne_110m_land.shp')
plot(L)
worldSpPnrL <- nowrapRecenter(L)

subL <- crop(worldSpPnrL, extent(100, 295, -60, 60))
plot(subL)
Pacific_Landmasses<-as(subL, "SpatialPolygonsDataFrame")
rgdal::writeOGR(obj=Pacific_Landmasses, layer="Pacific_Landmasses", dsn="tempdir", driver="ESRI Shapefile")


###To Join files

Test<-read.csv("X.csv")

First<-read.csv("Pacific_2016_fishingeffortflag_0.25_PartI.csv")
Second<-read.csv("Pacific_2015_fishingeffortflag_0.25_PartII.csv")
Third<-Second<-read.csv("Pacific_2016_fishingeffortflag_0.25.csv")

Fishing_Data<-rbind(First,Second)

### To aggregate monthly

Second<-read.csv("C:/Users/timot/Documents/Albacore_OLE/GFW_Data/processed_data/pacific/Daily_Pacific_2012-2016_fishingeffortflag_1.0.csv")


effort_all <- Second %>% 
  group_by(Lon,Lat, month, year,flag, geartype) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T), vessel_hours = sum(vessel_hours, na.rm = T))

write.csv(effort_all, "GFW_Data/processed_data/pacific/Monthly_Pacific_2012-2016_fishingeffortmmsi_1.0.csv")   

### To subset Fishing_Data

Fishing_Data<-Fishing_Data[which(Fishing_Data$geartype == 'drifting_longlines'),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$year == 2012),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$month == 6|Fishing_Data$month == 7),]
  

          
            
            




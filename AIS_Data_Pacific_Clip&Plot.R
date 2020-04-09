library("tidyverse")
library('lwgeom')
library('sf')

rm(list=ls())

setwd("C:/Users/timot/Documents/Albacore_OLE")

###Input is GFW global data aggregated at either 1.0 or 0.25 resolution
### START here if you want a smaller subset...

Fishing_Data<-read.csv("~/GFW_Data/2012_fishingeffortflag_1.0.csv")
Fishing_Data<-Fishing_Data[which(Fishing_Data$geartype == 'drifting_longlines'),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$month == 6),]
myvars <- c("date", "year", "month", "lon_bin", "lat_bin", "flag", "geartype", "vessel_hours", "fishing_hours", "mmsi_present")
Fishing_Data<-Fishing_Data[myvars]

###START here if you want complete data...

Fishing_Data<-read.csv("~/GFW_Data/2012-2016_fishingeffortflag_1.0.csv")
myvars <- c("date", "year", "month", "lon_bin", "lat_bin", "flag", "geartype", "vessel_hours", "fishing_hours", "mmsi_present")
Fishing_Data<-Fishing_Data[myvars]

getwd()

IATTC<-read_sf("C:/Users/timot/Documents/Albacore_OLE/shapefiles/RFMO_Boundaries/iattc.shp")
WCPFC<-read_sf('C:/Users/timot/Documents/Albacore_OLE/shapefiles/RFMO_Boundaries/WCPFC.shp')

plot(WCPFC)
plot(IATTC)

Convention_Areas<-rbind(IATTC, WCPFC)
Convention_Areas$area <- st_area(Convention_Areas)
Combined_Convention_Bounds <- Convention_Areas %>% summarise(area = sum(area))

ggplot(Convention_Areas) + geom_sf()
ggplot(Combined_Convention_Bounds) + geom_sf()


All_Spatial_Fishing_Data <- st_as_sf(Fishing_Data, coords = c('lon_bin', 'lat_bin'), crs = "+init=epsg:4326")
Pacific_Spatial_Fishing_Data<-st_intersection(All_Spatial_Fishing_Data, Combined_Convention_Bounds)

ggplot(Pacific_Spatial_Fishing_Data) + geom_sf()

###END and export data if all you want is the data to be clipped

##### START if we want to recenter data on the Pacifc

Pacific_Spatial_Fishing_Data<-st_shift_longitude(Pacific_Spatial_Fishing_Data) 
ggplot(Pacific_Spatial_Fishing_Data) + geom_sf()

### write.csv(Pacific_Spatial_Fishing_Data, "X.csv")

###Aggregate By Grid Cell

Lon_Lat_Coords<-as.data.frame(st_coordinates(Pacific_Spatial_Fishing_Data))
names(Lon_Lat_Coords)<-c("Lon","Lat")
Pacific_Fishing_df<-as.data.frame(Pacific_Spatial_Fishing_Data)
Pacific_Fishing_df<-cbind(Pacific_Fishing_df, Lon_Lat_Coords)
Pacific_Fishing_df<- Pacific_Fishing_df[c(-9,-10)]

effort_all <- Pacific_Fishing_df %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>% 
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>% 
  filter(fishing_hours >= 24)


effort_pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'), 
                               interpolate = 'linear')

Map<-read_sf('C:/Users/timot/Documents/Albacore_OLE/shapefiles/Pacific_Landmasses.shp')
Pacific_IATTC<-st_shift_longitude(IATTC)
Pacific_WCPFC<-st_shift_longitude(WCPFC)
Pacific_WCPFC$area <- st_area(Pacific_WCPFC)
Pacific_WCPFC <- Pacific_WCPFC %>% summarise(area = sum(area))

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
    values = scales::rescale(c(0, 1))) +
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







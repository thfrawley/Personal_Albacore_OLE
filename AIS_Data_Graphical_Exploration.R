library("tidyverse")
library('lwgeom')
library('sf')
library('raster')
library(maptools)

rm(list=ls())


setwd("C:/Users/timot/Documents/Albacore_OLE")

###Prepare shapefiles to use in clipping and plotting




Map<-read_sf('shapefiles/Pacific_Landmasses.shp')
IATTC<-read_sf("shapefiles/RFMO_Boundaries/iattc.shp")
WCPFC<-read_sf('shapefiles/RFMO_Boundaries/WCPFC.shp')
EEZ<-read_sf('shapefiles/Pacific_EEZ.shp')
EEZ_Simp<-st_simplify(EEZ, dTolerance = 1.0)
Pacific<-read_sf('shapefiles/Pacific_Ocean.shp')
Pacific_Simp<-st_simplify(Pacific, dTolerance = 1.0)
HighSeas <-st_difference(Pacific_Simp, EEZ_Simp) ### Good One

ggplot(HighSeas) + geom_sf()


Pacific_IATTC<-st_shift_longitude(IATTC)
Pacific_WCPFC<-st_shift_longitude(WCPFC)
Pacific_WCPFC$area<-st_area(Pacific_WCPFC)
Pacific_WCPFC <- Pacific_WCPFC %>% summarise(area = sum(area))

###High Seas Barplots

Small<-read.csv("C:/Users/timot/Documents/Albacore_OLE/GFW_Data/processed_data/pacific/Monthly_Pacific_2012-2016_fishingeffortflag_0.25.csv")
Small_Year<- Small %>% group_by(year,geartype, flag, Lon, Lat) %>%  summarize(fishing_hours = sum(fishing_hours, na.rm = T))
Big <-read.csv("C:/Users/timot/Documents/Albacore_OLE/GFW_Data/processed_data/pacific/Monthly_Pacific_2012-2016_fishingeffortflag_1.0.csv")
Big<-Big[which(Big$Lat > 0 ),]

Big_Year <- Big %>% group_by(year,geartype, flag, Lon, Lat) %>%  summarize(fishing_hours = sum(fishing_hours, na.rm = T))

Big_Year <- st_as_sf(Big_Year, coords = c('Lon', 'Lat'), crs = "+init=epsg:4326")
Fishing_Data <-st_intersection(Big_Year, HighSeas) ### Good One; This process only last 5 minutes

Test<-aggregate(fishing_hours~flag, data=Fishing_Data, FUN=sum)
Test <- Test[order(-Test$fishing_hours),]
Top<-c("CHN", "KOR", "TWN", "JPN", "RUS", "ESP", "VUT", "USA","FJI", "COL", "MEX", "ECU") ### For All
Top<-c("CHN", "KOR", "TWN", "JPN", "RUS", "USA", "MEX", "ESP", "VUT", "COL", "FJI", "MAC") ### For NP 
Top<-c("CHN", "KOR", "TWN", "JPN", "ECU", "USA", "COL", "ESP", "NZL", "COL", "FJI", "VUT") ### For SP
Other <- mutate(Fishing_Data, flag = fct_other(flag, keep = Top, other_level = 'Other')) 

effort_all <- Other %>% 
  group_by(year,flag) %>%  summarize(fishing_hours = sum(fishing_hours, na.rm = T))

effort_all <- Fishing_Data %>% 
  group_by(year,geartype) %>%  summarize(fishing_hours = sum(fishing_hours, na.rm = T))

options(scipen = 999)

ggplot(effort_all, aes(fill=flag, y=fishing_hours, x=year)) + 
  geom_bar(position="stack", stat="identity") + theme_bw() +ylim (0, 5000000)

ggsave("fishing_hours_flag_year.tiff", width=5.5, height=4, units="in", dpi=300) ##For geartype
ggsave("fishing_hours_flag_year.tiff", width=6, height=4, units="in", dpi=300) ##For flag


Fishing_Data<-read.csv("C:/Users/timot/Documents/Albacore_OLE/GFW_Data/processed_data/pacific/Monthly_Pacific_2012-2016_fishingeffortflag_0.25.csv")
Fishing_Data<-Fishing_Data[which(Fishing_Data$flag == 'CHN'),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$geartype == 'drifting_longlines'),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$month == 10|Fishing_Data$month == 11|Fishing_Data$month == 12),]
Fishing_Data<-Fishing_Data[which(Fishing_Data$Lat > 0 ),]

Fishing_Data <- st_as_sf(Fishing_Data, coords = c('Lon', 'Lat'), crs = "+init=epsg:4326")
Fishing_Data <-st_intersection(Fishing_Data, HighSeas)

Lon_Lat_Coords<-as.data.frame(st_coordinates(Fishing_Data))
names(Lon_Lat_Coords)<-c("Lon","Lat")
Fishing_Data<-as.data.frame(Fishing_Data)
Fishing_Data <-cbind(Fishing_Data, Lon_Lat_Coords)



effort_all <- Fishing_Data %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>%
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= .1, .1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 5, 5, log_fishing_hours)) %>% 
  filter(fishing_hours >= 24)

p1 <- effort_all %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = log_fishing_hours)) +
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral", lwd=.5) +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3", lwd=.5) +
  geom_sf(data = Map, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) +
  scale_fill_gradient2(
    "log (Fishing Hours)",
    na.value = NA,
    low="yellow",
    mid="orange",
    high="red",
    midpoint=2.0) + 
  labs(title = '"High Seas" Chinese Longline-Fourth Quarter') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(0,60) + xlim(110,300)
p1    

ggsave("First.tiff", width=9, height=4, units="in", dpi=300)


Pacific<-shapefile('shapefiles/Pacific_Ocean.shp')
Map1<-shapefile('shapefiles/Pacific_Landmasses.shp')
r<-raster(Pacific)

asp<-SpatialPoints(effort_all[,1:2],)
asp <- SpatialPointsDataFrame(asp, effort_all)
res(r)<-0.25
fishing_hours <-rasterize(coordinates(asp), r, asp$fishing_hours, fun='sum', background=0)
log_fishing_hours<-log(fishing_hours)
log_fishing_hours <- disaggregate(log_fishing_hours, 15, method='bilinear')


plot(asp)
plot(log_fishing_hours, ylim=c(0,60), zlim=c(0,10))
plot(Map1, add=TRUE, col='#7f8c8d')



asp <- SpatialPointsDataFrame(asp, Single_Year)

ggsave("First.tiff", width=9, height=4, units="in", dpi=300)




ggplot() + stat_density2d(
  aes(x = Lon, y = Lat, fill = log_fishing_hours),
  data = effort_all,
  geom = "polygon",
  alpha = .4,
  color = "red",
  bins = 20) + scale_fill_distiller(palette = "Spectral") +
  geom_sf(data = Map, fill = '#374a6d', color = '#0A1738',size = 0.1) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(0,60) + xlim(110,300)




ggsave("First.tiff", width=9, height=4, units="in", dpi=300)
getwd()
             

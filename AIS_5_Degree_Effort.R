library("tidyverse")
library('lwgeom')
library('sf')
library('raster')
library(maptools)
library(data.table)

rm(list=ls())


setwd("C:/Users/timot/Documents/Albacore_OLE")

###Prepare shapefiles to use in clipping and plotting

West_EEZ<-read_sf('shapefiles/World_EEZ_v11/eez_v11.shp')
a_poly = st_polygon(list(rbind(c(100, -90), c(100, 90), c(170, 90), c(170, -90), c(100, -90))))
a = st_sfc(a_poly, crs = "+init=epsg:4326")
West_EEZ= West_EEZ %>% filter(st_within(West_EEZ, a, sparse = FALSE))
West_EEZ-st_shift_longitude(West_EEZ)
West_EEZ$area<-st_area(West_EEZ)
West_EEZ<- West_EEZ %>% summarise(area = sum(area)) ### Patience Required
West_EEZ<-st_simplify(West_EEZ, dTolerance = 0.1)

ggplot() + geom_sf(data=West_EEZ)

Pacific<-read_sf('shapefiles/Pacific_Ocean.shp')
Pacific_Simp<-st_simplify(Pacific, dTolerance = 1.0)
HighSeas <-st_difference(Pacific_Simp, West_EEZ) ### Good One

Map<-read_sf('shapefiles/Pacific_Landmasses.shp')
IATTC<-read_sf("shapefiles/RFMO_Boundaries/iattc.shp")
WCPFC<-read_sf('shapefiles/RFMO_Boundaries/WCPFC.shp')
Pacific_IATTC<-st_shift_longitude(IATTC)
Pacific_WCPFC<-st_shift_longitude(WCPFC)
Pacific_WCPFC$area<-st_area(Pacific_WCPFC)
Pacific_WCPFC <- Pacific_WCPFC %>% summarise(area = sum(area))

mycols <- colors()[c(473,562,71,610,655,653,621,34)] 
mypalette <- colorRampPalette(mycols)(255)

###High Seas Barplots


Big <-fread("C:/Users/timot/Documents/Albacore_OLE/GFW_Data/processed_data/pacific/Monthly_Pacific_2012-2016_fishingeffortflag_0.25.csv")
Big<-as.data.frame(Big)
Big<-Big[which(Big$flag=="CHN"),]
Big<-Big[which(Big$gear=="drifting_longlines"),]
Big<-Big[which(Big$month == 1|Big$month ==2|Big$month == 3),]
Big<-Big[which(Big$year== 2016),]

effort_all <- Big %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>%
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= .1, .1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 4, 4, log_fishing_hours)) %>% 
  filter(fishing_hours >= 24)

###START for High Seas Subset
effort_all <- st_as_sf(effort_all, coords = c('Lon', 'Lat'), crs = "+init=epsg:4326")
effort_all <-st_intersection(effort_all, HighSeas)
Lon_Lat_Coords<-as.data.frame(st_coordinates(effort_all))
names(Lon_Lat_Coords)<-c("Lon","Lat")
effort_all<-as.data.frame(effort_all)
effort_all <-cbind(effort_all, Lon_Lat_Coords)
effort_all <-effort_all[which(effort_all$Lon > 145),]
### END for High Seas Subset

p1 <- effort_all %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = log_fishing_hours)) +
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral", lwd=.5) +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3", lwd=.5) +
  geom_sf(data = Map, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) +
  scale_fill_gradientn("log(fishing hours)", colours = mypalette, na.value = NA) +
  labs(title = 'Chinese Longline-2016') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(-60,60) + xlim(110,300)
p1    

ggsave("First.tiff", width=8, height=6, units="in", dpi=300)


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
  labs(title = '"High Seas" Longline (Nation X)- First Quarter (2012-2016)') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(0,60) + xlim(110,300)
p1   
ggsave("First.tiff", width=9, height=4, units="in", dpi=300)

   

####
EEZ<-read_sf('shapefiles/Pacific_EEZ.shp')
EEZ_Simp<-st_simplify(EEZ, dTolerance = 1.0)
Pacific<-read_sf('shapefiles/Pacific_Ocean.shp')
Pacific_Simp<-st_simplify(Pacific, dTolerance = 1.0)
HighSeas <-st_difference(Pacific_Simp, EEZ_Simp)








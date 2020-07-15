getwd()
rm(list=ls())

library(devtools)
library(rspatial)
library(sp)
library(dismo)
library(rgdal)
library(maptools)
library(PBSmapping)
library(raster)
library(gstat)
library(spex)
library(colorRamps)
library(rgeos)


setwd("C:/Users/timot/Documents/Albacore_OLE/NMFS_Data")

AS<- read.csv("1996-2019_PICDR-112990_AS_DETAIL.csv")
AS<- read.csv("1996-2019_PICDR-112990_AS_HDR.csv")



completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

AS<-completeFun(AS, c("BS_LON", "BS_LAT"))
AS <- AS[order(AS$BS_LON),]
AS$BS_LON <- ifelse(AS$BS_LON < 0, AS$BS_LON + 360, AS$BS_LON)
AS<- AS[!(AS$HDR_LANDYR==2020),]

AS <- AS[order(AS$BS_LON),]
AS<- AS[which(AS$HDR_LANDYR == 2014|
              AS$HDR_LANDYR == 2015),]
AS$TOTAL<-AS$NUMKEPT + AS$NUMRELEASED


O<-shapefile('C:/Users/timot/Documents/Albacore_OLE/shapefiles/ne_50m_ocean.shp')
plot(O)
worldSpPnrO <- nowrapRecenter(O)
subO <- crop(worldSpPnrO, extent(186, 196, -18, -8))
plot(subO)

L<-shapefile('C:/Users/timot/Documents/Albacore_OLE/shapefiles/ne_50m_land.shp')
plot(L)
worldSpPnrL <- nowrapRecenter(L)
subL <- crop(worldSpPnrL, extent(186, 196, -18, -8))
plot(subL)


Albacore <- AS[which(AS$ENGLISH_NAME == 'BIGEYE TUNA'),]
Albacore <- Albacore[c("NUMKEPT","NUMRELEASED","TOTAL", "BS_LAT", "BS_LON", "SETDEPTH", "BH_YR", "BH_MON")]

asp<-SpatialPoints(Albacore[, 5:4],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
asp <- SpatialPointsDataFrame(asp, Albacore)
asp<-intersect(asp, subO)
r<-raster(subO)
res(r)<-.25
r<-rasterize(subO, r)
catch<-rasterize(coordinates(asp), r, asp$TOTAL, fun='sum', background=0)
catch <- disaggregate(catch, 15, method='bilinear')

plot(catch)
plot(subL, add=TRUE, col='#7f8c8d',border="black")

###BIG EYE
plot(catch, col=rev(topo.colors(n=100)), zlim=c(0.5,120))
plot

###YELLOWFIN
plot(catch, col=rev(heat.colors(n=100)), zlim=c(0.5,120))
plot(subL, add=TRUE, col='#7f8c8d',border="black")

unique(AS$CATCH_FISHERY)

library(ggplot2)
library(dplyr)
library("tidyverse")

AS<- read.csv("1996-2019_PICDR-112990_AS_DETAIL.csv")


AS$TOTAL<-AS$NUMKEPT + AS$NUMRELEASED
AS<- AS[!(AS$HDR_LANDYR==2020),]


AS<-AS[which(AS$SETDEPTH=='S'),]

Totals<-aggregate(TOTAL~ENGLISH_NAME, FUN=sum, data=AS)
Totals <- Totals[order(Totals$TOTAL),]
                 
Top_Species<-c("ALBACORE", "SKIPJACK TUNA", "YELLOWFIN TUNA", "BIGEYE TUNA", "SWORDFISH")
AS <- mutate(AS, ENGLISH_NAME = fct_other(ENGLISH_NAME, keep = Top_Species, other_level = 'Other')) 

Yearly<-aggregate(TOTAL~BH_MON +ENGLISH_NAME, FUN=sum, data=AS)
ggplot(Yearly, aes(fill=ENGLISH_NAME, y=TOTAL, x=BH_MON)) + 
  geom_bar(position="stack", stat="identity")

Year_Total<-aggregate(TOTAL~BH_MON, FUN=sum, data=AS)
Percent_Total<-merge(Yearly, Year_Total, by="BH_MON", all.x=TRUE)
Percent_Total$Percent<-Percent_Total$TOTAL.x/Percent_Total$TOTAL.y

percent <- function(x, digits = 2, format = "f", ...)
{paste(formatC(100 * x, format = format, digits = digits, ...), "%", sep = "")}
Percent_Total$Label<-percent(Percent_Total$Percent)

ggplot(Percent_Total, aes(fill=ENGLISH_NAME, y=Percent, x=BH_MON)) + 
  geom_bar(position="stack", stat="identity") +  geom_text(aes(label = Label), size = 2, hjust = 0.25, vjust=3, position='stack')

Percent_Total<- Percent_Total[!(Percent_Total$ENGLISH_NAME=='ALBACORE'),]
Percent_Total<- Percent_Total[!(Percent_Total$ENGLISH_NAME=='Other'),]
ggplot(Percent_Total, aes(fill=ENGLISH_NAME, color=ENGLISH_NAME, y=Percent, x=BH_MON)) + 
  geom_smooth() 


####For Hawaii 
getwd()
AS <- read.csv("2019_PICDR-112990_HI-CA_DETAIL.csv")
AS <- read.csv("1990-2018_PICDR-112775_DETAIL.csv")
AS$BS_LON <- ifelse(AS$BS_LON < 0, AS$BS_LON + 360, AS$BS_LON)
AS <- AS[order(AS$BS_LAT),]
AS<- AS[!(AS$HDR_LANDYR==2020),]
AS<- AS[!(AS$HDR_LANDYR==2018),]
AS<- AS[!(AS$HDR_LANDYR==2019),]
AS$TOTAL<-AS$NUMKEPT + AS$NUMRELEASED

O<-shapefile('C:/Users/timot/Documents/Albacore_OLE/shapefiles/ne_50m_ocean.shp')
plot(O)
worldSpPnrO <- nowrapRecenter(O)
subO <- crop(worldSpPnrO, extent(180, 245, 10, 40))
plot(subO)

L<-shapefile('C:/Users/timot/Documents/Albacore_OLE/shapefiles/ne_50m_land.shp')
plot(L)
worldSpPnrL <- nowrapRecenter(L)
subL <- crop(worldSpPnrL, extent(180, 245, 10, 40))
plot(subL)


Albacore <- AS[which(AS$ENGLISH_NAME == 'SKIPJACK TUNA'),]
Albacore <- Albacore[c("NUMKEPT","NUMRELEASED","TOTAL", "BS_LAT", "BS_LON", "SETDEPTH", "BH_YR", "BH_MON")]

asp<-SpatialPoints(Albacore[, 5:4],proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
asp <- SpatialPointsDataFrame(asp, Albacore)
r<-raster(subO)
res(r)<-1
r<-rasterize(subO, r)
catch<-rasterize(coordinates(asp), r, asp$TOTAL, fun='sum', background=0)
catch <- disaggregate(catch, 15, method='bilinear')

plot(catch)
plot(subL, add=TRUE, col='#7f8c8d',border="black")






AIS Center of Gravity 

rm(list=ls())
setwd("C:/Users/timot/Documents/Albacore_OLE")

fishing_data <-read.csv("GFW_Data/processed_data/pacific/Monthly_Pacific_2016_fishingeffortmmsi_0.25.csv")   
Map<-read_sf('shapefiles/Pacific_Landmasses.shp')


### To look at and plot a single vessel

Top_Vessels <- fishing_data %>% 
  group_by(mmsi) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T))
  
Top_Vessels <- Top_Vessels[order(-Top_Vessels$fishing_hours),]

Single_Vessel<-fishing_data[which(fishing_data$mmsi==412326799),]	

Single_effort <- Single_Vessel %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup() %>%
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
         log_fishing_hours = ifelse(log_fishing_hours >= 10, 10, log_fishing_hours))

effort_pal <- colorRampPalette(c('#0C276C', '#3B9088', '#EEFF00', '#ffffff'), 
                               interpolate = 'linear')

p1 <- Single_effort %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = fishing_hours)) +
  geom_sf(data = Map, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) + 
  scale_fill_gradientn(
    "Fishing Hours",
    na.value = NA,
    colours = effort_pal(5)) + # Linear Green
  labs(fill  = 'Fishing hours',
       title = 'Global fishing effort in 2016') +
  guides(fill = guide_colourbar(barwidth = 10))

p1

######

Single_COG <- cgi(x = Single_effort$Lon, y = Single_effort$Lat, Single_effort$fishing_hours, plot = F)
Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2], Single_COG$xcg),
                           lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2], Single_COG$ycg),group = c("A", "A", "B", "B","C"))
Point_Coord<-Single_COG[which(Single_COG$group=="C"),]	
Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]	
Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]	

p1 <- Single_effort %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = fishing_hours)) +
  geom_point(data=Point_Coord, aes(x=lon, y=lat, colour='red'))+
  geom_line(data=Line_Coord_A, aes(x=lon, y=lat, colour='red')) +
  geom_line(data=Line_Coord_B, aes(x=lon, y=lat, colour='red')) +
  geom_sf(data = Map, 
          fill = '#374a6d', 
          color = '#0A1738',
          size = 0.1) + 
  scale_fill_gradientn(
    "Fishing Hours",
    na.value = NA,
    colours = effort_pal(5)) + # Linear Green
  labs(fill  = 'Fishing hours',
       title = 'Global fishing effort in 2016') +
  guides(fill = guide_colourbar(barwidth = 10))

p1

#### Get One COG

library("data.table")

rm(list=ls())

fishing_data <-read.csv("GFW_Data/processed_data/pacific/Monthly_Pacific_2016_fishingeffortmmsi_0.25.csv")

registry<-read.csv("GFW_Data/raw_data/fishing_vessels/fishing_vessels.csv")
registry<-registry[which(registry$geartype == 'drifting_longlines'),]
registry<-registry[which(registry$active_2016 == 'true'),]

Vessels<-as.data.frame(unique(registry$mmsi))
names(Vessels)<-c("mmsi")
fishing_data<-setDT(fishing_data)[mmsi %chin% Vessels$mmsi]

Selected_Vessels<-fishing_data
                      

load.file <- function(filename) {
  vessel_name<- as.character(unique(filename$mmsi))
  Single_COG <- cgi(x = filename$Lon, y = filename$Lat, filename$fishing_hours, plot = F)
  Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2], Single_COG$xcg),
                           lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2], Single_COG$ycg),group = c("A", "A", "B", "B","C"))
  Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
  Value<-as.data.frame(c(vessel_name, Point_Coord$lon, Point_Coord$lat))
}


Vessels.split<-split(Selected_Vessels, Selected_Vessels$mmsi)

data <- lapply(Vessels.split, load.file)
output <- matrix(unlist(data), ncol = 3, byrow = TRUE)
COG<-as.data.frame(output)
names(COG)<-c("mmsi", "Lon", "Lat")

####Get Quartlerly COG
rm(list=ls())

fishing_data <-read.csv("GFW_Data/processed_data/pacific/Monthly_Pacific_2016_fishingeffortmmsi_0.25.csv")

registry<-read.csv("GFW_Data/raw_data/fishing_vessels/fishing_vessels.csv")
registry<-registry[which(registry$geartype == 'drifting_longlines'),]
registry<-registry[which(registry$active_2016 == 'true'),]

Vessels<-as.data.frame(unique(registry$mmsi))
names(Vessels)<-c("mmsi")
fishing_data<-setDT(fishing_data)[mmsi %chin% Vessels$mmsi]


fishing_data$month <-as.factor(fishing_data$month)
fishing_data$quarter <- fct_collapse(fishing_data$month,
                                     one = c("1", "2", "3"),
                                     two = c("4", "5", "6"),
                                     three = c("7", "8", "9"),
                                     four = c("10", "11", "12"))
fishing_data$quarter<- factor(fishing_data$quarter, levels = c("one", "two", "three", "four"))

fishing_data <- fishing_data %>% 
  group_by(Lon,Lat, mmsi, quarter) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T))
fishing_data <- fishing_data[order(fishing_data$quarter),]

Selected_Vessels<-fishing_data

load.file <- function(filename) {
  vessel_name<- as.character(unique(filename$mmsi))
  Single_COG <- cgi(x = filename$Lon, y = filename$Lat, filename$fishing_hours, plot = F)
  Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2], Single_COG$xcg),
                           lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2], Single_COG$ycg),group = c("A", "A", "B", "B","C"))
  Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
  Value<-as.data.frame(c(vessel_name, Point_Coord$lon, Point_Coord$lat))
}

Unique_Quarter<-as.data.frame(unique(Selected_Vessels$quarter)) #determine vessel trip ids
names(Unique_Quarter) <- c("quarter")
List_Quarter<-as.list(as.character(Unique_Quarter$quarter))

df<- as.data.frame(unique(Selected_Vessels$mmsi))
names(df) <- c("mmsi")

for (i in 1:length(List_Quarter)){
  Quarter = List_Quarter[i]
  Single_Quarter <- Selected_Vessels[which(Selected_Vessels$quarter == Quarter),]
  Vessels.split<-split(Single_Quarter, Single_Quarter$mmsi)
  data <- lapply(Vessels.split, load.file)
  output <- matrix(unlist(data), ncol = 3, byrow = TRUE)
  COG1<-as.data.frame(output)
  COG1$quarter<-Quarter
  names(COG1)<-c("mmsi", "Lon", "Lat", "quarter")
  df=merge(df, COG1, by="mmsi", all.x=TRUE)
  names(df) <- c("mmsi")
  colnames(df)[1] <- "mmsi"
}

colnames(df) <- c("mmsi","Lon1", "Lat1", "quarter1", "Lon2", "Lat2", "quarter2", "Lon3", "Lat3", "quarter3", "Lon4", "Lat4", "quarter4")
myvars <- c("mmsi","Lon1", "Lat1", "Lon2", "Lat2", "Lon3", "Lat3", "Lon4", "Lat4" )
Quarterly_Vessel_COG<-df[myvars]

##### Get Monthly COG

Top_Vessels <- fishing_data %>% 
  group_by(mmsi) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T))
Top_Vessels <- Top_Vessels[order(-Top_Vessels$fishing_hours),]


###Do you want to take a subset of vessels?
Selected_Vessels<-Top_Vessels[which(Top_Vessels$fishing_hours > 3000),]
Selected_Vessels<-Top_Vessels[c(13500:13530),]

Vessels<-as.data.frame(unique(Selected_Vessels$mmsi))
names(Vessels)<-c("mmsi")
Selected_Vessels<-setDT(fishing_data)[mmsi %chin% Vessels$mmsi]


load.file <- function(filename) {
  vessel_name<- as.character(unique(filename$mmsi))
  Single_COG <- cgi(x = filename$Lon, y = filename$Lat, filename$fishing_hours, plot = F)
  Single_COG <- data.frame(lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2], Single_COG$xcg),
                           lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2], Single_COG$ycg),group = c("A", "A", "B", "B","C"))
  Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
  Value<-as.data.frame(c(vessel_name, Point_Coord$lon, Point_Coord$lat))
}

Unique_Month<-as.data.frame(unique(Selected_Vessels$month)) #determine vessel trip ids
names(Unique_Month) <- c("month")
List_Month<-as.list(as.character(Unique_Month$month))

df<- as.data.frame(unique(Selected_Vessels$mmsi))
names(df) <- c("mmsi")

for (i in 1:length(List_Month)){
  Month = List_Month[i]
  Single_Month <- Selected_Vessels[which(Selected_Vessels$month == Month),]
  Vessels.split<-split(Single_Month, Single_Month$mmsi)
  data <- lapply(Vessels.split, load.file)
  output <- matrix(unlist(data), ncol = 3, byrow = TRUE)
  COG1<-as.data.frame(output)
  COG1$month<-Month
  names(COG1)<-c("mmsi", "Lon", "Lat", "month")
  df=merge(df, COG1, by="mmsi", all.x=TRUE)
  names(df) <- c("mmsi")
  colnames(df)[1] <- "mmsi"
}


colnames(df) <- c("mmsi","Lon1", "Lat1", "month1", "Lon2", "Lat2", "month2", "Lon3", "Lat3", "month3", "Lon4", "Lat4", "month4",
                  "Lon5", "Lat5", "month5", "Lon6", "Lat6", "month6", "Lon7", "Lat7", "month7", "Lon8", "Lat8", "month8", "Lon9", "Lat9", "month9",
                  "Lon10", "Lat10", "month10", "Lon11", "Lat11", "month11", "Lon12", "Lat12", "month12")
myvars <- c("mmsi","Lon1", "Lat1", "Lon2", "Lat2","Lon3", "Lat3", "Lon4", "Lat4", 
            "Lon5", "Lat5", "Lon6", "Lat6", "Lon7", "Lat7", "Lon8", "Lat8", "Lon9", "Lat9",
            "Lon10", "Lat10", "Lon11", "Lat11", "Lon12", "Lat12")
Monthly_Vessel_COG<-df[myvars]


#####

library("NbClust")
library("cluster")
library("factoextra")
library("magrittr")
library('tidyverse')


Quarterly_Vessel_COG<-Quarterly_Vessel_COG[complete.cases(Quarterly_Vessel_COG),]
Clustering_COG<-Quarterly_Vessel_COG

Clustering_COG$Lon1<-as.character(Clustering_COG$Lon1)
Clustering_COG$Lon1<-as.numeric(Clustering_COG$Lon1)
Clustering_COG$Lat1<-as.character(Clustering_COG$Lat1)
Clustering_COG$Lat1<-as.numeric(Clustering_COG$Lat1)
Clustering_COG$Lon2<-as.character(Clustering_COG$Lon2)
Clustering_COG$Lon2<-as.numeric(Clustering_COG$Lon2)
Clustering_COG$Lat2<-as.character(Clustering_COG$Lat2)
Clustering_COG$Lat2<-as.numeric(Clustering_COG$Lat2)
Clustering_COG$Lon3<-as.character(Clustering_COG$Lon3)
Clustering_COG$Lon3<-as.numeric(Clustering_COG$Lon3)
Clustering_COG$Lat3<-as.character(Clustering_COG$Lat3)
Clustering_COG$Lat3<-as.numeric(Clustering_COG$Lat3)
Clustering_COG$Lon4<-as.character(Clustering_COG$Lon4)
Clustering_COG$Lon4<-as.numeric(Clustering_COG$Lon4)
Clustering_COG$Lat4<-as.character(Clustering_COG$Lat4)
Clustering_COG$Lat4<-as.numeric(Clustering_COG$Lat4)

Clustering_COG_Map<-Clustering_COG

rownames(Clustering_COG) <- Clustering_COG[,1]
Clustering_COG <- Clustering_COG[,-1]

Boat_data <- Clustering_COG %>% scale() 

###These are the two clustering methods
fviz_nbclust(Boat_data, kmeans, method ="gap_stat")
res.nbclust <- Boat_data %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")

km.res <- kmeans(Boat_data, 4, nstart = 25)
fviz_cluster(km.res, data = Boat_data, ellipse.type = "convex", palette = "jco", geom=c('point'), ggtheme = theme_minimal())

res.hc <- Boat_data %>% dist(method = "euclidean") %>% hclust(method = "ward.D2")
memb <- cutree(res.hc, k = 4)
Boat_Groups<-as.data.frame(Boat_data)
Boat_Groups$memb<-memb
Boat_Groups <- tibble::rownames_to_column(Boat_Groups, "mmsi")

Boat_Groups<-merge(Boat_Groups, registry, by="mmsi", all.x=TRUE)


Single_Group<-Boat_Groups[which(Boat_Groups$memb==4),]
Single_Group<-Single_Group[c(1,10,11,13,14,15)] 
Single_Group<-merge(Single_Group, Clustering_COG_Map, by="mmsi", all.x=TRUE)

Test<-Single_Group %>% 
  group_by(flag) %>%
  summarise(no = length(flag))

Total<-sum(Test$no)
Test$percentage<-Test$no/Total
Test <- Test[order(-Test$no),]

Test<-Single_Group %>% 
  group_by(memb) %>%
  summarise(length = mean(length, na.rm = T))

Test<-Single_Group %>% 
  group_by(memb) %>%
  summarise(length = sd(length, na.rm = T))

Test<-Single_Group %>% 
  group_by(memb) %>%
  summarise(tonnage = mean(tonnage, na.rm = T))

Test<-Single_Group %>% 
  group_by(memb) %>%
  summarise(tonnage = sd(tonnage, na.rm = T))

library(viridis)
library(MASS)

ggplot() + stat_density_2d(data = Single_Group, aes(Lon4, Lat4, fill = stat(scale(log1p(level)))), geom = "polygon", n = 200) +
  scale_fill_gradient(low = "yellow1", high = "red", guide = guide_legend(label.position = "bottom", title = "Level")) +
  geom_sf(data = Map, fill = '#999999', color = '#0A1738', size = 0.1) + theme_bw()  


ggplot() + geom_point(data = Single_Group, aes(Lon3, Lat3)) + geom_sf(data = Map, fill = '#999999', color = '#0A1738', size = 0.1) + theme_bw()  










  geom_point(aes(x = Lon2, y = Lat2)) + geom_density_2d()
  
  theme_bw() 

geom_point(aes(x = Lon2, y = Lat2, color=density2)) + scale_color_viridis() + 


p1 

str(Single_Group)



  
rownames(Vessels_Ports)

library(dplyr)

Test<-registry %>% 
  group_by(geartype) %>%
  summarise(no_rows = length(geartype))


###Move to clustering








### Density Function

get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

set.seed(1)
dat <- data.frame(
  x = c(
    rnorm(1e4, mean = 0, sd = 0.1),
    rnorm(1e3, mean = 0, sd = 0.1)
  ),
  y = c(
    rnorm(1e4, mean = 0, sd = 0.1),
    rnorm(1e3, mean = 0.1, sd = 0.2)
  )
)








#----COG function----
"cgi" <-  function(x = long, y = lat, z = NA, w = NA, modproj = NA, mlong = NA, 
                   mlat = NA, col = 1, plot = T)
{
  #===============================================================================
  # CENTER OF GRAVITY, INERTIA AND ISOTROPY
  #
  # Routine from Geostatistics for Estimating Fish Abundance (GEFA)
  # & EU program Fisboat, DG-Fish, STREP n° 502572
  # Authors : M.Woillez (Mines-ParisTech), N.Bez (IRD) 
  #           and J.Rivoirard (Mines-ParisTech)
  # Last update : 01 march 2008 
  #
  # Argument:
  #	x	      The x-coordinate (MUST be a vector).
  #	y	      The y-coordinates (MUST be a vector).
  #	z	      The regionalised variable in 2d (MUST be a vector). 
  #         If missing, the results of 'cgi' will concern the samples only.
  # w	      Optional. A weight or a area of influence. Set to 1 if missing
  #	modproj	Optional. Indicates the type of projection to perform.
  # mlong   mean longitude in DEGREES of the data set to be transformed
  # mlat    mean latitude in DEGREES of the data set to be transformed
  #	        See 'dg2nm' for precisions.
  #	col	    Color for representing the axes.
  #	plot	  If plot=T the principal axes of the inertia are automatically 
  #		      plotted on an ALREADY EXISTING figure.
  #
  #	The output consists in a list with :
  #	xcg, ycg	    the coordinates of the center of gravity of z
  #	I	            the value of the inertia of z around its center of gravity
  # Imax          the value of the inertia of z according to the first principal 
  #               axes of the inertia
  # Imin          the value of the inertia of z according to the second principal 
  #               axes of the inertia
  #	Iso           the value of the isotropy of z
  # xaxe1, yaxe1  the coordinates of the first principal axes of the inertia of z
  # xaxe2, yaxe2	the coordinates of the second principal axes of the inertia of z
  #
  #===============================================================================
  
  miss <- function(x){
    length(x) == 1 && is.na(x)
  }
  if(miss(z))
    z <- rep(1, length(x))
  if(miss(w))
    w <- rep(1, length(x))
  sel <- !is.na(x * y * z * w)
  x <- x[sel]
  y <- y[sel]
  z <- z[sel]
  w <- w[sel]
  if(length(x[!is.na(x)]) > 0) {
    if(!miss(modproj)) {
      bid <- dg2nm(x = x, y = y, modproj = modproj, mlong = mlong, mlat = mlat)
      x <- bid$x
      y <- bid$y
    }
    # Center of gravity coordinates
    xg <- sum(x * z * w)/sum(z * w)
    yg <- sum(y * z * w)/sum(z * w)
    
    # Inertia
    dx <- x - xg
    dy <- y - yg
    d <- sqrt(dx^2 + dy^2)
    inert <- sum(z * w * (d^2))/sum(z * w)
    I <- inert	
    
    # Weigthed PCA 
    if(!is.na(I)) {
      M11 <- sum(dx^2 * z * w)
      M22 <- sum(dy^2 * z * w)
      M21 <- sum(dx * dy * z * w)
      M12 <- M21
      M <- matrix(c(M11, M12, M21, M22), byrow = T, ncol = 2)
      x1 <- eigen(M)$vectors[1, 1]
      y1 <- eigen(M)$vectors[2, 1]
      x2 <- eigen(M)$vectors[1, 2]
      y2 <- eigen(M)$vectors[2, 2]
      r1 <- eigen(M)$values[1]/(eigen(M)$values[1] + eigen(M)$values[2])
      
      # Principal axis coordinates
      e1 <- (y1/x1)^2
      sx1 <- x1/abs(x1)
      sy1 <- y1/abs(y1)
      sx2 <- x2/abs(x2)
      sy2 <- y2/abs(y2)
      xa <- xg + sx1 * sqrt((r1 * inert)/(1 + e1))
      ya <- yg + sy1 * sqrt((r1 * inert)/(1 + (1/e1)))
      xb <- 2 * xg - xa
      yb <- 2 * yg - ya
      xc <- xg + sx2 * sqrt(((1 - r1) * inert)/(1 + (1/e1)))
      yc <- yg + sy2 * sqrt(((1 - r1) * inert)/(1 + e1))
      xd <- 2 * xg - xc
      yd <- 2 * yg - yc
      Imax <- r1*inert 
      Imin <- (1-r1)*inert
      Iso <- sqrt(Imin/Imax)
    }
    else {
      xa <- NA
      ya <- NA
      xb <- NA
      yb <- NA
      xc <- NA
      yc <- NA
      xd <- NA
      yd <- NA
      Imax <- NA
      Imin <- NA
      Iso <- NA
    }
    if(!miss(modproj)) {
      bid <- nm2dg(x = c(xg, xa, xb, xc, xd), y = c(yg, ya, yb, yc, yd), 
                   modproj = modproj, mlong = mlong, mlat = mlat)
      res <- list(xcg = bid$x[1], ycg = bid$y[1], I = I, Imax = Imax, 
                  Imin = Imin, Iso = Iso, xaxe1 = bid$x[2:3], yaxe1 = bid$y[2:3], 
                  xaxe2 = bid$x[4:5],	yaxe2 = bid$y[4:5])
    }
    else res <- list(xcg = xg, ycg = yg, I = I, Imax = Imax, Imin = Imin, 
                     Iso = Iso, xaxe1 = c(xa, xb), yaxe1 = c(ya, yb), xaxe2 = c(xc, xd), 
                     yaxe2 = c(yc, yd))
    if(plot == T) {
      segments(res$xaxe1[1], res$yaxe1[1], res$xaxe1[2], res$yaxe1[2], col = col)
      segments(res$xaxe2[1], res$yaxe2[1], res$xaxe2[2], res$yaxe2[2], col = col)
    }
  }
  else {
    res <- list(xcg = NA, ycg = NA, I = NA, Imax = NA, 
                Imin = NA, Iso = NA, xaxe1 = NA, yaxe1 = NA, xaxe2 = NA, yaxe2 = NA)
  }
  res
}



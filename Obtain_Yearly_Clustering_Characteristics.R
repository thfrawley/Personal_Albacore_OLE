AIS Center of Gravity 

library(ggplot2)
library(lubridate)
library(ncdf4)
library(reshape2)
library(dplyr)
library("tidyverse")
library('lwgeom')
library('sf')
library('raster')
library(maptools)
library("data.table")
library("NbClust")
library("cluster")
library("factoextra")
library("magrittr")
library('tidyverse')
library(viridis)
library(MASS)


rm(list=ls())
setwd("C:/Users/timot/Documents/Albacore_OLE")

fishing_data <-read.csv("GFW_Data/processed_data/pacific/Monthly_Pacific_2016_fishingeffortmmsi_0.25.csv")   

### To look at and plot a single vessel

Top_Vessels <- fishing_data %>% 
  group_by(mmsi) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T))
  
Top_Vessels <- Top_Vessels[order(-Top_Vessels$fishing_hours),]

Single_Vessel<-fishing_data[which(fishing_data$mmsi==	205366010),]	

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
Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]	
pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)

p1 <- Single_effort %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = fishing_hours)) +
  geom_point(data=Point_Coord, aes(x=lon, y=lat, colour='red')) +
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
  Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
  Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]	
  Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
  Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
  Value<-as.data.frame(c(vessel_name, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B))
}


Vessels.split<-split(Selected_Vessels, Selected_Vessels$mmsi)

data <- lapply(Vessels.split, load.file) ###Make sure COG function is loaded
output <- matrix(unlist(data), ncol = 5, byrow = TRUE)
COG<-as.data.frame(output)
names(COG)<-c("mmsi", "Lon", "Lat", "Distance_A", "Distance_B")
COG$Distance_A <- sub(NaN, 0, COG$Distance_A) ### Replace NaN with 0
COG$Distance_B <- sub(NaN, 0, COG$Distance_B)

Cluster_df<-merge(COG,registry, by="mmsi", all.x=TRUE)
myvars <- c("mmsi", "Lon", "Lat", "Distance_A", "Distance_B", "length")
Cluster_df<-Cluster_df[myvars]
Unscaled_Characteristics<-Cluster_df
rownames(Cluster_df) <- Cluster_df[,1]
Cluster_df <- Cluster_df[,-1]
Cluster_df<-Cluster_df[complete.cases(Cluster_df),]



Cluster_df$Lon<-as.character(Cluster_df$Lon)
Cluster_df$Lat<-as.character(Cluster_df$Lat)
Cluster_df$Lon<-as.numeric(Cluster_df$Lon)
Cluster_df$Lat<-as.numeric(Cluster_df$Lat)
Cluster_df$Distance_A<-as.numeric(Cluster_df$Distance_A)
Cluster_df$Distance_B<-as.numeric(Cluster_df$Distance_B)
str(Cluster_df)


#### Start your clustering
Boat_data <- Cluster_df %>% scale() 

###First determine the # of Clusters you want
fviz_nbclust(Boat_data, kmeans, method ="gap_stat") ###BootStraps
res.nbclust <- Boat_data %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all")

##Next visualize and assign membership
km.res <- kmeans(Boat_data, 5, nstart = 25)
fviz_cluster(km.res, data = Boat_data, ellipse.type = "convex", palette = "jco", geom=c('point'), ggtheme = theme_minimal())
res.hc <- Boat_data %>% dist(method = "euclidean") %>% hclust(method = "ward.D2")
memb <- cutree(res.hc, k = 5)
Boat_Groups<-as.data.frame(Boat_data)
Boat_Groups$memb<-memb
Boat_Groups <- tibble::rownames_to_column(Boat_Groups, "mmsi")
Boat_Specs_Scaled<-Boat_Groups
Boat_Groups<-Boat_Groups[c(1,7)]


##### Check out the results

Vessel_Specs<-merge(Boat_Groups, Unscaled_Characteristics, by="mmsi", all.x=TRUE)

Vessel_Specs_Scaled<-Boat_Specs_Scaled
Vessel_Specs_Scaled<-Vessel_Specs_Scaled[(-1)]



Group_Stats<-Vessel_Specs_Scaled %>% group_by(memb) %>% summarise_each(funs(mean, sd))
Group_Lon<-Group_Stats[c(1,2,7)]
Group_Lon$Var<-"Longitude_COG"
names(Group_Lon)<- c("memb", "mean", "sd", "Variable")
Group_Lat<-Group_Stats[c(1,3,8)]
Group_Lat$Var<-"Latitude_COG"
names(Group_Lat)<- c("memb", "mean", "sd", "Variable")
Group_IA<-Group_Stats[c(1,4,9)]
Group_IA$Var<-"Inertia_A"
names(Group_IA)<- c("memb", "mean", "sd", "Variable")
Group_IB<-Group_Stats[c(1,5,10)]
Group_IB$Var<-"Inertia_B"
names(Group_IB)<- c("memb", "mean", "sd", "Variable")
Group_Length<-Group_Stats[c(1,6,11)]
Group_Length$Var<-"Length"
names(Group_Length)<- c("memb", "mean", "sd", "Variable")

Group_Stats_Wide<-rbind(Group_Lon, Group_Lat, Group_IA, Group_IB, Group_Length)

ggplot(Group_Stats_Wide, aes(memb, y=mean, fill=Variable)) + geom_bar(stat='identity', position=position_dodge(.9), color="black") + 
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd, group=Variable), width = 0.4, position=position_dodge(.9)) + 
  theme_bw()


##### 


Single_Group<-Vessel_Specs[which(Vessel_Specs$memb==5),]
Member_Vessels<-as.data.frame(unique(Single_Group$mmsi))
names(Member_Vessels)<-c("mmsi")
Member_Vessels$mmsi<-as.character(Member_Vessels$mmsi)
Member_Vessels$mmsi<-as.numeric(Member_Vessels$mmsi)

Member_data<-setDT(fishing_data)[mmsi %chin% Member_Vessels$mmsi]

Member_effort <- Member_data %>% 
  group_by(Lon,Lat) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T),
            log_fishing_hours = log10(sum(fishing_hours, na.rm = T))) %>% 
  ungroup()  %>% 
  mutate(log_fishing_hours = ifelse(log_fishing_hours <= 1, 1, log_fishing_hours),
       log_fishing_hours = ifelse(log_fishing_hours >= 10, 10, log_fishing_hours))



### Plot it up
Map<-read_sf('shapefiles/Pacific_Landmasses.shp')

p1 <- Member_effort %>%
  ggplot() +
  geom_raster(aes(x = Lon, y = Lat, fill = log_fishing_hours)) +
  geom_sf(data = Map, fill = '#999999', color = '#0A1738', size = 0.1) + theme_bw()  

plot(p1)

Single_Group$Lon<-as.character(Single_Group$Lon)
Single_Group$Lat<-as.character(Single_Group$Lat)
Single_Group$Lon<-as.numeric(Single_Group$Lon)
Single_Group$Lat<-as.numeric(Single_Group$Lat)

ggplot() + stat_density_2d(data = Single_Group, aes(Lon, Lat, fill = stat(scale(log1p(level)))), geom = "polygon", n = 200) +
  scale_fill_gradient(low = "yellow1", high = "red", guide = guide_legend(label.position = "bottom", title = "Level")) +
  geom_sf(data = Map, fill = '#999999', color = '#0A1738', size = 0.1) + theme_bw()  











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

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  
  library(plyr) ### Must detach dplyr
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


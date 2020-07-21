setwd("C:/Users/timot/Documents/Albacore_OLE/RFMO_Data")
rm(list=ls())


library(ggplot2)
library("tidyverse")

Registry<-read.csv("Partial_Registry_Clean.csv", na.strings=c("", "NA"))
Registry$ID <- seq.int(nrow(Registry))
Registry$Date<-as.POSIXct(Registry$Version_Date, format="%m/%d/%Y")
Registry$Year <- as.numeric(format(Registry$Date,'%Y'))

###Set Timeframe
Registry<- Registry[!(Registry$Year==2008),]
Registry<- Registry[!(Registry$Year==2020),]

Registry<- Registry[which(Registry$Year==2016),]
                  

myvars <- c("ID", "Year", "WCPFC_Vessel_ID", "Version_Number", "Version_Date", "CCM_Flag", "Vessel_Type", "Master_CCM" ,
            "Vessel_Tonnage", "Vessel_Auth_Type", "Vessel_Auth_Species_Cleaned", "Vessel_Previous_Flags", "Vessel_Length", "Vessel_Length_Units",
            "Owner_Name", "Owner_Address", "Port_RegisteredIn", "Chartering_CCM")
Registry <- Registry[myvars]
Registry<- Registry[!(Registry$Vessel_Type=='Seiner'),]
Registry<- Registry[!(Registry$Vessel_Type=='Trawler'),]

Registry <- Registry %>% mutate(Length_M = case_when(Vessel_Length_Units == "Meters" ~ Vessel_Length,
                                                     Vessel_Length_Units == "Feet" ~ Vessel_Length * 0.3048,
                                                     TRUE ~ Vessel_Length))


Registry$Vessel_Tonnage<-as.factor(Registry$Vessel_Tonnage)
Dominant_Tonnage= Registry %>% group_by(WCPFC_Vessel_ID) %>% summarize(Vessel_Tonnage=names(which.max(table(Vessel_Tonnage))))
Registry<-merge(Registry, Dominant_Tonnage, by="WCPFC_Vessel_ID", all.x=TRUE)
Registry$Vessel_Tonnage.x<-Registry$Vessel_Tonnage.y
Registry <- Registry[ , -which(names(Registry) %in% c("Vessel_Tonnage.y"))]

Registry$Length_M<-as.factor(Registry$Length_M)
Dominant_Length= Registry %>% group_by(WCPFC_Vessel_ID) %>% summarize(Length_M=names(which.max(table(Length_M))))
Registry<-merge(Registry, Dominant_Length, by="WCPFC_Vessel_ID", all.x=TRUE)
Registry$Vessel_Length<-Registry$Length_M.y
Registry <- Registry[ , -which(names(Registry) %in% c("Vessel_Length_Units", "Length_M.x", "Length_M.y"))]

Dominant_Flag= Registry %>% group_by(WCPFC_Vessel_ID) %>% summarize(CCM_Flag=names(which.max(table(CCM_Flag))))
Registry<-merge(Registry, Dominant_Flag, by="WCPFC_Vessel_ID", all.x=TRUE)
Registry$CCM_Flag.x<-Registry$CCM_Flag.y
Registry <- Registry[ , -which(names(Registry) %in% c("CCM_Flag.y"))]
names(Registry) <- gsub("CCM_Flag.x", "CCM_Flag", names(Registry))

Dominant_Port= Registry %>% group_by(WCPFC_Vessel_ID) %>% summarize(Port_RegisteredIn =names(which.max(table(Port_RegisteredIn))))
Registry<-merge(Registry, Dominant_Port, by="WCPFC_Vessel_ID", all.x=TRUE)
Registry$Port_RegisteredIn.x<-Registry$Port_RegisteredIn.y
Registry <- Registry[ , -which(names(Registry) %in% c("Port_RegisteredIn.y"))]
names(Registry) <- gsub("Port_RegisteredIn.x", "Port_RegisteredIn", names(Registry))


Vessels<- unique(Registry[,c('WCPFC_Vessel_ID', 'CCM_Flag', 'Port_RegisteredIn', 'Vessel_Length', "Vessel_Tonnage.x")])
Vessels$Vessel_Length<-as.numeric(Vessels$Vessel_Length)
Vessels$Vessel_Tonnage.x<-as.numeric(Vessels$Vessel_Tonnage.x)


Port_Coords<-read.csv("Ports&Coords_Manual.csv")

Vessels_Ports<-merge(Vessels, Port_Coords, by=c("Port_RegisteredIn", "CCM_Flag"), all.x=TRUE)
Vessels_Ports <- Vessels_Ports[ , -which(names(Vessels_Ports) %in% c("WCPFC_Vessel_ID.y", "city_ascii", "iso2", "iso3", "admin_name", "capital", "population", "id"))]
Vessels_Ports <- Vessels_Ports[complete.cases(Vessels_Ports),]
Vessels_Ports$lng <- ifelse(Vessels_Ports$lng < 0, Vessels_Ports$lng + 360, Vessels_Ports$lng)
Vessels_Ports <- Vessels_Ports[order(Vessels_Ports$lng),]
Vessels_Ports <-Vessels_Ports[!(Vessels_Ports$lng > 300),]

Vessels_Ports <-Vessels_Ports[!(Vessels_Ports$WCPFC_Vessel_ID.x == "11439"),]
Vessels_Ports <-Vessels_Ports[!(Vessels_Ports$WCPFC_Vessel_ID.x == "9861"),]


Ports <-aggregate(WCPFC_Vessel_ID.x~lat+lng+Port_RegisteredIn, FUN=length, data=Vessels_Ports)
Map<-read_sf('C:/Users/timot/Documents/Albacore_OLE/shapefiles/Pacific_Landmasses.shp')
ggplot() + geom_sf(data = Map, fill = '#999999', color = '#0A1738', size = 0.1) +
  geom_point(data=Ports, aes(x=lng, y=lat, size=WCPFC_Vessel_ID.x)) + theme_bw()


rownames(Vessels_Ports) <- Vessels_Ports[,3] ### START HERE; can't have duplicate Vessel ID'S; Problem is with duplicate city names

Vessels_Ports_Cluster<-Vessels_Ports[c(4,5,7,8)]
Vessels_Ports_Cluster <- Vessels_Ports_Cluster %>% scale() 


library("NbClust")
library("cluster")
library("factoextra")
library("magrittr")
library('tidyverse')

res.nbclust <- Vessels_Ports_Cluster %>% NbClust(distance = "euclidean", min.nc = 2, max.nc = 10, method = "complete", index ="all") ### This process also takes forever
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())
fviz_nbclust(Vessels_Ports_Cluster, kmeans, method ="gap_stat") ### This process takes forever, says 6 is the optimal number
km.res <- kmeans(Vessels_Ports_Cluster, 6, nstart = 25)
fviz_cluster(km.res, data = Vessels_Ports_Cluster, ellipse.type = "convex", palette = "jco", ggtheme = theme_minimal())
fviz_cluster(km.res, data = Vessels_Ports_Cluster, ellipse.type = "convex", geom=c('point'), palette = "jco", ggtheme = theme_minimal())


res.hc <- Vessels_Ports_Cluster %>% dist(method = "euclidean") %>% hclust(method = "ward.D2")
memb <- cutree(res.hc, k = 6)
Vessels_Ports_Cluster<-Ports_Cluseter<-as.data.frame(Vessels_Ports_Cluster)
Vessels_Ports_Cluster$memb<-memb


fviz_dend(res.hc, cex = 0.5, k = 6, color_labels_by_k = TRUE) ### Long process to compute dendrogram figure






ttps://ourcodingclub.github.io/2017/03/21/data-clustering.html
http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf
https://rstudio-pubs-static.s3.amazonaws.com/33876_1d7794d9a86647ca90c4f182df93f0e8.html
https://www.datanovia.com/en/blog/types-of-clustering-methods-overview-and-quick-start-r-code/
  
install.packages("factoextra")
install.packages("cluster")
install.packages("magrittr")
install.packages('rstatix')
install.packages('tidyverse')
install.packages('tibble')

library("cluster")
library("factoextra")
library("magrittr")
library('tidyverse')

data("USArrests")

my_data <- USArrests %>%
  na.omit() %>%          # Remove missing values (NA)
  scale() 


res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")

fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

library("factoextra")
fviz_nbclust(my_data, kmeans, method = "gap_stat")
set.seed(123)

km.res <- kmeans(my_data, 4, nstart = 25)

fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

res.hc <- USArrests %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")

memb <- cutree(res.hc, k = 4)
USArrests$memb<-memb
count(USArrests$memb)


fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5, # label size
          label_cols=TRUE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)


install.packages("NbClust")
library(NbClust)

set.seed(123)

res.nbclust <- USArrests %>%
  scale() %>%
  NbClust(distance = "euclidean",
          min.nc = 2, max.nc = 20, 
          method = "complete", index ="all")

library(factoextra)
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

####
Boats<-read.csv('Boat_Clustering.csv')
Boats1 <- Boats[,-1]
rownames(Boats1) <- Boats[,1]
Boats1<-Boats1[c(-7)] 
Boats2<-Boats1[c(-5)] 
Boats3<-Boats2[c(-2)] 


Boat_data <- Boats3 %>%
  na.omit() %>%          # Remove missing values (NA)
  scale() 

fviz_nbclust(Boat_data, kmeans, method ="gap_stat")

km.res <- kmeans(Boat_data, 4, nstart = 25)


fviz_cluster(km.res, data = Boat_data,
             ellipse.type = "norm",
             palette = "nrc",
             geom="point",
             pointsize= 1.5,
             ggtheme = theme_minimal())


Boats3<-Boats3[complete.cases(Boats3),]

memb <- cutree(res.hc, k = 4)

Boats3$X<-memb
Boats3$Vessel_Name.x<-rownames(Boats3)


Test<-merge(Boats, Boats3, by="Vessel_Name.x", all.y=FALSE)

Test1<-aggregate(Year_Built.y~X, data=Test, FUN=mean)

library(plyr)

count(Test$X)


###

Boats<-read.csv('Boat_Clustering.csv')
Boats1 <- Boats[,-1]
rownames(Boats1) <- Boats[,1]
Boats1<-Boats1[c(-7)] 
Boats2<-Boats1[c(-5)] 
Boats3<-Boats2[c(-2)] 

Boats3<-Boats3[complete.cases(Boats3),]

res.hc <- Boats3 %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")

memb <- cutree(res.hc, k = 4)

Boats3$X<-memb
Boats3$Vessel_Name.x<-rownames(Boats3)
Test<-merge(Boats, Boats3, by="Vessel_Name.x", all.y=FALSE)
Test1<-aggregate(Length.m..x~X, data=Test, FUN=mean)

count(Test$X)

Test<-Test[which(Test$X==2),]
count(Test$Hull_Material.y)




albacore2<-albacore[which(albacore$YEAR=='2013'),]


fviz_dend(res.hc, k = 4, # Cut in four groups
          cex = 0.5,# label size
          show_labels=FALSE,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

res.nbclust <- Boats3 %>%
  scale() %>%
  NbClust(distance = "euclidean",
          method = "complete", index ="all")

library(factoextra)
fviz_nbclust(res.nbclust, ggtheme = theme_minimal())

PP<-as.data.frame(res.hc$order)

res.hc$merge

?NbClust



ellipse.type	
Character specifying frame type. Possible values are 'convex', 'confidence' 
or types supported by stat_ellipse including one of c("t", "norm", "euclid").


hc<-hclust(dist(Boats3), 'complete')
memb <- cutree(hc, k = 4)

plot(hc)  


?hclust 

?fviz_cluster


X<-km.res$cluster



fviz_nbclust(x, FUNcluster = NULL, method = c("silhouette", "wss",
                                              "gap_stat"), diss = NULL, k.max = 10, nboot = 100,
             
             
             ?fviz_nbclust










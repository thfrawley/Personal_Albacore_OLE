rm(list=ls())
library('matditr')

getwd()
setwd("C:/Users/timot/Documents/Albacore_OLE/RFMO_Data")


Registry<-read.csv("WCPFC_Vessel_Registry.csv", na.strings=c("", "NA"))
Registry <- Registry[c(1,6)]
Registry <- Registry[complete.cases(Registry),]

Unique_Vessel_Flag<-unique(Registry[,c('WCPFC_Vessel_ID','CCM_Flag')])
Switch_Flags<- Unique_Vessel_Flag %>%  group_by(WCPFC_Vessel_ID) %>%  filter(n()>1)


Registry<-read.csv("WCPFC_Vessel_Registry.csv")
All_Records_Swapping_Vessels<-merge(Switch_Flags, Registry, by="WCPFC_Vessel_ID", all.x=TRUE)
All_Records_Swapping_Vessels <- All_Records_Swapping_Vessels[order(All_Records_Swapping_Vessels$WCPFC_Vessel_ID, All_Records_Swapping_Vessels$Version_Number),]
All_Records_Swapping_Vessels <- All_Records_Swapping_Vessels[c(-2)]
All_Records_Swapping_Vessels<-unique(All_Records_Swapping_Vessels)
All_Records<-All_Records_Swapping_Vessels[c(1:4,6,10,19,26,30,37,39,42)]  

All_Records$Vessel_FishHold_Capacity<-as.factor(All_Records$Vessel_FishHold_Capacity)
Dominant_Hold= All_Records %>% group_by(WCPFC_Vessel_ID) %>% summarize(Vessel_FishHold_Capacity=names(which.max(table(Vessel_FishHold_Capacity))))
All_Records<-merge(All_Records, Dominant_Hold, by="WCPFC_Vessel_ID", all.x=TRUE)
All_Records$Vessel_FishHold_Capacity.x<-All_Records$Vessel_FishHold_Capacity.y
All_Records<-All_Records[c(-13)] 


All_Records$Date<-as.POSIXct(All_Records$Version_Date, format="%m/%d/%Y")
All_Records$Year <- as.numeric(format(All_Records$Date,'%Y'))
All_Records<- All_Records[c(1,5,6,9,10,11,12,14)]
All_Records <- All_Records[order(All_Records$WCPFC_Vessel_ID, All_Records$Year),]


First<- All_Records %>% group_by(WCPFC_Vessel_ID) %>%   filter(row_number()==1)
All_Records <- All_Records[order(All_Records$WCPFC_Vessel_ID, -All_Records$Year),]
Last <- All_Records %>% group_by(WCPFC_Vessel_ID) %>%   filter(row_number()==1)

Change<-First
Change <- Change[c(1,2,4,8)]
Change$End_Year<-Last$Year
Change$Target<-Last$CCM_Flag.y
colnames(Change)[2] <- "Source"
colnames(Change)[3] <- "FishHold"
colnames(Change)[4] <- "Start_Year"
Change$FishHold<-as.numeric(Change$FishHold)


Test <- aggregate(FishHold~Source+Target, FUN=length, data=Change)
Test <- Test[order(-Test$FishHold),]
Test<- Test[(Test$FishHold > 1000),]


nodes <- data.frame( name=c(as.character(Test$Source), as.character(Test$Target)) %>% unique())
Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1

Test <- Test[c(-2,-5),]

p <- sankeyNetwork(Links = Test, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "FishHold", NodeID = "name", 
                   sinksRight=TRUE)

p



####
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)

data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname) %>%
  filter(value > 0)
colnames(data_long) <- c("source", "target", "value")
data_long$target <- paste(data_long$target, " ", sep="")












dcast(Change, Vessel_FishHold_Capacity.x~CCM_Flag.y, fun=SUM)

dcast(DT, diet ~ variable, fun=mean)

install.packages('networkD3')
library(networkD3)

links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)


nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)



nodes <- data.frame(
  name=c(as.character(Test$Source), 
         as.character(Test$Target)) %>% unique()
)

Test$IDsource <- match(Test$Source, nodes$name)-1 
Test$IDtarget <- match(Test$Target, nodes$name)-1


p <- sankeyNetwork(Links = Test, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "FishHold", NodeID = "name", 
                   sinksRight=TRUE)

p


Registry<-read.csv("WCPFC_Vessel_Registry.csv", na.strings=c("", "NA"))
Registry <- Registry[c(1,6,42)]
Registry <- Registry[complete.cases(Registry),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='NONE'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='none'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='None'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='N.A.'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='NONE KNOWN'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='N/A'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='N/V'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='na'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='non'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='NON'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='UNKNOWN'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='NEWLY BUILT'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='Nil'),]
Registry<- Registry[!(Registry$Vessel_Previous_Flags=='UNK'),]

Unique_Vessel_Flag1<-unique(Registry[,c('WCPFC_Vessel_ID','CCM_Flag', 'Vessel_Previous_Flags')])

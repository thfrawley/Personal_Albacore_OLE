getwd()
setwd("C:/Users/timot/Documents/Albacore_OLE/RFMO_Data")

Activity<-read.csv("WCPFC_Vessel_Activity.csv")

Registry<-read.csv("WCPFC_Vessel_Registry.csv")
Registry <- Registry[c(1,6)]
Unique_Vessel_Flag<-unique(Registry[,c('WCPFC_Vessel_ID','CCM_Flag')])
Switch_Flags<- Unique_Vessel_Flag %>%  group_by(WCPFC_Vessel_ID) %>%  filter(n()>1)


Registry<-read.csv("WCPFC_Vessel_Registry.csv")
All_Records_Swapping_Vessels<-merge(Switch_Flags, Registry, by="WCPFC_Vessel_ID", all.x=TRUE)
All_Records_Swapping_Vessels <- All_Records_Swapping_Vessels[order(All_Records_Swapping_Vessels$WCPFC_Vessel_ID, All_Records_Swapping_Vessels$Version_Number),]
All_Records_Swapping_Vessles <- All_Records_Swapping_Vessels[c(-2)]

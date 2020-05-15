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
library(scatterpie)

rm(list=ls())

# Load public longline data
setwd("C:/Users/timot/Documents/Albacore_OLE")


pllBoth <- read.csv("C:/Users/timot/Documents/Albacore_OLE/RFMO_Data/albLonglineIATTC_WCPFC_Flag.csv", head = TRUE, sep = ",") # Note WPC only has qrtr not month
Map<-read_sf('shapefiles/Pacific_Landmasses.shp')
IATTC<-read_sf("shapefiles/RFMO_Boundaries/iattc.shp")
WCPFC<-read_sf('shapefiles/RFMO_Boundaries/WCPFC.shp')
Pacific_IATTC<-st_shift_longitude(IATTC)
Pacific_WCPFC<-st_shift_longitude(WCPFC)
Pacific_WCPFC$area<-st_area(Pacific_WCPFC)
Pacific_WCPFC <- Pacific_WCPFC %>% summarise(area = sum(area))

pllBoth <- within(pllBoth, flag_id[flag_id == "CHN"] <- "CN")
pllBoth <- within(pllBoth, flag_id[flag_id == "JPN"] <- "JP")
pllBoth <- within(pllBoth, flag_id[flag_id == "TWN"] <- "TW")
pllBoth <- within(pllBoth, flag_id[flag_id == "KOR"] <- "KR")
pllBoth <- within(pllBoth, flag_id[flag_id == "USA"] <- "US")

pllBoth <- pllBoth[which(pllBoth$flag_id == "CN"),] 


pllBoth<-pllBoth[which(pllBoth$yy > 2011),] ### Use if you want to subset years
pllBoth<-pllBoth[which(pllBoth$yy < 2017),]

pllBoth<-pllBoth[which(pllBoth$qtr == 1),]

pllBoth<-pllBoth[which(pllBoth$yy == 2016),]


pllBoth <- subset(pllBoth, pllBoth$lat5 > 0) ### Use if you only want Northen Hemisphere
pllBoth$lon360 <- ifelse(pllBoth$lon5 < 0, pllBoth$lon5 + 360, pllBoth$lon5)

### Reomve effort outliers
upper <- quantile(pllBoth$hooks, 0.999, na.rm = TRUE) # to remove strong upper outliers, likely errors
pllBoth <- within(pllBoth, hooks[hooks > upper] <- NA)

# Aggregate in space and time, "count" is to enable exclusion of rarely fishes times/locations later (if desired)

pllEffort <- aggregate(hooks  ~ lon360 + lat5, pllBoth, 
                          FUN = sum, na.rm = TRUE)
pllEffort$loghooks <- log(pllEffort$hooks)
pllEffort$scaledhooks<- pllEffort$hooks/1000


mycols <- colors()[c(473,562,71,610,655,653,621,34)] 
mypalette <- colorRampPalette(mycols)(255)

ggplot() + 
  geom_tile(data = pllEffort, aes(x = lon360, y = lat5, fill = loghooks)) +
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral", lwd=.5) +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3", lwd=.5) +
  geom_sf(data = Map,  fill = '#374a6d', color = '#0A1738', size = 0.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(-60,60) + xlim(110,300) +
        scale_fill_gradientn("log(hooks)", colours = mypalette, na.value = NA) + labs(title = ' Chinese Longline Effort (2016)')

ggsave("RFMO_ALL.tiff", width=9, height=4, units="in", dpi=300)
ggsave("RFMO_ALL.tiff", width=8, height=6, units="in", dpi=300)




##Produce Scattertile

capture_all <- pllBoth %>% 
  group_by(lat5, lon360) %>% 
  summarize(alb_n = sum(alb_n, na.rm = T),
            bet_n= sum(bet_n, na.rm = T),
            swon_n  = sum(swo_n, na.rm = T),
            yft_n = sum(yft_n, na.rm = T))

capture_all$idx <- as.integer(interaction(capture_all$lat5, capture_all$lon360))
capture_all$total<-capture_all$alb_n + capture_all$bet_n + capture_all$swon_n + capture_all$yft_n


ggplot() + geom_scatterpie(aes(x=lon360, y=lat5, group=idx), data=capture_all,
                           cols=c("alb_n", "bet_n", "swon_n", "yft_n"), pie_scale=.75) +  
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral", lwd=.5) +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3", lwd=.5) +
  geom_sf(data = Map,  fill = '#374a6d', color = '#0A1738', size = 0.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(-60,60) + xlim(100,300) +
        scale_fill_manual(values=c("royalblue1", "darkolivegreen", "maroon2", "yellow3")) + labs(title = 'Longline (2012-2016)')


ggplot() + geom_scatterpie(aes(x=lon360, y=lat5, group=idx, r=(total/50000)), data=capture_all,
                           cols=c("alb_n", "bet_n", "swon_n", "yft_n")) + coord_equal() +  
  geom_sf(data= Pacific_WCPFC, fill=NA, color="coral", lwd=.5) +
  geom_sf(data= Pacific_IATTC, fill=NA, color="darkorchid3", lwd=.5) +
  geom_sf(data = Map,  fill = '#374a6d', color = '#0A1738', size = 0.1) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylim(-5,60) + xlim(100,300) +
        scale_fill_manual(values=c("royalblue1", "darkolivegreen", "maroon2", "yellow3")) + labs(title = 'Longline (2012-2016); First Quarter')



ggsave("RFMO_ALL.tiff", width=9, height=4, units="in", dpi=300)




install.packages("scatterpie")


# Change major IATTC flags to WCPFC ones (IATTC uses 3 character flag codes, WCPFC uses 2 characters)
# If you want to do this for all flags you'll need something a bit more elegant!
pllBoth <- within(pllBoth, flag_id[flag_id == "CHN"] <- "CN")
pllBoth <- within(pllBoth, flag_id[flag_id == "JPN"] <- "JP")
pllBoth <- within(pllBoth, flag_id[flag_id == "TWN"] <- "TW")
pllBoth <- within(pllBoth, flag_id[flag_id == "KOR"] <- "KR")
pllBoth <- within(pllBoth, flag_id[flag_id == "USA"] <- "US")

# Remove southern hemisphere, calculate lon360
pllBoth <- subset(pllBoth, pllBoth$lat5 > 0)
pllBoth$lon360 <- ifelse(pllBoth$lon5 < 0, pllBoth$lon5 + 360, pllBoth$lon5)

# Assign fishing area (see map in stock assessment report: there are 5 areas)
pllBoth$area <- ifelse(pllBoth$lat5 <= 30 & pllBoth$lon360 <= 160, 2, 
                       ifelse(pllBoth$lat5 <= 30 & pllBoth$lon360 >  160, 4,
                              ifelse(pllBoth$lat5 >  30 & pllBoth$lon360 <= 180, 3, 5)))
pllBoth$area <- ifelse(pllBoth$lat5 >= 25 & pllBoth$lat5 <= 35 &
                         pllBoth$lon360 >= 130 & pllBoth$lon360 <= 140, 1, pllBoth$area)

# Calculate fleet number, see stock assessment report for definitions
# Just calculating a few ones useful for distinguishing juveniles from adults for now, 
# based on length distribution histograms in the assessment, leaving others as NA
pllBoth$fleet <- NA
for (i in 1:nrow(pllBoth)) {
  if(pllBoth$flag_id[i] == "JP" & pllBoth$qtr[i] == 1 &
     (pllBoth$area[i] == 1 | pllBoth$area[i] == 3)) {
    pllBoth$fleet[i] <- 1 
  } else if (pllBoth$flag_id[i] == "JP" & pllBoth$qtr[i] == 2 &
             (pllBoth$area[i] == 1 | pllBoth$area[i] == 3)) {
    pllBoth$fleet[i] <- 2 
  } else if (pllBoth$flag_id[i] == "JP" & pllBoth$qtr[i] == 1 &
             pllBoth$area[i] == 2) {
    pllBoth$fleet[i] <- 9 
  } else if (pllBoth$flag_id[i] == "JP" & pllBoth$qtr[i] > 1 &
             pllBoth$area[i] == 2) {
    pllBoth$fleet[i] <- 10 
  } else if (pllBoth$flag_id[i] == "JP" & pllBoth$area[i] == 4) {
    pllBoth$fleet[i] <- 13 
  } else if (pllBoth$flag_id[i] == "US" & 
             (pllBoth$area[i] == 2 | pllBoth$area[i] == 4)) {
    pllBoth$fleet[i] <- 20 
  } else if (pllBoth$flag_id[i] == "KR") {
    pllBoth$fleet[i] <- 23 
  } else if (pllBoth$flag_id[i] == "CN" & 
             (pllBoth$area[i] == 2 | pllBoth$area[i] == 4)) {
    pllBoth$fleet[i] <- 25 
  }
}

# Using length frequencies from assessment, assign fleets to adults, juveniles, or NA
pllBoth$stage <- ifelse(pllBoth$fleet == 1 | pllBoth$fleet == 2, "juvenile", 
                        ifelse(is.na(pllBoth$fleet) == TRUE, NA, "adult"))

# Calculate cpue and log cpue
pllBoth$albCPUE <- (pllBoth$alb_n / pllBoth$hooks)*1000
#plot(pllBoth$albCPUE)#, ylim = c(0, 500))
upper <- quantile(pllBoth$albCPUE, 0.999, na.rm = TRUE) # to remove strong upper outliers, likely errors
pllBoth <- within(pllBoth, albCPUE[albCPUE > upper] <- NA)
pllBoth$logALBcpue <- log(pllBoth$albCPUE + 1)

# Aggregate in space and time, "count" is to enable exclusion of rarely fishes times/locations later (if desired)
pllAgg       <- aggregate(logALBcpue ~ lon360 + lat5 + qtr + stage, pllBoth, 
                          FUN = mean, na.rm = TRUE)
pllAgg$count <- aggregate(logALBcpue ~ lon360 + lat5 + qtr + stage, pllBoth, 
                          FUN = length)[,ncol(pllAgg)]
meanHooks    <- aggregate(hooks      ~ lon360 + lat5 + qtr + stage, pllBoth, 
                          FUN = mean, na.rm = TRUE)
pllAgg <- dplyr::full_join(pllAgg, meanHooks, by = c("lon360", "lat5", "qtr", "stage"))

# Plot prep
pac.coast <- borders("world2", colour="gray50", fill="gray50", xlim = c(100, 260),ylim = c(0, 60))
# I hate the default R palettes
mycols <- colors()[c(473,562,71,610,655,653,621,34)] 
mypalette <- colorRampPalette(mycols)(255)
# This theme just makes map text larger
myTheme = theme(axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14),
                axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
                plot.title = element_text(size = 16), legend.title = element_text(size = 12),
                legend.text = element_text(size = 12), strip.text.x = element_text(size = 12))
# Name seasons
pllAgg$season <- as.factor(pllAgg$qtr)
levels(pllAgg$season) <- c("Winter", "Spring", "Summer", "Fall") 

# Map juveniles. Currently excluding times/places with < 5 records
# geom_segments show boundaries of fishing areas
p1 <- ggplot() +
  geom_tile(data = subset(pllAgg, pllAgg$count > 4 & pllAgg$stage == "juvenile"), 
            aes(x = lon360, y = lat5, fill = logALBcpue)) +
  scale_fill_gradientn("/1000 hooks", colours = mypalette, na.value = NA) +
  geom_segment(data = pllAgg, aes(x = 140 , y = 30, xend = Inf, yend = 30)) + # 30N line
  geom_segment(data = pllAgg, aes(x = 180 , y = 30, xend = 180, yend = 55)) + # 180E line
  geom_segment(data = pllAgg, aes(x = 160 , y = 0,  xend = 160, yend = 30)) + # 160E line
  geom_rect(data = pllAgg, aes(xmin = 130, xmax = 140, ymin = 25, ymax = 35), 
            color = "black", fill = NA) + # shows area 1
  pac.coast + coord_quickmap(xlim = c(125, 260), ylim = c(0, 55)) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ season, ncol = 2) + theme_bw()
p1
# Map adults. Currently excluding times/places with < 5 records
# geom_segments show boundaries of fishing areas
p2 <- ggplot() +
  geom_tile(data = subset(pllAgg, pllAgg$count > 4 & pllAgg$stage == "adult"), 
            aes(x = lon360, y = lat5, fill = logALBcpue)) +
  scale_fill_gradientn("/1000 hooks", colours = mypalette, na.value = NA) +
  geom_segment(data = pllAgg, aes(x = 140 , y = 30, xend = Inf, yend = 30)) + # 30N line
  geom_segment(data = pllAgg, aes(x = 180 , y = 30, xend = 180, yend = 55)) + # 180E line
  geom_segment(data = pllAgg, aes(x = 160 , y = 0,  xend = 160, yend = 30)) + # 160E line
  geom_rect(data = pllAgg, aes(xmin = 130, xmax = 140, ymin = 25, ymax = 35), 
            color = "black", fill = NA) + # shows area 1
  pac.coast + coord_quickmap(xlim = c(125, 260), ylim = c(0, 55)) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ season, ncol = 2) + theme_bw()
p2

##################################################################################################
# Load US troll data. These cover 1990-2017, and have been aggregated by season and 5x5 resolution, 
# and only show non-confidential (3+ vessels) locations
trollAgg <- read.csv("alb_troll_nonconf_agg_season_5x5.csv", head = TRUE, sep = ",")

# Map troll
p3 <- ggplot() +
  geom_tile(data = subset(trollAgg, trollAgg$count > 4 & trollAgg$totalALB > 0), 
            aes(x = lon360, y = lat5mid, fill = totalALB)) + # a bit suspicious of some logb points near HI w 0 catch, even if nonconf
  scale_fill_gradientn("/vessel/day", colours = mypalette, na.value = NA) +
  geom_segment(data = pllAgg, aes(x = 140 , y = 30, xend = Inf, yend = 30)) + # 30N line
  geom_segment(data = pllAgg, aes(x = 180 , y = 30, xend = 180, yend = 55)) + # 180E line
  geom_segment(data = pllAgg, aes(x = 160 , y = 0,  xend = 160, yend = 30)) + # 160E line
  geom_rect(data = pllAgg, aes(xmin = 130, xmax = 140, ymin = 25, ymax = 35), 
            color = "black", fill = NA) + # shows area 1
  pac.coast + coord_quickmap(xlim = c(100, 260), ylim = c(0, 55)) +
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~ season, ncol = 2) + theme_bw()
p3

# Save: PLL juveniles
jpeg(filename = "ALB Juvs IATTC_WCPFC PLL.jpg", quality =1000, res=300, width = 6, height = 4, units = 'in', restoreConsole = TRUE)
p1
dev.off()
# PLL adults
jpeg(filename = "ALB Adults IATTC_WCPFC PLL.jpg", quality =1000, res=300, width = 6, height = 4, units = 'in', restoreConsole = TRUE)
p2
dev.off()
# Troll juveniles
jpeg(filename = "ALB Juvs US Surface Logb.jpg", quality =1000, res=300, width = 6, height = 4, units = 'in', restoreConsole = TRUE)
p3
dev.off()



set.seed(123)
long <- rnorm(50, sd=100)
lat <- rnorm(50, sd=50)
d <- data.frame(long=long, lat=lat)
d <- with(d, d[abs(long) < 150 & abs(lat) < 70,])
n <- nrow(d)
d$region <- factor(1:n)
d$A <- abs(rnorm(n, sd=1))
d$B <- abs(rnorm(n, sd=2))
d$C <- abs(rnorm(n, sd=3))
d$D <- abs(rnorm(n, sd=4))
d[1, 4:7] <- d[1, 4:7] * 3
head(d)


ggplot() + geom_scatterpie(aes(x=long, y=lat, group=region), data=d,
                           cols=LETTERS[1:4]) + coord_equal()


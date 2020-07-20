

rm(list=ls())

library(tidyverse) # for general data wrangling and plotting
library(parallel)
library(furrr) # for parallel operations on lists
library(lubridate) # for working with dates
library(sf) # for vector data 
library(raster) # for working with rasters
library(maps) # additional helpful mapping packages
library(maptools)
library(rgeos)
library(ggplot2)

###This should be the directory where the daily files downloaded from the GFW website are located
data_dir <- 'C:/Users/timot/Documents/Albacore_OLE/GFW_data/raw_data/fishing_effort/'

effort_files <- tibble(
  file = list.files(paste0(data_dir, 'daily_fishing_effort_bymmsi'), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

### At 0.01 degree resolution these are very large files; had to read in and convert the data 
### One year at a time 2012-2015 and then split 2016 in 6 month intervals

effort_dates <- seq(ymd('2016-01-01'), ymd('2016-12-31'), by='days')

# Filter to files within our date range of interest
effort_files <- filter(effort_files, date %in% effort_dates)

# Read in data (uncomment to read in parallel)
plan(multisession) # Windows users should change this to plan(multisession)
effort_df <- furrr::future_map_dfr(effort_files$file, .f = read_csv)

# Add date information
effort_df <- effort_df %>% 
  mutate(year  = year(date),
         month = month(date))


### Options to further shrink the size of the dataframe
### Reduce number of digits for unecessarily precise variables

effort_df$fishing_hours <- format(round(effort_df$fishing_hours, 2), nsmall = 2)
effort_df$fishing_hours <- as.numeric(effort_df$fishing_hours)
effort_df <- effort_df[ , -which(names(effort_df) %in% c("date"))]

# Specify new (lower) resolution in degrees for aggregating data
res <- .25

##METHOD 1
##Transform data across all fleets and geartypes; use for fishing effort by flag
effort_df <- effort_df %>% 
  mutate(
    # convert from hundreths of a degree to degrees
    lat_bin = lat_bin / 10, 
    lon_bin = lon_bin / 10,
    # calculate new lat lon bins with desired resolution
    lat_bin = floor(lat_bin/res) * res + 0.5 * res, 
    lon_bin = floor(lon_bin/res) * res + 0.5 * res)

# Re-aggregate the data to 0.25 degrees
effort_df <- effort_df %>% 
  group_by(year, month, lon_bin, lat_bin, mmsi) %>% 
  summarize(fishing_hours = sum(fishing_hours, na.rm = T))

write.csv(effort_df, "Monthly_2016_fishingeffortmmsi_0.25.csv")






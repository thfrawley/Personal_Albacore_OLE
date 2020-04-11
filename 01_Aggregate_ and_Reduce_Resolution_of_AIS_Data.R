

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

###This should be the directory where files downloaded from the GFW website are located
data_dir <- 'C:/Users/timot/Documents/GFw_Data/data/fishing_effort/'

effort_files <- tibble(
  file = list.files(paste0(data_dir, 'fishing_effort_byflag'), 
                    pattern = '.csv', recursive = T, full.names = T),
  date = ymd(str_extract(file, 
                         pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))

### At native resolution these are very large files; had to read in and convert the data 
### One year at a time 2012-2015 and then split 2016 in 6 month intervals

effort_dates <- seq(ymd('2015-01-01'), ymd('2015-12-31'), by='days')

# Filter to files within our date range of interest
effort_files <- filter(effort_files, date %in% effort_dates)

# Read in data (uncomment to read in parallel)
plan(multisession) # Windows users should change this to plan(multisession)
effort_df <- furrr::future_map_dfr(effort_files$file, .f = read_csv)

# Add date information
effort_df <- effort_df %>% 
  mutate(year  = year(date),
         month = month(date))


# Specify new (lower) resolution in degrees for aggregating data
res <- .25

# Transform data across all fleets and geartypes
effort_df <- effort_df %>% 
  mutate(
    # convert from hundreths of a degree to degrees
    lat_bin = lat_bin / 100, 
    lon_bin = lon_bin / 100,
    # calculate new lat lon bins with desired resolution
    lat_bin = floor(lat_bin/res) * res + 0.5 * res, 
    lon_bin = floor(lon_bin/res) * res + 0.5 * res)

# Re-aggregate the data to 0.25 degrees
effort_df <- effort_df %>% 
  group_by(date, year, month, lon_bin, lat_bin, flag, geartype) %>% 
  summarize(vessel_hours = sum(vessel_hours, na.rm = T),
            fishing_hours = sum(fishing_hours, na.rm = T),
            mmsi_present  = sum(mmsi_present, na.rm = T))

write.csv(effort_df, "2015_fishingeffortflag_0.25.csv")








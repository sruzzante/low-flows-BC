# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script extracts daily and monthly SWE data from ERA5-Land datasets 
# available at https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview 
# and https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview


closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(sf)
library(reshape2)
library(stringr)
library(terra)


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


watersheds<-terra::vect("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")



# Load ERA-5 SWE data (monthly)
SNOW_SWE_grid<-terra::rast("2.data/1.original/ERA5_LAND_SWE/Monthly/adaptor.mars.internal-1706465874.8028717-7610-7-c5361b75-b20d-44b9-8d18-83908b5d11c8.grib")

time(SNOW_SWE_grid)
watersheds_SWE<-terra::extract(SNOW_SWE_grid,watersheds%>%
                                 terra::project(crs(SNOW_SWE_grid)),
                               fun = mean,
                               exact= TRUE,
                               # weights = TRUE,
                               touches = TRUE,
                               na.rm = TRUE)


watersheds_SWE$ID<-watersheds$StationNum

names(watersheds_SWE)<-c("ID",time(SNOW_SWE_grid)%>%as.character())

data_SWE<-melt(watersheds_SWE,id.vars = "ID")

write.csv(data_SWE,"2.data/2.working/SWE_data_by_catchment.csv",row.names = FALSE)

# Now daily

# First save all rasters to one tif

SNOW_SWE_grid<-terra::rast(list.files("2.data/1.original/ERA5_LAND_SWE/Daily/",full.names = TRUE,pattern = ".grib"))
dts<-time(SNOW_SWE_grid)%>%
  as.Date()

dts_need<-ymd("1950-01-01"):ymd("2022-12:31")%>%
  as.Date(origin = "1970-01-01")
dts_need<-dts_need[!month(dts_need)%in%7:10]
dts[!dts%in%dts_need]

SNOW_SWE_grid<-SNOW_SWE_grid[[which(!duplicated(dts))]]
sum(duplicated(dts))
dts<-time(SNOW_SWE_grid)%>%
  as.Date()

names(SNOW_SWE_grid)<-dts

SNOW_SWE_grid1000<-SNOW_SWE_grid*1000


watersheds_SWE<-terra::extract(SNOW_SWE_grid,watersheds%>%
                                 vect()%>%
                                 project(crs(SNOW_SWE_grid)),
                               fun = mean,
                               exact= TRUE,
                               # weights = TRUE,
                               touches = TRUE,
                               na.rm = TRUE)


watersheds_SWE$ID<-watersheds$StationNum

names(watersheds_SWE)<-c("ID",time(SNOW_SWE_grid)%>%as.character())

data_SWE_dly<-melt(watersheds_SWE,id.vars = "ID")

write.csv(data_SWE_dly,"2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.csv",row.names = FALSE)
save(data_SWE_dly,file = "2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.gz",compress = "gzip")

load("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.gz")

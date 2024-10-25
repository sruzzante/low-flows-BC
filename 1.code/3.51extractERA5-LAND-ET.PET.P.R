# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-08-19

# This script extracts monthly ET and PET data from ERA5-Land datasets 
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
ET_grid<-terra::rast("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ERA5_Land/ERA5-Land_ET_monthly.grib")
PET_grid<-terra::rast("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ERA5_Land/ERA5-Land_PET_monthly.grib")
T_grid<-terra::rast("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ERA5_Land/ERA5-Land_T_monthly.grib")
P_grid<-terra::rast("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ERA5_Land/ERA5-Land_P_monthly.grib")

time(ET_grid)

watersheds_ET<-
  terra::extract(ET_grid,watersheds%>%
                   terra::project(crs(ET_grid)),
                 fun = mean,
                 exact= TRUE,
                 # weights = TRUE,
                 touches = TRUE,
                 na.rm = TRUE)

watersheds_PET<-
  terra::extract(PET_grid,watersheds%>%
                   terra::project(crs(PET_grid)),
                 fun = mean,
                 exact= TRUE,
                 # weights = TRUE,
                 touches = TRUE,
                 na.rm = TRUE)

watersheds_T<-
  terra::extract(T_grid,watersheds%>%
                   terra::project(crs(T_grid)),
                 fun = mean,
                 exact= TRUE,
                 # weights = TRUE,
                 touches = TRUE,
                 na.rm = TRUE)

watersheds_P<-
  terra::extract(P_grid,watersheds%>%
                   terra::project(crs(P_grid)),
                 fun = mean,
                 exact= TRUE,
                 # weights = TRUE,
                 touches = TRUE,
                 na.rm = TRUE)


watersheds_ET$ID<-watersheds$StationNum

names(watersheds_ET)<-c("ID",time(ET_grid)%>%as.character())

data_ET<-melt(watersheds_SWE,id.vars = "ID")

write.csv(data_SWE,"2.data/2.working/SWE_data_by_catchment.csv",row.names = FALSE)




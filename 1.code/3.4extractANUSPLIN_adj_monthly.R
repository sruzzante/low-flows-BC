# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-09


# This script extracts monthly adjusted precipitation climate data , using the catchment polygons.
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Climate-archives_Archives-climatologiques/canada_adjpcp/
# These are not included in the repository due to size limitations 
# The script will take a few hours to run



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
library(tictoc)


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


watersheds<-terra::vect("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  terra::project("EPSG:4326")

watersheds$ID_num<-1:nrow(watersheds)

data<-expand.grid(Year = 1900:2015,Month = 1:12,ID = watersheds$ID_num,pcp = NA)%>% 
  arrange_all()
data<-left_join(data,data.frame(watersheds[,c("ID_num","ID")]),by = c("ID" = "ID_num"))

tm_shape(rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",it_year,"/pcp_",it_month_pad,".asc/pcp_",it_month_pad,".asc")))+
  tm_raster()+
  tm_shape(st_as_sf(watersheds))+tm_borders()

# watersheds<-watersheds%>%vect()%>%project("EPSG:4326")

data_all<-data.frame()
# data_all<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly_adjpcp.rds")%>%select(!NA_frac)
for(it_year in 1945:1949){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    xt<-terra::extract(rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",it_year,"/pcp_",it_month_pad,".asc/pcp_",it_month_pad,".asc")),
                       watersheds,
                       fun = "mean",
                       na.rm = TRUE)
    names(xt)[1]<-"ID_num"
    names(xt)[2]<-"pcp"
    
    xt$Year<-it_year
    xt$Month<-it_month
     xt<-left_join(xt,data.frame(watersheds)
                  )
    data_all<-rbind(data_all,xt)
    toc()
   
  }
}
x_na<-terra::extract(rast("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/1950/pcp_01.asc/pcp_01.asc"),
                          watersheds,
                          fun = function(x){mean(is.na(x))})
names(x_na)<-c("ID_num","NA_frac")

data_all<-left_join(data_all,x_na)

data_all<-data_all%>%arrange(Year,Month,ID)

saveRDS(data_all,"2.data/2.working/WeatherDataANUSPLIN/dataMonthly_adjpcp.rds")






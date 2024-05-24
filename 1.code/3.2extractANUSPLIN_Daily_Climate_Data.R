# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10


# This script extracts daily climate data from T and P rasters, using the catchment polygons.
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Climate-archives_Archives-climatologiques/daily/
# These are not included in the repository due to size limitations (~85 GB)
# The script will take a few hours to run


library(stringr)
library(terra)
library(lubridate)
library(dplyr)
library(sf)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%st_transform(st_crs("EPSG:4269"))%>%vect()

stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv")

data<-data.frame()


for(it_year in c(1949:2020)){
  
  tictoc::tic()
  # tmpRastFiles<-list.files(paste0("../1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_daily/",it_year,"/"),
  #                          pattern = ".tif",recursive = TRUE)
  # 
  # allRasts<-rast(paste0("../1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_daily/",it_year,"/",
  #                       tmpRastFiles))
  
  allRasts<-rast(paste0("../1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim2_ANUSPLIN_daily/ANUSPLIN_Daily_cropped_",it_year,".tif"))
  
  x<-extract(allRasts,watersheds,fun = mean,exact = TRUE)
  
  x$ID<-watersheds$StationNum
  x1<-reshape2::melt(x,id.vars = "ID")
  
  x1$year<-str_extract(x1$variable,"\\d{4}")%>%as.numeric()
  x1$day<-str_extract(x1$variable,"(?<=\\_)\\d*")%>%as.numeric()
  x1$var<-str_extract(x1$variable,"[a-z]*")
  x2<-reshape2::dcast(x1,formula = ID+year+day~var,value = "value")
  data<-rbind(data,x2)
  
  
  mask<-!leap_year(x2$year)&x2$day>60
  x2$day[mask]<-x2$day[mask]-1
  
  x2$Mean.Temp..C.<-(x2$mint+x2$maxt)/2
  x2$Total.Precip..mm.<-x2$pcp
  x2$Date<-as.Date(x2$day-1,origin = paste0(x2$year,"-01-01"))
  x2$month<-month(x2$Date)
  x2$day<-day(x2$Date)
  
  write.csv(x2,sprintf("2.data/2.working/WeatherDataANUSPLIN/dataDaily_%d.csv",it_year),row.names=FALSE)
  tictoc::toc()
  print(sprintf("Done year %d",it_year))
  
  
  
}

data<-lapply(list.files(path = "../data/BC_Watersheds/2.data/2.working/WeatherDataANUSPLIN/",pattern = "dataDaily_\\d{4}.csv",full.names = TRUE),
             FUN = read.csv
             )

data2<-bind_rows(data)


data2$Mean.Temp..C.<-(data2$mint+data2$maxt)/2
data2$Total.Precip..mm.<-data2$pcp
data2<-data2[,c(c("year","month","day","Date","ID","Mean.Temp..C.","Total.Precip..mm."))]

# write.csv(data2,"2.data/2.working/WeatherDataANUSPLIN/dataDaily.csv",row.names=FALSE)
saveRDS(data,file = "2.data/2.working/WeatherDataANUSPLIN/dataDaily.RDS")

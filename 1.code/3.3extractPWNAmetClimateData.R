# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10


# This script extracts daily climate data from T and P rasters, using the catchment polygons.
# https://data.pacificclimate.org/portal/gridded_observations/map/
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


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


watersheds<-terra::vect("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  terra::project("EPSG:4326")

watersheds$ID_num<-1:nrow(watersheds)

data<-expand.grid(Year = 1945:2012,Month = 1:12,ID = watersheds$ID_num,mint=NA,maxt = NA,pcp = NA)%>% 
  arrange_all()
data<-left_join(data,data.frame(watersheds[,c("ID_num","StationNum","NameNom")]),by = c("ID" = "ID_num"))

# watersheds<-watersheds%>%vect()%>%project("EPSG:4326")



pr<-rast("2.data/1.original/WeatherDataPWNAmet/PNWNAmet.nc",subds = "pr")
tasmin<-rast("2.data/1.original/WeatherDataPWNAmet/PNWNAmet.nc",subds = "tasmin")
tasmax<-rast("2.data/1.original/WeatherDataPWNAmet/PNWNAmet.nc",subds = "tasmax")
# tasmean<-(tasmin+tasmax)/2
data<-expand.grid(Month = 1:12 ,Year = 1945:2012)
data$ind<-1:nrow(data)

dts<-data.frame(dt = ymd(time(pr)))
dts$Year = year(dts$dt)
dts$Month = month(dts$dt)
dts<-left_join(dts,data)


pr_m<-subset(pr,1:365)

pr_m<-tapp(pr,dts$ind,fun = sum,filename = "2.data/1.original/WeatherDataPWNAmet/PNWNAmet_pr_m.tif",cores = 8,overwrite = TRUE)
# tasmean_m<-tapp(tasmean,dts$ind,fun = mean,filename = "D:/PNWNAmet_tasmin_m.tif",cores = 8,overwrite = TRUE)

tasmin_m<-tapp(tasmin,dts$ind,fun = mean,filename = "2.data/1.original/WeatherDataPWNAmet/PNWNAmet_tasmin_m.tif",cores = 8,overwrite = TRUE)
tasmax_m<-tapp(tasmax,dts$ind,fun = mean,filename = "2.data/1.original/WeatherDataPWNAmet/PNWNAmet_tasmax_m.tif",cores = 8,overwrite = TRUE)


pr_m<-rast("2.data/1.original/WeatherDataPWNAmet/PNWNAmet_pr_m.tif")

dts_m<-ymd(paste(data$Year,data$Month,1,sep = "-"))

time(pr_m)<-dts_m
time(tasmax_m)<-dts_m
time(tasmin_m)<-dts_m
x<-terra::extract(pr,watersheds[1,])



x_date<-data.frame(names(tasmax_m),dts_m)
x_date$Year<-year(x_date$dts_m)
x_date$Month<-month(x_date$dts_m)

maxt<-extract(tasmax_m,watersheds,fun = mean,touches = TRUE)
maxt$ID<-watersheds$StationNum
maxt_<-melt(maxt,id.vars = "ID",value.name = "maxt")
mint<-extract(tasmin_m,watersheds,fun = mean,touches = TRUE)
mint_<-melt(mint,id.vars = "ID",value.name = "mint")
mint_$ID<-watersheds$StationNum
pcp<-extract(pr_m,watersheds,fun = mean,touches = TRUE)
pcp_<-melt(pcp,id.vars = "ID",value.name = "pcp")
pcp_$ID<-watersheds$StationNum

data<-left_join(maxt_,mint_)%>%
  left_join(pcp_)

data$Year<-plyr::mapvalues(data$variable,from = x_date$names.tasmax_m.,to = x_date$Year)%>%as.character()%>%as.numeric()

data$Month<-plyr::mapvalues(data$variable,from = x_date$names.tasmax_m.,to = x_date$Month)%>%as.character()%>%as.numeric()



# streamData<-read.csv("HydroStations/08HA003.Koksilah.River/daily_20230705T0019.csv",skip = 1)

data$Mean.Temp..C.<-(data$mint+data$maxt)/2
data$Total.Precip..mm.<-data$pcp
data$SeptWaterYear<-data$Year
data$SeptWaterYear[data$Month%in%c(9,10,11,12)] <- data$SeptWaterYear[data$Month%in%c(9,10,11,12)]+1

saveRDS(data,"2.data/2.working/WeatherDataPWNAmet/dataMonthly.RDS")



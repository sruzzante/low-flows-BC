
# It is necessary to download the climate data from:
# https://ftp.maps.canada.ca/pub/nrcan_rncan/Climate-archives_Archives-climatologiques/NAM_monthly/monthly_by_year/
# These are not included in the repository due to size limitations (~260 GB)
# The script will take a few hours to run

closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(sf)
library(tmap)
library(scico)
library(reshape2)
library(stringr)
library(mice)
library(readxl)
library(colf)
library(terra)


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


watersheds<-terra::vect("2.data/2.working/CatchmentPolygons/Watersheds_final.gpkg")%>%
  terra::project("EPSG:4269")


watersheds$ID_num<-1:nrow(watersheds)
data<-expand.grid(Year = 1900:2022,Month = 1:12,ID = watersheds$ID_num,mint=NA,maxt = NA,pcp = NA)%>% 
  arrange_all()
data<-left_join(data,data.frame(watersheds[,c("ID_num","StationNum","NameNom")]),by = c("ID" = "ID_num"))


# Loop through months from 1900 to 2022
tictoc::tic()
for(it in seq(1,nrow(data),by = nrow(watersheds))){

  if((((it-1)/nrow(watersheds)/12) %% 1) ==0 ){tictoc::toc()
    tictoc::tic()}
  mnth<-data$Month[it]%>%
    str_pad(2,side = "left",pad = "0")
  
  if(!file.exists(paste0("2.data/1.original/WeatherDataANUSPLIN/Monthly/",data$Year[it],"/mint60_",mnth,".tif"))){next}
  
  mint<-terra::rast(paste0("2.data/1.original/WeatherDataANUSPLIN/Monthly/",data$Year[it],"/mint60_",mnth,".tif"))
  maxt<-terra::rast(paste0("2.data/1.original/WeatherDataANUSPLIN/Monthly/",data$Year[it],"/maxt60_",mnth,".tif"))
  pcp<-terra::rast(paste0("2.data/1.original/WeatherDataANUSPLIN/Monthly/",data$Year[it],"/pcp60_",mnth,".tif"))
  
  
  data$mint[it:(it+nrow(watersheds)-1)]<-terra::extract(mint,watersheds,fun = mean,touches = TRUE)[,2]
  data$maxt[it:(it+nrow(watersheds)-1)]<-terra::extract(maxt,watersheds,fun = mean,touches = TRUE)[,2]
  data$pcp[it:(it+nrow(watersheds)-1)]<-terra::extract(pcp,watersheds,fun = mean,touches = TRUE)[,2]
  
  
  
  
}


data$Mean.Temp..C.<-(data$mint+data$maxt)/2
data$Total.Precip..mm.<-data$pcp

data<-data[,c("Year","Month","StationNum","NameNom","Mean.Temp..C.","Total.Precip..mm.")]
saveRDS(data,"2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")

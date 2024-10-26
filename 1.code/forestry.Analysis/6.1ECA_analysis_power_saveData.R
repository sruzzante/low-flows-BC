# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script prepares the data for the ECA power analysis.


closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(tidyr)

library("tseries")
library(forecast)
library(sf)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")

# Load Station metadata
stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")


# Load monthly weather data
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")%>%
  filter(ID%in%unique(streamDataMonthly$ID))

streamDataMonthly$NovWaterYear<-streamDataMonthly$year
streamDataMonthly$NovWaterYear[streamDataMonthly$month%in%c(11,12)]<-
  streamDataMonthly$year[streamDataMonthly$month%in%c(11,12)]+1

### ECA  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")



# maskPrivateBC<-crop(maskPrivate)



sum(stations$PrivateForestry<0.1)
sum(stations$PrivateForestry<0.1&stations$Total.Cut.Area>0.1)




stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)

stn_MC<-stn_MC[order(stn_MC$ID),]


# stns_with_brks<-c()

stn_MC_ls<-split(stn_MC,stn_MC$ID)

ECA<-ECA%>%
  dplyr::filter(StationNum%in%stn_MC$ID)
ECA<-ECA[order(ECA$StationNum),]
ECA_ls<-split(ECA,ECA$StationNum)




stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)



streamDataMonthly<-left_join(streamDataMonthly,stations[,c("ID","month_bgn","month_end")])

streamDataMonthly<-streamDataMonthly%>%
  filter(year>=1950&month%in% month_bgn:month_end)%>%
  group_by(ID,NovWaterYear)%>%
  dplyr::summarize(minSumFlow7 = min(minMonFlow7))


streamDataMonthly<-left_join(streamDataMonthly,ECA,by = c("NovWaterYear" = "year","ID" = "StationNum"))
streamDataMonthly<-dplyr::filter(streamDataMonthly,!is.na(minSumFlow7)&!is.na(ECA_60)&
                                   ID%in%stn_MC$ID)

streamDataMonthly<-streamDataMonthly[order(streamDataMonthly$ID),]
streamDataMonthly_ls<-split(streamDataMonthly,streamDataMonthly$ID)



Div<-lapply(paste0("2.data/2.working/WaterUse/estimates/",stn_MC$ID,".csv"),
            read.csv)
for(it in 1:nrow(stn_MC)){
  Div[[it]]$ID<-stn_MC$ID[it]
}
Div<-Div%>%
  bind_rows()
Div<-Div[,c("ID","year","Total.cms","August.cms","September.cms","October.cms")]

WeatherData<-left_join(WeatherData,Div,by = c("ID","Year"= "year"))


WeatherData<-WeatherData[(WeatherData$ID%in%stn_MC$ID),]
WeatherData<-WeatherData[order(WeatherData$ID),]
WeatherData_ls<-split(WeatherData,WeatherData$ID )

mdlFiles<-list.files("2.data/2.working/RegressionOptimization/BestModels/",
                     pattern = "step2_lm_.*rds")
bestModels<-lapply(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",
                          stn_MC$ID,".rds"),
                   readRDS)
names(bestModels)<-stn_MC$ID





save(list = c("bestModels","stn_MC_ls",
              "streamDataMonthly_ls","WeatherData_ls"),
     file = "2.data/2.working/ECA/ECA_power_analysis.RData"
)

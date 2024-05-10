# Prepare ECA power analysis data
closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(tidyr)

library("tseries")
library(forecast)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly_2.RDS")

# Load Station metadata
stations<-readRDS("2.data/2.working/StationMetadata/stations_final_2.RDS")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")

# Load monthly weather data
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly_2.RDS")%>%
  filter(ID%in%unique(streamDataMonthly$ID))

streamDataMonthly$NovWaterYear<-streamDataMonthly$year
streamDataMonthly$NovWaterYear[streamDataMonthly$month%in%c(11,12)]<-
  streamDataMonthly$year[streamDataMonthly$month%in%c(11,12)]+1

### ECA  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")



x<-rast("../DATA/1.Spatial_data/regional/Canada/lulc_landuse_landcover/lulc1_ForestManagement/Canada_MFv2020.tif")

maskPrivate<-x==50

plot(maskPrivate)
bc<-bcmaps::bc_neighbours()%>%filter(name=="British Columbia")%>%
  vect()%>%
  project(crs(maskPrivate))
maskPrivate_bc<-terra::mask(maskPrivate,bc)
summary(maskPrivate_bc)
global(maskPrivate_bc,mean,na.rm = TRUE)
plot(maskPrivate_bc)

# maskPrivateBC<-crop(maskPrivate)

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")%>%
  st_transform(st_crs(maskPrivate))

watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private[,"Canada_MFv2020"])
watersheds$PrivateForestry<-watersheds$watersheds_Private....Canada_MFv2020..
stations<-left_join(stations,st_drop_geometry(watersheds[,c("ID","PrivateForestry")]),by = c("ID"))

TCA<-readRDS("2.data/2.working/ECA/TCA_2022.rds")
names(TCA)[3]<-"Total.Cut.Area"
stations<-left_join(stations,TCA%>%select(StationNum,Total.Cut.Area),by = c("ID" = "StationNum"))

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

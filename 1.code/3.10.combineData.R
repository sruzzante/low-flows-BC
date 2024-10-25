# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-07

# This script combines and saves clean versions of various pieces of data.
# It also runs the regime classification algorithm and includes the results in the saved data



closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(tmap)


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")

stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")

splitString<- regex("\u00B0|\u2032|\u2033|\u201D|\u2019|'|\\\"| |o")
stations$Lat<- as.numeric(str_split_i(stations$Latitude,splitString,1))+
  + as.numeric(str_split_i(stations$Latitude,splitString,2))/60+
  + as.numeric(str_split_i(stations$Latitude,splitString,3))/3600
stations$Lon<- - as.numeric(str_split_i(stations$Longitude,splitString,1))+
  - as.numeric(str_split_i(stations$Longitude,splitString,2))/60+
  - as.numeric(str_split_i(stations$Longitude,splitString,3))/3600
stations$Lon[stations$ID=="08NE006"]<- -117.730643


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%st_transform("EPSG:4326")


WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")


WeatherData<-WeatherData%>%
  # mutate(ID = StationNum)%>%
  select(c(Year,Month,ID,Mean.Temp..C.,Total.Precip..mm.))

watersheds<-watersheds%>%filter(ID%in%stations$ID)
WeatherData<-WeatherData%>%filter(ID%in%stations$ID)
streamDataAll<-streamDataAll%>%filter(ID%in%stations$ID)

unique(streamDataAll$ID)
streamDataAll<-streamDataAll%>%select(!Discharge7)

streamDataCast<-streamDataAll%>%
  pivot_wider(id_cols = c(Date,year,month,day),
              names_from = ID,
              values_from = Discharge)
streamDataCast<-streamDataCast[order(streamDataCast$Date),]

funFillNa<-function(Discharge){
  DischargeRoll<-RcppRoll::roll_mean(Discharge,n=5,
                                     # weights =c(0.333,0.666,1,0.666,0.333),
                                     # normalize = TRUE,
                                     na.rm = TRUE,
                                     fill = NA)
  
  Discharge[is.na(Discharge)]<-DischargeRoll[is.na(Discharge)]
  return(Discharge)
}
streamDataCast_fill<-cbind(streamDataCast[,1:4],
                           streamDataCast%>%
                             select(!c(Date,year,month,day))%>%
                             apply(MARGIN =2,FUN = funFillNa))

streamDataCast7<-streamDataCast_fill%>%
  dplyr::select(!c(Date,year,month,day))%>%
  apply(2,
        FUN = function(x){RcppRoll::roll_meanr(x,n = 7,fill = NA)})%>%
  as.data.frame()

streamDataCast7<-cbind(streamDataCast_fill[,1:4],streamDataCast7)
streamData7<-streamDataCast7%>%
  tidyr::pivot_longer(cols = !c(Date,year,month,day),
                      names_to = "ID",
                      values_to = "Discharge7")


streamDataAll<-left_join(streamDataAll,streamData7)
rm(streamDataCast,streamData7,streamDataCast7,streamDataCast_fill)


SWE<-readRDS("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment.RDS")



SWE$year<-year(SWE$variable)
SWE$month<-month(SWE$variable)
SWE.2<-SWE%>%
  filter(year %in%(1991:2020))%>%
  group_by(ID,month)%>%
  dplyr::summarize(SWE = mean(value))

SWE.2.b<-SWE.2%>%
  group_by(ID)%>%
  dplyr::summarize(maxSWE = max(SWE),
                   minSWE = min(SWE))

SWE.2<-left_join(SWE.2,SWE.2.b)
SWE.3<-SWE.2%>%
  group_by(ID)%>%
  dplyr::summarize(SDD = min(month[(SWE<((maxSWE-minSWE)*0.1+minSWE))|SWE<0.001]),
                   SAD = max(month[(SWE<((maxSWE-minSWE)*0.1+minSWE))|SWE<0.001]),
                   SMD = month[which.max(SWE)])

SWE.3$SMD[SWE.3$SMD==12]<-0
stations<-left_join(stations,SWE.3)


data_glaciers<-readRDS("2.data/2.working/GlacierRasters/glacier_change.RDS")%>%
  filter(year%in%(1991:2020))%>%
  group_by(ID)%>%
  dplyr::summarise(perc_Gl = mean(perc_Gl))


## Regime Classification

COT_func<-function(x){
  AD=sum(x)
  cumQ<-cumsum(x)
  ind = which(cumQ>AD/2)[1]
  return(ind)
}

stations$minFlowDate<-as.Date(NA)
stations$minSumFlowDate<-as.Date(NA)
stations$minWinterQ<-NA
stations$minSummerQ<-NA
stations$maxLateSumQ<-NA
stations$freshetFactor<-NA
stations$summerRainFactor<-NA
stations$regime<-NA

stations<-select(stations,!perc_Gl)%>%
  left_join(data_glaciers)


for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID%in%"13235000")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]%>%
    filter(year%in%(1991:2020))
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$DayOfYear[streamData$DayOfYear==366]<-365
  
  streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
  streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
  
  
  dataSmooth2<-streamData%>%group_by(DateFake,DayOfYear)%>%
    dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE))
  
  
  
  dataSmooth2$month<-month(dataSmooth2$DateFake)
  
  coldMonths<-dataSmooth2$month>stations$SAD[it_stn]|
    dataSmooth2$month<stations$SDD[it_stn]
  
  warmMonths<-!coldMonths
  warmMonths.p1<-warmMonths|(dataSmooth2$month==((stations$SAD[it_stn]%%12)+1))
  
  
  minMonths<- dataSmooth2$month %in%(((stations$SMD[it_stn]-1):(stations$SMD[it_stn]+1)-1)%%12+1)
  
  
  springMonths<-dataSmooth2$month>=stations$SMD[it_stn]&
    dataSmooth2$month<=stations$SDD[it_stn]
  
  # springMonths.p1<-springMonths|dataSmooth2$month=stations$SDD[it_stn]
  
  dataSmooth2Summer<-dataSmooth2[warmMonths,]
  dataSmooth2Summer$Discharge_deriv<-c(dataSmooth2Summer$Discharge[2:nrow(dataSmooth2Summer)]-
                                         dataSmooth2Summer$Discharge[1-(nrow(dataSmooth2Summer)-1)],NA)
  
  firstNegative<-which(dataSmooth2Summer$Discharge_deriv<0)%>%min
  dataSmooth2Summer<-dataSmooth2Summer[firstNegative:nrow(dataSmooth2Summer),]
  minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
  stations$minFlowDate[it_stn]<-minDischargeDate
  minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
  
  
  stations$minSumFlowDate[it_stn]<-minSumDischargeDate
  
  
  # Classify as snow-affected if there is a freshet
  
  
  if(sum(coldMonths)==0){
    stations$regime[it_stn]<-"Rainfall"
    stations$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[!coldMonths])
    ind_min<-which(dataSmooth2$Discharge==stations$minSummerQ[it_stn]&!coldMonths )
    
    stations$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[!coldMonths&
                                                              dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations$summerRainFactor[it_stn]<-stations$maxLateSumQ[it_stn]/stations$minSummerQ[it_stn]
    
  }else{
    
    stations$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[minMonths])
    
    ind_min<-which(dataSmooth2$Discharge==stations$minWinterQ[it_stn]&minMonths)
    
    stations$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[springMonths&
                                                              dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations$freshetFactor[it_stn]<-stations$maxFreshetQ[it_stn]/stations$minWinterQ[it_stn]
    
    stations$minSummerQ[it_stn]<-min(dataSmooth2Summer$Discharge)
    ind_min<-which(dataSmooth2Summer$Discharge==stations$minSummerQ[it_stn])
    
    stations$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[warmMonths.p1&
                                                              dataSmooth2$DateFake>dataSmooth2Summer$DateFake[ind_min]])
    
    stations$summerRainFactor[it_stn]<-stations$maxLateSumQ[it_stn]/stations$minSummerQ[it_stn]
    
    stations$summerRainFactor[it_stn]
    stations$freshetFactor[it_stn]
    snow = FALSE
    rain = FALSE
    
    if(stations$freshetFactor[it_stn]>1.5){
      snow = TRUE
    }
    if(stations$summerRainFactor[it_stn]>1.5){
      rain = TRUE
    }
    
    if(rain&snow){
      stations$regime[it_stn]<-"Hybrid"
    }else if(rain){
      stations$regime[it_stn]<-"Rainfall"
    }else if(snow){
      stations$regime[it_stn]<-"Snowfall"
    }else{
      stations$regime[it_stn]<-NA
    }
    
    if(stations$minSummerQ[it_stn]/stations$minWinterQ[it_stn]>2){
      
    }
    
    
    
    
    if(stations$perc_Gl[it_stn] > 0.05){
      stations$regime[it_stn]<-"Glacial"
    }
  }
  
  
  
  # Reclassify two Tarundl Creek and Honna River
  if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
    stations$regime[it_stn]<-"Rainfall"
  }
  
  
}

stations$minSumFlowMonth<-month(stations$minSumFlowDate)


stations_sf<-st_as_sf(stations,coords = c("Lon","Lat"),crs = "EPSG:4326")

stations_sf$regime<-factor(stations_sf$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                           ordered = TRUE)
summary(stations_sf$regime)
# stations_sf<-left_join(stations_sf,stns)
# stations_sf2<-stations_sf
tmap_mode("view")
tm_shape(stations_sf)+
  tm_dots(col = "regime",
          popup.vars = c("ID","Station.Name","regime","perc_Gl"),
          palette = c("#DF9C41","#599D7A","#B2E3F0","#FCF4D9"))





streamDataMonthly<-streamDataAll%>%
  group_by(ID,year,month)%>%
  dplyr::summarize(
    
    numMonQnonNans = sum(!is.na(Discharge)),
    numMonQNans = sum(is.na(Discharge)),
    
    minMonFlow = min(Discharge,na.rm = TRUE),
    
    
  )
streamDataMonthly$daysinmonth<-days_in_month(ym(paste(streamDataMonthly$year,streamDataMonthly$month,sep = "/")))
streamDataMonthly$minMonFlow[streamDataMonthly$numMonQnonNans<(streamDataMonthly$daysinmonth-5)]<-NA

stations$DetectLim<-0.001

streamDataMonthly<-left_join(streamDataMonthly,stations[,c("ID","SDD","SAD","DetectLim","minSumFlowMonth")])
streamDataMonthly2<-dplyr::filter(streamDataMonthly,
                                  month>=(minSumFlowMonth-1)&month<=(minSumFlowMonth+1))
stns<-streamDataMonthly2%>%
  group_by(ID,year)%>%
  dplyr::summarize(nZero = sum(minMonFlow<DetectLim,na.rm = TRUE),
                   nNA = sum(is.na(minMonFlow)))%>%
  filter(nNA==0)%>%
  group_by(ID)%>%
  dplyr::summarize(
    nZeroYrs = sum(nZero>0),
    nYrs = length(nZero)
  )

stns$intermittent<-(stns$nZeroYrs/stns$nYrs)>0.05
stations<-select(stations,!c(intermittent,nZero,nYears))
stations<-left_join(stations,stns)


sum(stations$intermittent)

streamDataAll<-left_join(streamDataAll,stations[,c("ID","DetectLim")])

streamDataAll$Discharge[which(streamDataAll$Discharge<streamDataAll$DetectLim)]<-
  0.5*streamDataAll$DetectLim[which(streamDataAll$Discharge<streamDataAll$DetectLim)]


stations<-stations%>%filter(!intermittent)
streamDataAll<-streamDataAll%>%dplyr::filter(ID%in%stations$ID)
WeatherData<-WeatherData%>%dplyr::filter(ID%in%stations$ID)
watersheds<-watersheds%>%dplyr::filter(ID%in%stations$ID)



streamDataAll<-streamDataAll%>%select(!Discharge7)

streamDataCast<-streamDataAll%>%
  pivot_wider(id_cols = c(Date,year,month,day),
              names_from = ID,
              values_from = Discharge)
streamDataCast<-streamDataCast[order(streamDataCast$Date),]

funFillNa<-function(Discharge){
  DischargeRoll<-RcppRoll::roll_mean(Discharge,n=5,
                                     # weights =c(0.333,0.666,1,0.666,0.333),
                                     # normalize = TRUE,
                                     na.rm = TRUE,
                                     fill = NA)
  
  Discharge[is.na(Discharge)]<-DischargeRoll[is.na(Discharge)]
  return(Discharge)
}
streamDataCast_fill<-cbind(streamDataCast[,1:4],
                           streamDataCast%>%
                             select(!c(Date,year,month,day))%>%
                             apply(MARGIN =2,FUN = funFillNa))

streamDataCast7<-streamDataCast_fill%>%
  dplyr::select(!c(Date,year,month,day))%>%
  apply(2,
        FUN = function(x){RcppRoll::roll_meanr(x,n = 7,fill = NA)})%>%
  as.data.frame()

streamDataCast7<-cbind(streamDataCast_fill[,1:4],streamDataCast7)
streamData7<-streamDataCast7%>%
  tidyr::pivot_longer(cols = !c(Date,year,month,day),
                      names_to = "ID",
                      values_to = "Discharge7")


streamDataAll<-left_join(streamDataAll,streamData7)
rm(streamDataCast,streamData7,streamDataCast7,streamDataCast_fill)





streamDataAll$DayOfYear<-lubridate::yday(ymd(streamDataAll$Date))

streamDataMonthly<-streamDataAll%>%
  group_by(ID,year,month)%>%
  dplyr::summarize(
    
    numMonQnonNans = sum(!is.na(Discharge)),
    numMonQNans = sum(is.na(Discharge)),
    
    minMonFlow7 = min(Discharge7,na.rm = TRUE),
    
    
  )

streamDataMonthly<-streamDataMonthly[order(streamDataMonthly$ID),]



streamDataMonthly$minMonFlow7[streamDataMonthly$numMonQnonNans<(days_in_month(streamDataMonthly$month)-5)]<-NA


streamDataMonthly$minMonFlow7[is.infinite(streamDataMonthly$minMonFlow7)]<-NA
# streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
# streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA

# streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA

## Water use estimates

fls<-list.files("2.data/2.working/WaterUse/estimates/")
Div<-lapply(paste0("2.data/2.working/WaterUse/estimates/",fls),
            read.csv)
Div2<- mapply(FUN = cbind,Div,ID =split(str_remove(fls,".csv"),f= fls),
              SIMPLIFY = FALSE)
Div3<-Div2%>%
  bind_rows()

watersheds<-st_transform(watersheds,st_crs("EPSG:3005"))
watersheds$Area_km2<-as.numeric(st_area(watersheds)/10^6)
stations<-select(stations,!Area_km2)
stations<-left_join(stations,st_drop_geometry(watersheds))

watersheds<-select(watersheds,!Area_km2)


#Resave these data
saveRDS(stations,"2.data/2.working/stationMetadata/stations_final.RDS")
saveRDS(streamDataAll,"2.data/2.working/Discharge/streamDataFinal.rds")
saveRDS(streamDataMonthly,"2.data/2.working/Discharge/streamDataMonthly.RDS")
saveRDS(WeatherData,"2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")
st_write(watersheds,"2.data/2.working/CatchmentPolygons/watersheds_final.gpkg",append = FALSE)
saveRDS(Div3,"2.data/2.working/WaterUse/estimates_all.RDS")

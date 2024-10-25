# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-12


# This script examines the sensitivity of the regime classification to the SWE data. It perturbs the SDD, SAD, and SMD by +/- 1 month and reruns the classfication algorithm

# closeAllConnections()
# rm(list=ls())
# graphics.off()

library(dplyr)
library(sf)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(tmap)

library(tictoc)
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


stations<-select(stations,!perc_Gl)%>%
  left_join(data_glaciers)

delDat<-expand.grid(
  del_SDD = c(-1,0,1),
  del_SAD = c(-1,0,1),
  del_SMD = c(-1,0,1)
)
stations_dels<-data.frame()
for(it in 1:27){
  tic(sprintf("starting loop %d",it))
  stations_x<-stations
  stations_x$SAD<-stations_x$SAD+delDat$del_SAD[it]
  stations_x$SMD<-stations_x$SMD+delDat$del_SMD[it]
  stations_x$SDD<-stations_x$SDD+delDat$del_SDD[it]
  
  
  stations_x$minFlowDate<-as.Date(NA)
  stations_x$minSumFlowDate<-as.Date(NA)
  stations_x$minWinterQ<-NA
  stations_x$minSummerQ<-NA
  stations_x$maxLateSumQ<-NA
  stations_x$freshetFactor<-NA
  stations_x$summerRainFactor<-NA
  stations_x$regime<-NA
  for(it_stn in 1:length(stations_x$ID)){
    # it_stn = which(stations_x$ID%in%"13235000")
    streamData<-streamDataAll[streamDataAll$ID==stations_x$ID[it_stn],]%>%
      filter(year%in%(1991:2020))
    
    
    streamData$DayOfYear<-yday(ymd(streamData$Date))
    streamData$DayOfYear[streamData$DayOfYear==366]<-365
    
    streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
    streamData$Discharge30<-RcppRoll::roll_meanr(streamData$Discharge,n = 30,fill = NA)
    
    
    dataSmooth2<-streamData%>%group_by(DateFake,DayOfYear)%>%
      dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE))
    
    
    
    dataSmooth2$month<-month(dataSmooth2$DateFake)
    
    coldMonths<-dataSmooth2$month>stations_x$SAD[it_stn]|
      dataSmooth2$month<stations_x$SDD[it_stn]
    
    warmMonths<-!coldMonths
    warmMonths.p1<-warmMonths|(dataSmooth2$month==((stations_x$SAD[it_stn]%%12)+1))
    
    
    minMonths<- dataSmooth2$month %in%(((stations_x$SMD[it_stn]-1):(stations_x$SMD[it_stn]+1)-1)%%12+1)
    
    
    springMonths<-dataSmooth2$month>=stations_x$SMD[it_stn]&
      dataSmooth2$month<=stations_x$SDD[it_stn]
    
    # springMonths.p1<-springMonths|dataSmooth2$month=stations_x$SDD[it_stn]
    
    dataSmooth2Summer<-dataSmooth2[warmMonths,]
    dataSmooth2Summer$Discharge_deriv<-c(dataSmooth2Summer$Discharge[2:nrow(dataSmooth2Summer)]-
                                           dataSmooth2Summer$Discharge[1-(nrow(dataSmooth2Summer)-1)],NA)
    
    firstNegative<-which(dataSmooth2Summer$Discharge_deriv<0)%>%min
    if(!is.infinite(firstNegative)){
      dataSmooth2Summer<-dataSmooth2Summer[firstNegative:nrow(dataSmooth2Summer),]
      
    }
    minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
    stations_x$minFlowDate[it_stn]<-minDischargeDate
    minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
    
    
    stations_x$minSumFlowDate[it_stn]<-minSumDischargeDate
    
    
    # Classify as snow-affected if there is a freshet
    
    
    if(sum(coldMonths)==0){
      stations_x$regime[it_stn]<-"Rainfall"
      stations_x$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[!coldMonths])
      ind_min<-which(dataSmooth2$Discharge==stations_x$minSummerQ[it_stn]&!coldMonths )
      
      stations_x$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[!coldMonths&
                                                                  dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
      
      stations_x$summerRainFactor[it_stn]<-stations_x$maxLateSumQ[it_stn]/stations_x$minSummerQ[it_stn]
      
    }else{
      
      stations_x$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[minMonths])
      
      ind_min<-which(dataSmooth2$Discharge==stations_x$minWinterQ[it_stn]&minMonths)
      
      stations_x$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[springMonths&
                                                                  dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
      
      stations_x$freshetFactor[it_stn]<-stations_x$maxFreshetQ[it_stn]/stations_x$minWinterQ[it_stn]
      
      stations_x$minSummerQ[it_stn]<-min(dataSmooth2Summer$Discharge)
      ind_min<-which(dataSmooth2Summer$Discharge==stations_x$minSummerQ[it_stn])
      
      stations_x$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[warmMonths.p1&
                                                                  dataSmooth2$DateFake>dataSmooth2Summer$DateFake[ind_min]])
      
      stations_x$summerRainFactor[it_stn]<-stations_x$maxLateSumQ[it_stn]/stations_x$minSummerQ[it_stn]
      
      stations_x$summerRainFactor[it_stn]
      stations_x$freshetFactor[it_stn]
      snow = FALSE
      rain = FALSE
      
      if(stations_x$freshetFactor[it_stn]>1.5){
        snow = TRUE
      }
      if(stations_x$summerRainFactor[it_stn]>1.5){
        rain = TRUE
      }
      
      if(rain&snow){
        stations_x$regime[it_stn]<-"Hybrid"
      }else if(rain){
        stations_x$regime[it_stn]<-"Rainfall"
      }else if(snow){
        stations_x$regime[it_stn]<-"Snowfall"
      }else{
        stations_x$regime[it_stn]<-NA
      }
      
      if(stations_x$minSummerQ[it_stn]/stations_x$minWinterQ[it_stn]>2){
        
      }
      
      
      
      
      if(stations_x$perc_Gl[it_stn] > 0.05){
        stations_x$regime[it_stn]<-"Glacial"
      }
    }
    
    
    
    # Reclassify two Tarundl Creek and Honna River
    if(stations_x$ID[it_stn]%in%c("08OA004","08OA005")){
      stations_x$regime[it_stn]<-"Rainfall"
    }
    
    
    # 
    #     p1<-ggplot(streamData,aes(DateFake,Discharge))+
    #       geom_line(col = "gray50",alpha = 0.1,aes(group = year))+
    # 
    #       geom_line(data = dataSmooth2,aes(col = "Average Q30"),linewidth = 1)+
    #       annotate("text",label = paste(stations_x$Station.Name[it_stn],
    #                                     "\nSummer-Autumn Minimum Flow Date: ",substr(minSumDischargeDate,6,10),
    #                                     "\nregime: ",stations_x$regime[it_stn]),
    #                x= as.Date("1999-01-01"),y =max(streamData$Discharge,na.rm = TRUE)/2,hjust = 0,
    #                size = 5)+
    # 
    #       geom_vline(xintercept = as.Date(minSumDischargeDate))+
    # 
    #       scale_x_date(date_labels = "%b",name = "Date")+
    #       scale_y_log10(name = "Discharge (cms)")+
    #       scale_color_manual(name = NULL,
    #                          values = c("blue","red"))+
    #       theme_bw()
    #     p1
    # 
    # 
    #     watershed<-watersheds2[watersheds2$GAGE_ID==stations_x$ID[it_stn],]
    #     p_map<-
    #       ggplot()+geom_sf(data = basemap_sf)+
    #       geom_sf(data = stations_x_sf[it_stn,],col = "blue",fill = "lightblue",
    #               shape = 24)+
    #       geom_sf(data = watershed,col = "blue",fill = "lightblue")+
    # 
    #       theme_void()
    # 
    #     plot.with.inset <-
    #       ggdraw() +
    #       draw_plot(p1) +
    #       draw_plot(p_map, x = .8, y = 0.8, width = .2, height = .2)
    #   #   ggsave(paste0("3.figures/Hydrographs/Yearly-CA-",stations_x$Province[it_stn],"-",stations_x$ID[it_stn],".png"),plot.with.inset,width = 10,height = 6)
    #   
    #   
    
    streamDataYrly<-streamData%>%
      group_by(wateryear)%>%
      dplyr::summarize(COT = COT_func(Discharge),
                       numQDays = sum(!is.na(Discharge)))
    
    streamDataYrly<-streamDataYrly[streamDataYrly$numQDays>=365,]
    
    stations_x$COT[it_stn]<-median(streamDataYrly$COT,na.rm = TRUE)
    
    
  }
  
  stations_dels<-rbind(stations_dels,
                       stations_x%>%mutate(
                         del_SAD = delDat$del_SAD[it],
                         del_SMD = delDat$del_SMD[it],
                         del_SDD = delDat$del_SDD[it]
                       ))
  toc()
}


saveRDS(stations_dels,"2.data/2.working/StationMetadata/stations_SWE_regime_sensitivity.RDS")
stations_dels<-readRDS("2.data/2.working/StationMetadata/stations_SWE_regime_sensitivity.RDS")
watersheds<-st_transform(watersheds,st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
watersheds$Area_km2<-(st_area(watersheds)/10^6)%>%as.numeric()

stations_dels<-left_join(stations_dels%>%dplyr::select(!Area_km2),
                         watersheds%>%dplyr::select(ID,Area_km2)%>%
                           st_drop_geometry(),
                         by = c("ID" ))
stations_dels%>%
  # dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  dplyr::filter(Area_km2<100)%>%
  group_by(ID,Area_km2)%>%
  dplyr::summarize(rainfall_frac = sum(regime=="Rainfall"),
                   hybrid_frac = sum(regime=="Hybrid"),
                   snowmelt_frac = sum(regime=="Snowfall"),
                   glacial_frac = sum(regime =="Glacial"))%>%
  print(n = 100)


stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

station_sensitive<-
  stations_dels%>%
  left_join(stations%>%dplyr::select(ID,regime),by = "ID",suffix = c(".delta",".orig"))%>%
  # dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  dplyr::filter(Area_km2<100)%>%
  # filter(del_SAD>=0)%>%
  group_by(ID,Area_km2)%>%
  dplyr::summarize(correct_frac = sum(regime.delta==regime.orig))%>%
  filter(correct_frac<27)



stations_dels%>%
  filter(ID %in%station_sensitive$ID)%>%
  
  dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  dplyr::filter(Area_km2<100)%>%
  group_by(ID,Area_km2)%>%
  dplyr::summarize(rainfall_frac = sum(regime=="Rainfall"),
                   hybrid_frac = sum(regime=="Hybrid"),
                   snowmelt_frac = sum(regime=="Snowfall"),
                   glacial_frac = sum(regime =="Glacial"))



stations_dels%>%
  left_join(stations%>%dplyr::select(ID,regime),by = "ID",suffix = c(".delta",".orig"))%>%
  # filter(del_SAD>=0)%>%
  
  dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  # dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  dplyr::filter(Area_km2<100)%>%
  dplyr::filter(regime.delta!=regime.orig)%>%
  arrange(ID,del_SAD,del_SMD,del_SMD)%>%
  select(ID,Station.Name,nYears,Area_km2,del_SDD,SDD,del_SAD,SAD,del_SMD,SMD,regime.orig,regime.delta)
  
dem_10km<-terra::rast("2.data/2.working/dem_merit_ERAgrid.tif")

station_metadata<-read.csv("../DATA/BC_Watersheds/2.data/2.working/StationMetadata/stations_metadata.csv")%>%
  filter(ID %in%station_sensitive$ID)

watersheds_sensitive<-watersheds%>%filter(ID%in%station_sensitive$ID)

elev<-terra::extract(dem_10km,watersheds_sensitive)%>%
  mutate(ID=plyr::mapvalues(ID,from = 1:12,to=watersheds_sensitive$ID))%>%
  group_by(ID)%>%
  summarise(ERA_grid_elev = mean(dtm_elevation_merit.dem_m_250m_s0..0cm_2017_v1.0),
            N=n())%>%
  left_join(station_metadata%>%select(ID,dtm.elevation.merit.mean,dtm.elevation.merit.min,dtm.elevation.merit.max))%>%
  mutate(elev_diff = ERA_grid_elev-dtm.elevation.merit.mean)


summary(elev$ERA_grid_elev-elev$dtm.elevation.merit.mean)


stations_dels%>%
  left_join(stations%>%dplyr::select(ID,regime),by = "ID",suffix = c(".delta",".orig"))%>%
  # filter(del_SAD>=0)%>%
  
  dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  # dplyr::filter((abs(del_SAD)+abs(del_SMD)+abs(del_SDD))<=1)%>%
  dplyr::filter(Area_km2<100)%>%
  dplyr::filter(regime.delta!=regime.orig)%>%
  arrange(ID,del_SAD,del_SMD,del_SMD)%>%
  select(ID,Station.Name,nYears,Area_km2,del_SDD,SDD,del_SAD,SAD,del_SMD,SMD,regime.orig,regime.delta)%>%
  left_join(elev%>%select(ID,elev_diff))
library(dplyr)
WeatherData%>%
  filter(ID=="08LE077"&Year %in% (1991:2020))%>%
  group_by(Month)%>%
  dplyr::summarise(mean(Mean.Temp..C.))

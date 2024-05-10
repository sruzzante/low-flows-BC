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


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%st_transform("EPSG:4326")%>%
  dplyr::select(c("StationNum"))%>%
  dplyr::rename(ID = StationNum)


WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")


WeatherData<-WeatherData%>%
  mutate(ID = StationNum)%>%
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
  filter(year %in%(1985:2014))%>%
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
## Regime Classification

COT_func<-function(x){
  AD=sum(x)
  cumQ<-cumsum(x)
  ind = which(cumQ>AD/2)[1]
  return(ind)
}

# I could fix this by enforcing a decrease in Q before looking for summer minimum
# stations$SDD[stations$ID%in%c("08AA008","09AC007","09AG001","10243260")]<-7

stations$minFlowDate<-as.Date(NA)
stations$minSumFlowDate<-as.Date(NA)
stations$minWinterQ<-NA
stations$minSummerQ<-NA
stations$maxLateSumQ<-NA
stations$freshetFactor<-NA
stations$summerRainFactor<-NA
stations$regime<-NA

for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID%in%"13235000")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
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
    
    if(stations$freshetFactor[it_stn]>2){
      snow = TRUE
    }
    if(stations$summerRainFactor[it_stn]>1.05){
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
    
    
    
    
    if(stations$perc_Gl[it_stn] >0.05){
      stations$regime[it_stn]<-"Glacial"
    }
  }
  
  
  
  # Reclassify two Tarundl Creek and Honna River
  if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
    stations$regime[it_stn]<-"Rainfall"
  }
  
  
  # 
  #     p1<-ggplot(streamData,aes(DateFake,Discharge))+
  #       geom_line(col = "gray50",alpha = 0.1,aes(group = year))+
  # 
  #       geom_line(data = dataSmooth2,aes(col = "Average Q30"),linewidth = 1)+
  #       annotate("text",label = paste(stations$Station.Name[it_stn],
  #                                     "\nSummer-Autumn Minimum Flow Date: ",substr(minSumDischargeDate,6,10),
  #                                     "\nregime: ",stations$regime[it_stn]),
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
  #     watershed<-watersheds2[watersheds2$GAGE_ID==stations$ID[it_stn],]
  #     p_map<-
  #       ggplot()+geom_sf(data = basemap_sf)+
  #       geom_sf(data = stations_sf[it_stn,],col = "blue",fill = "lightblue",
  #               shape = 24)+
  #       geom_sf(data = watershed,col = "blue",fill = "lightblue")+
  # 
  #       theme_void()
  # 
  #     plot.with.inset <-
  #       ggdraw() +
  #       draw_plot(p1) +
  #       draw_plot(p_map, x = .8, y = 0.8, width = .2, height = .2)
  #   #   ggsave(paste0("3.figures/Hydrographs/Yearly-CA-",stations$Province[it_stn],"-",stations$ID[it_stn],".png"),plot.with.inset,width = 10,height = 6)
  #   
  #   
  
  streamDataYrly<-streamData%>%
    group_by(wateryear)%>%
    dplyr::summarize(COT = COT_func(Discharge),
                     numQDays = sum(!is.na(Discharge)))
  
  streamDataYrly<-streamDataYrly[streamDataYrly$numQDays>=365,]
  
  stations$COT[it_stn]<-median(streamDataYrly$COT,na.rm = TRUE)
  
  
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
          palette = c("#005804","#599D7A","#B2E3F0","#FCF4D9"))





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

streamDataMonthly<-left_join(streamDataMonthly,Div3[,c("ID","year","Total.cms")])





# Regime classification comparison to other authors
stations$regime_Wenger<-NA

stations$regime_Wenger[stations$COT<150]<-"Rainfall"
stations$regime_Wenger[stations$COT>200]<-"Snowfall"
stations$regime_Wenger[stations$COT>=150&stations$COT<=200]<-"Hybrid"
stations$regime_Wenger<-factor(stations$regime_Wenger,levels = c("Rainfall","Hybrid","Snowfall"))
stations$regime<-factor(stations$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"))
table(stations[,c("regime","regime_Wenger")])

# Fleming et al (2007)
regimes_fleming<-read.csv("2.data/2.working/StationMetadata/Fleming_classification.csv")

x<-inner_join(stations%>%select(ID,regime),regimes_fleming)
x
table(x$regime,x$Regime_Fleming)
# Dery et al (2009)
regimes_dery<-read.csv("2.data/2.working/StationMetadata/Dery_classification.csv")
x2<-inner_join(stations%>%select(ID,regime),regimes_dery%>%select(ID,Regime))
x2
table(x2$regime,x2$Regime)
## Chang et al (2012)
watersheds4326<-terra::project(terra::vect(watersheds),"EPSG:4326")

elevation<-terra::rast("D:/DATA/1.Spatial_data/global/dem_topography/dem_derivatives_Hengl/dem1_derivatives/dtm_elevation_merit.dem_m_250m_s0..0cm_2017_v1.0.tif")
stationsMASL<-terra::extract(elevation,watersheds,fun = mean)
stationsMASL$ID<-watersheds$ID

stationsMASL$regime_Chang<-cut(stationsMASL$dtm_elevation_merit.dem_m_250m_s0..0cm_2017_v1.0,
                               breaks = c(0,1000,2000,Inf),
                               labels = c("Rainfall","Hybrid","Snowmelt"))


x3<-inner_join(stations%>%select(ID,regime),
               stationsMASL%>%select(ID,regime_Chang))
table(x3[,c("regime","regime_Chang")])


# Wade et al (2001)
regimes_wade<-read.csv("2.data/2.working/StationMetadata/Wade_classification.csv")
regimes_wade$regime_wade<-factor(regimes_wade$regime_wade,levels = c("Rainfall","Hybrid","Snow.Glacial"))
x4<-inner_join(stations%>%select(ID,regime),regimes_wade%>%select(ID,regime_wade))
table(x4%>%select(regime, regime_wade))


# Cooper et al (2001)
SWE<-read.csv("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.csv")
SWE$date<-SWE$variable%>%ymd()
SWE$month<-month(SWE$date)
SWE$year<-year(SWE$date)

SWE$wateryear<-SWE$year
SWE$wateryear[SWE$month%in%c(10,11,12)]<-
  SWE$year[SWE$month%in%c(10,11,12)]+1

SWE_min_val<-SWE%>%
  group_by(ID)%>%
  summarise(minSWE = min(value))# Need to subtract the glaciers


SWE_yrly_max<-SWE%>%
  group_by(ID,wateryear)%>%
  summarize(maxSWE.mm = max(value)*1000)%>%
  left_join(SWE_min_val)%>%
  mutate(maxSWE.mm = maxSWE.mm-minSWE)

WeatherData$wateryear<-WeatherData$Year
WeatherData$wateryear[WeatherData$Month%in%c(10,11,12)]<-
  WeatherData$Year[WeatherData$Month%in%c(10,11,12)]+1

P_yearly<-WeatherData%>%
  group_by(ID,wateryear)%>%
  summarise(totP = sum(Total.Precip..mm.))

SWE.P<-inner_join(SWE_yrly_max,P_yearly,by = c("ID","wateryear"))

SWE.P$SWE.P.ratio<-SWE.P$maxSWE.mm/SWE.P$totP
SWE.P.stn<-SWE.P%>%
  group_by(ID)%>%
  summarise(SWE.P.ratio = mean(SWE.P.ratio))

SWE.P.stn$regime_cooper<-cut(SWE.P.stn$SWE.P.ratio,c(0,0.2,Inf),labels = c("Rainfall","Snowfall"))
  
summary(SWE.P.stn$regime_cooper)

x5<-inner_join(SWE.P.stn,stations%>%select(ID,regime))

table(x5$regime,x5$regime_cooper)

regimes_wade<-read.csv("2.data/2.working/StationMetadata/Wade_classification.csv")
regimes_wade$regime_wade<-factor(regimes_wade$regime_wade,levels = c("Rainfall","Hybrid","Snow.Glacial"))
x4<-inner_join(stations%>%select(ID,regime),regimes_wade%>%select(ID,regime_wade))
table(x4%>%select(regime, regime_wade))

# before/after 1997 regimes ###############




stations_pre1997<-stations%>%select(ID,Station.Name,perc_Gl,SDD,SAD,SMD)

stations_pre1997$minFlowDate<-as.Date(NA)
stations_pre1997$minSumFlowDate<-as.Date(NA)
stations_pre1997$minWinterQ<-NA
stations_pre1997$minSummerQ<-NA
stations_pre1997$maxLateSumQ<-NA
stations_pre1997$freshetFactor<-NA
stations_pre1997$summerRainFactor<-NA
stations_pre1997$regime<-NA

for(it_stn in 1:length(stations_pre1997$ID)){
  # it_stn = which(stations_pre1997$ID%in%"08HD011")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$DayOfYear[streamData$DayOfYear==366]<-365
  
  streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
  streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
  
    
  dataSmooth2<-streamData%>%
    filter(wateryear<=1999)%>%
    group_by(DateFake,DayOfYear)%>%
    dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE),
                     n = sum(!is.na(Discharge30)))
  if(nrow(dataSmooth2)<365){next}
  if(min(dataSmooth2$n)<10){next}

  
  dataSmooth2$month<-month(dataSmooth2$DateFake)
  
  coldMonths<-dataSmooth2$month>stations_pre1997$SAD[it_stn]|
    dataSmooth2$month<stations_pre1997$SDD[it_stn]
  
  warmMonths<-!coldMonths
  warmMonths.p1<-warmMonths|(dataSmooth2$month==((stations_pre1997$SAD[it_stn]%%12)+1))
  
  
  minMonths<- dataSmooth2$month %in%(((stations_pre1997$SMD[it_stn]-1):(stations_pre1997$SMD[it_stn]+1)-1)%%12+1)
  
  
  springMonths<-dataSmooth2$month>=stations_pre1997$SMD[it_stn]&
    dataSmooth2$month<=stations_pre1997$SDD[it_stn]
  
  # springMonths.p1<-springMonths|dataSmooth2$month=stations_pre1997$SDD[it_stn]
  
  dataSmooth2Summer<-dataSmooth2[warmMonths,]
  dataSmooth2Summer$Discharge_deriv<-c(dataSmooth2Summer$Discharge[2:nrow(dataSmooth2Summer)]-
                                         dataSmooth2Summer$Discharge[1-(nrow(dataSmooth2Summer)-1)],NA)
  
  firstNegative<-which(dataSmooth2Summer$Discharge_deriv<0)%>%min
  dataSmooth2Summer<-dataSmooth2Summer[firstNegative:nrow(dataSmooth2Summer),]
  minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
  stations_pre1997$minFlowDate[it_stn]<-minDischargeDate
  minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
  
  
  stations_pre1997$minSumFlowDate[it_stn]<-minSumDischargeDate
  
  
  # Classify as snow-affected if there is a freshet
  
  
  if(sum(coldMonths)==0){
    stations_pre1997$regime[it_stn]<-"Rainfall"
    stations_pre1997$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[!coldMonths])
    ind_min<-which(dataSmooth2$Discharge==stations_pre1997$minSummerQ[it_stn]&!coldMonths )
    
    stations_pre1997$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[!coldMonths&
                                                              dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_pre1997$summerRainFactor[it_stn]<-stations_pre1997$maxLateSumQ[it_stn]/stations_pre1997$minSummerQ[it_stn]
    
  }else{
    
    stations_pre1997$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[minMonths])
    
    ind_min<-which(dataSmooth2$Discharge==stations_pre1997$minWinterQ[it_stn]&minMonths)
    
    stations_pre1997$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[springMonths&
                                                              dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_pre1997$freshetFactor[it_stn]<-stations_pre1997$maxFreshetQ[it_stn]/stations_pre1997$minWinterQ[it_stn]
    
    stations_pre1997$minSummerQ[it_stn]<-min(dataSmooth2Summer$Discharge)
    ind_min<-which(dataSmooth2Summer$Discharge==stations_pre1997$minSummerQ[it_stn])
    
    stations_pre1997$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[warmMonths.p1&
                                                              dataSmooth2$DateFake>dataSmooth2Summer$DateFake[ind_min]])
    
    stations_pre1997$summerRainFactor[it_stn]<-stations_pre1997$maxLateSumQ[it_stn]/stations_pre1997$minSummerQ[it_stn]
    
    stations_pre1997$summerRainFactor[it_stn]
    stations_pre1997$freshetFactor[it_stn]
    snow = FALSE
    rain = FALSE
    
    if(stations_pre1997$freshetFactor[it_stn]>2){
      snow = TRUE
    }
    if(stations_pre1997$summerRainFactor[it_stn]>1.05){
      rain = TRUE
    }
    
    if(rain&snow){
      stations_pre1997$regime[it_stn]<-"Hybrid"
    }else if(rain){
      stations_pre1997$regime[it_stn]<-"Rainfall"
    }else if(snow){
      stations_pre1997$regime[it_stn]<-"Snowfall"
    }else{
      stations_pre1997$regime[it_stn]<-NA
    }
    
    if(stations_pre1997$minSummerQ[it_stn]/stations_pre1997$minWinterQ[it_stn]>2){
      
    }
    
    
    
    
    if(stations_pre1997$perc_Gl[it_stn] >0.05){
      stations_pre1997$regime[it_stn]<-"Glacial"
    }
  }
  
  
  
  # Reclassify two Tarundl Creek and Honna River
  if(stations_pre1997$ID[it_stn]%in%c("08OA004","08OA005")){
    stations_pre1997$regime[it_stn]<-"Rainfall"
  }
  
  
  
}


stations_post1997<-stations%>%select(ID,Station.Name,perc_Gl,SDD,SAD,SMD)

stations_post1997$minFlowDate<-as.Date(NA)
stations_post1997$minSumFlowDate<-as.Date(NA)
stations_post1997$minWinterQ<-NA
stations_post1997$minSummerQ<-NA
stations_post1997$maxLateSumQ<-NA
stations_post1997$freshetFactor<-NA
stations_post1997$summerRainFactor<-NA
stations_post1997$regime<-NA

for(it_stn in 1:length(stations_post1997$ID)){
  # it_stn = which(stations$ID%in%"13235000")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$DayOfYear[streamData$DayOfYear==366]<-365
  
  streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
  streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
  
  
  
  dataSmooth2<-streamData%>%
    filter(wateryear>1999)%>%
    group_by(DateFake,DayOfYear)%>%
    dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE),
                     n = sum(!is.na(Discharge30)))
  if(nrow(dataSmooth2)<365){next}
  if(min(dataSmooth2$n)<10){next}
  
  dataSmooth2$month<-month(dataSmooth2$DateFake)
  
  coldMonths<-dataSmooth2$month>stations_post1997$SAD[it_stn]|
    dataSmooth2$month<stations_post1997$SDD[it_stn]
  
  warmMonths<-!coldMonths
  warmMonths.p1<-warmMonths|(dataSmooth2$month==((stations_post1997$SAD[it_stn]%%12)+1))
  
  
  minMonths<- dataSmooth2$month %in%(((stations_post1997$SMD[it_stn]-1):(stations_post1997$SMD[it_stn]+1)-1)%%12+1)
  
  
  springMonths<-dataSmooth2$month>=stations_post1997$SMD[it_stn]&
    dataSmooth2$month<=stations_post1997$SDD[it_stn]
  
  # springMonths.p1<-springMonths|dataSmooth2$month=stations_post1997$SDD[it_stn]
  
  dataSmooth2Summer<-dataSmooth2[warmMonths,]
  dataSmooth2Summer$Discharge_deriv<-c(dataSmooth2Summer$Discharge[2:nrow(dataSmooth2Summer)]-
                                         dataSmooth2Summer$Discharge[1-(nrow(dataSmooth2Summer)-1)],NA)
  
  firstNegative<-which(dataSmooth2Summer$Discharge_deriv<0)%>%min
  dataSmooth2Summer<-dataSmooth2Summer[firstNegative:nrow(dataSmooth2Summer),]
  minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
  stations_post1997$minFlowDate[it_stn]<-minDischargeDate
  minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
  
  
  stations_post1997$minSumFlowDate[it_stn]<-minSumDischargeDate
  
  
  # Classify as snow-affected if there is a freshet
  
  
  if(sum(coldMonths)==0){
    stations_post1997$regime[it_stn]<-"Rainfall"
    stations_post1997$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[!coldMonths])
    ind_min<-which(dataSmooth2$Discharge==stations_post1997$minSummerQ[it_stn]&!coldMonths )
    
    stations_post1997$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[!coldMonths&
                                                                      dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_post1997$summerRainFactor[it_stn]<-stations_post1997$maxLateSumQ[it_stn]/stations_post1997$minSummerQ[it_stn]
    
  }else{
    
    stations_post1997$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[minMonths])
    
    ind_min<-which(dataSmooth2$Discharge==stations_post1997$minWinterQ[it_stn]&minMonths)
    
    stations_post1997$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[springMonths&
                                                                      dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_post1997$freshetFactor[it_stn]<-stations_post1997$maxFreshetQ[it_stn]/stations_post1997$minWinterQ[it_stn]
    
    stations_post1997$minSummerQ[it_stn]<-min(dataSmooth2Summer$Discharge)
    ind_min<-which(dataSmooth2Summer$Discharge==stations_post1997$minSummerQ[it_stn])
    
    stations_post1997$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[warmMonths.p1&
                                                                      dataSmooth2$DateFake>dataSmooth2Summer$DateFake[ind_min]])
    
    stations_post1997$summerRainFactor[it_stn]<-stations_post1997$maxLateSumQ[it_stn]/stations_post1997$minSummerQ[it_stn]
    
    stations_post1997$summerRainFactor[it_stn]
    stations_post1997$freshetFactor[it_stn]
    snow = FALSE
    rain = FALSE
    
    if(stations_post1997$freshetFactor[it_stn]>2){
      snow = TRUE
    }
    if(stations_post1997$summerRainFactor[it_stn]>1.05){
      rain = TRUE
    }
    
    if(rain&snow){
      stations_post1997$regime[it_stn]<-"Hybrid"
    }else if(rain){
      stations_post1997$regime[it_stn]<-"Rainfall"
    }else if(snow){
      stations_post1997$regime[it_stn]<-"Snowfall"
    }else{
      stations_post1997$regime[it_stn]<-NA
    }
    
    if(stations_post1997$minSummerQ[it_stn]/stations_post1997$minWinterQ[it_stn]>2){
      
    }
    
    
    
    
    if(stations_post1997$perc_Gl[it_stn] >0.05){
      stations_post1997$regime[it_stn]<-"Glacial"
    }
  }
  
  
  
  # Reclassify two Tarundl Creek and Honna River
  if(stations_post1997$ID[it_stn]%in%c("08OA004","08OA005")){
    stations_post1997$regime[it_stn]<-"Rainfall"
  }
  
  
  
}
stations_pre1997$regime.pre<-stations_pre1997$regime%>%
  paste0(".pre")%>%
  factor(levels = c("Rainfall.pre","Hybrid.pre","Snowfall.pre","Glacial.pre"))

stations_post1997$regime.post<-stations_post1997$regime%>%
  paste0(".post")%>%
  factor(levels = c("Rainfall.post","Hybrid.post","Snowfall.post","Glacial.post"))

regimeChange<-inner_join(stations_pre1997%>%select(ID,regime.pre,freshetFactor,summerRainFactor),
                         stations_post1997%>%select(ID,regime.post,freshetFactor,summerRainFactor),
                         by = join_by(ID),
                         suffix = c(".pre",".post"))%>%
  filter(!is.na(regime.pre)&!is.na(regime.post)&
           !regime.pre=="Glacial.pre")

table(regimeChange$regime.pre,regimeChange$regime.post)

toHybrid = regimeChange$ID[regimeChange$regime.pre=="Snowfall.pre"&regimeChange$regime.post=="Hybrid.post"]

tmap_mode("view")
tm_shape(watersheds%>%filter(ID%in%toHybrid))+tm_polygons()


regimeChange2<-inner_join(stations%>%select(ID,regime,freshetFactor,summerRainFactor),
                         stations_post1997%>%select(ID,regime,freshetFactor,summerRainFactor),
                         by = join_by(ID),
                         suffix = c(".pre",".post"))%>%
  filter(!is.na(regime.pre)&!is.na(regime.post))
table(regimeChange2$regime.pre,regimeChange2$regime.post)



# Check pre-2000 with wade et al analysis

stations_pre2000<-stations%>%select(ID,Station.Name,perc_Gl,SDD,SAD,SMD)

stations_pre2000$minFlowDate<-as.Date(NA)
stations_pre2000$minSumFlowDate<-as.Date(NA)
stations_pre2000$minWinterQ<-NA
stations_pre2000$minSummerQ<-NA
stations_pre2000$maxLateSumQ<-NA
stations_pre2000$freshetFactor<-NA
stations_pre2000$summerRainFactor<-NA
stations_pre2000$regime<-NA

for(it_stn in 1:length(stations_pre2000$ID)){
  # it_stn = which(stations_pre2000$ID%in%"08MF062")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$DayOfYear[streamData$DayOfYear==366]<-365
  
  streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
  streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
  
  
  dataSmooth2<-streamData%>%
    filter(wateryear<=2000)%>%
    group_by(DateFake,DayOfYear)%>%
    dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE),
                     n = sum(!is.na(Discharge30)))
  if(nrow(dataSmooth2)<365){next}
  if(min(dataSmooth2$n)<5){next}
  
  
  dataSmooth2$month<-month(dataSmooth2$DateFake)
  
  coldMonths<-dataSmooth2$month>stations_pre2000$SAD[it_stn]|
    dataSmooth2$month<stations_pre2000$SDD[it_stn]
  
  warmMonths<-!coldMonths
  warmMonths.p1<-warmMonths|(dataSmooth2$month==((stations_pre2000$SAD[it_stn]%%12)+1))
  
  
  minMonths<- dataSmooth2$month %in%(((stations_pre2000$SMD[it_stn]-1):(stations_pre2000$SMD[it_stn]+1)-1)%%12+1)
  
  
  springMonths<-dataSmooth2$month>=stations_pre2000$SMD[it_stn]&
    dataSmooth2$month<=stations_pre2000$SDD[it_stn]
  
  # springMonths.p1<-springMonths|dataSmooth2$month=stations_pre2000$SDD[it_stn]
  
  dataSmooth2Summer<-dataSmooth2[warmMonths,]
  dataSmooth2Summer$Discharge_deriv<-c(dataSmooth2Summer$Discharge[2:nrow(dataSmooth2Summer)]-
                                         dataSmooth2Summer$Discharge[1-(nrow(dataSmooth2Summer)-1)],NA)
  
  firstNegative<-which(dataSmooth2Summer$Discharge_deriv<0)%>%min
  dataSmooth2Summer<-dataSmooth2Summer[firstNegative:nrow(dataSmooth2Summer),]
  minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
  stations_pre2000$minFlowDate[it_stn]<-minDischargeDate
  minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
  
  
  stations_pre2000$minSumFlowDate[it_stn]<-minSumDischargeDate
  
  
  # Classify as snow-affected if there is a freshet
  
  
  if(sum(coldMonths)==0){
    stations_pre2000$regime[it_stn]<-"Rainfall"
    stations_pre2000$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[!coldMonths])
    ind_min<-which(dataSmooth2$Discharge==stations_pre2000$minSummerQ[it_stn]&!coldMonths )
    
    stations_pre2000$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[!coldMonths&
                                                                      dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_pre2000$summerRainFactor[it_stn]<-stations_pre2000$maxLateSumQ[it_stn]/stations_pre2000$minSummerQ[it_stn]
    
  }else{
    
    stations_pre2000$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[minMonths])
    
    ind_min<-which(dataSmooth2$Discharge==stations_pre2000$minWinterQ[it_stn]&minMonths)
    
    stations_pre2000$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[springMonths&
                                                                      dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
    
    stations_pre2000$freshetFactor[it_stn]<-stations_pre2000$maxFreshetQ[it_stn]/stations_pre2000$minWinterQ[it_stn]
    
    stations_pre2000$minSummerQ[it_stn]<-min(dataSmooth2Summer$Discharge)
    ind_min<-which(dataSmooth2Summer$Discharge==stations_pre2000$minSummerQ[it_stn])
    
    stations_pre2000$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[warmMonths.p1&
                                                                      dataSmooth2$DateFake>dataSmooth2Summer$DateFake[ind_min]])
    
    stations_pre2000$summerRainFactor[it_stn]<-stations_pre2000$maxLateSumQ[it_stn]/stations_pre2000$minSummerQ[it_stn]
    
    stations_pre2000$summerRainFactor[it_stn]
    stations_pre2000$freshetFactor[it_stn]
    snow = FALSE
    rain = FALSE
    
    if(stations_pre2000$freshetFactor[it_stn]>2){
      snow = TRUE
    }
    if(stations_pre2000$summerRainFactor[it_stn]>1.05){
      rain = TRUE
    }
    
    if(rain&snow){
      stations_pre2000$regime[it_stn]<-"Hybrid"
    }else if(rain){
      stations_pre2000$regime[it_stn]<-"Rainfall"
    }else if(snow){
      stations_pre2000$regime[it_stn]<-"Snowfall"
    }else{
      stations_pre2000$regime[it_stn]<-NA
    }
    
    if(stations_pre2000$minSummerQ[it_stn]/stations_pre2000$minWinterQ[it_stn]>2){
      
    }
    
    
    
    
    if(stations_pre2000$perc_Gl[it_stn] >0.05){
      stations_pre2000$regime[it_stn]<-"Glacial"
    }
  }
  
  
  
  # Reclassify two Tarundl Creek and Honna River
  if(stations_pre2000$ID[it_stn]%in%c("08OA004","08OA005")){
    stations_pre2000$regime[it_stn]<-"Rainfall"
  }
  
  
  
}

x6<-inner_join(stations_pre2000%>%select(ID,regime),
               regimes_wade)

table(x6$regime,x6$regime_wade)


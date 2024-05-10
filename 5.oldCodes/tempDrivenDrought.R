# how often do low flows (10th percentile) follow years with normal precip?

closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(sf)
library(tmap)
library(lubridate)
library(bcmaps)
library(stringr)
library(terra)
library(cowplot)
library(scico)
library(grwat)
library(reshape2)
library(ggplot2)
library(ggpattern)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.RDS")

# Load Station metadata
stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

# Load monthly weather data
WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")%>%
  filter(StationNum%in%unique(streamDataAll$ID))


data_SWE<-read.csv("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment.csv")

data_SWE_dly<-read.csv("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.csv")

names(data_SWE_dly)<-c("ID","Date","SWE")


data_SWE$date <- data_SWE$variable%>%ymd()

data_SWE$Year<-year(data_SWE$date)
data_SWE$Month<-month(data_SWE$date)



getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
maxMonth<-data_SWE%>%
  group_by(ID,Year)%>%
  dplyr::summarise(maxMonth = Month[which.max(value)])

maxMonth$maxMonth<-plyr::mapvalues(maxMonth$maxMonth,
                                   from = c(10,11,12),
                                   to = c(-2,-1,0))

maxMonth<-maxMonth%>%
  group_by(ID)%>%
  dplyr::summarise(maxMonth = ceiling(median(maxMonth)))

# maxMonth$maxMonth[maxMonth$maxMonth<3]<-3
data_SWE<-left_join(data_SWE,maxMonth)
data_SWE<-data_SWE%>%
  filter(Month == maxMonth)

data_SWE$WaterYear<-data_SWE$Year

data_SWE$WaterYear[data_SWE$Month%in%c(10,11,12)]<-data_SWE$Year[data_SWE$Month%in%c(10,11,12)]+1



streamDataAll<-left_join(streamDataAll,data_SWE_dly,by = c("ID","Date"))
streamDataAll$NovWaterYear<-streamDataAll$year
streamDataAll$NovWaterYear[streamDataAll$month%in%c(11,12)]<-
  streamDataAll$year[streamDataAll$month%in%c(11,12)]+1


for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08LB069")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  # streamData<-streamData%>%
  #   filter(year>=1950)%>%
  #   filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  # streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)
  # if(sum(!is.na(streamData$Mean.Temp..C.))==0){next}
  # 
  # streamData$BaseFlow_lynehollick<-gr_baseflow(streamData$Discharge, method = 'lynehollick', a = 0.925, passes = 3)
  # streamData$BaseFlow_boughton<-gr_baseflow(streamData$Discharge, method = 'boughton', k = 0.975, passes = 3)
  # streamData$BaseFlow_jakeman<-gr_baseflow(streamData$Discharge, method = 'jakeman', a = 0.925, passes = 3,aq = -0.5)
  # streamData$BaseFlow_maxwell<-gr_baseflow(streamData$Discharge, method = 'maxwell', k = 0.975, passes = 3)
  # streamData$BaseFlow_chapman<-gr_baseflow(streamData$Discharge, method = 'chapman', a = 0.925, passes = 3)
  # streamData$BaseFlow_Eckhardt0.97<-NA
  # streamData$BaseFlow_Eckhardt0.995<-NA
  # streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
  #   FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
  # streamData$BaseFlow_Eckhardt0.995[!is.na(streamData$Discharge)]<-
  #   FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.995, 0.8)
  
  # ggplot(streamData)+
  #   geom_area(aes(Date,Discharge), fill = 'steelblue', color = 'black')+
  #   geom_area(aes(Date,BaseFlow_maxwell), fill = 'orangered', color = 'black' )+
  #   scale_x_date(limits =  c(ymd(20000101), ymd(20021231)))+
  #   scale_y_sqrt()
  
  #Custom function to find day of min flow, accounting for possibility of no data
  findMinFlowDay <- function(DayOfYear,Discharge7,month,monthSelect){
    whichMin<-which.min(Discharge7[month==monthSelect])
    noData <- FALSE
    if(length(whichMin)==0){ 
      whichMin<-1
      noData <- TRUE
    }
    
    minFlowDay <- (DayOfYear[month==monthSelect])[whichMin]
    if(noData){
      minFlowDay <- NA
    }
    return(minFlowDay)
  }
  
  
  
  streamDataYrly<-streamData%>%
    # filter(year==1986)%>%
    dplyr::group_by(NovWaterYear)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minAugFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,8),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minSepFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,9),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     minOctFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,10),
                     
                     minSumFlow7 = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     
                     # # 
                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     
    )
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  ## Add in SWE Data
  snowData<-data_SWE[data_SWE$ID==stations$ID[it_stn],]%>%
    group_by(WaterYear)%>%
    dplyr::summarize(SWE = mean(value),
                     maxMonth = maxMonth[1])
  
  
  
  streamDataYrly<-left_join(streamDataYrly,snowData,by = c("NovWaterYear" = "WaterYear"))
  
  
  stations$minFlowMonth[it_stn]<-getMode(streamDataYrly$minSumFlowMonth)
  
  if(stations$minFlowMonth[it_stn]==8){
    streamDataYrly$minMonFlow7<-streamDataYrly$minAugFlow7
  }else if(stations$minFlowMonth[it_stn]==9){
    streamDataYrly$minMonFlow7<-streamDataYrly$minSepFlow7
  }else if(stations$minFlowMonth[it_stn]==10){
    streamDataYrly$minMonFlow7<-streamDataYrly$minOctFlow7
  }
  
  
  data<-WeatherData[WeatherData$StationNum == stations$ID[it_stn],]
  
  data$NovWaterYear<-data$Year
  data$NovWaterYear[data$Month%in%c(11,12)] <- data$Year[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(NovWaterYear)%>%
    dplyr::summarize(
      
      winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
      Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),
      
      Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      
      Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      
      Precip_2 = sum(Total.Precip..mm.[Month%in%c((stations$minFlowMonth[it_stn]-1):stations$minFlowMonth[it_stn])],na.rm = TRUE),
      Temp_2 =  mean(Mean.Temp..C.[Month%in%c((stations$minFlowMonth[it_stn]-1):stations$minFlowMonth[it_stn])],na.rm = TRUE),
    )
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))
  
  
  
  if(stations$minFlowMonth[it_stn]==8){
    dataYearly2$TempSummer<-dataYearly2$Temp_MJJA
    dataYearly2$PcpSummer<-dataYearly2$Precip_MJJA
    dataYearly2$monthlyDiv.cms<-dataYearly2$August.cms
  }else if(stations$minFlowMonth[it_stn]==9){
    dataYearly2$TempSummer<-dataYearly2$Temp_MJJAS
    dataYearly2$PcpSummer<-dataYearly2$Precip_MJJAS
    dataYearly2$monthlyDiv.cms<-dataYearly2$September.cms
  }else if(stations$minFlowMonth[it_stn]==10){
    dataYearly2$TempSummer<-dataYearly2$Temp_MJJASO
    dataYearly2$PcpSummer<-dataYearly2$Precip_MJJASO
    dataYearly2$monthlyDiv.cms<-dataYearly2$October.cms
  }
  
  dataYearly2$minSumFlow7log<-log(dataYearly2$minSumFlow7)
  dataYearly2<-filter(dataYearly2,!is.na(dataYearly2$minSumFlow7))
  dataYearly2$minSumFlow7.10<-dataYearly2$minSumFlow7<quantile(dataYearly2$minSumFlow7,0.1,na.rm = TRUE)
  
  dataYearly2$TempSummer.10<-dataYearly2$TempSummer<quantile(dataYearly2$TempSummer,0.1,na.rm = TRUE)
  dataYearly2$TempSummer.50<-dataYearly2$TempSummer<quantile(dataYearly2$TempSummer,0.5,na.rm = TRUE)
  
  dataYearly2$PcpSummer.10<-dataYearly2$PcpSummer<quantile(dataYearly2$PcpSummer,0.1,na.rm = TRUE)
  dataYearly2$PcpSummer.50<-dataYearly2$PcpSummer<quantile(dataYearly2$PcpSummer,0.5,na.rm = TRUE)
  
  dataYearly2$maxSWEdly.10<-dataYearly2$maxSWEdly<quantile(dataYearly2$maxSWEdly,0.1,na.rm = TRUE)
  dataYearly2$maxSWEdly.50<-dataYearly2$maxSWEdly<quantile(dataYearly2$maxSWEdly,0.5,na.rm = TRUE)
  
  dataYearly2$winterPrecip.10<-dataYearly2$winterPrecip<quantile(dataYearly2$winterPrecip,0.1,na.rm = TRUE)
  dataYearly2$winterPrecip.50<-dataYearly2$winterPrecip<quantile(dataYearly2$winterPrecip,0.5,na.rm = TRUE)
  
  dataYearly2$meanWinterTemp.10<-dataYearly2$meanWinterTemp<quantile(dataYearly2$meanWinterTemp,0.1,na.rm = TRUE)
  dataYearly2$meanWinterTemp.50<-dataYearly2$meanWinterTemp<quantile(dataYearly2$meanWinterTemp,0.5,na.rm = TRUE)
  
  dataYearly2$Precip_2.10<-dataYearly2$Precip_2<quantile(dataYearly2$Precip_2,0.1,na.rm = TRUE)
  dataYearly2$Precip_2.50<-dataYearly2$Precip_2<quantile(dataYearly2$Precip_2,0.5,na.rm = TRUE)
  
  
  stations$fracNotPrecipDriven[it_stn]<-
    sum(dataYearly2$minSumFlow7.10&!(dataYearly2$maxSWEdly.50|dataYearly2$PcpSummer.50))/sum(dataYearly2$minSumFlow7.10)
}

sum(stations$fracNotPrecipDriven>0,na.rm = TRUE)
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")
watersheds<-left_join(watersheds,stations,by = c("StationNum" = "ID" ))
tm_shape(watersheds)+
  tm_polygons(col = "fracNotPrecipDriven",
              palette = "BrBG",
              midpoint = 0)

stations%>%
  group_by(regime)%>%
  summarize(sum(fracNotPrecipDriven>0,na.rm = TRUE)/n())

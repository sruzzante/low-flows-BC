# Climate sensitivity plot

# 6 main variable to investigateplot:
# - Winter SWE
# - Winter Baseflow
# - Summer T
# - Summer P
# - Summer T7
# - Water use licenses


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

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/data/BC_Watersheds/")) #Set the working directory

# Load streamflow data
streamDataAll<-read.csv("2.data/2.working/StreamflowData/streamDataFinal_wSnow.csv")

# Load Station metadata
stations<-read.csv("2.data/2.working/StationMetadata/stations_final_wSnow.csv",fileEncoding = "UTF-8-BOM")

# Load catchment polygons
watersheds <- st_read("../../low-flows-BC/2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

# Load monthly weather data
WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")%>%
  filter(StationNum%in%unique(streamDataAll$ID))

# Load daily weather data
WeatherDataDaily<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataDaily.csv")%>%
  filter(ID%in%unique(streamDataAll$ID))

streamDataAll<-left_join(streamDataAll,WeatherDataDaily[,-which(names(WeatherDataDaily)=="Date")])

st_bbox(watersheds%>%st_transform("EPSG:4326"))

# SWE Data prep ####
extractSnowData = FALSE
if(extractSnowData == TRUE){
  
  # Load ERA-5 SWE data (monthly)
  SNOW_SWE_grid<-terra::rast("../1.Spatial_data/regional/BC/clim_climate_precip_aridity_permafrost/clim1_ERA5_LAND_SWE_monthly/adaptor.mars.internal-1706465874.8028717-7610-7-c5361b75-b20d-44b9-8d18-83908b5d11c8.grib")
  
  time(SNOW_SWE_grid)
  watersheds_SWE<-terra::extract(SNOW_SWE_grid,watersheds%>%
                                   vect()%>%
                                   project(crs(SNOW_SWE_grid)),
                                 fun = mean,
                                 exact= TRUE,
                                 # weights = TRUE,
                                 touches = TRUE,
                                 na.rm = TRUE)
  
  
  watersheds_SWE$ID<-watersheds$StationNum
  
  names(watersheds_SWE)<-c("ID",time(SNOW_SWE_grid)%>%as.character())
  
  data_SWE<-melt(watersheds_SWE,id.vars = "ID")
  
  write.csv(data_SWE,"2.data/2.working/SWE_data_by_catchment.csv",row.names = FALSE)
}
extractSnowDataDaily = FALSE
if(extractSnowDataDaily){
  
  # Load ERA-5 SWE data (daily)
  SNOW_SWE_grid<-terra::rast("../1.Spatial_data/regional/BC/clim_climate_precip_aridity_permafrost/clim2_ERA5_LAND_SWE/SNOW_ERA5.tif")
  time(SNOW_SWE_grid)
  watersheds_SWE<-terra::extract(SNOW_SWE_grid,watersheds%>%
                                   vect()%>%
                                   project(crs(SNOW_SWE_grid)),
                                 fun = mean,
                                 exact= TRUE,
                                 # weights = TRUE,
                                 touches = TRUE,
                                 na.rm = TRUE)
  
  
  watersheds_SWE$ID<-watersheds$StationNum
  
  names(watersheds_SWE)<-c("ID",time(SNOW_SWE_grid)%>%as.character())
  
  data_SWE_dly<-melt(watersheds_SWE,id.vars = "ID")
  
  write.csv(data_SWE_dly,"2.data/2.working/SWE_data_by_catchment_dly.csv",row.names = FALSE)
  
}

data_SWE<-read.csv("2.data/2.working/SWE_data_by_catchment.csv")

data_SWE_dly<-read.csv("2.data/2.working/SWE_data_by_catchment_dly.csv")
names(data_SWE_dly)<-c("ID","Date","SWE")


data_SWE$date <- data_SWE$variable%>%ymd()

data_SWE$Year<-year(data_SWE$date)
data_SWE$Month<-month(data_SWE$date)

data_SWE<-data_SWE%>%
  group_by(ID,Year,Month)%>%
  summarize(value = mean(value))

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



## Loop through stations and extract variables ################




stations[,c('rSWE.max','rSWE.max.p','rTWin','rTWin.p','rPWin','rPWin.p','rBF_lynehollick_fxd',
            'rBF_lynehollick_fxd.p','rBF_boughton_fxd','rBF_boughton_fxd.p','rBF_jakeman_fxd',
            'rBF_jakeman_fxd.p','rBF_maxwell_fxd','rBF_maxwell_fxd.p','rBF_chapman_fxd',
            'rBF_chapman_fxd.p','rBF_Eckhardt0.995_fxd','rBF_Eckhardt0.995_fxd.p',
            'rBF_Eckhardt0.97_fxd','rBF_Eckhardt0.97_fxd.p','rBF_lynehollick_30day',
            'rBF_lynehollick_30day.p','rBF_boughton_30day','rBF_boughton_30day.p',
            'rBF_jakeman_30day','rBF_jakeman_30day.p','rBF_maxwell_30day',
            'rBF_maxwell_30day.p','rBF_chapman_30day','rBF_chapman_30day.p',
            'rBF_Eckhardt0.995_30day','rBF_Eckhardt0.995_30day.p','rBF_Eckhardt0.97_30day',
            'rBF_Eckhardt0.97_30day.p','rSWE','rSWE.p','rBF','rBF.p','rTSum','rTSum.p','spTSumsq','spTSumsq.p',
            'rPSum','rPSum.p','rT7','rT7.p','r.lcd.sw','r.lcd.sw.p','spSWE','spSWE.p',
            'spBF','spBF.p','spTSum','spTSum.p','spPSum','spPSum.p','spT7','spT7.p',
            'spTSum.T7','spTSum.T7.p','sp.lcd.sw','sp.lcd.sw.p','lowQ.10_highSWE',
            'llowQ.10_highSWE.90','minFlowMonth',"meanSummerTemp")]<-NA


my_lms<-list()
my_lms.pre<-list()
my_lms.post<-list()
availYears<-list()

for(it_stn in 1:length(stations$ID)){
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)
  if(sum(!is.na(streamData$Mean.Temp..C.))==0){next}
  
  streamData$BaseFlow_lynehollick<-gr_baseflow(streamData$Discharge, method = 'lynehollick', a = 0.925, passes = 3)
  streamData$BaseFlow_boughton<-gr_baseflow(streamData$Discharge, method = 'boughton', k = 0.975, passes = 3)
  streamData$BaseFlow_jakeman<-gr_baseflow(streamData$Discharge, method = 'jakeman', a = 0.925, passes = 3,aq = -0.5)
  streamData$BaseFlow_maxwell<-gr_baseflow(streamData$Discharge, method = 'maxwell', k = 0.975, passes = 3)
  streamData$BaseFlow_chapman<-gr_baseflow(streamData$Discharge, method = 'chapman', a = 0.925, passes = 3)
  streamData$BaseFlow_Eckhardt0.97<-NA
  streamData$BaseFlow_Eckhardt0.995<-NA
  streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
  streamData$BaseFlow_Eckhardt0.995[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.995, 0.8)
  
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
                     
                     SumT7=T7[DayOfYear=minSumFlowDay],
                     AugT7=T7[DayOfYear=minAugFlowDay],
                     SepT7=T7[DayOfYear=minSepFlowDay],
                     OctT7=T7[DayOfYear=minOctFlowDay],
                     
                     JuneBF_lynehollick = mean(BaseFlow_lynehollick[month==6]),
                     MayBF_lynehollick = mean(BaseFlow_lynehollick[month==5]),
                     AprilBF_lynehollick = mean(BaseFlow_lynehollick[month==4]),
                     MarchBF_lynehollick = mean(BaseFlow_lynehollick[month==3]),
                     FebruaryBF_lynehollick = mean(BaseFlow_lynehollick[month==2]),
                     
                     
                     JuneBF_boughton = mean(BaseFlow_boughton[month==6]),
                     MayBF_boughton = mean(BaseFlow_boughton[month==5]),
                     AprilBF_boughton = mean(BaseFlow_boughton[month==4]),
                     MarchBF_boughton = mean(BaseFlow_boughton[month==3]),
                     FebruaryBF_boughton = mean(BaseFlow_boughton[month==2]),
                     
                     
                     JuneBF_jakeman = mean(BaseFlow_jakeman[month==6]),
                     MayBF_jakeman = mean(BaseFlow_jakeman[month==5]),
                     AprilBF_jakeman = mean(BaseFlow_jakeman[month==4]),
                     MarchBF_jakeman = mean(BaseFlow_jakeman[month==3]),
                     FebruaryBF_jakeman = mean(BaseFlow_jakeman[month==2]),
                     
                     
                     JuneBF_maxwell = mean(BaseFlow_maxwell[month==6]),
                     MayBF_maxwell = mean(BaseFlow_maxwell[month==5]),
                     AprilBF_maxwell = mean(BaseFlow_maxwell[month==4]),
                     MarchBF_maxwell = mean(BaseFlow_maxwell[month==3]),
                     FebruaryBF_maxwell = mean(BaseFlow_maxwell[month==2]),
                     
                     JuneBF_chapman = mean(BaseFlow_chapman[month==6]),
                     MayBF_chapman = mean(BaseFlow_chapman[month==5]),
                     AprilBF_chapman = mean(BaseFlow_chapman[month==4]),
                     MarchBF_chapman = mean(BaseFlow_chapman[month==3]),
                     FebruaryBF_chapman = mean(BaseFlow_chapman[month==2]),
                     
                     JuneBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==6]),
                     MayBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==5]),
                     AprilBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==4]),
                     MarchBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==3]),
                     FebruaryBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==2]),
                     
                     JuneBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==6]),
                     MayBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==5]),
                     AprilBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==4]),
                     MarchBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==3]),
                     FebruaryBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==2]),
                     # 
                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     
                     # # 
                     BF_30day_lynehollick = mean(BaseFlow_lynehollick[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_boughton = mean(BaseFlow_boughton[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_jakeman = mean(BaseFlow_jakeman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_maxwell = mean(BaseFlow_maxwell[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_chapman = mean(BaseFlow_chapman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     
                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)])
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
  
  
  if(snowData$maxMonth[1]%in%c(12,1,2,4,6)){
    streamDataYrly$winterBF_lynehollick<-streamDataYrly$AprilBF_lynehollick
    streamDataYrly$winterBF_boughton<-streamDataYrly$AprilBF_boughton
    streamDataYrly$winterBF_jakeman<-streamDataYrly$AprilBF_jakeman
    streamDataYrly$winterBF_maxwell<-streamDataYrly$AprilBF_maxwell
    streamDataYrly$winterBF_chapman<-streamDataYrly$AprilBF_chapman
    streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$AprilBF_Eckhardt0.995
    streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$AprilBF_Eckhardt0.97
    
    
  }else if(snowData$maxMonth[1]%in%3){
    
    
    streamDataYrly$winterBF_lynehollick<-streamDataYrly$MarchBF_lynehollick
    streamDataYrly$winterBF_boughton<-streamDataYrly$MarchBF_boughton
    streamDataYrly$winterBF_jakeman<-streamDataYrly$MarchBF_jakeman
    streamDataYrly$winterBF_maxwell<-streamDataYrly$MarchBF_maxwell
    streamDataYrly$winterBF_chapman<-streamDataYrly$MarchBF_chapman
    streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$MarchBF_Eckhardt0.995
    streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$MarchBF_Eckhardt0.97
    
  }else if(snowData$maxMonth[1]%in%5){
    
    streamDataYrly$winterBF_lynehollick<-streamDataYrly$MayBF_lynehollick
    streamDataYrly$winterBF_boughton<-streamDataYrly$MayBF_boughton
    streamDataYrly$winterBF_jakeman<-streamDataYrly$MayBF_jakeman
    streamDataYrly$winterBF_maxwell<-streamDataYrly$MayBF_maxwell
    streamDataYrly$winterBF_chapman<-streamDataYrly$MayBF_chapman
    streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$MayBF_Eckhardt0.995
    streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$MayBF_Eckhardt0.97
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
    )
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))
  
  Div<-read.csv(paste0("../../low-flows-BC/2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  
  
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
  
  
  stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  
  
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$SWE,use = "complete")
  if(x$parameter>=8){
    stations$rSWE[it_stn]<-x$estimate
    stations$rSWE.p[it_stn]<-x$p.value
  }  
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$maxSWEdly,use = "complete")
  if(x$parameter>=8){
    stations$rSWE.max[it_stn]<-x$estimate
    stations$rSWE.max.p[it_stn]<-x$p.value
  }
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$meanWinterTemp,use = "complete")
  if(x$parameter>=8){
    stations$rTWin[it_stn]<-x$estimate
    stations$rTWin.p[it_stn]<-x$p.value
  }
  
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterPrecip,use = "complete")
  if(x$parameter>=8){
    stations$rPWin[it_stn]<-x$estimate
    stations$rPWin.p[it_stn]<-x$p.value
  }
  
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$TempSummer,use = "complete")
  if(x$parameter>=8){
    stations$rTSum[it_stn]<-x$estimate
    stations$rTSum.p[it_stn]<-x$p.value
  }
  dataYearly3<-dataYearly2[!is.na(dataYearly2$minSumFlow7log)&!is.na(dataYearly2$TempSummer),c("minSumFlow7log","TempSummer")]
  dataYearly3$TempSummer_z<-(dataYearly3$TempSummer-mean(dataYearly3$TempSummer))/sd(dataYearly3$TempSummer)
  dataYearly3$TempSummer_z2<-dataYearly3$TempSummer_z^2
  
  
  dataYearly3<-dataYearly3[,c("minSumFlow7log","TempSummer_z","TempSummer_z2")]
  x<-  ppcor::spcor.test(x=dataYearly3$TempSummer_z2,y=dataYearly3$minSumFlow7log,z=dataYearly3$TempSummer_z, method="pearson")
  
  if(x$n>=8){
    stations$spTSumsq[it_stn]<-x$estimate
    stations$spTSumsq.p[it_stn]<-x$p.value
  }
  
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$PcpSummer,use = "complete")
  if(x$parameter>=8){
    stations$rPSum[it_stn]<-x$estimate
    stations$rPSum.p[it_stn]<-x$p.value
    
  }
  x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$SumT7,use = "complete")
  if(x$parameter>=8){
    stations$rT7[it_stn]<-x$estimate
    stations$rT7.p[it_stn]<-x$p.value
  }
  
  
  if(!stations$ID[it_stn]%in%c("08ED004","08OA004")){
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_lynehollick,use = "complete")
    if(x$parameter>=8){
      stations$rBF_lynehollick_fxd[it_stn]<-x$estimate
      stations$rBF_lynehollick_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_boughton,use = "complete")
    if(x$parameter>=8){
      stations$rBF_boughton_fxd[it_stn]<-x$estimate
      stations$rBF_boughton_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_jakeman,use = "complete")
    if(x$parameter>=8){
      stations$rBF_jakeman_fxd[it_stn]<-x$estimate
      stations$rBF_jakeman_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_maxwell,use = "complete")
    if(x$parameter>=8){
      stations$rBF_maxwell_fxd[it_stn]<-x$estimate
      stations$rBF_maxwell_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_chapman,use = "complete")
    if(x$parameter>=8){
      stations$rBF_chapman_fxd[it_stn]<-x$estimate
      stations$rBF_chapman_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_Eckhardt0.995,use = "complete")
    if(x$parameter>=8){
      stations$rBF_Eckhardt0.995_fxd[it_stn]<-x$estimate
      stations$rBF_Eckhardt0.995_fxd.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterBF_Eckhardt0.97,use = "complete")
    if(x$parameter>=8){
      stations$rBF_Eckhardt0.97_fxd[it_stn]<-x$estimate
      stations$rBF_Eckhardt0.97_fxd.p[it_stn]<-x$p.value
    }
    ## now with 
    
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_lynehollick,use = "complete")
    if(x$parameter>=8){
      stations$rBF_lynehollick_30day[it_stn]<-x$estimate
      stations$rBF_lynehollick_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_boughton,use = "complete")
    if(x$parameter>=8){
      stations$rBF_boughton_30day[it_stn]<-x$estimate
      stations$rBF_boughton_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_jakeman,use = "complete")
    if(x$parameter>=8){
      stations$rBF_jakeman_30day[it_stn]<-x$estimate
      stations$rBF_jakeman_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_maxwell,use = "complete")
    if(x$parameter>=8){
      stations$rBF_maxwell_30day[it_stn]<-x$estimate
      stations$rBF_maxwell_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_chapman,use = "complete")
    if(x$parameter>=8){
      stations$rBF_chapman_30day[it_stn]<-x$estimate
      stations$rBF_chapman_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_Eckhardt0.995,use = "complete")
    if(x$parameter>=8){
      stations$rBF_Eckhardt0.995_30day[it_stn]<-x$estimate
      stations$rBF_Eckhardt0.995_30day.p[it_stn]<-x$p.value
    }
    x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$BF_30day_Eckhardt0.97,use = "complete")
    if(x$parameter>=8){
      stations$rBF_Eckhardt0.97_30day[it_stn]<-x$estimate
      stations$rBF_Eckhardt0.97_30day.p[it_stn]<-x$p.value
    }
  }
  
  if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)|
      any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)|
      any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE))&
     sd(dataYearly2$Total.cms)>0){
    stations$r.lcd.sw[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$Total.cms,use = "complete")
    stations$r.lcd.sw.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$Total.cms,use = "complete")$p.value
    
  }
  
  
  dat2<-dataYearly2%>%filter(!is.na(maxSWEdly)&!is.na(minSumFlow7log))
  stations$lowQ.10_highSWE[it_stn]<-sum((dat2$minSumFlow7log<=quantile(dat2$minSumFlow7log,0.1))[dat2$maxSWEdly>mean(dat2$maxSWEdly)])/
    sum(dat2$minSumFlow7log<=quantile(dat2$minSumFlow7log,0.1))
  
  stations$lowQ.10_highSWE.90[it_stn]<-sum((dat2$minSumFlow7log<=quantile(dat2$minSumFlow7log,0.1))[dat2$maxSWEdly>=quantile(dat2$maxSWEdly,0.9)])/
    sum(dat2$minSumFlow7log<=quantile(dat2$minSumFlow7log,0.1))
  
  
  # if(stations$ID[it_stn]%in%c("08OA004")){next} #temporary
  summary(mylm<-lm(minSumFlow7log~maxSWEdly+BF_30day_Eckhardt0.97+TempSummer+PcpSummer+SumT7+Total.cms,
                   data = dataYearly2,na.action= "na.exclude"))
  
  
  dat<-dataYearly2[!is.na(residuals(mylm)),]
  availYears[[it_stn]]<-dat$NovWaterYear
  
  if(nrow(dat)<10){next}
  
  mylm.pre1996<-NULL
  mylm.post1996<-NULL
  
  # anova(mylm)
  
  
  if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)|
      any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)|
      any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE))&
     sd(dat$Total.cms)>0&
     stations$ID[it_stn]!="08OA003"){
    
    
    
    x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","BF_30day_Eckhardt0.97","TempSummer","SumT7","PcpSummer","Total.cms")])
    stations$sp.lcd.sw[it_stn]<-x$estimate["Total.cms","minSumFlow7log"]
    stations$sp.lcd.sw.p[it_stn]<-x$p.value["Total.cms","minSumFlow7log"]
    
    # compare early-late stationarity
    if(sum(dat$NovWaterYear<=1996)>=20&sum(dat$NovWaterYear>1996)>=20){
      mylm.pre1996<-lm(minSumFlow7log~maxSWEdly+BF_30day_Eckhardt0.97+TempSummer+PcpSummer+monthlyDiv.cms,
                       data = dataYearly2%>%filter(NovWaterYear<=1996),na.action= "na.exclude")
      mylm.post1996<-lm(minSumFlow7log~maxSWEdly+BF_30day_Eckhardt0.97+TempSummer+PcpSummer+monthlyDiv.cms,
                        data = dataYearly2%>%filter(NovWaterYear>1996),na.action= "na.exclude")
    }
    
  } else {
    mylm<-update(mylm,.~.-Total.cms)
    summary(mylm<-lm(minSumFlow7log~SWE+BF_30day_Eckhardt0.97+TempSummer+SumT7+PcpSummer+SumT7+Total.cms,
                     data = dataYearly2,na.action= "na.exclude"))
    
    
    dat<-dataYearly2[!is.na(residuals(mylm)),]
    
    
    x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","BF_30day_Eckhardt0.97","TempSummer","SumT7","PcpSummer")])
    
    # compare early-late stationarity
    if(sum(dat$NovWaterYear<=1996)>=20&sum(dat$NovWaterYear>1996)>=20){
      mylm.pre1996<-lm(minSumFlow7log~maxSWEdly+BF_30day_Eckhardt0.97+TempSummer+PcpSummer,
                       data = dataYearly2%>%filter(NovWaterYear<=1996),na.action= "na.exclude")
      mylm.post1996<-lm(minSumFlow7log~maxSWEdly+BF_30day_Eckhardt0.97+TempSummer+PcpSummer,
                        data = dataYearly2%>%filter(NovWaterYear>1996),na.action= "na.exclude")
    }
    
    
  }
  # print(x$estimate["TempSummer",])
  # # anova(mylm)
  # 
  # 
  # print(corrplot::corrplot(cor(dat[,c("minSumFlow7log","maxSWEdly","BF_30day_Eckhardt0.97","TempSummer","SumT7","PcpSummer")]),
  #                          addCoef.col = "black"))
  # readline(prompt="Press [enter] to continue")
  
  
  stations$spSWE[it_stn]<-x$estimate["maxSWEdly","minSumFlow7log"]
  stations$spSWE.p[it_stn]<-x$p.value["maxSWEdly","minSumFlow7log"]
  
  stations$spBF[it_stn]<-x$estimate["BF_30day_Eckhardt0.97","minSumFlow7log"]
  stations$spBF.p[it_stn]<-x$p.value["BF_30day_Eckhardt0.97","minSumFlow7log"]
  
  stations$spTSum[it_stn]<-x$estimate["TempSummer","minSumFlow7log"]
  stations$spTSum.p[it_stn]<-x$p.value["TempSummer","minSumFlow7log"]
  # 
  # stations$spTSum.T7[it_stn]<-x$estimate["TempSummer.T7","minSumFlow7log"]
  # stations$spTSum.T7.p[it_stn]<-x$p.value["TempSummer.T7","minSumFlow7log"]
  
  stations$spPSum[it_stn]<-x$estimate["PcpSummer","minSumFlow7log"]
  stations$spPSum.p[it_stn]<-x$p.value["PcpSummer","minSumFlow7log"]
  
  stations$spT7[it_stn]<-x$estimate["SumT7","minSumFlow7log"]
  stations$spT7.p[it_stn]<-x$p.value["SumT7","minSumFlow7log"]
  
  my_lms[[it_stn]]<-mylm
  
  # ggplot(dataYearly2,aes(x = SWE,y = minSumFlow7log))+geom_point()
  
  my_lms.pre[[it_stn]]<-mylm.pre1996
  my_lms.post[[it_stn]]<- mylm.post1996
}

## Analysis and plots #######
stations%>%group_by(regime)%>%
  summarize(mean(lowQ.10_highSWE),
            mean(lowQ.10_highSWE.90))

stations%>%group_by()%>%
  summarize(mean(lowQ.10_highSWE),
            mean(lowQ.10_highSWE.90))



r.vals<-melt(stations,id.vars = c("ID","Station.Name","regime"),
             measure.vars = c("rSWE.max","rBF_Eckhardt0.97_30day","rTSum","rPSum","rT7","r.lcd.sw"))
r.p.vals<-melt(stations,id.vars = c("ID","Station.Name","regime"),
               measure.vars = c("rSWE.max.p","rBF_Eckhardt0.97_30day.p","rTSum.p","rPSum.p","rT7.p","r.lcd.sw.p"))
r.p.vals$p.val<-r.p.vals$value
r.p.vals$variable<-str_remove(r.p.vals$variable,"\\.p")
r.vals<-left_join(r.vals,r.p.vals[,c("ID","variable","p.val")])

r.vals$variable<-factor(r.vals$variable,
                        levels = c("rSWE.max","rBF_Eckhardt0.97_30day","rPSum","rTSum","rT7","r.lcd.sw"),
                        labels = c("SWE[max]","Baseflow[winter]","P[summer]","T[summer]","T[7]","Water~Use"))
r.vals<-r.vals%>%filter(!is.na(value))

r.vals%>%
  group_by(variable,regime)%>%
  summarize(median(value),
            mean(value),
            sum(p.val<0.05)/n())%>%
  print(n = 100)

r.vals%>%
  group_by(variable)%>%
  summarize(median(value),
            mean(value),
            sum(p.val<0.05)/n())%>%
  print(n = 100)
summary(r.vals$regime%>%factor()
)

r.vals$regime<-factor(r.vals$regime,
                      levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())

ggplot(r.vals,aes(y = regime,
                  # x = value
                  x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(Pearson~r),
                     limits = c(-0.8,0.8))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "variable", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.17,0.96),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())

ggsave("Figures/Sens_boxplots.png",width = 3.5, height = 7.5)
ggsave("Figures/Sens_boxplots.svg",width = 3.5, height = 7.5)


r.vals$variable<-factor(r.vals$variable,
                        levels = c("SWE[max]","P[summer]","Baseflow[winter]","T[summer]"),
                        labels = c("SWE[max]","P[summer]","Baseflow[winter]","T[summer]"))

ggplot(r.vals%>%filter(variable%in%c("SWE[max]","Baseflow[winter]","T[summer]","P[summer]")),
       aes(y = regime,
           # x = value
           x = value
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(Pearson~r),
                     limits = c(-0.8,0.8))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "variable", ncol = 2,strip.position = "left",
             as.table = TRUE,
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.65,0.90),
        legend.background = element_rect(color = "black", alpha('white', 0.8)),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())
ggsave("Figures/Sens_boxplots_4.png",width = 6, height = 3,dpi = 1200)





r.vals<-melt(stations,id.vars = c("ID","Station.Name","regime"),
             measure.vars = c("rSWE","rBF","rTSum","rPSum","rT7","r.lcd.sw",
                              "rSWE.p","rBF.p","rTSum.p","rPSum.p","rT7.p","r.lcd.sw.p",
                              "rSWE.max","rSWE.max.p","rTWin","rTWin.p","rPWin","rPWin.p",
                              "rBF_lynehollick_fxd","rBF_lynehollick_fxd.p","rBF_boughton_fxd",
                              "rBF_boughton_fxd.p","rBF_jakeman_fxd","rBF_jakeman_fxd.p","rBF_maxwell_fxd",
                              "rBF_maxwell_fxd.p","rBF_chapman_fxd","rBF_chapman_fxd.p","rBF_Eckhardt0.995_fxd",
                              "rBF_Eckhardt0.995_fxd.p","rBF_Eckhardt0.97_fxd","rBF_Eckhardt0.97_fxd.p",
                              "rBF_lynehollick_30day","rBF_lynehollick_30day.p","rBF_boughton_30day",
                              "rBF_boughton_30day.p","rBF_jakeman_30day","rBF_jakeman_30day.p",
                              "rBF_maxwell_30day","rBF_maxwell_30day.p","rBF_chapman_30day","rBF_chapman_30day.p",
                              "rBF_Eckhardt0.995_30day","rBF_Eckhardt0.995_30day.p","rBF_Eckhardt0.97_30day",
                              "rBF_Eckhardt0.97_30day.p"))

r.vals$sigVal<-str_detect(r.vals$variable,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("value","p.val"))
r.vals$var<-str_remove(r.vals$variable,"\\.p")

r.vals$regime<-factor(r.vals$regime,
                      levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


r.vals<-tidyr::pivot_wider(r.vals,
                           id_cols = c("ID","Station.Name","var","regime"),
                           names_from = sigVal,
                           values_from = value
)

r.vals%>%
  group_by(var)%>%
  summarize(median(value),
            mean(value),
            sum(p.val<0.05)/n())%>%
  print(n = 100)
r.vals%>%
  group_by(var,regime)%>%
  summarize(median(value),
            mean(value),
            sum(p.val<0.05)/n())%>%
  print(n = 100)

r.vals%>%
  group_by(var)%>%
  summarize(median(value),
            mean(value),
            sum(p.val<0.05)/n())%>%
  print(n = 100)

r.vals.BF<-r.vals[substr(r.vals$var,1,3)=="rBF",]
r.vals.BF$algorithm<-str_split_fixed(r.vals.BF$var,"_",3)[,2]%>%
  factor(levels = c("Eckhardt0.97","Eckhardt0.995","boughton","chapman","jakeman","lynehollick","maxwell"),
         labels = c("Eckhardt",'"Eckhardt, a\\u003D0.995"',"Boughton","Chapman","Jakeman","Lyne-Hollick","Maxwell"))

r.vals.BF$period<-str_split_fixed(r.vals.BF$var,"_",3)[,3]%>%
  factor(levels = c("30day","fxd"),
         labels = c("30-day~mean~before~SWE[max]","Fixed~Month"))



r.vals.BF<-filter(r.vals.BF,algorithm!="")


ggplot(r.vals.BF%>%filter(!is.na(value)),aes(y = regime,
                                             # x = value
                                             x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(Pearson~r),
                     limits = c(-0.8,0.8),
                     breaks = c(-0.5,0,0.5))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_grid(rows = vars(algorithm),cols = vars(period) ,
             # strip.position = "left",
             labeller = label_parsed
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black"),
    legend.margin = margin(t=-3,r=1,b=-2,l=0,unit = "pt"),
    legend.key = element_blank(),
    legend.justification = "right"
    #     legend.title = element_blank()
  )

ggsave("Figures/Sens_boxplots_BF_alternatives.png",width = 7, height = 10)
# ggsave("Figures/Sens_boxplots_BF_alternatives.svg",width = 7, height = 10)


r.vals.BF%>%
  filter(!is.na(value))%>%
  group_by(var)%>%
  summarise(mean(value),
            sum(p.val<0.05)/n())


r.valsSWE<-filter(r.vals,substr(var,1,4)=="rSWE")
r.valsSWE$var<-factor(r.valsSWE$var,levels = c("rSWE.max","rSWE"),
                      labels = c("SWE[max]","SWE[fixed~month]"))

ggplot(r.valsSWE,aes(y = regime,
                     # x = value
                     x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(Pearson~r),
                     limits = c(-0.8,0.8),
                     breaks = c(-0.5,0,0.5))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = vars(var),
             strip.position = "left",
             ncol = 1,
             labeller = label_parsed
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black"),
    legend.margin = margin(t=-3,r=1,b=-2,l=0,unit = "pt"),
    legend.key = element_blank(),
    legend.justification = "right"
    #     legend.title = element_blank()
  )
ggsave("Figures/Sens_Boxplots_SWE.png",width = 3.5,height = 4)

r.valsTPWin<-filter(r.vals,var%in%c("rTWin","rPWin"))

r.valsTPWin$var<-factor(r.valsTPWin$var,
                        levels = c("rTWin","rPWin"),
                        labels = c("T[winter]","P[Winter]"))

ggplot(r.valsTPWin,aes(y = regime,
                       # x = value
                       x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(Pearson~r),
                     limits = c(-0.8,0.8),
                     breaks = c(-0.5,0,0.5))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = vars(var),
             ncol =1,
             strip.position = "left",
             labeller = label_parsed
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black"),
    legend.margin = margin(t=-3,r=1,b=-2,l=0,unit = "pt"),
    legend.key = element_blank(),
    legend.justification = "right"
    #     legend.title = element_blank()
  )

ggsave("Figures/Sens_boxplots_TPWin.png",width = 3.5,height = 4 )



sp.vals<-melt(stations,id.vars = c("ID","Station.Name","regime"),
              measure.vars = c("spSWE","spBF","spTSum","spT7","spPSum","sp.lcd.sw",
                               "spSWE.p","spBF.p","spTSum.p","spT7.p","spPSum.p","sp.lcd.sw.p"))

sp.vals$sigVal<-str_detect(sp.vals$variable,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("value","p.val"))
sp.vals$var<-str_remove(sp.vals$variable,"\\.p")

sp.vals$regime<-factor(sp.vals$regime,
                       levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                       labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


sp.vals<-tidyr::pivot_wider(sp.vals,
                            id_cols = c("ID","Station.Name","var","regime"),
                            names_from = sigVal,
                            values_from = value
)
sp.vals$var<-factor(sp.vals$var,
                    levels = c("spSWE","spBF","spPSum","spTSum","spT7","sp.lcd.sw"),
                    labels = c("SWE[max]","Baseflow[winter]","P[summer]","T[summer]","T[7]","Water~Use"))

ggplot(sp.vals%>%filter(!is.na(value)),aes(y = regime,
                                           # x = value
                                           x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = "semipartial correlation",
                     limits = c(-0.8,0.8))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "var", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.17,0.96),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())

ggsave("Figures/Sens_boxplots_sp.png",width = 3.5, height = 7.5)


# is there non-linearity in TempSummer?

sum(stations$spTSumsq>0)
sum(stations$spTSumsq<0)
stations%>%
  group_by(regime)%>%
  summarize(prop = sum(spTSumsq>0)/n(),
            binomTest = binom.test(sum(spTSumsq>0),n(),p=0.5)$p.value,
            
            binomTestp0.05 = binom.test(sum(spTSumsq.p<0.05),n(),p=0.05,alternative = "greater")$p.value)
sum(stations$spTSumsq.p<0.05)
sum(stations$spTSumsq.p<0.05& stations$spTSumsq>0)
sum(stations$spTSumsq.p<0.05& stations$spTSumsq<0)
binom.test(sum(stations$spTSumsq>0),230,p=0.5)

cor.test(stations$rTSum,stations$meanSummerTemp)
summary(lm(rTSum~meanSummerTemp,stations))
summary(lm(rTSum~meanSummerTemp,stations%>%filter(regime == "Rainfall")))
summary(lm(rTSum~meanSummerTemp,stations%>%filter(regime == "Hybrid")))
summary(lm(rTSum~meanSummerTemp,stations%>%filter(regime == "Snowfall")))
summary(lm(rTSum~meanSummerTemp,stations%>%filter(regime == "Glacial")))


# create Maps #########
watersheds_2<-left_join(watersheds,stations[,-which(names(stations)%in%"Area_km2")],by = c("StationNum"="ID"))

watersheds_2<-watersheds_2%>%filter(substr(StationNum,1,2)%in%c("08","09"))

stations_metadata<-read.csv("2.data/2.working/StationMetadata/stations_metadata.csv")
# stations<-stations%>%filter(!ID%in%stations_metadata$ID[stations_metadata$perc_Gl>0.05])

splitString<- regex("\u00B0|\u2032|\u2033|\u201D|\u2019|'|\\\"| |o")
stations$Lat<- as.numeric(str_split_i(stations$Latitude,splitString,1))+
  + as.numeric(str_split_i(stations$Latitude,splitString,2))/60+
  + as.numeric(str_split_i(stations$Latitude,splitString,3))/3600
stations$Lon<- - as.numeric(str_split_i(stations$Longitude,splitString,1))+
  - as.numeric(str_split_i(stations$Longitude,splitString,2))/60+
  - as.numeric(str_split_i(stations$Longitude,splitString,3))/3600
stations$Lon[stations$ID=="08NE006"]<- -117.730643
stations_sf<-st_as_sf(stations,coords = c("Lon","Lat"),crs = "EPSG:4326")%>%
  st_transform(st_crs(watersheds))


# stations_sf$Area_km2<-stations_sf$Area_km2%>%str_remove(",")%>%as.numeric()
# stations_sf$Area_km2[is.na(stations_sf$Area_km2)]<-st_area(watersheds[is.na(stations_sf$Area_km2),])*10^-6
stations_sf<-left_join(stations_sf[,-which(names(stations)%in%"Area_km2")],st_drop_geometry(watersheds[,c("StationNum","Area_km2")]),by = c("ID" = "StationNum"))

stations_sf<-stations_sf%>%
  dplyr::rename("StationNum" = "ID",
                "NameNom" = "Station.Name")

stations_sf2<-st_buffer(stations_sf,dist = 9000)
st_geometry(stations_sf2)<-"geom"

nms_both<-names(watersheds_2)[names(watersheds_2)%in%names(stations_sf2)]

plot_sf<-rbind(watersheds_2[watersheds_2$Area_km2>200,nms_both],
               stations_sf2[stations_sf2$Area_km2<=200,nms_both])
plot_sf<-plot_sf[order(plot_sf$Area_km2,decreasing = TRUE),]
# for(it in 1:nrow(plot_sf)){
#   plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),]))
# }


bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")

createPanelPlot_r<-function(bc,plot_sf,varName,p_varName,labName,include.legend,corType){
  
  plot_sf<-plot_sf[!is.na(st_drop_geometry(plot_sf[,varName])),]
  varName <- sym(varName)
  p_varName <- sym(p_varName)
  
  # brks<-c(-1,-0.5,-0.3,-0.1,0,0.1,0.3,0.5,1)
  brks<-c(-1,-0.75,-0.5,-0.25,-0.1,0.1,0.25,0.5,0.75,1)
  fctrs<-cut(seq(-1,1,0.05),breaks = brks,include.lowest = TRUE)
  fctsLabs<- c("-1.00 to -0.75","-0.75 to -0.50","-0.50 to -0.25","-0.25 to -0.10","-0.10 to 0.10",
               "0.10 to 0.25","0.25 to 0.50","0.50 to 0.75","0.75 to 1.00")
  # fctsLabs<- c("-1.0 to -0.5","-0.5 to -0.3","-0.3 to -0.1","-0.1 to 0.0",
  #              "0.0 to 0.1","0.1 to 0.3","0.3 to 0.5","0.5 to 1.0")
  
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "grey90",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "white",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = plot_sf,col = "white",lwd  =0.25)+
    geom_sf_pattern(data = plot_sf,aes(group = -1,,fill = cut(!!varName,breaks = brks,include.lowest = TRUE),
                                       pattern = !!p_varName<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey50",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600)+
    scale_pattern_manual(breaks = c(TRUE,FALSE),
                         values = c("none","stripe"),
                         labels = c("p<0.05","p\u22650.05"),
                         name = "statistical\nsignificance")+
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    scale_fill_manual(
      limits = levels(fctrs),
      labels = fctsLabs,
      values = scico(9,palette = "vik",direction = -1),
      drop = FALSE)+
    labs(fill = corType)+
    # ggtitle(labName)+
    annotate("text",  x=920000, y = 1710000, label = labName, vjust=0, hjust=0.5,
             fontface = "bold")+
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    theme(legend.position = c(0.83,0.7),
          plot.title = element_text(hjust = 0.5,vjust=0,face = "bold",),
          legend.title = element_text(face = "bold",hjust = 0.5,size = 8),
          legend.background = element_rect(fill = "grey85",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "white",colour = "grey50"),
          legend.text = element_text(size = 8),
          legend.key.size = unit(8,"pt"))+
    guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           col = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 6,angle = 0)))
  
  if(include.legend=="FALSE"){
    p1<-p1+theme(legend.position = "none")
  }
  # p1_inset<-ggplot(plot_sf,aes(x = !!varName))+geom_histogram()+
  #   scale_x_continuous(limits = c(-1,1),
  #                      name = corType)+
  #   scale_y_continuous(name= "# catchments")+
  #   theme_bw()+
  #   theme(plot.background = NULL,
  #         axis.text = element_text(size = 8),
  #         axis.title = element_text(size = 8),
  #         panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  # 
  # p1 <-
  #   ggdraw() +
  #   draw_plot(p1) +
  #   draw_plot(p1_inset, x = -0.02, y = 0, width = .4, height = .35)
  
  p1
  
}



p1<-createPanelPlot_r(bc,plot_sf,"rSWE.max","rSWE.max.p",expression(bold("A:"~SWE[max])),include.legend = FALSE,
                      corType = "Pearson r")
# ggsave("Figures/SensMaps_rSWE_wSnow.png",p1,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rSWE_wSnow.svg",p1,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

# ggsave("Figures/SensMaps_rSWE_wSnow.svg",p1,width = 3.75/3,height = 3.225/3,
#        bg = "white",dpi = 600,
#        device="svg")

p2<-createPanelPlot_r(bc,plot_sf,"rBF_Eckhardt0.97_30day","rBF_Eckhardt0.97_30day.p","B: End-of-Winter Baseflow",include.legend = TRUE,
                      corType = expression(r^2%*%sign(r)))
# ggsave("Figures/SensMaps_rBF_wSnow.png",p2,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rBF_wSnow.svg",p2,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

p3<-createPanelPlot_r(bc,plot_sf,"rTSum","rTSum.p","C: Summer Temperature",include.legend = TRUE,
                      corType = expression(r^2%*%sign(r)))
# ggsave("Figures/SensMaps_rTSum_wSnow.png",p3,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rTSum_wSnow.svg",p3,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

p4<-createPanelPlot_r(bc,plot_sf,"rPSum","rPSum.p","D: Summer Precipitation",include.legend = TRUE,
                      corType = expression(r^2%*%sign(r)))
# ggsave("Figures/SensMaps_rPSum_wSnow.png",p4,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rPSum_wSnow.svg",p4,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

p5<-createPanelPlot_r(bc,plot_sf,"rT7","rT7.p","E: 7-day Temperature",include.legend = TRUE,
                      corType = expression(r^2%*%sign(r)))
# ggsave("Figures/SensMaps_rT7_wSnow.png",p5,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rT7_wSnow.svg",p5,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

p6<-createPanelPlot_r(bc,plot_sf,"r.lcd.sw","r.lcd.sw.p","F: Licensed Abstraction",include.legend = TRUE,
                      corType = expression(r^2%*%sign(r)))
# ggsave("Figures/SensMaps_rSW.lic_wSnow.png",p6,width = 3.75,height = 3.225,
#        bg = "white",dpi = 600)
ggsave("Figures/SensMaps_rSW.lic_wSnow.svg",p6,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device="svg")

# pSens_r<-cowplot::plot_grid(p1,p2,p3,p4,p5,p6,nrow = 3,scale = 0.99)

# ggsave("Figures/SensMaps_r.png",pSens_r,width = 7.5,height = 6.45*1.5,
#        bg = "white",dpi = 600)



p1<-createPanelPlot_r(bc,plot_sf,"spSWE","spSWE.p",expression(bold("A:"~SWE[max])),include.legend = FALSE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spSWE_wSnow.png",p1,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)
p2<-createPanelPlot_r(bc,plot_sf,"spBF","spBF.p","B: End-of-Winter Baseflow",include.legend = TRUE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spBF_wSnow.png",p2,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)
p3<-createPanelPlot_r(bc,plot_sf,"spTSum","spTSum.p","C: Summer Temperature",include.legend = FALSE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spTSum_wSnow.png",p3,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)
p4<-createPanelPlot_r(bc,plot_sf,"spPSum","spPSum.p","D: Summer Precipitation",include.legend = FALSE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spPSum_wSnow.png",p4,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)
p5<-createPanelPlot_r(bc,plot_sf,"spT7","spT7.p","E: 7-day Temperature",include.legend = FALSE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spT7_wSnow.png",p5,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)
p6<-createPanelPlot_r(bc,plot_sf,"sp.lcd.sw","sp.lcd.sw.p","F: Licensed Abstraction",include.legend = FALSE,
                      corType = "semipartial r")
ggsave("Figures/SensMaps_spSW.lic_wSnow.png",p6,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)







# Check stationarity of variables ##########
spltDF<-data.frame(spltYr = 1983:2002,numWtrshds= NA)
cntFunc = function(x,splt){sum(x<=splt)>=20&sum(x>splt)>20}
for(it_splt in 1:20){
  spltDF$numWtrshds[it_splt] = lapply(availYears,cntFunc,spltDF$spltYr[it_splt])%>%unlist()%>%sum()
  
}



summary(my_lms.post[[1]])
# mylm.post1996$coefficients
names(my_lms.post)<-stations$ID 
names(my_lms.pre)<-stations$ID 

my_lms.post.2<-lapply(my_lms.post,FUN = function(x){x$coefficients})
my_lms.post.3<-unlist(my_lms.post.2)


my_lms.post.df<-data.frame(ID = substr(names(my_lms.post.3),1,7),
                           # month = substr(names(agreementVect),9,11),
                           var = substr(names(my_lms.post.3),9, nchar(names(my_lms.post.3))),
                           coeff = my_lms.post.3)%>%
  filter(!var%in%"(Intercept)")

my_lms.post.sd<-
  lapply(my_lms.post[vapply(my_lms.post, Negate(is.null), NA)],FUN = function(x){summary(x)$coefficients[,2]})%>%
  unlist()%>%
  data.frame()

my_lms.post.sd$ID<-substr(rownames(my_lms.post.sd),1,7)
my_lms.post.sd$var<-substr(rownames(my_lms.post.sd),9, nchar(rownames(my_lms.post.sd)))
names(my_lms.post.sd)[1]<-"std.err"
my_lms.post.df<-left_join(my_lms.post.df,my_lms.post.sd)



my_lms.pre.2<-lapply(my_lms.pre,FUN = function(x){x$coefficients})
my_lms.pre.3<-unlist(my_lms.pre.2)
my_lms.pre.df<-data.frame(ID = substr(names(my_lms.pre.3),1,7),
                          # month = substr(names(agreementVect),9,11),
                          var = substr(names(my_lms.pre.3),9, nchar(names(my_lms.pre.3))),
                          coeff = my_lms.pre.3)%>%
  filter(!var%in%"(Intercept)")


my_lms.pre.sd<-
  lapply(my_lms.pre[vapply(my_lms.pre, Negate(is.null), NA)],FUN = function(x){summary(x)$coefficients[,2]})%>%
  unlist()%>%
  data.frame()

my_lms.pre.sd$ID<-substr(rownames(my_lms.pre.sd),1,7)
my_lms.pre.sd$var<-substr(rownames(my_lms.pre.sd),9, nchar(rownames(my_lms.pre.sd)))
names(my_lms.pre.sd)[1]<-"std.err"
my_lms.pre.df<-left_join(my_lms.pre.df,my_lms.pre.sd)


Coeffs<-left_join(my_lms.pre.df,my_lms.post.df,by = c("ID","var"),suffix = c(".pre",".post"))

Coeffs<-left_join(Coeffs,stations[,c("ID","regime")])


Coeffs$sigIncrease = (((Coeffs$coeff.post)>(Coeffs$coeff.pre+Coeffs$std.err.pre*1.96))+
                        ((Coeffs$coeff.post-Coeffs$std.err.post*1.96)>(Coeffs$coeff.pre)))/2

Coeffs$sigDecrease = (((Coeffs$coeff.post)<(Coeffs$coeff.pre-Coeffs$std.err.pre*1.96))+
                        ((Coeffs$coeff.post+Coeffs$std.err.post*1.96)<(Coeffs$coeff.pre)))/2

Coeffs%>%group_by(var,regime)%>%
  summarize(nIncrease = sum(coeff.post>coeff.pre,na.rm = TRUE),
            num = n(),
            p = pmin(pbinom(nIncrease,num,0.5),
                     pbinom(nIncrease,num,0.5,lower.tail = FALSE)),
            sig = p<0.025,
            sigIncrease = sum(sigIncrease,na.rm = TRUE)/sum(!is.na(sigIncrease)),
            sigDecrease = sum(sigDecrease,na.rm = TRUE)/sum(!is.na(sigDecrease))
            
            
  )


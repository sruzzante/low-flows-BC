
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
library(lm.beta)
library(tidyr)

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

# Load daily weather data
WeatherDataDaily<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataDaily.csv")%>%
  filter(ID%in%unique(streamDataAll$ID))

streamDataAll<-left_join(streamDataAll,WeatherDataDaily[,-which(names(WeatherDataDaily)=="Date")])


data_SWE<-read.csv("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment.csv")

data_SWE_dly<-read.csv("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.csv")


names(data_SWE_dly)<-c("ID","Date","SWE")


data_SWE$date <- data_SWE$variable%>%ymd()

data_SWE$Year<-year(data_SWE$date)
data_SWE$Month<-month(data_SWE$date)

# data_SWE<-data_SWE%>%
#   group_by(ID,Year,Month)%>%
#   summarize(value = mean(value))

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

### ECA  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")



x<-rast("../DATA/1.Spatial_data/regional/Canada/lulc_landuse_landcover/lulc1_ForestManagement/Canada_MFv2020.tif")

maskPrivate<-x==50

plot(maskPrivate)
bc<-bcmaps::bc_neighbours()%>%filter(name=="British Columbia")%>%
  vect()%>%
  project(crs(maskPrivate))
maskPrivate_bc<-mask(maskPrivate,bc)
summary(maskPrivate_bc)
global(maskPrivate_bc,mean,na.rm = TRUE)
plot(maskPrivate_bc)

maskPrivateBC<-crop(maskPrivate)

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  st_transform(st_crs(maskPrivate))

watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private[,"Canada_MFv2020"])
watersheds$PrivateForestry<-watersheds$watersheds_Private....Canada_MFv2020..
stations<-left_join(stations,st_drop_geometry(watersheds[,c("StationNum","PrivateForestry")]),by = c("ID" = "StationNum"))

TCA<-readRDS("2.data/2.working/ECA/TCA_2022.rds")
names(TCA)[3]<-"Total.Cut.Area"
stations<-left_join(stations,TCA%>%select(StationNum,Total.Cut.Area),by = c("ID" = "StationNum"))

sum(stations$PrivateForestry<0.1)
sum(stations$PrivateForestry<0.1&stations$Total.Cut.Area>0.1)
## Loop through stations and extract variables ################




stations[,c('rSWE.max','rSWE.max.p','rTWin','rTWin.p','rPWin','rPWin.p',
            # 'rBF_lynehollick_fxd',
            # 'rBF_lynehollick_fxd.p','rBF_boughton_fxd','rBF_boughton_fxd.p','rBF_jakeman_fxd',
            # 'rBF_jakeman_fxd.p','rBF_maxwell_fxd','rBF_maxwell_fxd.p','rBF_chapman_fxd',
            # 'rBF_chapman_fxd.p','rBF_Eckhardt0.995_fxd','rBF_Eckhardt0.995_fxd.p',
            # 'rBF_Eckhardt0.97_fxd','rBF_Eckhardt0.97_fxd.p','rBF_lynehollick_30day',
            # 'rBF_lynehollick_30day.p','rBF_boughton_30day','rBF_boughton_30day.p',
            # 'rBF_jakeman_30day','rBF_jakeman_30day.p','rBF_maxwell_30day',
            # 'rBF_maxwell_30day.p','rBF_chapman_30day','rBF_chapman_30day.p',
            # 'rBF_Eckhardt0.995_30day','rBF_Eckhardt0.995_30day.p','rBF_Eckhardt0.97_30day',
            # 'rBF_Eckhardt0.97_30day.p',
            'rSWE','rSWE.p','rBF','rBF.p','rTSum','rTSum.p','spTSumsq','spTSumsq.p',
            'rPSum','rPSum.p','rT7','rT7.p','r.lcd.sw','r.lcd.sw.p','spSWE','spSWE.p',
            'spBF','spBF.p','spTSum','spTSum.p','spPSum','spPSum.p','spT7','spT7.p',
            'spTSum.T7','spTSum.T7.p','sp.lcd.sw','sp.lcd.sw.p',
            'rECA5','rECA10','rECA20','rECA60',
            'rECA5.p','rECA10.p','rECA20.p','rECA60.p',
            'sp.ECA5','sp.ECA10','sp.ECA20','sp.ECA60',
            'sp.ECA5.p','sp.ECA10.p','sp.ECA20.p','sp.ECA60.p',
            
            'lowQ.10_highSWE',
            'llowQ.10_highSWE.90','minFlowMonth',"meanSummerTemp")]<-NA


my_lms<-list()
my_lms.pre<-list()
my_lms.post<-list()
availYears<-list()

for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08LB069")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  # streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)
  # if(sum(!is.na(streamData$Mean.Temp..C.))==0){next}
  # 
  # streamData$BaseFlow_lynehollick<-gr_baseflow(streamData$Discharge, method = 'lynehollick', a = 0.925, passes = 3)
  # streamData$BaseFlow_boughton<-gr_baseflow(streamData$Discharge, method = 'boughton', k = 0.975, passes = 3)
  # streamData$BaseFlow_jakeman<-gr_baseflow(streamData$Discharge, method = 'jakeman', a = 0.925, passes = 3,aq = -0.5)
  # streamData$BaseFlow_maxwell<-gr_baseflow(streamData$Discharge, method = 'maxwell', k = 0.975, passes = 3)
  # streamData$BaseFlow_chapman<-gr_baseflow(streamData$Discharge, method = 'chapman', a = 0.925, passes = 3)
  streamData$BaseFlow_Eckhardt0.97<-NA
  # streamData$BaseFlow_Eckhardt0.995<-NA
  streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
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
                     
                     # SumT7=T7[DayOfYear=minSumFlowDay],
                     # AugT7=T7[DayOfYear=minAugFlowDay],
                     # SepT7=T7[DayOfYear=minSepFlowDay],
                     # OctT7=T7[DayOfYear=minOctFlowDay],
                     # 
                     # JuneBF_lynehollick = mean(BaseFlow_lynehollick[month==6]),
                     # MayBF_lynehollick = mean(BaseFlow_lynehollick[month==5]),
                     # AprilBF_lynehollick = mean(BaseFlow_lynehollick[month==4]),
                     # MarchBF_lynehollick = mean(BaseFlow_lynehollick[month==3]),
                     # FebruaryBF_lynehollick = mean(BaseFlow_lynehollick[month==2]),
                     # 
                     # 
                     # JuneBF_boughton = mean(BaseFlow_boughton[month==6]),
                     # MayBF_boughton = mean(BaseFlow_boughton[month==5]),
                     # AprilBF_boughton = mean(BaseFlow_boughton[month==4]),
                     # MarchBF_boughton = mean(BaseFlow_boughton[month==3]),
                     # FebruaryBF_boughton = mean(BaseFlow_boughton[month==2]),
                     # 
                     # 
                     # JuneBF_jakeman = mean(BaseFlow_jakeman[month==6]),
                     # MayBF_jakeman = mean(BaseFlow_jakeman[month==5]),
                     # AprilBF_jakeman = mean(BaseFlow_jakeman[month==4]),
                     # MarchBF_jakeman = mean(BaseFlow_jakeman[month==3]),
                     # FebruaryBF_jakeman = mean(BaseFlow_jakeman[month==2]),
                     # 
                     # 
                     # JuneBF_maxwell = mean(BaseFlow_maxwell[month==6]),
                     # MayBF_maxwell = mean(BaseFlow_maxwell[month==5]),
                     # AprilBF_maxwell = mean(BaseFlow_maxwell[month==4]),
                     # MarchBF_maxwell = mean(BaseFlow_maxwell[month==3]),
                     # FebruaryBF_maxwell = mean(BaseFlow_maxwell[month==2]),
                     # 
                     # JuneBF_chapman = mean(BaseFlow_chapman[month==6]),
                     # MayBF_chapman = mean(BaseFlow_chapman[month==5]),
                     # AprilBF_chapman = mean(BaseFlow_chapman[month==4]),
                     # MarchBF_chapman = mean(BaseFlow_chapman[month==3]),
                     # FebruaryBF_chapman = mean(BaseFlow_chapman[month==2]),
                     # 
                     # JuneBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==6]),
                     # MayBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==5]),
                     # AprilBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==4]),
                     # MarchBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==3]),
                     # FebruaryBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==2]),
                     # 
                     # JuneBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==6]),
                     # MayBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==5]),
                     # AprilBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==4]),
                     # MarchBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==3]),
                     # FebruaryBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==2]),
                     # # 
                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     # 
                     # # # 
                     # BF_30day_lynehollick = mean(BaseFlow_lynehollick[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_boughton = mean(BaseFlow_boughton[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_jakeman = mean(BaseFlow_jakeman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_maxwell = mean(BaseFlow_maxwell[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_chapman = mean(BaseFlow_chapman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # 
                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)])
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
  
  
  # if(snowData$maxMonth[1]%in%c(12,1,2,4,6)){
  #   streamDataYrly$winterBF_lynehollick<-streamDataYrly$AprilBF_lynehollick
  #   streamDataYrly$winterBF_boughton<-streamDataYrly$AprilBF_boughton
  #   streamDataYrly$winterBF_jakeman<-streamDataYrly$AprilBF_jakeman
  #   streamDataYrly$winterBF_maxwell<-streamDataYrly$AprilBF_maxwell
  #   streamDataYrly$winterBF_chapman<-streamDataYrly$AprilBF_chapman
  #   streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$AprilBF_Eckhardt0.995
  #   streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$AprilBF_Eckhardt0.97
  #   
  #   
  # }else if(snowData$maxMonth[1]%in%3){
  #   
  #   
  #   streamDataYrly$winterBF_lynehollick<-streamDataYrly$MarchBF_lynehollick
  #   streamDataYrly$winterBF_boughton<-streamDataYrly$MarchBF_boughton
  #   streamDataYrly$winterBF_jakeman<-streamDataYrly$MarchBF_jakeman
  #   streamDataYrly$winterBF_maxwell<-streamDataYrly$MarchBF_maxwell
  #   streamDataYrly$winterBF_chapman<-streamDataYrly$MarchBF_chapman
  #   streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$MarchBF_Eckhardt0.995
  #   streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$MarchBF_Eckhardt0.97
  #   
  # }else if(snowData$maxMonth[1]%in%5){
  #   
  #   streamDataYrly$winterBF_lynehollick<-streamDataYrly$MayBF_lynehollick
  #   streamDataYrly$winterBF_boughton<-streamDataYrly$MayBF_boughton
  #   streamDataYrly$winterBF_jakeman<-streamDataYrly$MayBF_jakeman
  #   streamDataYrly$winterBF_maxwell<-streamDataYrly$MayBF_maxwell
  #   streamDataYrly$winterBF_chapman<-streamDataYrly$MayBF_chapman
  #   streamDataYrly$winterBF_Eckhardt0.995<-streamDataYrly$MayBF_Eckhardt0.995
  #   streamDataYrly$winterBF_Eckhardt0.97<-streamDataYrly$MayBF_Eckhardt0.97
  # }
  
  
  ## add in ECA data
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  
  
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
  
  
  stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  
  # 
  # x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$SWE,use = "complete")
  # if(x$parameter>=8){
  #   stations$rSWE[it_stn]<-x$estimate
  #   stations$rSWE.p[it_stn]<-x$p.value
  # }  
  # x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$maxSWEdly,use = "complete")
  # if(x$parameter>=8){
  #   stations$rSWE.max[it_stn]<-x$estimate
  #   stations$rSWE.max.p[it_stn]<-x$p.value
  # }
  # x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$meanWinterTemp,use = "complete")
  # if(x$parameter>=8){
  #   stations$rTWin[it_stn]<-x$estimate
  #   stations$rTWin.p[it_stn]<-x$p.value
  # }
  # 
  # x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$winterPrecip,use = "complete")
  # if(x$parameter>=8){
  #   stations$rPWin[it_stn]<-x$estimate
  #   stations$rPWin.p[it_stn]<-x$p.value
  # }
  # 
  # x<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$TempSummer,use = "complete")
  # if(x$parameter>=8){
  #   stations$rTSum[it_stn]<-x$estimate
  #   stations$rTSum.p[it_stn]<-x$p.value
  # }
  
  
  
  
  # if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)|
  #     any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)|
  #     any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE))&
  #    sd(dataYearly2$Total.cms)>0){
  #   stations$r.lcd.sw[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$Total.cms,use = "complete")
  #   stations$r.lcd.sw.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$Total.cms,use = "complete")$p.value
  #   
  # }
  
  # if(stations$PrivateForestry[it_stn]<0.1&
  #    any(streamDataYrly$ECA_60>0.1)){
  #   stations$rECA5[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$ECA_5,use = "complete")
  #   stations$rECA5.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$ECA_5,use = "complete")$p.value
  #   
  #   stations$rECA10[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$ECA_10,use = "complete")
  #   stations$rECA10.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$ECA_10,use = "complete")$p.value
  #   
  #   stations$rECA20[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$ECA_20,use = "complete")
  #   stations$rECA20.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$ECA_20,use = "complete")$p.value
  #   
  #   stations$rECA60[it_stn]<-cor(dataYearly2$minSumFlow7log,dataYearly2$ECA_60,use = "complete")
  #   stations$rECA60.p[it_stn]<-cor.test(dataYearly2$minSumFlow7log,dataYearly2$ECA_60,use = "complete")$p.value
  #   
  # }
  
  
  
  summary(mylm<-lm(minSumFlow7log~maxSWEdly+TempSummer+PcpSummer+Total.cms,
                   data = dataYearly2,na.action= "na.exclude"))
  
  
  dat<-dataYearly2[!is.na(residuals(mylm)),]
  # availYears[[it_stn]]<-dat$NovWaterYear
  
  if(nrow(dat)<10){next}
  if(stations$PrivateForestry[it_stn]<0.1&
     any(streamDataYrly$ECA_60>0.1)){
    
    
    if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)|
        any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)|
        any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE))&
       sd(dat$Total.cms)>0&
       !stations$ID[it_stn]%in%c("08OA003","08OA004")){
      
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","Total.cms","ECA_5")])
      # stations$sp.ECA5[it_stn]<-x$estimate["ECA_5","minSumFlow7log"]
      # stations$sp.ECA5.p[it_stn]<-x$p.value["ECA_5","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","Total.cms","ECA_10")])
      # stations$sp.ECA10[it_stn]<-x$estimate["ECA_10","minSumFlow7log"]
      # stations$sp.ECA10.p[it_stn]<-x$p.value["ECA_10","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","Total.cms","ECA_20")])
      # stations$sp.ECA20[it_stn]<-x$estimate["ECA_20","minSumFlow7log"]
      # stations$sp.ECA20.p[it_stn]<-x$p.value["ECA_20","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","Total.cms","ECA_60")])
      # stations$sp.ECA60[it_stn]<-x$estimate["ECA_60","minSumFlow7log"]
      # stations$sp.ECA60.p[it_stn]<-x$p.value["ECA_60","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","Total.cms","ECA_60","ECA_10")])
      # stations$sp.ECA10.b[it_stn]<-x$estimate["ECA_10","minSumFlow7log"]
      # stations$sp.ECA10.b.p[it_stn]<-x$p.value["ECA_10","minSumFlow7log"]
      # 
      # stations$sp.ECA60.b[it_stn]<-x$estimate["ECA_60","minSumFlow7log"]
      # stations$sp.ECA60.b.p[it_stn]<-x$p.value["ECA_60","minSumFlow7log"]
      
      summary(mylm<-lm(log(minMonFlow7)~maxSWEdly+TempSummer+PcpSummer+ECA_5+ECA_20+ECA_60+Temp_2+Precip_2+Total.cms,
                       data = dataYearly2,na.action= "na.exclude"))
      # summary(mylm<-lm(minSumFlow7log~maxSWEdly+TempSummer+PcpSummer+ECA_10+ECA_60+Total.cms,
      #                  data = dataYearly2,na.action= "na.exclude"))
      
      dat_x<-data.frame(maxSWEdly=0,TempSummer = 0,PcpSummer = 0,winterPrecip=0,
                        meanWinterTemp=0,Total.cms=0 ,yr = 0:100, Temp_2=0,Precip_2=0
      )
      dat_x$ECA_5 = pmax((5-dat_x$yr)/5,0)
      dat_x$ECA_10 = pmax((10-dat_x$yr)/10,0)
      dat_x$ECA_20 = pmax((20-dat_x$yr)/20,0)
      dat_x$ECA_60= pmax((60-dat_x$yr)/60,0)
      dat_x$pr<-predict(mylm,dat_x)
      
      dat_x$pr<-dat_x$pr-dat_x$pr[101]
      ggplot(dat_x,aes(x = yr,y=pr))+geom_line()+
        ggtitle(paste0(stations$Station.Name[it_stn],", ", stations$regime[it_stn]))+
        scale_y_continuous(name = "Effect on Low Flows (unitless)")+
        scale_x_continuous("Years since clearcut")+
        theme_bw()
      
      ggsave(filename = paste0("3.figures/ECA_shape/ECA_shape3_",stations$ID[it_stn],".png"),
             width = 6,height=4,dpi = 150)
      
      
    } else {
      
      # summary(mylm<-lm(log(minMonFlow7)~maxSWEdly+TempSummer+PcpSummer+ECA_5+ECA_60,
      #                  data = dataYearly2,na.action= "na.exclude"))
      # 
      # dat<-dataYearly2[!is.na(residuals(mylm)),]
      
      
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","ECA_5")])
      # stations$sp.ECA5[it_stn]<-x$estimate["ECA_5","minSumFlow7log"]
      # stations$sp.ECA5.p[it_stn]<-x$p.value["ECA_5","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","ECA_10")])
      # stations$sp.ECA10[it_stn]<-x$estimate["ECA_10","minSumFlow7log"]
      # stations$sp.ECA10.p[it_stn]<-x$p.value["ECA_10","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","ECA_20")])
      # stations$sp.ECA20[it_stn]<-x$estimate["ECA_20","minSumFlow7log"]
      # stations$sp.ECA20.p[it_stn]<-x$p.value["ECA_20","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","ECA_60")])
      # stations$sp.ECA60[it_stn]<-x$estimate["ECA_60","minSumFlow7log"]
      # stations$sp.ECA60.p[it_stn]<-x$p.value["ECA_60","minSumFlow7log"]
      # 
      # x<-ppcor::spcor(dat[,c("minSumFlow7log","maxSWEdly","TempSummer","PcpSummer","ECA_60","ECA_10")])
      # stations$sp.ECA10.b[it_stn]<-x$estimate["ECA_10","minSumFlow7log"]
      # stations$sp.ECA10.b.p[it_stn]<-x$p.value["ECA_10","minSumFlow7log"]
      # 
      # stations$sp.ECA60.b[it_stn]<-x$estimate["ECA_60","minSumFlow7log"]
      # stations$sp.ECA60.b.p[it_stn]<-x$p.value["ECA_60","minSumFlow7log"]
      
      summary(mylm<-lm(log(minMonFlow7)~maxSWEdly+TempSummer+PcpSummer+Temp_2+Precip_2+ECA_5+ECA_20+ECA_60+Total.cms,
                       data = dataYearly2,na.action= "na.exclude"))
      
      dat_x<-data.frame(maxSWEdly=0,TempSummer = 0,PcpSummer = 0,winterPrecip=0,
                        meanWinterTemp=0,Total.cms=0 , Temp_2=0,Precip_2=0,yr = 0:100)
      dat_x$ECA_5 = pmax((5-dat_x$yr)/5,0)
      dat_x$ECA_10 = pmax((10-dat_x$yr)/10,0)
      dat_x$ECA_20 = pmax((20-dat_x$yr)/20,0)
      dat_x$ECA_60= pmax((60-dat_x$yr)/60,0)
      dat_x$pr<-predict(mylm,dat_x)
      dat_x$pr<-dat_x$pr-dat_x$pr[101]
      ggplot(dat_x,aes(x = yr,y=pr))+geom_line()+
        ggtitle(paste0(stations$Station.Name[it_stn],", ", stations$regime[it_stn]))+
        scale_y_continuous(name = "Effect on Low Flows (unitless)")+
        scale_x_continuous("Years since clearcut")+
        theme_bw()
      
      ggsave(filename = paste0("3.figures/ECA_shape/ECA_shape3_",stations$ID[it_stn],".png"),
             width = 6,height=4,dpi = 150)
      
    }
    
    
  }
  
  
  
}

dat_x<-data.frame(maxSWEdly=0,TempSummer = 0,PcpSummer = 0,winterPrecip=0,meanWinterTemp=0,Total.cms=0 ,yr = 0:100)
dat_x$ECA_5 = pmax((5-dat_x$yr)/5,0)
dat_x$ECA_10 = pmax((10-dat_x$yr)/10,0)
dat_x$ECA_20 = pmax((20-dat_x$yr)/20,0)
dat_x$ECA_60= pmax((60-dat_x$yr)/60,0)

ggplot(dat_x%>%melt(measure.vars = c("ECA_5","ECA_20","ECA_60"),
                    id.vars = "yr"),aes(x = yr,y=value,col = variable))+geom_line(linewidth=2)+
  scale_y_continuous(name = "ECA Coefficient (%)",labels = function(x){x*100})+
  scale_x_continuous("Years since clearcut")+
  scale_color_brewer(palette = "Set2",
                     labels = c("5 year","20 year","60 year"),
                     name = "ECA")+
  
  theme_bw()
ggsave("3.figures/ECA_figs/functions.png")

maxECA<-ECA%>%
  group_by(StationNum)%>%
  dplyr::summarize(maxECA60 = max(ECA_60))

ECA_r<-stations%>%
  dplyr::select(ID,regime,rECA5,rECA10,rECA20,rECA60)%>%
  pivot_longer(cols = c(rECA5,rECA10,rECA20,rECA60))
ECA_r.p<-stations%>%
  dplyr::select(ID,regime,rECA5.p,rECA10.p,rECA20.p,rECA60.p)%>%
  pivot_longer(cols = c(rECA5.p,rECA10.p,rECA20.p,rECA60.p))
ECA_r.p$name<-str_remove(ECA_r.p$name,".p")
ECA_r<-left_join(ECA_r,ECA_r.p,by = c("ID","regime","name"),suffix = c("",".p"))

# ggplot(ECA_r,aes(x = value,fill = name))+
#   geom_histogram()+
#   geom_vline(aes(xintercept = 0))+
#   facet_wrap(facets = vars(regime,name))
#   
ECA_r<-left_join(ECA_r,maxECA,by = c("ID" = "StationNum"))

ECA_r$regime<-factor(ECA_r$regime,
                     levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                     labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())

ECA_r$name<-factor(ECA_r$name,levels = c("rECA5","rECA10","rECA20","rECA60"))

ggplot(ECA_r%>% filter(maxECA60>0.1),aes(y = regime,
                                         # x = value
                                         x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = value.p<0.05),size = 2,height = 0.2,alpha=0.5)+
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
  
  facet_wrap(facets = "name", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.17,0.96),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())


ECA_sp<-stations%>%
  filter(PrivateForestry<0.1)%>%
  dplyr::select(ID,regime,sp.ECA5,sp.ECA10,sp.ECA20,sp.ECA60,sp.ECA10.b,sp.ECA60.b)%>%
  pivot_longer(cols = c(sp.ECA5,sp.ECA10,sp.ECA20,sp.ECA60,sp.ECA10.b,sp.ECA60.b))
ECA_sp.p<-stations%>%
  filter(PrivateForestry<0.1)%>%
  dplyr::select(ID,regime,sp.ECA5.p,sp.ECA10.p,sp.ECA20.p,sp.ECA60.p,sp.ECA10.b.p,sp.ECA60.b.p)%>%
  pivot_longer(cols = c(sp.ECA5.p,sp.ECA10.p,sp.ECA20.p,sp.ECA60.p,sp.ECA10.b.p,sp.ECA60.b.p))

ECA_sp.p$name<-str_remove(ECA_sp.p$name,"\\.p")
ECA_sp<-left_join(ECA_sp,ECA_sp.p,by = c("ID","regime","name"),suffix = c("",".p"))

# ggplot(ECA_r,aes(x = value,fill = name))+
#   geom_histogram()+
#   geom_vline(aes(xintercept = 0))+
#   facet_wrap(facets = vars(regime,name))
#   
ECA_sp<-left_join(ECA_sp,maxECA,by = c("ID" = "StationNum"))

ECA_sp$regime<-factor(ECA_sp$regime,
                      levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())

ECA_sp$name<-factor(ECA_sp$name,levels = c("sp.ECA5","sp.ECA10","sp.ECA20","sp.ECA60","sp.ECA10.b","sp.ECA60.b"))


ggplot(ECA_sp%>% filter(maxECA60>0.1),aes(y = regime,
                                          # x = value
                                          x = value
))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = value.p<0.05),size = 2,height = 0.2,alpha=0.5)+
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
  
  facet_wrap(facets = "name", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.17,0.96),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())




## Analysis #2 #############
stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(StationNum,Area_km2),
                    by = c("ID" = "StationNum"))


varsI<-data.frame()
varsIII<-data.frame()
betasI<-data.frame()
betasIII<-data.frame()
p.valsI<-data.frame()
p.valsIII<-data.frame()

for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08LF051")
  if(stations$PrivateForestry[it_stn]>=0.1){next}
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  if(!any(ECA$ECA_60>0.1)){next}
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  streamData<-streamData%>%
    # filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  
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
  
  ## add in ECA data
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  
  
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
      
      Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
      Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),
      
      Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      
      Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
      Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),
      
      Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      
      Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
      Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),
      
      
    )
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))
  
  
  
  # stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  
  
  summary(mylm<-lm(log(minSumFlow7)~Temp_MJJASO+Precip_MJJASO+Total.cms,
                   data = dataYearly2,na.action= "na.exclude"))
  
  
  dat<-dataYearly2[!is.na(residuals(mylm)),]
  # availYears[[it_stn]]<-dat$NovWaterYear
  
  if(nrow(dat)<10){next}
  if(stations$PrivateForestry[it_stn]<0.1&
     any(streamDataYrly$ECA_60>0.1)){
    
    
    
    dataYearly2Aug<-dataYearly2%>%filter(!is.na(minAugFlow7))
    
    vrs<-expand.grid(
      var_I = c("ECA_I_1", "ECA_I_3","ECA_I_5","ECA_I_7","ECA_I_9","ECA_I_11","ECA_I_13","ECA_I_15","ECA_I_17"),
      var_III = c("ECA_III_12","ECA_III_14","ECA_III_16","ECA_III_18","ECA_III_20","ECA_III_22","ECA_III_24",
                  "ECA_III_26","ECA_III_28","ECA_III_30","ECA_III_32","ECA_III_34","ECA_III_36",
                  "1"),
      var_I_coeff = NA,
      var_III_coeff = NA,
      bts_I = NA,
      bts_III = NA
    )
    
    mylm<-lm(log(minAugFlow7)~
               Temp_MJJA+Precip_MJJA+
               maxSWEdly
             # Temp_JA+Precip_JA+
             # meanWinterTemp+winterPrecip
             # ECA_I_1+
             # ECA_I_3+ECA_I_5+ECA_I_7+ECA_I_9+ECA_I_11+ECA_I_13+ECA_I_15+ECA_I_17+
             # ECA_III_12+ECA_III_14+ECA_III_16+ECA_III_18+ECA_III_20+ECA_III_22+ECA_III_24+
             # ECA_III_26+ECA_III_28+ECA_III_30+ECA_III_32+ECA_III_34+ECA_III_36+
             # August.cms,
             ,
             data =dataYearly2Aug,na.action= "na.fail")
    
    shft=0
    # if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)|
    #     any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)|
    #     any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE))&
    #    sd(dat$Total.cms)>0&
    #    !stations$ID[it_stn]%in%c("08OA003","08OA004")){
    #   mylm<-update(mylm,~.+August.cms)
    #   shft = 1
    # }
    
    
    for(it_var in 1:nrow(vrs)){
      if(vrs$var_III[it_var]!="1"){
        if(max(dataYearly2Aug[,vrs$var_III[it_var]])>0.05){
          mylm_x<-update(mylm,paste0("~.+",vrs$var_I[it_var],"+",vrs$var_III[it_var]))
          vrs$var_I_coeff[it_var]<-mylm_x$coefficients[5+shft]
          vrs$var_I_p[it_var]<-summary(mylm_x)$coefficients[5+shft,4]
          vrs$var_III_coeff[it_var]<-mylm_x$coefficients[6+shft]
          vrs$var_III_p[it_var]<-summary(mylm_x)$coefficients[6+shft,4]
          vrs$bts_I[it_var] =lm.beta(mylm_x)$standardized.coefficients[5+shft]
          vrs$bts_III[it_var] =lm.beta(mylm_x)$standardized.coefficients[6+shft]
        }
      }else{
        mylm_x<-update(mylm,paste0("~.+",vrs$var_I[it_var]))
        vrs$var_I_coeff[it_var]<-mylm_x$coefficients[5+shft]
        vrs$var_I_p[it_var]<-summary(mylm_x)$coefficients[5+shft,4]
        vrs$bts_I[it_var]<-lm.beta(mylm_x)$standardized.coefficients[5+shft]
        vrs$var_III_coeff[it_var]<-NA
        vrs$var_III_p[it_var]<-NA
        vrs$bts_III[it_var] = NA
        
      }
      
      lm.beta(mylm_x)
      
      
    }
    
    varsI<-vrs%>%
      # filter()
      group_by(var_I,inclVarIII = var_III=="1")%>%
      summarize(mn = mean(var_I_coeff,na.rm = TRUE))%>%
      pivot_wider(values_from = c(mn),names_from = c(var_I))%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(varsI)   
    
    p.valsI<-vrs%>%
      group_by(var_I,inclVarIII = var_III=="1")%>%
      summarize(signif = mean(var_I_p<0.05,na.rm = TRUE))%>%
      pivot_wider(values_from = c(signif),names_from = c(var_I))%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(p.valsI)
    
    betasI<-vrs%>%
      group_by(var_I,inclVarIII = var_III=="1")%>%
      summarize(mn = mean(bts_I,na.rm = TRUE))%>%
      pivot_wider(values_from = c(mn),names_from = c(var_I))%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(betasI)
    
    
    
    
    
    
    varsIII<-vrs%>%
      group_by(var_III)%>%
      summarize(mn = mean(var_III_coeff))%>%
      pivot_wider(values_from = mn,names_from = var_III)%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(varsIII)
    
    p.valsIII<-vrs%>%
      group_by(var_III)%>%
      summarize(signif = mean(var_III_p<0.05,na.rm = TRUE))%>%
      pivot_wider(values_from = signif,names_from = var_III)%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(p.valsIII)
    
    betasIII<-vrs%>%
      group_by(var_III)%>%
      summarize(mn = mean(bts_III))%>%
      pivot_wider(values_from = mn,names_from = var_III)%>%
      mutate(ID = stations$ID[it_stn])%>%
      rbind(betasIII)
    
    
    
    
    
    
    
    
  }
}


varsI%>%
  summarize(across(ECA_I_1:ECA_I_17,~median(.x,na.rm = TRUE)))%>%
  pivot_longer(cols = ECA_I_1:ECA_I_17)%>%
  data.frame()


varsI.df<-varsI%>%pivot_longer(cols = ECA_I_1:ECA_I_17)
varsI.df$yr<-varsI.df$name%>%str_remove("ECA_I_")%>%as.numeric()

plotly::ggplotly(
  varsI.df%>%filter(substr(ID,1,3)=="08H")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ,linetype = inclVarIII))+geom_line()+
    theme_bw())

plotly::ggplotly(
  varsI.df%>%
    
    ggplot(aes(x = yr,y =value,col = ID ,linetype = inclVarIII))+geom_line()+
    theme_bw())

betasI.df<-betasI%>%pivot_longer(cols = ECA_I_1:ECA_I_17)
betasI.df$yr<-betasI.df$name%>%str_remove("ECA_I_")%>%as.numeric()

plotly::ggplotly(
  betasI.df%>%filter(substr(ID,1,3)=="08H")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ,linetype = inclVarIII))+geom_line()+
    theme_bw())


plotly::ggplotly(
  betasI.df%>%filter(substr(ID,1,1)=="0")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ,linetype = inclVarIII))+geom_line()+
    theme_bw())





varsIII.df<-varsIII%>%pivot_longer(cols = ECA_III_12:`1`)
varsIII.df$yr<-varsIII.df$name%>%str_remove("ECA_III_")%>%as.numeric()
plotly::ggplotly(
  varsIII.df%>%filter(substr(ID,1,3)=="08H")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ))+geom_line()+
    theme_bw())






betasIII.df<-betasIII%>%pivot_longer(cols = ECA_III_12:`1`)
betasIII.df$yr<-betasIII.df$name%>%str_remove("ECA_III_")%>%as.numeric()
plotly::ggplotly(
  betasIII.df%>%filter(substr(ID,1,3)=="08H")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ))+geom_line()+
    theme_bw())

plotly::ggplotly(
  betasIII.df%>%filter(substr(ID,1,1)=="0")%>%
    
    ggplot(aes(x = yr,y =value,col = ID ))+geom_line()+
    theme_bw())


unique(substr(betasI$ID,1,3))


betasI.df.2<-betasI.df%>%
  filter(inclVarIII)%>%
  group_by(ID)%>%
  summarize(mnBeta_I = mean(value))

watersheds<-left_join(watersheds,betasI.df.2, by = c("StationNum" = "ID"))
tmap_mode("view")
tm_shape(watersheds)+tm_polygons(col = "mnBeta")
sum(betasI.df.2$mnBeta_I>0
)

stnECA<-ECA%>%
  group_by(StationNum)%>%
  summarize(maxECA_I_9 = max(ECA_I_9),
            maxECA_III_24 = max(ECA_III_24))

x<-inner_join(betasI.df.2,stnECA,by = c("ID"="StationNum"))

plot(x$maxECA_I_9,x$mnBeta_I)


betasIII.df.2<-betasIII.df%>%
  # filter(inclVarIII)%>%
  group_by(ID)%>%
  summarize(mnBeta_III = mean(value,na.rm = TRUE))

x<-inner_join(betasIII.df.2,x)
x<-left_join(x,stations)




ggplot(x,aes(maxECA_I_9,mnBeta_I))+geom_point()+facet_wrap("regime")
ggplot(x,aes(maxECA_III_24,mnBeta_III))+geom_point()+facet_wrap("regime")

plot(x$maxECA9,x$mnBeta)




## analysis 3 ###############

stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(StationNum,Area_km2)%>%
                      st_drop_geometry(),
                    by = c("ID" = "StationNum"))

stnCoeffs<-data.frame()

for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08JE001")
  
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamData$BaseFlow_Eckhardt0.97<-NA
  streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  
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
                     
                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)],na.rm = TRUE)
                     
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
  
  # if(stations$minFlowMonth[it_stn]==8){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minAugFlow7
  # }else if(stations$minFlowMonth[it_stn]==9){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minSepFlow7
  # }else if(stations$minFlowMonth[it_stn]==10){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minOctFlow7
  # }
  
  ## add in ECA data
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  
  
  data<-WeatherData[WeatherData$StationNum == stations$ID[it_stn],]
  
  data$NovWaterYear<-data$Year
  data$NovWaterYear[data$Month%in%c(11,12)] <- data$Year[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(NovWaterYear)%>%
    dplyr::summarize(
      
      winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      summerPrecip = sum(Total.Precip..mm.[Month%in%c(5:stations$minFlowMonth[it_stn])],na.rm = TRUE),
      meanSummerTemp = mean(Mean.Temp..C.[Month%in%c(5:stations$minFlowMonth[it_stn])],na.rm = TRUE),
      
      Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
      Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),
      
      Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
      Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),
      
      Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      
      Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
      Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),
      
      Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      
      Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
      Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),
      
      
      
      
    )
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))
  
  
  
  # stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  dataYearly2$minSumFlow7.log<-log(dataYearly2$minSumFlow7)
  
  dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","maxSWEdly",
                      "BF_30day_Eckhardt0.97","ECA_I_9","ECA_III_24","Total.cms",
                      "August.cms","September.cms","October.cms")]%>%
    na.omit()%>%
    scale(center = TRUE, scale = TRUE)%>%
    data.frame()
  
  
  summary(mylm<-lm(minSumFlow7.log~summerPrecip+meanSummerTemp+
                     maxSWEdly+BF_30day_Eckhardt0.97-1,
                   data = dat,na.action= "na.exclude"))
  
  
  
  
  if(stations$PrivateForestry[it_stn]<=0.1&stations$Total.Cut.Area[it_stn]>0.1){
    mylm<-update(mylm,~.+ECA_I_9)
    if(any(!is.na(dat$ECA_III_24))){
      mylm<-update(mylm,~.+ECA_III_24)
    }
  }
  
  
  if((any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.1,na.rm = TRUE)|
      any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.1,na.rm = TRUE)|
      any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.1,na.rm = TRUE))&
     any(!is.na(dat$Total.cms))){
    mylm<-update(mylm,~.+Total.cms)
    
  }
  
  
  p.vals<-summary(mylm)$coefficients[,4]
  names(p.vals)<-paste0(names(p.vals),".p")
  d<-cbind(ID=stations$ID[it_stn],
           t(mylm$coefficients),
           t(p.vals))%>%
    data.frame()
  
  stnCoeffs<-plyr::rbind.fill(stnCoeffs,d)
  
  # availYears[[it_stn]]<-dat$NovWaterYear
  
  
  
}

stnCoeffs<-left_join(stnCoeffs,stations%>%select(ID,regime))

stnCoeffs$regime<-factor(stnCoeffs$regime,
                         levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                         labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


stnCoeffs.long<-stnCoeffs%>%
  select(!.p)%>%
  pivot_longer(,cols = summerPrecip:Total.cms.p)

stnCoeffs.long$p.val<-str_detect(stnCoeffs.long$name,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("Beta","p.val"))

stnCoeffs.long$name<-str_remove(stnCoeffs.long$name,"\\.p")
stnCoeffs.long<-pivot_wider(stnCoeffs.long,
                            names_from = "p.val",
                            values_from = value)

stnCoeffs.long$Beta<-as.numeric(stnCoeffs.long$Beta)
stnCoeffs.long$p.val<-as.numeric(stnCoeffs.long$p.val)
ggplot(stnCoeffs.long%>%subset(!is.na(Beta)&
                                 ID!="08OA004"),
       
       aes(y = regime,
           # x = value
           x = Beta
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
  scale_x_continuous(name = expression(Beta),
                     limits = c(-0.8,0.8))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "name", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.17,0.96),
        legend.background = element_rect(color = "black"),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())


ggsave("3.figures/Sens_boxplots_beta.png",width = 3.5, height = 7.5)

# Zhang and Wei 2012 analysis ###########
# 08KE016


library("tseries")
library(forecast)
stnRs<-data.frame()
stns_with_brks<-c()
sum(stations$PrivateForestry>0.1)
for(it_stn in 1:nrow(stations)){
  # it_stn = which(stations$ID=="08HA003")
  if(stations$PrivateForestry[it_stn]>0.1|stations$Total.Cut.Area[it_stn]<=0.1){next}
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  
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
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stations$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stations$ID[it_stn])
  }
  
  
  
  # fitECA60<-auto.arima(streamDataYrly$ECA_60,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # 
  # 
  # fitECA20<-auto.arima(streamDataYrly$ECA_20,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA10<-auto.arima(streamDataYrly$ECA_10,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA5<-auto.arima(streamDataYrly$ECA_5,trace = TRUE,
  #                     max.p = 1,max.d=1,max.q = 0,
  #                     allowdrift = FALSE,
  #                     stepwise=FALSE , approximation=FALSE)
  
  fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  fitECA20<-arima(streamDataYrly$ECA_20,c(1,1,0))
  fitECA10<-arima(streamDataYrly$ECA_10,c(1,1,0))
  fitECA5<-arima(streamDataYrly$ECA_5,c(1,1,0))
  
  
  
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  logQ7.60<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA60$coef,fitECA60$coef),sides = 1)
  logQ7.20<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA20$coef,fitECA20$coef),sides = 1)
  logQ7.10<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA10$coef,fitECA10$coef),sides = 1)
  logQ7.5<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA5$coef,fitECA5$coef),sides = 1)
  
  
  # ccf(fitECA60$residuals,logQ7.60,na.action=na.omit,lag.max=10 )
  # ccf(fitECA20$residuals,logQ7.20,na.action=na.omit)
  # ccf(fitECA10$residuals,logQ7.10,na.action=na.omit)
  # ccf(fitECA5$residuals,logQ7.5,na.action=na.omit)
  
  # 
  # acf(streamDataYrly$ECA_60)
  # pacf(streamDataYrly$ECA_60)
  # diff1x=diff(streamDataYrly$ECA_60,1)
  # acf(diff1x)
  # pacf(diff1x)
  # plot(logQ7.60)
  c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  c1.60<-cor.test(logQ7.60[2:length(logQ7.60)],
                  fitECA60$residuals[1:(length(logQ7.60)-1)],method = "spearman")
  c2.60<-cor.test(logQ7.60[3:length(logQ7.60)],
                  fitECA60$residuals[1:(length(logQ7.60)-2)],method = "spearman")
  
  c0.20<-cor.test(logQ7.20,fitECA20$residuals,method = "spearman")
  
  c1.20<-cor.test(logQ7.20[2:length(logQ7.20)],
                  fitECA20$residuals[1:(length(logQ7.20)-1)],method = "spearman")
  c2.20<-cor.test(logQ7.20[3:length(logQ7.20)],
                  fitECA20$residuals[1:(length(logQ7.20)-2)],method = "spearman")
  
  
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  
  
  c0.10<-cor.test(logQ7.10,fitECA10$residuals,method = "spearman")
  
  c1.10<-cor.test(logQ7.10[2:length(logQ7.10)],
                  fitECA10$residuals[1:(length(logQ7.10)-1)],method = "spearman")
  c2.10<-cor.test(logQ7.10[3:length(logQ7.10)],
                  fitECA10$residuals[1:(length(logQ7.10)-2)],method = "spearman")
  
  
  
  c0.5<-cor.test(logQ7.5,fitECA5$residuals,method = "spearman")
  
  c1.5<-cor.test(logQ7.5[2:length(logQ7.5)],
                 fitECA5$residuals[1:(length(logQ7.5)-1)],method = "spearman")
  c2.5<-cor.test(logQ7.5[3:length(logQ7.5)],
                 fitECA5$residuals[1:(length(logQ7.5)-2)],method = "spearman")
  
  
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-2)])
  # 
  # 
  # c1.20<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.20<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA$residuals)-2)])
  # 
  # c0.60<-cor.test(fitQ$residuals,fitECA60$residuals)
  # 
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-2)])
  stnRs<-rbind(stnRs,
               data.frame(
                 ID = stations$ID[it_stn],
                 regime = stations$regime[it_stn],
                 c0.60 =c0.60$estimate,
                 c0.60.p =c0.60$p.value,
                 c0.20 =c0.20$estimate,
                 c0.20.p =c0.20$p.value,
                 c0.10 =c0.10$estimate,
                 c0.10.p =c0.10$p.value,
                 c0.5 =c0.5$estimate,
                 c0.5.p =c0.5$p.value,
                 
                 
                 c1.60 =c1.60$estimate,
                 c1.60.p =c1.60$p.value,
                 c1.20 =c1.20$estimate,
                 c1.20.p =c1.20$p.value,
                 c1.10 =c1.10$estimate,
                 c1.10.p =c1.10$p.value,
                 c1.5 =c1.5$estimate,
                 c1.5.p =c1.5$p.value,
                 
                 
                 c2.60 =c2.60$estimate,
                 c2.60.p =c2.60$p.value,
                 c2.20 =c2.20$estimate,
                 c2.20.p =c2.20$p.value,
                 c2.10 =c2.10$estimate,
                 c2.10.p =c2.10$p.value,
                 c2.5 =c2.5$estimate,
                 c2.5.p =c2.5$p.value
                 
                 # c2.60 =c2$estimate,
                 # c2.60.p =c2$p.value
               ))
  
}

# stnRs$maxR<-
sum(stnRs$c0.60>0)

stnRs<-left_join(watersheds,stnRs,by = c( "StationNum" = "ID"))

stnRs<-left_join(stnRs,stations)
stnRs<-filter(stnRs,!is.na(c0.60))
tmap_mode("view")
tm_shape(stnRs%>%filter(!is.na(c0.60)))+tm_dots(col = "c0.60")

ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_hline(yintercept = 0)
ggplot(stnRs,aes(x = Area_km2,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_hline(yintercept = 0)+
  scale_x_log10()

ggplot(stnRs,aes(x = Total.Cut.Area,y = c1.60))+geom_point(aes(color = c0.60.p<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_hline(yintercept = 0)
ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.10))+geom_point(aes(color = c0.10.p<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_hline(yintercept = 0)
ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.5))+geom_point(aes(color = c0.5.p<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_hline(yintercept = 0)


stnRs_long<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c0.60:c0.5.p)%>%
  pivot_longer(cols = c0.60:c0.5.p)
stnRs_long$meas<-str_detect(stnRs_long$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long$name<-str_remove(stnRs_long$name,"\\.p")
stnRs_long<-pivot_wider(stnRs_long,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long<-filter(stnRs_long,!regime=="Glacial")
stnRs_long$regime<-factor(stnRs_long$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                          labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long$name<-factor(stnRs_long$name,levels = c("c0.60","c0.20","c0.10","c0.5"),
                        labels = c("ECA (60 year)",
                                   "ECA (20 year)",
                                   "ECA (10 year)",
                                   "ECA (5 year)"
                        )
)

ggplot(stnRs_long,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r",
                     limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut.png",width =7,height = 4)


stnRs_long%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       out = "4.output/ECA_analysis.doc")



stnRs_long_lag1<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c1.60:c1.5.p)%>%
  pivot_longer(cols = c1.60:c1.5.p)
stnRs_long_lag1$meas<-str_detect(stnRs_long_lag1$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag1$name<-str_remove(stnRs_long_lag1$name,"\\.p")
stnRs_long_lag1<-pivot_wider(stnRs_long_lag1,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long_lag1<-filter(stnRs_long_lag1,!regime=="Glacial")
stnRs_long_lag1$regime<-factor(stnRs_long_lag1$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                               labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long_lag1$name<-factor(stnRs_long_lag1$name,levels = c("c1.60","c1.20","c1.10","c1.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag1%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())

ggplot(stnRs_long_lag1,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r",
                     limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut_lag1.png",width =7,height = 4)



stnRs_long_lag2<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c2.60:c2.5.p)%>%
  pivot_longer(cols = c2.60:c2.5.p)
stnRs_long_lag2$meas<-str_detect(stnRs_long_lag2$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag2$name<-str_remove(stnRs_long_lag2$name,"\\.p")
stnRs_long_lag2<-pivot_wider(stnRs_long_lag2,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long_lag2<-filter(stnRs_long_lag2,!regime=="Glacial")
stnRs_long_lag2$regime<-factor(stnRs_long_lag2$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                               labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long_lag2$name<-factor(stnRs_long_lag2$name,levels = c("c2.60","c2.20","c2.10","c2.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag2%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())

ggplot(stnRs_long_lag2,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r",
                     limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut_lag2.png",width =7,height = 4)





stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)%>%
  mutate(
    fracSignif_0 = NA,
    # fracSignif_0.1 = NA,
    # fracSignif_0.25 = NA,
    # fracSignif_0.5 = NA,
    fracSignif_1 = NA,
    fracSignif_2 = NA,
    fracSignif_3 = NA,
    fracNeg_0 = NA,
    # fracNeg_0.1 = NA,
    # fracNeg_0.25 = NA,
    # fracNeg_0.5 = NA,
    fracNeg_1 = NA,
    fracNeg_2 = NA,
    fracNeg_3 = NA,
    
    
  )

for(it_stn in 1:nrow(stn_MC)){
  tictoc::tic(stn_MC$ID[it_stn])
  ECA_x<-filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stn_MC$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stn_MC$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  
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
                     
                     
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stn_MC$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stn_MC$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stn_MC$ID[it_stn])
  }
  
  
  
  # fitECA60<-auto.arima(streamDataYrly$ECA_60,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # 
  # 
  # fitECA20<-auto.arima(streamDataYrly$ECA_20,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA10<-auto.arima(streamDataYrly$ECA_10,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA5<-auto.arima(streamDataYrly$ECA_5,trace = TRUE,
  #                     max.p = 1,max.d=1,max.q = 0,
  #                     allowdrift = FALSE,
  #                     stepwise=FALSE , approximation=FALSE)
  
  # fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  # fitECA60<-sarima(streamDataYrly$ECA_60,1,1,0)
  # fitECA20<-arima(streamDataYrly$ECA_20,c(1,1,0))
  # fitECA10<-arima(streamDataYrly$ECA_10,c(1,1,0))
  # fitECA5<-arima(streamDataYrly$ECA_5,c(1,1,0))
  fitECA10<-sarima(streamDataYrly$ECA_10,1,1,0,details = FALSE)
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  
  medianQ7<-median(streamDataYrly$minSumFlow7)
  sd.log.Q7<-sd(log(streamDataYrly$minSumFlow7))
  
  fctrs <- c(0, 0.1, 0.25,0.5,1)
  fctrs <- c(0,1,2,3)
  for(it_fctr in 1:4){
    p_vals<-c()
    neg<-c()
    for(it_MC in 1:1000){
      set.seed(it_MC)
      Q7_i.log<-log(streamDataYrly$minSumFlow7[sample(1:nrow(streamDataYrly),nrow(streamDataYrly),replace = FALSE)])
      Q7_i.log_shift<-Q7_i.log - (streamDataYrly$ECA_10*fctrs[it_fctr])*sd.log.Q7
      logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(resid(fitECA10$fit), logQ7.60,method = "spearman")
      p_vals[it_MC]<-c0.60$p.value
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i))
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i_shift))
      # plot(streamDataYrly$ECA_5,log(Q7_i_shift))
      
      neg[it_MC]<-c0.60$estimate<0
      # ccf(resid(fitECA5$fit), logQ7.60,na.action = na.omit)
      
      
    }
    stn_MC[it_stn,paste0("fracNeg_",fctrs[it_fctr])] <- sum(neg)/1000
    stn_MC[it_stn,paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05)/1000
  }
  
  tictoc::toc()
}


stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)%>%
  mutate(
    fracSignif_0 = NA,
    # fracSignif_0.1 = NA,
    # fracSignif_0.25 = NA,
    # fracSignif_0.5 = NA,
    fracSignif_1 = NA,
    # fracSignif_2 = NA,
    # fracSignif_3 = NA,
    fracSignif_5 = NA,
    fracSignif_10 = NA,
    fracSignif_100 = NA,
    fracNeg_0 = NA,
    # fracNeg_0.1 = NA,
    # fracNeg_0.25 = NA,
    # fracNeg_0.5 = NA,
    fracNeg_1 = NA,
    # fracNeg_2 = NA,
    # fracNeg_3 = NA,
    fracNeg_5 = NA,
    fracNeg_10 = NA,
    fracNeg_100 = NA,
    
    
  )

for(it_stn in 1:nrow(stn_MC)){
  tictoc::tic(stn_MC$ID[it_stn])
  ECA_x<-filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stn_MC$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stn_MC$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  
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
                     
                     
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stn_MC$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stn_MC$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stn_MC$ID[it_stn])
  }
  
  
  
  # fitECA60<-auto.arima(streamDataYrly$ECA_60,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # 
  # 
  # fitECA20<-auto.arima(streamDataYrly$ECA_20,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA10<-auto.arima(streamDataYrly$ECA_10,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA5<-auto.arima(streamDataYrly$ECA_5,trace = TRUE,
  #                     max.p = 1,max.d=1,max.q = 0,
  #                     allowdrift = FALSE,
  #                     stepwise=FALSE , approximation=FALSE)
  
  # fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  # fitECA60<-sarima(streamDataYrly$ECA_60,1,1,0)
  # fitECA20<-arima(streamDataYrly$ECA_20,c(1,1,0))
  # fitECA10<-arima(streamDataYrly$ECA_10,c(1,1,0))
  # fitECA5<-arima(streamDataYrly$ECA_5,c(1,1,0))
  fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  
  # medianQ7<-median(streamDataYrly$minSumFlow7)
  sd.log.Q7<-sd(log(streamDataYrly$minSumFlow7))
  
  # fctrs <- c(0, 0.1, 0.25,0.5,1)
  fctrs <- c(0,1,5,10,100)
  for(it_fctr in 1:5){
    p_vals<-c()
    neg<-c()
    # tictoc::tic()
    for(it_MC in 1:100){
      set.seed(it_MC)
      Q7_i.log<-log(streamDataYrly$minSumFlow7[sample(1:nrow(streamDataYrly),nrow(streamDataYrly),replace = FALSE)])
      Q7_i.log_shift<-Q7_i.log - (streamDataYrly$ECA_60*fctrs[it_fctr]*sd.log.Q7)
      fitQ7<- arima(Q7_i.log_shift,c(1,1,0))
      logQ7.60 <-residuals(fitQ7)
      # logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(residuals(fitECA60), logQ7.60,method = "spearman")
      c0.60<-cor.test(streamDataYrly$ECA_60, Q7_i.log_shift,method = "spearman")
      # c0.60
      # plot(as.numeric(streamDataYrly$ECA_60),Q7_i.log_shift)
      # cor.test(streamDataYrly$ECA_60, logQ7.60,method = "spearman")
      p_vals[it_MC]<-c0.60$p.value
      
      # plot(Q7_i.log_shift,logQ7.60)
      # 
      # 
      # plot(as.numeric(residuals(fitECA60)),Q7_i.log)
      # plot(as.numeric(residuals(fitECA60)),Q7_i.log_shift)
      # plot(as.numeric(streamDataYrly$ECA_60),Q7_i.log_shift)
      # plot(as.numeric(streamDataYrly$ECA_60),((streamDataYrly$ECA_60*fctrs[it_fctr])*sd.log.Q7))
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i_shift))
      # plot(streamDataYrly$ECA_5,log(Q7_i_shift))
      
      neg[it_MC]<-c0.60$estimate<0
      # ccf(resid(fitECA5$fit), logQ7.60,na.action = na.omit)
      
      
    }
    # tictoc::toc()
    (stn_MC[it_stn,paste0("fracNeg_",fctrs[it_fctr])] <- sum(neg)/100)
    (stn_MC[it_stn,paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05)/100)
  }
  
  tictoc::toc()
}


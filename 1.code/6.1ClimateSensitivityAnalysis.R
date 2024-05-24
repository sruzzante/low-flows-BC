# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10


# This script performs the main driver/sensitivity analysis presented in the manuscript
# First it loads all the relevant, prepared data
# Then it loops through the stations, fitting the 'explanatory' regressions to each


closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(lubridate)
library(sf)
library(terra)
library(stringr)
library(scico)
library(ggplot2)
library(lm.beta)
library(tidyr)
library(Hmisc) 
library(grwat)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")
# streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly_2.RDS")

# Load Station metadata
# stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")
stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

# Load monthly weather data
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")

# Load daily weather data
WeatherDataDaily<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataDaily.RDS")


streamDataAll<-left_join(streamDataAll,WeatherDataDaily)

#Load monthly SWE data
data_SWE<-readRDS("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment.RDS")
#Load daily SWE data
data_SWE_dly<-readRDS("2.data/2.working/ERA5_LAND_SWE/SWE_data_by_catchment_dly.RDS")


names(data_SWE_dly)<-c("ID","Date","SWE")


data_SWE$date <- data_SWE$variable%>%ymd()

data_SWE$Year<-year(data_SWE$date)
data_SWE$Month<-month(data_SWE$date)


# find the median month of maximum snow accumulation for each station
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



streamDataAll<-dplyr::left_join(streamDataAll,data_SWE_dly,by = c("ID","Date"))
streamDataAll$NovWaterYear<-streamDataAll$year
streamDataAll$NovWaterYear[streamDataAll$month%in%c(11,12)]<-
  streamDataAll$year[streamDataAll$month%in%c(11,12)]+1

### Load ECA data  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")

## Loop through stations and extract variables ################

watersheds<-st_transform(watersheds,st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
watersheds$Area_km2<-(st_area(watersheds)/10^6)%>%as.numeric()

stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(ID,Area_km2)%>%
                      st_drop_geometry(),
                    by = c("ID" ))

stnCoeffs<-data.frame()
stnRs<-data.frame()

BF.altCoeffs_all<-data.frame()
SWE.altcoeffs_all<-data.frame()
stnCoeffs.weather<-data.frame()

VIF_all<-data.frame()
CORS_T_summer<-data.frame()
CORS_P_summer<-data.frame()
CORS_maxSWEdly<-data.frame()


stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)


# lowQ.10_highSWE.90.years<-list()
lowQ.10_highSWE.90<-data.frame()
stations$tempDriven.5<-NA
stations$tempDriven.1<-NA

stns_T2<-stations[,c("ID","regime")]
stns_T2$beta_T2<-NA
stns_T2$p.val<-NA
stations$meanSummerTemp<-NA
stations$covMinSumFlow7<-NA

my_lms.pre<-list()
my_lms.post<-list()

for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08HD023")
  
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(stations$month_bgn[it_stn]:stations$month_end[it_stn])]<-NA
  
  
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
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)
  
  
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
  
  
  
  ## Add in SWE Data
  snowData<-data_SWE[data_SWE$ID==stations$ID[it_stn],]%>%
    group_by(WaterYear)%>%
    dplyr::summarize(SWE = mean(value),
                     maxMonth = maxMonth[1])
  
  fixed_BF_month<-snowData$maxMonth[1]%>%
    pmax(3)%>%
    pmin(5)
  
  
  streamData<-left_join(streamData,stations[,c("ID","month_bgn","month_end")])
  streamDataYrly<-streamData%>%
    
    dplyr::group_by(NovWaterYear)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     
                     numJunQnonNans = sum(!is.na(Discharge[month ==6])),
                     numJulQnonNans = sum(!is.na(Discharge[month ==7])),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     
                     minJunFlow7 = min(Discharge7[month==6],na.rm = TRUE),
                     minJunFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,6),
                     minJulFlow7 = min(Discharge7[month==7],na.rm = TRUE),
                     minJulFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,7),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minAugFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,8),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minSepFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,9),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     minOctFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,10),
                     
                     minSumFlow7 = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     # minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     lengthSum = sum(month%in%c(month_bgn:month_end)),
                     
                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     SumT7=T7[DayOfYear=minSumFlowDay],
                     
                     BF_30day_lynehollick = mean(BaseFlow_lynehollick[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_boughton = mean(BaseFlow_boughton[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_jakeman = mean(BaseFlow_jakeman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_maxwell = mean(BaseFlow_maxwell[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_chapman = mean(BaseFlow_chapman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     
                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     BF_30day_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     
                     
                     winterBF_lynehollick = mean(BaseFlow_lynehollick[month==fixed_BF_month]),
                     winterBF_boughton = mean(BaseFlow_boughton[month==fixed_BF_month]),
                     winterBF_jakeman = mean(BaseFlow_jakeman[month==fixed_BF_month]),
                     winterBF_maxwell = mean(BaseFlow_maxwell[month==fixed_BF_month]),
                     winterBF_chapman = mean(BaseFlow_chapman[month==fixed_BF_month]),
                     winterBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==fixed_BF_month]),
                     winterBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==fixed_BF_month])
                     
                     
    )
  
  
  streamDataYrly$minJunFlow7[streamDataYrly$numJunQnonNans<26]<-NA
  streamDataYrly$minJulFlow7[streamDataYrly$numJulQnonNans<26]<-NA
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minJulFlow7[is.infinite(streamDataYrly$minJulFlow7)]<-NA
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[(streamDataYrly$lengthSum-streamDataYrly$numQDaysSum)>5]<-NA
  
  # if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
  #   streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minJulFlow7,streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  # }
  
  
  
  
  streamDataYrly<-left_join(streamDataYrly,snowData,by = c("NovWaterYear" = "WaterYear"))
  
  
  # stations$minFlowMonth[it_stn]<-getMode(streamDataYrly$minSumFlowMonth)
  
  # if(stations$minFlowMonth[it_stn]==8){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minAugFlow7
  # }else if(stations$minFlowMonth[it_stn]==9){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minSepFlow7
  # }else if(stations$minFlowMonth[it_stn]==10){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minOctFlow7
  # }
  
  ## add in ECA data
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  
  
  data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
  
  data$NovWaterYear<-data$Year
  data$NovWaterYear[data$Month%in%c(11,12)] <- data$Year[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(NovWaterYear)%>%
    dplyr::summarize(
      
      winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      summerPrecip = sum(Total.Precip..mm.[Month%in%c(5:stations$minSumFlowMonth[it_stn])],na.rm = TRUE),
      meanSummerTemp = mean(Mean.Temp..C.[Month%in%c(5:stations$minSumFlowMonth[it_stn])],na.rm = TRUE),
      
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
  rownames(dataYearly2)<-dataYearly2$NovWaterYear
  
  dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                      "BF_30day_Eckhardt0.97","ECA_I_9","ECA_III_24","Total.cms",
                      "August.cms","September.cms","October.cms","NovWaterYear")]%>%
    na.omit()
  dat<-cbind(dat%>%
               scale(scale = TRUE,center = TRUE)%>%
               data.frame()%>%
               select(!NovWaterYear),
             dat[,"NovWaterYear"])



  
  if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
    dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                        "ECA_I_9","ECA_III_24","Total.cms",
                        "August.cms","September.cms","October.cms","NovWaterYear")]%>%
      na.omit()
    dat<-cbind(dat%>%
                 scale(scale = TRUE,center = TRUE)%>%
                 data.frame()%>%
                 select(!NovWaterYear),
               dat[,"NovWaterYear"])
    dat$BF_30day_Eckhardt0.97<-0}
  
  
  tryWaterUse = FALSE
  if(any(dataYearly2$Total.cms>dataYearly2$minSumFlow7*0.1,na.rm = TRUE)&
     any(!is.na(dat$Total.cms))){
    tryWaterUse = TRUE
  } 
  
  summary(mylm<-lm(minSumFlow7.log~summerPrecip+meanSummerTemp+SumT7+
                     maxSWEdly+BF_30day_Eckhardt0.97-1,
                   data = dat,na.action= "na.exclude"))
  
  
  
  # summary(dataYearly2$maxSWEdly)
  # ggplot(dataYearly2,aes(x = NovWaterYear,y = maxSWEdly))+geom_point()
  # ggplot(dataYearly2,aes(x = maxSWEdly,y = minSumFlow7.log))+geom_point()
  
  
  if(stations$PrivateForestry[it_stn]<=0.1&stations$Total.Cut.Area[it_stn]>0.1&
     any(!is.na(dat$ECA_I_9))){
    mylm<-update(mylm,~.+ECA_I_9)
    if(any(!is.na(dat$ECA_III_24))){
      mylm<-update(mylm,~.+ECA_III_24)
    }
  }
  
  
  if(tryWaterUse){
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
  
  dat2<-mylm$model
  d2<-dat2%>%
    as.matrix()%>%
    rcorr(type = "pearson")
  
  
  d2.r<-d2$r[1,]%>%
    t()%>%
    data.frame()%>%
    dplyr::mutate(ID = stations$ID[it_stn])
  
  d2.p<-d2$P[1,]%>%
    t()%>%
    data.frame()
  names(d2.p)<-paste0(names(d2.p),".p")
  
  stnRs<-plyr::rbind.fill(stnRs,cbind(d2.r,d2.p))
  
  ## correlation matrix
  if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
    VIF_all<-plyr::rbind.fill(VIF_all,
                              data.frame(
                                ID = stations$ID[it_stn],
                                t(car::vif(update(mylm,~.-BF_30day_Eckhardt0.97)))
                              ))
    
  }else{
    VIF_all<-plyr::rbind.fill(VIF_all,
                              data.frame(
                                ID = stations$ID[it_stn],
                                t(car::vif(mylm))
                              ))
    
  }
  
  CORS_T_summer<-plyr::rbind.fill(CORS_T_summer,
                                  data.frame(ID = stations$ID[it_stn],
                                             t(cor(mylm$model)["meanSummerTemp",])))
  
  CORS_P_summer<-plyr::rbind.fill(CORS_P_summer,
                                  data.frame(ID = stations$ID[it_stn],
                                             t(cor(mylm$model)["summerPrecip",])))
  
  CORS_maxSWEdly<-plyr::rbind.fill(CORS_maxSWEdly,
                                   data.frame(ID = stations$ID[it_stn],
                                              t(cor(mylm$model)["maxSWEdly",])))
  
  ## Test squared temperature term
  
  dat$meanSummerTemp2<-scale(dat$meanSummerTemp^2)
  mylmupdateT2<-update(mylm,~.+ meanSummerTemp2  )
  
  stns_T2$beta_T2[it_stn]<-mylmupdateT2$coefficients["meanSummerTemp2"]
  stns_T2$p.val[it_stn]<-summary(mylmupdateT2)$coefficients["meanSummerTemp2",4]
  
  
  stations$meanSummerTemp[it_stn]<-mean(dataYearly2$meanSummerTemp,na.rm = TRUE)
  
  
  mylm.pre1996<-NULL
  mylm.post1996<-NULL
  
  if(sum(dat$NovWaterYear<=1996)>=20&sum(dat$NovWaterYear>1996)>=20){
    mylm.pre1996<-update(mylm,data = dat%>%filter(NovWaterYear<=1996))
    mylm.post1996<-update(mylm,data = dat%>%filter(NovWaterYear>1996))
  }
  
  my_lms.pre[[it_stn]]<-mylm.pre1996
  my_lms.post[[it_stn]]<- mylm.post1996
  
  ## alternatives for BF filtering
  
  dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                      "ECA_I_9","ECA_III_24","Total.cms","winterPrecip","meanWinterTemp",
                      "SWE",
                      
                      "BF_30day_lynehollick"  ,
                      "BF_30day_boughton","BF_30day_jakeman"   ,    
                      "BF_30day_maxwell","BF_30day_chapman",
                      "BF_30day_Eckhardt0.97","BF_30day_Eckhardt0.995" ,
                      
                      "winterBF_lynehollick","winterBF_boughton",     
                      "winterBF_jakeman","winterBF_maxwell"   ,  "winterBF_chapman"   ,    
                      "winterBF_Eckhardt0.995" ,"winterBF_Eckhardt0.97" 
  )]%>%
    # na.omit()%>%
    scale(center = TRUE, scale = TRUE)%>%
    data.frame()
  
  if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
    dat[,c("BF_30day_lynehollick"  ,
           "BF_30day_boughton","BF_30day_jakeman"   ,    
           "BF_30day_maxwell","BF_30day_chapman",
           "BF_30day_Eckhardt0.97","BF_30day_Eckhardt0.995" ,
           
           "winterBF_lynehollick","winterBF_boughton",     
           "winterBF_jakeman","winterBF_maxwell","winterBF_chapman",     
           "winterBF_Eckhardt0.995" ,"winterBF_Eckhardt0.97" )]<-0
  }
  
  if(all(is.na(dat$Total.cms))){dat<-select(dat,!Total.cms)}
  if(all(is.na(dat$ECA_I_9))){dat<-select(dat,!ECA_I_9)}
  if(all(is.na(dat$ECA_III_24))){dat<-select(dat,!ECA_III_24)}
  
  dat<-na.omit(dat)
  summary(mylm)
  
  alt_BF_vars<-c("BF_30day_lynehollick"  ,
                 "BF_30day_boughton","BF_30day_jakeman"   ,    
                 "BF_30day_maxwell","BF_30day_chapman",
                 "BF_30day_Eckhardt0.97","BF_30day_Eckhardt0.995" ,
                 
                 "winterBF_lynehollick","winterBF_boughton",     
                 "winterBF_jakeman","winterBF_maxwell","winterBF_chapman"   ,    
                 "winterBF_Eckhardt0.995" ,"winterBF_Eckhardt0.97")
  altCoeffs<-c()
  altCoeffs.p<-c()
  for(it_var in 1:14){
    if (stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){next}
    if(it_var<=7){mylmupdate<-update(mylm,
                                     data = dat)}
    if(it_var>=8){mylmupdate<-update(mylm,~.+ SWE - maxSWEdly,
                                     data = dat)}
    
    mylmupdate<-update(mylmupdate,
                       ~.-BF_30day_Eckhardt0.97)
    mylmupdate<-update(mylmupdate,
                       paste("~.+",alt_BF_vars[it_var]))
    
    altCoeffs<-c(altCoeffs,mylmupdate$coefficients[alt_BF_vars[it_var]])
    
    
    altCoeffs.p<-c(altCoeffs.p,
                   summary(mylmupdate)$coefficients[alt_BF_vars[it_var],4])
    names(altCoeffs.p)<-paste(names(altCoeffs),".p")
  }
  
  
  
  BF.altCoeffs_all<-plyr::rbind.fill(BF.altCoeffs_all,
                                     data.frame(t(c(ID = stations$ID[it_stn],altCoeffs,altCoeffs.p))))
  
  ## alternatives for SWE
  mylmupdate.SWE<-update(mylm,~.+ SWE - maxSWEdly,
                         data = dat)
  SWE.altcoeffs_all<-rbind(SWE.altcoeffs_all,
                           data.frame(
                             ID = stations$ID[it_stn],
                             maxSWEdly = mylm$coefficients["maxSWEdly"],
                             maxSWEdly.p = summary(mylm)$coefficients["maxSWEdly",4],
                             SWE = mylmupdate.SWE$coefficients["SWE"],
                             SWE.p = summary(mylmupdate.SWE)$coefficients["SWE",4]
                           ))
  
  ## use T_winter and P_winter instead of SWE and baseflow
  
  mylmupdate.weather<-update(mylm,~. - maxSWEdly -BF_30day_Eckhardt0.97+meanWinterTemp+winterPrecip  ,
                             data = dat)
  
  p.vals<-summary(mylmupdate.weather)$coefficients[,4]
  names(p.vals)<-paste0(names(p.vals),".p")
  d<-cbind(ID=stations$ID[it_stn],
           t(mylmupdate.weather$coefficients),
           t(p.vals))%>%
    data.frame()
  
  stnCoeffs.weather<-plyr::rbind.fill(stnCoeffs.weather,d)
  
  
  dat2<-dataYearly2%>%filter(!is.na(maxSWEdly)&!is.na(minSumFlow7.log))
  stations$lowQ.10_highSWE[it_stn]<-sum((dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))[dat2$maxSWEdly>median(dat2$maxSWEdly)])/
    sum(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))
  
  stations$lowQ.10_highSWE.90[it_stn]<-sum((dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))[dat2$maxSWEdly>=quantile(dat2$maxSWEdly,0.9)])/
    sum(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))
  
  lowQ.10_highSWE.90.years[[it_stn]]<-dat2$NovWaterYear[(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))&
                                                           dat2$maxSWEdly>=quantile(dat2$maxSWEdly,0.9)]
  msk<-(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1))&
    dat2$maxSWEdly>=quantile(dat2$maxSWEdly,0.9)
  if(sum(msk)>0){
    lowQ.10_highSWE.90<-lowQ.10_highSWE.90%>%
      rbind(data.frame(ID=stations$ID[it_stn],
                       yr = dat2$NovWaterYear[msk],
                       Psum_pctl = cume_dist(dat2$summerPrecip)[msk],
                       Tsum_pctl = cume_dist(dat2$meanSummerTemp)[msk]))
    
  }
  
  
  stations$tempDriven.5[it_stn]<-
    sum(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1)&
          dat2$meanSummerTemp<quantile(dat2$meanSummerTemp,0.5)&
          dat2$summerPrecip>quantile(dat2$summerPrecip,0.5)&
          dat2$maxSWEdly>quantile(dat2$maxSWEdly,0.5))/
    sum(dat2$meanSummerTemp<quantile(dat2$meanSummerTemp,0.5)&
          dat2$summerPrecip>quantile(dat2$summerPrecip,0.5)&
          dat2$maxSWEdly>quantile(dat2$maxSWEdly,0.5))
  
  
  stations$tempDriven.1[it_stn]<-
    sum(dat2$minSumFlow7.log<=quantile(dat2$minSumFlow7.log,0.1)&
          dat2$meanSummerTemp<quantile(dat2$meanSummerTemp,0.1)&
          dat2$summerPrecip>quantile(dat2$summerPrecip,0.5)&
          dat2$maxSWEdly>quantile(dat2$maxSWEdly,0.5))/
    sum(dat2$meanSummerTemp<quantile(dat2$meanSummerTemp,0.1)&
          dat2$summerPrecip>quantile(dat2$summerPrecip,0.5)&
          dat2$maxSWEdly>quantile(dat2$maxSWEdly,0.5))
  
  
  stations$covMinSumFlow7[it_stn]<-sd(streamDataYrly$minSumFlow7,na.rm = TRUE)/
    mean(streamDataYrly$minSumFlow7,na.rm = TRUE)
  

  
  
  
}

stnCoeffs<-left_join(stnCoeffs,stations%>%select(ID,regime))

stnCoeffs$regime<-factor(stnCoeffs$regime,
                         levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                         labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


stnCoeffs.long<-stnCoeffs%>%
  # select(!.p)%>%
  pivot_longer(cols = summerPrecip:Total.cms.p)

stnCoeffs.long$p.val<-str_detect(stnCoeffs.long$name,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("Beta","p.val"))

stnCoeffs.long$name<-str_remove(stnCoeffs.long$name,"\\.p")
stnCoeffs.long<-pivot_wider(stnCoeffs.long,
                            names_from = "p.val",
                            values_from = value)

stnCoeffs.long$Beta<-as.numeric(stnCoeffs.long$Beta)
stnCoeffs.long$p.val<-as.numeric(stnCoeffs.long$p.val)

stnCoeffs.long$variable<-factor(stnCoeffs.long$name,
                                levels = c("maxSWEdly","BF_30day_Eckhardt0.97","summerPrecip","meanSummerTemp","SumT7","Total.cms","ECA_I_9","ECA_III_24"),
                                labels = c("SWE[max]","BF[winter]","P[summer]","T[summer]","T[7]","Abstraction","ECA[I]", "ECA[III]"))

N_vals<-stnCoeffs.long%>%
  filter(!is.na(Beta))%>%
  group_by(regime, variable)%>%
  dplyr::summarize(N=n())

ggplot(stnCoeffs.long%>%
         subset(!is.na(Beta))%>%
         mutate(Beta = pmax(pmin(Beta,0.8),-0.8)),
       
       aes(y = regime,
           # x = value
           x = Beta
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  geom_text(data = N_vals, aes(x = 0.9,y = regime,label = N),size = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(italic(beta)),
                     # oob = oob_squish,
                     # limits = c(-.8,.8),
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "variable", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.15,0.93),
        legend.background = element_rect(color = "black",fill = alpha('white',0.5)),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.minor.y = element_blank()
  )


ggsave("3.figures/Sens_boxplots_beta.png",width = 4, height = 7.5)

stnCoeffs.long%>%
  group_by(variable,regime)%>%
  dplyr::summarize(median(Beta,na.rm = TRUE),
                   mean(Beta,na.rm = TRUE),
                   sum(p.val<0.05,na.rm = TRUE)/sum(!is.na(p.val)),
                   sum(p.val<0.05&Beta<0,na.rm = TRUE)/sum(!is.na(p.val)))%>%
  print(n = 100)

stnCoeffs.long%>%
  group_by(variable)%>%
  dplyr::summarize(median(Beta,na.rm = TRUE),
                   mean(Beta,na.rm = TRUE),
                   sum(p.val<0.05,na.rm = TRUE)/sum(!is.na(p.val)),
                   sum(p.val<0.05&Beta>0,na.rm = TRUE)/sum(!is.na(p.val)),
                   sum(p.val<0.05&Beta<0,na.rm = TRUE)/sum(!is.na(p.val)))%>%
  print(n = 100)



stnRs<-left_join(stnRs,stations%>%select(ID,regime))

stnRs$regime<-factor(stnRs$regime,
                     levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                     labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


stnRs.long<-stnRs%>%
  # select(!.p)%>%
  pivot_longer(cols = summerPrecip:Total.cms.p&!ID&!minSumFlow7.log.p
  )

stnRs.long$p.val<-str_detect(stnRs.long$name,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("r","p.val"))

stnRs.long$name<-str_remove(stnRs.long$name,"\\.p")
stnRs.long<-pivot_wider(stnRs.long,
                        names_from = "p.val",
                        values_from = value)

stnRs.long$r<-as.numeric(stnRs.long$r)
stnRs.long$p.val<-as.numeric(stnRs.long$p.val)

stnRs.long$variable<-factor(stnRs.long$name,
                            levels = c("maxSWEdly","BF_30day_Eckhardt0.97","summerPrecip","meanSummerTemp","SumT7","Total.cms","ECA_I_9","ECA_III_24"),
                            labels = c("SWE[max]","BF[winter]","P[summer]","T[summer]","T[7]","Abstraction","ECA[I]", "ECA[III]"))

N_vals<-stnRs.long%>%
  filter(!is.na(r))%>%
  group_by(regime, variable)%>%
  dplyr::summarize(N=n())

ggplot(stnRs.long%>%
         subset(!is.na(r))%>%
         mutate(r = pmax(pmin(r,0.8),-0.8)),
       
       aes(y = regime,
           # x = value
           x = r
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  geom_text(data = N_vals, aes(x = 0.9,y = regime,label = N),size = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = "Pearson r",
                     # limits = c(-0.8,1),
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "variable", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.15,0.93),
        legend.background = element_rect(color = "black",fill = alpha('white',0.5)),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank())


ggsave("3.figures/Sens_boxplots_r.png",width = 4, height = 7.5)

stnRs.long%>%
  group_by(variable,regime)%>%
  dplyr::summarize(median(r,na.rm = TRUE),
                   mean(r,na.rm = TRUE),
                   sum(p.val<0.05,na.rm = TRUE)/sum(!is.na(p.val)),
                   sum(p.val<0.05&r>0,na.rm = TRUE)/sum(!is.na(p.val)))%>%
  print(n = 100)

stnRs.long%>%
  group_by(variable)%>%
  dplyr::summarize(median(r,na.rm = TRUE),
                   mean(r,na.rm = TRUE),
                   sum(p.val<0.05,na.rm = TRUE)/sum(!is.na(p.val)),
                   sum(p.val<0.05&r>0,na.rm = TRUE)/sum(!is.na(p.val)))%>%
  print(n = 100)


## BF alternatives ########
BF.altCoeffs_all.long<-BF.altCoeffs_all%>%
  pivot_longer(cols = c(BF_30day_lynehollick:winterBF_Eckhardt0.97..p))%>%
  mutate(meas = str_detect(name,"\\.\\.p")%>%
           plyr::mapvalues(from = c(FALSE,TRUE),to = c("coef","p.val")),
         name = str_remove(name,"\\.\\.p"))%>%
  pivot_wider(id_cols = c(ID,name),
              names_from = meas)%>%
  mutate(coef = as.numeric(coef),
         p.val = as.numeric(p.val))

BF.altCoeffs_all.long<-left_join(BF.altCoeffs_all.long,stations%>%select(ID,regime))

BF.altCoeffs_all.long$algorithm <-
  str_remove(BF.altCoeffs_all.long$name,"winterBF_|BF_30day_")%>%
  factor(levels = c("Eckhardt0.97","Eckhardt0.995","boughton","chapman","jakeman","lynehollick","maxwell"),
         labels = c("Eckhardt",'"Eckhardt, a\\u003D0.995"',"Boughton","Chapman","Jakeman","Lyne-Hollick","Maxwell"))

BF.altCoeffs_all.long$period<-str_split_fixed(BF.altCoeffs_all.long$name,"_",3)[,1]%>%
  factor(levels = c("BF","winterBF"),
         labels = c("30-day~mean~before~SWE[max]","Fixed~Month"))

BF.altCoeffs_all.long$regime<-factor(BF.altCoeffs_all.long$regime,
                                     levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                                     labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


ggplot(BF.altCoeffs_all.long%>%
         filter(!is.na(coef))%>%
         mutate(coef = pmax(pmin(coef,0.8),-0.8))
       ,aes(y = regime,
            # x = value
            x = coef
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(beta),
                     # limits = c(-0.8,0.8) ,
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
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

ggsave("3.figures/Sens_boxplots_BF_alternatives.png",width = 7, height = 10)

## SWE alternatives ########
SWE.altcoeffs_all.long<-SWE.altcoeffs_all%>%
  pivot_longer(cols = c(maxSWEdly:SWE.p))%>%
  mutate(meas = str_detect(name,"\\.p")%>%
           plyr::mapvalues(from = c(FALSE,TRUE),to = c("coef","p.val")),
         name = str_remove(name,"\\.p"))%>%
  pivot_wider(id_cols = c(ID,name),
              names_from = meas)%>%
  mutate(coef = as.numeric(coef),
         p.val = as.numeric(p.val))

SWE.altcoeffs_all.long<-left_join(SWE.altcoeffs_all.long,stations%>%select(ID,regime))


SWE.altcoeffs_all.long$period<-SWE.altcoeffs_all.long$name%>%
  factor(levels = c("maxSWEdly","SWE"),
         labels = c("SWE[max]","SWE[fixed~month]"))

SWE.altcoeffs_all.long$regime<-factor(SWE.altcoeffs_all.long$regime,
                                      levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


ggplot(SWE.altcoeffs_all.long%>%
         filter(!is.na(coef))%>%
         mutate(coef = pmax(pmin(coef,0.8),-0.8))
       ,aes(y = regime,
            # x = value
            x = coef
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(beta),
                     # limits = c(-0.8,0.8) ,
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_grid(rows = vars(period) ,
             # strip.position = "left",
             switch = "y",
             labeller = label_parsed
  )+
  theme_bw()+
  theme(
    legend.position = "bottom",
    legend.background = element_rect(color = "black"),
    legend.margin = margin(t=-3,r=1,b=-2,l=0,unit = "pt"),
    legend.key = element_blank(),
    legend.justification = "right",
    # strip.placement = "left"
    #     legend.title = element_blank()
  )

ggsave("3.figures/Sens_boxplots_SWE_alternatives.png",width = 3.5,height = 4)

## Using winter T and P


stnCoeffs.weather<-left_join(stnCoeffs.weather,stations%>%select(ID,regime))

stnCoeffs.weather$regime<-factor(stnCoeffs.weather$regime,
                                 levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                                 labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())


stnCoeffs.weather.long<-stnCoeffs.weather%>%
  # select(!.p)%>%
  pivot_longer(cols = summerPrecip:winterPrecip.p)

stnCoeffs.weather.long$meas<-str_detect(stnCoeffs.weather.long$name,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("Beta","p.val"))

stnCoeffs.weather.long$name<-str_remove(stnCoeffs.weather.long$name,"\\.p")
stnCoeffs.weather.long<-pivot_wider(stnCoeffs.weather.long,
                                    names_from = "meas",
                                    values_from = value)

stnCoeffs.weather.long$Beta<-as.numeric(stnCoeffs.weather.long$Beta)
stnCoeffs.weather.long$p.val<-as.numeric(stnCoeffs.weather.long$p.val)

stnCoeffs.weather.long$variable<-factor(stnCoeffs.weather.long$name,
                                        levels = c("winterPrecip","meanWinterTemp","summerPrecip","meanSummerTemp","SumT7","Total.cms","ECA_I_9","ECA_III_24"),
                                        labels = c("P[winter]","T[winter]","P[summer]","T[summer]","T[7]","Abstraction","ECA[I]", "ECA[III]"))

N_vals<-stnCoeffs.weather.long%>%
  filter(!is.na(Beta))%>%
  group_by(regime, variable)%>%
  dplyr::summarize(N=n())

ggplot(stnCoeffs.weather.long%>%
         subset(!is.na(Beta))%>%
         mutate(Beta = pmax(pmin(Beta,0.8),-0.8)),
       
       aes(y = regime,
           # x = value
           x = Beta
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  geom_text(data = N_vals, aes(x = 0.9,y = regime,label = N),size = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(italic(beta)),
                     # oob = oob_squish,
                     # limits = c(-.8,.8),
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_wrap(facets = "variable", ncol = 1,strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(legend.position = c(0.15,0.93),
        legend.background = element_rect(color = "black",fill = alpha('white',0.5)),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.minor.y = element_blank()
  )


ggsave("3.figures/Sens_boxplots_beta_PT.png",width = 4, height = 7.5)


## VIF analysis

VIF_all%>%
  reframe(across(summerPrecip:Total.cms,~quantile(.x,na.rm = TRUE)))

CORS_T_summer%>%
  reframe(across(summerPrecip:Total.cms,~quantile(abs(.x),na.rm = TRUE)))
CORS_P_summer%>%
  reframe(across(summerPrecip:Total.cms,~quantile(abs(.x),na.rm = TRUE)))

CORS_maxSWEdly%>%
  reframe(across(summerPrecip:Total.cms,~quantile(abs(.x),na.rm = TRUE)))

## High SWE low Q

stations%>%group_by(regime)%>%
  dplyr::summarize(mean(lowQ.10_highSWE),
                   mean(lowQ.10_highSWE.90),
                   sum(lowQ.10_highSWE.90>0))

stations%>%group_by()%>%
  dplyr::summarize(mean(lowQ.10_highSWE),
                   mean(lowQ.10_highSWE.90))
sum(stations$lowQ.10_highSWE>0)
sum(stations$lowQ.10_highSWE.90>0)

lowQ.10_highSWE.90.years%>%
  unlist()%>%
  factor()%>%
  summary()

lowQ.10_highSWE.90$Psum_pctl%>%summary()
lowQ.10_highSWE.90$Tsum_pctl%>%summary()


# Temperature-driven drought
sum(stations$tempDriven.5>0)

sum(stations$tempDriven.1>0,na.rm = TRUE)/sum(!is.na(stations$tempDriven.1))

mean(stations$tempDriven.5,na.rm  = TRUE)
mean(stations$tempDriven.1,na.rm  = TRUE)


# is there non-linearity in TempSummer?

sum(stns_T2$beta_T2>0)
sum(stns_T2$beta_T2<0)
stns_T2%>%
  group_by(regime)%>%
  dplyr::summarize(prop = sum(beta_T2>0)/n(),
                   binomTest = binom.test(sum(beta_T2>0),n(),p=0.5)$p.value,
                   
                   negSignif = sum(p.val<0.05&beta_T2<0)/n(),
                   posSignif = sum(p.val<0.05&beta_T2>0)/n(),     
                   binomTestp0.05.neg = binom.test(sum(p.val<0.05&beta_T2<0),n(),p=0.05,alternative = "greater")$p.value,
                   binomTestp0.05.pos = binom.test(sum(p.val<0.05&beta_T2>0),n(),p=0.05,alternative = "greater")$p.value)


x<-inner_join(stations%>%select(ID,regime,meanSummerTemp),
              stnCoeffs%>%select(ID,meanSummerTemp),
              by = "ID",
              suffix = c(".avg",".coef"))

summary(lm(meanSummerTemp.coef~meanSummerTemp.avg,x))
summary(lm(meanSummerTemp.coef~meanSummerTemp.avg,x%>%filter(regime == "Rainfall")))
summary(lm(meanSummerTemp.coef~meanSummerTemp.avg,x%>%filter(regime == "Hybrid")))
summary(lm(meanSummerTemp.coef~meanSummerTemp.avg,x%>%filter(regime == "Snowfall")))
summary(lm(meanSummerTemp.coef~meanSummerTemp.avg,x%>%filter(regime == "Glacial")))


# Cov of q7min

stations%>%group_by(regime)%>%
  dplyr::summarise(mean(covMinSumFlow7))

## stationarity ######


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

(testTable<-
    Coeffs%>%group_by(var,regime)%>%
  dplyr::summarize(nIncrease = sum(coeff.post>coeff.pre,na.rm = TRUE),
            num = n(),
            
            p = binom.test(nIncrease,num,p=0.5,alternative = c("two.sided"))$p.value,
            # p = pmin(pbinom(nIncrease,num,0.5),
            #          pbinom(nIncrease,num,0.5,lower.tail = FALSE)),
            
            sigInc.frac = sum(sigIncrease,na.rm = TRUE)/sum(!is.na(sigIncrease)),
            sigInc.pval = binom.test(round(sum(sigIncrease,na.rm = TRUE)),sum(!is.na(sigIncrease)),p=0.05,alternative = "greater")$p.value,
            sigDec.frac = sum(sigDecrease,na.rm = TRUE)/sum(!is.na(sigDecrease)),
            sigDec.pval = binom.test(round(sum(sigDecrease,na.rm = TRUE)),sum(!is.na(sigDecrease)),p=0.05,alternative = "greater")$p.value
            
            
  ))%>%
  print(n=100)

k<-rank(testTable$p, ties.method = "first")

m_prime<-32+1-k

testTable$T1.sig<-""
sigFunc<-function(x1,m_prime){
  if(x1<(0.001/m_prime)){return("***")}
  if(x1<(0.01/m_prime)){return("**")}
  if(x1<(0.05/m_prime)){return("*")}
  return("")
}

for(it in 1:32){
  it_row<-which(k==it)
  testTable$T1.sig[it_row]<-sigFunc(testTable$p[it_row],m_prime[it_row])
  if(testTable$T1.sig[it_row]==""){break}
}

k<-rank(testTable$sigInc.pval, ties.method = "first")
m_prime<-32+1-k

testTable$T2.Inc.sig<-""
for(it in 1:32){
  it_row<-which(k==it)
  testTable$T2.Inc.sig[it_row]<-sigFunc(testTable$sigInc.pval[it_row],m_prime[it_row])
  if(testTable$T2.Inc.sig[it_row]==""){break}
}



k<-rank(testTable$sigDec.pval, ties.method = "first")
m_prime<-32+1-k

testTable$T2.Dec.sig<-""
for(it in 1:32){
  it_row<-which(k==it)
  testTable$T2.Dec.sig[it_row]<-sigFunc(testTable$sigDec.pval[it_row],m_prime[it_row])
  if(testTable$T2.Dec.sig[it_row]==""){break}
}



  ## Now try each month separately################

stnCoeffs<-data.frame()
stnRs<-data.frame()
for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08HD023")
  
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(stations$month_bgn[it_stn]:stations$month_end[it_stn])]<-NA
  
  
  streamData$BaseFlow_Eckhardt0.97<-NA
  
  streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
  
  
  
  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])
  
  streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)
  
  
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
  
  
  
  ## Add in SWE Data
  snowData<-data_SWE[data_SWE$ID==stations$ID[it_stn],]%>%
    group_by(WaterYear)%>%
    dplyr::summarize(SWE = mean(value),
                     maxMonth = maxMonth[1])
  
  fixed_BF_month<-snowData$maxMonth[1]%>%
    pmax(3)%>%
    pmin(5)
  
  
  streamData<-left_join(streamData,stations[,c("ID","month_bgn","month_end")])
  
  
  for(it_mn in 7:10){
    streamDataYrly<-streamData%>%
      
      dplyr::group_by(NovWaterYear)%>%
      dplyr::summarize(numQNans = sum(is.na(Discharge)),
                       numQnotNAN = sum(!is.na(Discharge)),
                       
                       numMonQnonNans = sum(!is.na(Discharge[month ==it_mn])),
                       
                       
                       minMonFlow7 = min(Discharge7[month==it_mn],na.rm = TRUE),
                       minMonFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,it_mn),
                       
                       
                       
                       lengthMon = sum(month==it_mn),
                       
                       maxSWEdly = max(SWE,na.rm = TRUE),
                       maxSWEdly_day = DayOfYear[which.max(SWE)],
                       SumT7=T7[DayOfYear=minMonFlowDay],
                       
                       
                       BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
      )
    streamDataYrly$minMonFlow7[(streamDataYrly$lengthMon-streamDataYrly$numMonQnonNans)>5]<-NA    
    streamDataYrly$minMonFlow7[is.infinite(streamDataYrly$minMonFlow7)]<-NA
    
    
    streamDataYrly<-left_join(streamDataYrly,snowData,by = c("NovWaterYear" = "WaterYear"))
    
    
    # stations$minFlowMonth[it_stn]<-getMode(streamDataYrly$minSumFlowMonth)
    
    # if(stations$minFlowMonth[it_stn]==8){
    #   streamDataYrly$minMonFlow7<-streamDataYrly$minAugFlow7
    # }else if(stations$minFlowMonth[it_stn]==9){
    #   streamDataYrly$minMonFlow7<-streamDataYrly$minSepFlow7
    # }else if(stations$minFlowMonth[it_stn]==10){
    #   streamDataYrly$minMonFlow7<-streamDataYrly$minOctFlow7
    # }
    
    ## add in ECA data
    streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
    
    
    data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
    
    data$NovWaterYear<-data$Year
    data$NovWaterYear[data$Month%in%c(11,12)] <- data$Year[data$Month%in%c(11,12)]+1
    
    
    
    dataYearly<-data%>%
      group_by(NovWaterYear)%>%
      dplyr::summarize(
        
        winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        
        summerPrecip = sum(Total.Precip..mm.[Month%in%c(5:it_mn)],na.rm = TRUE),
        meanSummerTemp = mean(Mean.Temp..C.[Month%in%c(5:it_mn)],na.rm = TRUE),
        
      )
    
    dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))
    
    Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
    
    
    dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))
    
    
    
    # stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
    dataYearly2$minMonFlow7.log<-log(dataYearly2$minMonFlow7)
    
    dat<-dataYearly2[,c("minMonFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                        "BF_30day_Eckhardt0.97","ECA_I_9","ECA_III_24","Total.cms",
                        "August.cms","September.cms","October.cms")]%>%
      na.omit()%>%
      scale(center = TRUE, scale = TRUE)%>%
      data.frame()
    
    if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
      dat<-dataYearly2[,c("minMonFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                          "ECA_I_9","ECA_III_24","Total.cms",
                          "August.cms","September.cms","October.cms")]%>%
        na.omit()%>%
        scale(center = TRUE, scale = TRUE)%>%
        data.frame()
      dat$BF_30day_Eckhardt0.97<-0}
    
    
    if(nrow(dat)<20){next}
    
    
    tryWaterUse = FALSE
    if(any(dataYearly2$Total.cms>dataYearly2$minMonFlow7*0.1,na.rm = TRUE)&
       any(!is.na(dat$Total.cms))){
      tryWaterUse = TRUE
    } 
    
    summary(mylm<-lm(minMonFlow7.log~summerPrecip+meanSummerTemp+SumT7+
                       maxSWEdly+BF_30day_Eckhardt0.97-1,
                     data = dat,na.action= "na.exclude"))
    
    
    
    # summary(dataYearly2$maxSWEdly)
    # ggplot(dataYearly2,aes(x = NovWaterYear,y = maxSWEdly))+geom_point()
    # ggplot(dataYearly2,aes(x = maxSWEdly,y = minSumFlow7.log))+geom_point()
    
    
    if(stations$PrivateForestry[it_stn]<=0.1&stations$Total.Cut.Area[it_stn]>0.1&
       any(!is.na(dat$ECA_I_9))){
      mylm<-update(mylm,~.+ECA_I_9)
      if(any(!is.na(dat$ECA_III_24))){
        mylm<-update(mylm,~.+ECA_III_24)
      }
    }
    
    
    if(tryWaterUse){
      mylm<-update(mylm,~.+Total.cms)
      
    }
    
    
    p.vals<-summary(mylm)$coefficients[,4]
    names(p.vals)<-paste0(names(p.vals),".p")
    d<-cbind(ID=stations$ID[it_stn],
             month = it_mn,
             t(mylm$coefficients),
             t(p.vals))%>%
      data.frame()
    
    stnCoeffs<-plyr::rbind.fill(stnCoeffs,d)
    
    # availYears[[it_stn]]<-dat$NovWaterYear
    
    dat2<-mylm$model
    d2<-dat2%>%
      as.matrix()%>%
      rcorr(type = "pearson")
    
    
    d2.r<-d2$r[1,]%>%
      t()%>%
      data.frame()%>%
      dplyr::mutate(ID = stations$ID[it_stn],
                    month = it_mn)
    
    d2.p<-d2$P[1,]%>%
      t()%>%
      data.frame()
    names(d2.p)<-paste0(names(d2.p),".p")
    
    stnRs<-plyr::rbind.fill(stnRs,cbind(d2.r,d2.p))
  }
  
  
}
stnCoeffs<-left_join(stnCoeffs,stations%>%select(ID,regime))

stnCoeffs$regime<-factor(stnCoeffs$regime,
                         levels = c("Rainfall","Hybrid","Snowfall","Glacial")%>%rev(),
                         labels = c("Rainfall","Hybrid","Snowmelt","Glacial")%>%rev())

# for(it_mn in 7:10){
#   
#   
#   
#   
#   stnCoeffs.long<-stnCoeffs%>%
#     filter(month==it_mn)%>%
#     # select(!.p)%>%
#     pivot_longer(cols = summerPrecip:Total.cms.p)
#   
#   stnCoeffs.long$p.val<-str_detect(stnCoeffs.long$name,"\\.p")%>%
#     plyr::mapvalues(from = c(FALSE,TRUE),
#                     to = c("Beta","p.val"))
#   
#   stnCoeffs.long$name<-str_remove(stnCoeffs.long$name,"\\.p")
#   stnCoeffs.long<-pivot_wider(stnCoeffs.long,
#                               names_from = "p.val",
#                               values_from = value)
#   
#   stnCoeffs.long$Beta<-as.numeric(stnCoeffs.long$Beta)
#   stnCoeffs.long$p.val<-as.numeric(stnCoeffs.long$p.val)
#   
#   stnCoeffs.long$variable<-factor(stnCoeffs.long$name,
#                                   levels = c("maxSWEdly","BF_30day_Eckhardt0.97","summerPrecip","meanSummerTemp","SumT7","Total.cms","ECA_I_9","ECA_III_24"),
#                                   labels = c("SWE[max]","BF[winter]","P[summer]","T[summer]","T[7]","Abstraction","ECA[I]", "ECA[III]"))
#   
#   N_vals<-stnCoeffs.long%>%
#     filter(!is.na(Beta))%>%
#     group_by(regime, variable)%>%
#     dplyr::summarize(N=n())
#   
#   ggplot(stnCoeffs.long%>%
#            subset(!is.na(Beta))%>%
#            mutate(Beta = pmax(pmin(Beta,0.8),-0.8)),
#          
#          aes(y = regime,
#              # x = value
#              x = Beta
#          ))+
#     geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
#     geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
#     geom_text(data = N_vals, aes(x = 0.9,y = regime,label = N),size = 3)+
#     # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
#     geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
#     # geom_dotplot()+
#     # geom_violin(scale = "width",adjust = 0.5)+
#     # geom_point(stat = "summary",fun = "median")+
#     scale_y_discrete(
#       # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
#       name = NULL,
#       position = "right")+
#     scale_x_continuous(name = expression(italic(beta)),
#                        # oob = oob_squish,
#                        limits = c(-.8,.9),
#                        breaks = c(-.8,-0.4,0,0.4,.8),
#                        labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
#     scale_color_manual(name = "",values = c("grey","red"),
#                        labels = c("p≥0.05","p<0.05"))+
#     # scale_x_continuous(name = expression(r^2%*%sign(r)))+
#     # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
#     
#     facet_wrap(facets = "variable", ncol = 1,strip.position = "left",
#                labeller = label_parsed)+
#     theme_bw()+
#     theme(legend.position = c(0.15,0.93),
#           legend.background = element_rect(color = "black",fill = alpha('white',0.5)),
#           legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
#           legend.key = element_blank(),
#           legend.title = element_blank(),
#           # panel.grid.major.y = element_blank(),
#           # panel.grid.minor.y = element_blank()
#     )
#   
#   
#   ggsave(paste0("3.figures/Sens_boxplots_beta_",it_mn,".png"),width = 4, height = 7.5)
#   
#   
# }



stnCoeffs.long<-stnCoeffs%>%
  # filter(month==it_mn)%>%
  # select(!.p)%>%
  pivot_longer(cols = summerPrecip:Total.cms.p)

stnCoeffs.long$p.val<-str_detect(stnCoeffs.long$name,"\\.p")%>%
  plyr::mapvalues(from = c(FALSE,TRUE),
                  to = c("Beta","p.val"))

stnCoeffs.long$name<-str_remove(stnCoeffs.long$name,"\\.p")
stnCoeffs.long<-pivot_wider(stnCoeffs.long,
                            names_from = "p.val",
                            values_from = value)

stnCoeffs.long$Beta<-as.numeric(stnCoeffs.long$Beta)
stnCoeffs.long$p.val<-as.numeric(stnCoeffs.long$p.val)

stnCoeffs.long$variable<-factor(stnCoeffs.long$name,
                                levels = c("maxSWEdly","BF_30day_Eckhardt0.97","summerPrecip","meanSummerTemp","SumT7","Total.cms","ECA_I_9","ECA_III_24"),
                                labels = c("SWE[max]","BF[winter]","P[summer]","T[summer]","T[7]","Abstraction","ECA[I]", "ECA[III]"))
stnCoeffs.long$month_fctr<-factor(stnCoeffs.long$month,levels = 7:10,
                                  labels = c("July","August","September","October"))
N_vals<-stnCoeffs.long%>%
  filter(!is.na(Beta))%>%
  group_by(regime, variable,month_fctr)%>%
  dplyr::summarize(N=n())


ggplot(stnCoeffs.long%>%
         subset(!is.na(Beta))%>%
         mutate(Beta = pmax(pmin(Beta,0.8),-0.8)),
       
       aes(y = regime,
           # x = value
           x = Beta
       ))+
  geom_vline(xintercept = 0,col = "grey50",linewidth = 1)+
  geom_boxplot(outlier.size = 0.5,outlier.shape = NA)+
  geom_text(data = N_vals, aes(x = 0.85,y = regime,label = N),size = 3)+
  # geom_jitter(data = . %>% filter(p.val <0.05),color = "red",size = 0.5,height = 0.2)+
  geom_jitter(aes(color = p.val<0.05),size = 0.5,height = 0.2,alpha=0.5)+
  # geom_dotplot()+
  # geom_violin(scale = "width",adjust = 0.5)+
  # geom_point(stat = "summary",fun = "median")+
  scale_y_discrete(
    # labels = c("Glacial","Snowmelt","Hybrid","Rainfall"),
    name = NULL,
    position = "right")+
  scale_x_continuous(name = expression(italic(beta)),
                     # oob = oob_squish,
                     limits = c(-.8,.9),
                     breaks = c(-.8,-0.4,0,0.4,.8),
                     labels = c("≤ -0.8","-0.4","0","0.4","≥ 0.8"))+
  scale_color_manual(name = "",values = c("grey","red"),
                     labels = c("p≥0.05","p<0.05"))+
  # scale_x_continuous(name = expression(r^2%*%sign(r)))+
  # scale_x_continuous(name = expression(Pearson~r%*%"| r |"))+
  
  facet_grid(rows = vars(variable),cols = vars(month_fctr),
             # strip.position = "left",
             labeller = label_parsed)+
  theme_bw()+
  theme(
    # legend.position = c(0.15,0.93),
    legend.position = "bottom",
        legend.background = element_rect(color = "black",fill = alpha('white',0.5)),
        legend.margin = margin(t=-4,r=1,b=-2,l=0,unit = "pt"),
        legend.key = element_blank(),
        legend.title = element_blank(),
        # panel.grid.major.y = element_blank(),
        # panel.grid.minor.y = element_blank()
  )

ggsave(paste0("3.figures/Sens_boxplots_beta_months.png"),width = 8, height = 9)

# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-07


# This script performs the stationarity analysis presented in Section 3.2.2


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
library(tictoc)

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
# WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")%>%
#   filter(StationNum%in%unique(streamDataAll$ID))
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



streamDataAll<-dplyr::left_join(streamDataAll,data_SWE_dly,by = c("ID","Date"))
streamDataAll$NovWaterYear<-streamDataAll$year
streamDataAll$NovWaterYear[streamDataAll$month%in%c(11,12)]<-
  streamDataAll$year[streamDataAll$month%in%c(11,12)]+1

## Loop through stations and extract variables ################
# 
# watersheds<-st_transform(watersheds,st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
# watersheds$Area_km2<-(st_area(watersheds)/10^6)%>%as.numeric()
# 
# stations<-left_join(stations%>%select(!Area_km2),
#                     watersheds%>%select(ID,Area_km2)%>%
#                       st_drop_geometry(),
#                     by = c("ID" ))


stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)

# stations[,c("rBF_30day_Eckhardt0.97.diff","rTotal.cms.diff","rmaxSWEdly.diff",
#             "rTempSummer.diff","rPcpSummer.diff","rSumT7.diff",
#             "rBF_30day_Eckhardt0.97.diff.p","rTotal.cms.diff.p","rmaxSWEdly.diff.p",
#             "rTempSummer.diff.p","rPcpSummer.diff.p","rSumT7.diff.p",
#             "rECAI.diff","rECAI.diff.p","rECAIII.diff","rECAIII.diff.p")]<-NA


stnDiffs<-data.frame()
splitYr = 1997
n_shuffles<-10000

for(it_stn in 1:length(stations$ID)){
  tic()
  # it_stn = which(stations$ID=="08LG056")
  if(stations$ID[it_stn]%in%stnDiffs$ID){next}
  # ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(stations$month_bgn[it_stn]:stations$month_end[it_stn])]<-NA
  
  
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
                     
                     # BF_30day_lynehollick = mean(BaseFlow_lynehollick[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_boughton = mean(BaseFlow_boughton[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_jakeman = mean(BaseFlow_jakeman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_maxwell = mean(BaseFlow_maxwell[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_chapman = mean(BaseFlow_chapman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     
                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     
                     
                     # winterBF_lynehollick = mean(BaseFlow_lynehollick[month==fixed_BF_month]),
                     # winterBF_boughton = mean(BaseFlow_boughton[month==fixed_BF_month]),
                     # winterBF_jakeman = mean(BaseFlow_jakeman[month==fixed_BF_month]),
                     # winterBF_maxwell = mean(BaseFlow_maxwell[month==fixed_BF_month]),
                     # winterBF_chapman = mean(BaseFlow_chapman[month==fixed_BF_month]),
                     # winterBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==fixed_BF_month]),
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
  # streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  
  
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
  
  # Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  
  
  # dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))
  
  
  
  # stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  dataYearly2$minSumFlow7.log<-log(dataYearly2$minSumFlow7)
  
  
  dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                      "BF_30day_Eckhardt0.97",
                      # ,"ECA_I_9","ECA_III_24","Total.cms",
 
                      "NovWaterYear"
                      )]%>%
    na.omit()
  dat<-cbind(dat%>%
               scale(scale = TRUE,center = TRUE)%>%
               data.frame()%>%
               select(!NovWaterYear),
             dat[,"NovWaterYear"])
  
  
  
  
  if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
    dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                        # "ECA_I_9","ECA_III_24","Total.cms",
                        "NovWaterYear")]%>%
      na.omit()
    dat<-cbind(dat%>%
                 scale(scale = TRUE,center = TRUE)%>%
                 data.frame()%>%
                 select(!NovWaterYear),
               dat[,"NovWaterYear"])
    dat$BF_30day_Eckhardt0.97<-0}
  
  
  # if(!(sum(dat$NovWaterYear<=1997)>=20&sum(dat$NovWaterYear>1997)>=20)){next}
  if(!(sum(dat$NovWaterYear<=splitYr)>=20&sum(dat$NovWaterYear>splitYr)>=20)){next}
  
  tryWaterUse = FALSE
  # if(any(dataYearly2$Total.cms>dataYearly2$minSumFlow7*0.1,na.rm = TRUE)&
  #    any(!is.na(dat$Total.cms))){
  #   tryWaterUse = TRUE
  # } 
  
  tryECAI = FALSE
  tryECAIII = FALSE
  # if(stations$PrivateForestry[it_stn]<=0.1&stations$Total.Cut.Area[it_stn]>0.1){
  #   
  #   tryECAI = TRUE
  #   if(any(!is.na(dat$ECA_III_24))){
  #     tryECAIII=TRUE
  #     
  #   }
  # }
  
  
  summary(mylm<-lm(minSumFlow7.log~summerPrecip+meanSummerTemp+SumT7+
                     maxSWEdly+BF_30day_Eckhardt0.97-1,
                   data = dat,na.action= "na.exclude"))
  
  
  
  # summary(dataYearly2$maxSWEdly)
  # ggplot(dataYearly2,aes(x = NovWaterYear,y = maxSWEdly))+geom_point()
  # ggplot(dataYearly2,aes(x = maxSWEdly,y = minSumFlow7.log))+geom_point()
  
  
  # if(tryECAI){
  #   mylm<-update(mylm,~.+ECA_I_9)
  # }
  # if(tryECAIII){
  #   mylm<-update(mylm,~.+ECA_III_24)
  # }
  # 
  # if(tryWaterUse){
  #   mylm<-update(mylm,~.+Total.cms)
  #   
  # }
  
  
  
  msk<-dat$NovWaterYear<=splitYr
  
  mylm.pre1996<-update(mylm,data = dat%>%filter(msk)%>%scale(center = TRUE,scale = FALSE)%>%data.frame())
  mylm.post1996<-update(mylm,data =  dat%>%filter(!msk)%>%scale(center = TRUE,scale = FALSE)%>%data.frame())
  
  
  d<-cbind(ID=stations$ID[it_stn],
           t(mylm.post1996$coefficients-mylm.pre1996$coefficients))%>%
    data.frame()
  d[,2:ncol(d)]<-as.numeric(  d[,2:ncol(d)])
  
  
  nyr<-length(msk)
  set.seed(1)
  
  # diffR<-c()
  
  diffsRand<-data.frame()
  for(it_r in 1:n_shuffles){
    set.seed(it_r)
    mskRand<-sample(msk,size = nyr,replace = FALSE)
    
    mylm.pre1996<-update(mylm,data = dat%>%filter(mskRand)%>%scale(center = TRUE,scale = FALSE)%>%data.frame())
    mylm.post1996<-update(mylm,data =  dat%>%filter(!mskRand)%>%scale(center = TRUE,scale = FALSE)%>%data.frame())
    
    
    dRand<-cbind(
      # stations$ID[it_stn],
                 (t(mylm.post1996$coefficients-mylm.pre1996$coefficients)))%>%
      data.frame()
    
    diffsRand<-plyr::rbind.fill(diffsRand,dRand)
    
    
    
  }  
  
  # for(i in 1:ncol(diffsRand)){
  #   diffsRand[,i]<-as.numeric(  diffsRand[,i])
  # }
  
  
  x1<-apply(FUN = function(x){abs(x)>abs(d[,2:ncol(d)])}, diffsRand,MARGIN = 1)%>%
    apply(mean,MARGIN = 1)
  
  names(x1)<-paste0(names(diffsRand),".p")
  
  d2<-cbind(d,t(x1))
  
  stnDiffs<-plyr::rbind.fill(stnDiffs,d2)
  
  
  print(sprintf("Done %d stations",it_stn))
  toc()
}

stnDiffs<-left_join(stnDiffs,stations[,c("ID","regime")])

stnDiffs%>%dplyr::summarize(across(ends_with(".p"),~sum(.x<0.05,na.rm = TRUE)/sum(!is.na(.x))))

stnDiffs%>%
  group_by(regime)%>%
  dplyr::summarize(
    across(ends_with(".p"),~sum(.x<0.05,na.rm = TRUE)/sum(!is.na(.x))))



Coeffs<-stnDiffs%>%
  select(ID,regime,summerPrecip:BF_30day_Eckhardt0.97.p)%>%
  pivot_longer(cols = summerPrecip:BF_30day_Eckhardt0.97.p)%>%
  mutate(meas = str_detect(name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("r","p.val")),
         variable = str_remove(name,"\\.p"))%>%
  pivot_wider(id_cols = c(ID,regime,variable),values_from = value,
              names_from = meas)%>%
  filter(!is.na(p.val)&!is.na(r))


Coeffs$regime<-factor(Coeffs$regime,
                      levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))


Coeffs$variable<-factor(Coeffs$variable,levels =c("maxSWEdly","BF_30day_Eckhardt0.97",
                                                  "summerPrecip","meanSummerTemp","SumT7"
                                                  #"Total.cms","ECA_I_9","ECA_III_24"
                                                  ) ,
                        labels = c("SWE.max","BF.winter","P.summer","T.summer","T.7"#,"Abstraction","ECA.I", "ECA.III"
                        ))


(testTable<-
    Coeffs%>%
    group_by(variable,regime)%>%
    dplyr::summarize(num = sum(!is.na(r)),
                     nIncrease = paste0(round(sum(r>0,na.rm = TRUE)/num*100),"%"),
                     
                     p = binom.test(sum(r>0,na.rm = TRUE),num,p=0.5,alternative = c("two.sided"))$p.value,
                     
                     
                     sigInc.frac = paste0(round(sum(r>0&p.val<0.05,na.rm = TRUE)/n()*100),"%"),
                     sigInc.pval = binom.test(sum(r>0&p.val<0.05,na.rm = TRUE),n(),p=0.05,alternative = "greater")$p.value,
                     sigDec.frac = paste0( round(sum(r<0&p.val<0.05,na.rm = TRUE)/n()*100),"%"),
                     sigDec.pval = binom.test(sum(r<0&p.val<0.05,na.rm = TRUE),n(),p=0.05,alternative = "greater")$p.value
                     
                     
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

testTable.df<-
  testTable%>%data.frame()%>%
  mutate(variable=as.character(variable),
         regime = as.character(regime),
         # p=round(p,3),
         # sigInc.pval = round(sigInc.pval,3),
         # sigDec.pval = round(sigDec.pval,3)
         
  )

testTable.df<-testTable.df%>%select(variable,regime,num,nIncrease,p,T1.sig,sigInc.frac,sigInc.pval,T2.Inc.sig,sigDec.frac,sigDec.pval,T2.Dec.sig)

testTable.df

# stargazer::stargazer(testTable.df,
#                      type = "html",
#                      out = "4.output/stationarity_test.doc",
#                      summary = FALSE,
#                      digits = 2,
#                      digits.extra = 2,
#                      rownames = FALSE
# )

write.csv(testTable.df,sprintf("4.output/stationarity_test_%d.csv",splitYr))



closeAllConnections()
rm(list=ls())
graphics.off()

library(sensemakr)
library(dplyr)
library(sf)
library(tmap)
# library(lmSupport)
library(car)
library(lubridate)
library(ggplot2)
# library(bcdata)
library(bcmaps)
library(stringr)
library(MuMIn)
library(hydroGOF)
library(scico)
library(foreach)
library(doParallel)
library(tidyr)


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")

stations<-read.csv("2.data/2.working/StationMetadata/stations_final_wSnow.csv",fileEncoding = "UTF-8-BOM")
# stations_CC<-read.csv("2.data/2.working/Stations_ClimateChange.csv")
# Stations<-Stations[Stations$]
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_wSnow.gpkg")

# stations<-left_join(stations,stations_CC[,c(2,5:12)],by = c("ID" ="StationNum" ))


WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")

stations$NSE<-NA
stations$KGE<-NA
stations$R2<-NA
stations$acc<-NA
stations$sens<-NA
stations$spec<-NA
stations$prev<-NA


# load("2.data/2.working/RegressionOptimization/RegressionOptimization.RData")
# stations<-stations[!stations$ID%in%names(allAugModels_KGEs),]



allAugModels_KGEs<-list()
allSepModels_KGEs<-list()
allOctModels_KGEs<-list()

AugustBestModel<-list()
SeptemberBestModel<-list()
OctoberBestModel<-list()

## First cross-validation
for(it_stn in 1:length(stations$ID)){
  tictoc::tic()
  # it_stn = which(stations$ID=="08HA003")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$Discharge30 = zoo::rollmean(streamData$Discharge,30,fill = NA)
  
  streamData$DayOfYear<-lubridate::yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     minFlow30CP= min(Discharge30[month%in%c(7,8,9)]),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     
                     MAD = mean(Discharge,na.rm = TRUE)
                     
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  
  
  
  
  ## Weather data
  
  data<-WeatherData[WeatherData$StationNum == stations$ID[it_stn],]
  
  data$WaterYear<-data$Year
  data$WaterYear[data$Month%in%c(11,12)] <- data$WaterYear[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(WaterYear)%>%
    dplyr::summarize(winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     
                     Precip_O = sum(Total.Precip..mm.[Month%in%c(10)],na.rm = TRUE),
                     Precip_S = sum(Total.Precip..mm.[Month%in%c(9)],na.rm = TRUE),
                     Precip_A = sum(Total.Precip..mm.[Month%in%c(8)],na.rm = TRUE),
                     Precip_Jl = sum(Total.Precip..mm.[Month%in%c(7)],na.rm = TRUE),
                     Precip_Jn =sum(Total.Precip..mm.[Month%in%c(6)],na.rm = TRUE),
                     Precip_M =sum(Total.Precip..mm.[Month%in%c(5)],na.rm = TRUE),
                     
                     Temp_O =  mean(Mean.Temp..C.[Month%in%c(10)],na.rm = TRUE),
                     Temp_S =  mean(Mean.Temp..C.[Month%in%c(9)],na.rm = TRUE),
                     Temp_A =  mean(Mean.Temp..C.[Month%in%c(8)],na.rm = TRUE),
                     Temp_Jl =  mean(Mean.Temp..C.[Month%in%c(7)],na.rm = TRUE),
                     Temp_Jn =mean(Mean.Temp..C.[Month%in%c(6)],na.rm = TRUE),
                     Temp_M =mean(Mean.Temp..C.[Month%in%c(5)],na.rm = TRUE),
                     
                     #August
                     
                     Precip_MJJ = sum(Total.Precip..mm.[Month%in%c(5,6,7)],na.rm = TRUE),
                     Precip_JJ = sum(Total.Precip..mm.[Month%in%c(6,7)],na.rm = TRUE),
                     Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Precip_JJA = sum(Total.Precip..mm.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     Temp_MJJ =  mean(Mean.Temp..C.[Month%in%c(5,6,7)],na.rm = TRUE),
                     Temp_JJ =  mean(Mean.Temp..C.[Month%in%c(6,7)],na.rm = TRUE),
                     Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Temp_JJA =  mean(Mean.Temp..C.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     # September
                     
                     Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Precip_JJAS = sum(Total.Precip..mm.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Precip_JAS = sum(Total.Precip..mm.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Temp_JJAS =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Temp_JAS =  mean(Mean.Temp..C.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     # October
                     
                     Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JJASO = sum(Total.Precip..mm.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JASO = sum(Total.Precip..mm.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Precip_ASO = sum(Total.Precip..mm.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JJASO =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JASO =  mean(Mean.Temp..C.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Temp_ASO =  mean(Mean.Temp..C.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     
                     
    )
  
  
  
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
  dataYearly2<-dataYearly2[dataYearly$WaterYear<2024,]
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/",stations$ID[it_stn],".csv"))
  # names(licensedDiv)[2]<-"lcd_SW_div"
  # Div$Total<-Div$Total_SW+Div$Total_GW_UL+Div$Total_GW_LC
  # 
  # Div$August.cms<-(Div$August_SW+Div$August_GW_LC+Div$August_GW_UL)/(31*24*3600)# convert to cms
  # Div$September.cms<-(Div$September_SW+Div$September_GW_LC+Div$September_GW_UL)/(30*24*3600)# convert to cms
  # Div$October.cms<-(Div$October_SW+Div$October_GW_LC+Div$October_GW_UL)/(31*24*3600)# convert to cms
  # 
  
  Div<-Div[,c("year","Total.cms","August.cms","September.cms","October.cms")]
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  
  
  
  dataYearly2<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  set.seed(13)
  
  sets<-sample(1:nrow(dataYearly2)%%5+1,nrow(dataYearly2),replace = FALSE)
  
  # August Regressions ####
  combinationsAug<-list()
  combinationsSep<-list()
  combinationsOct<-list()
  
  data_testedAug = data.frame()
  data_testedSep = data.frame()
  data_testedOct = data.frame()
  data_tested_meas<-data.frame()
  
  
  # Create model for August with all predictors
  dataYearly2_trA<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  globalmodelAug <- lm(log(minAugFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJA+Precip_JJA+Precip_JA+Precip_A+Precip_Jl+
                         Temp_MJJA+Temp_JJA+Temp_JA+Temp_A+Temp_Jl+
                         August.cms,
                       data= dataYearly2_trA,
                       na.action = "na.fail")
  
  # Create model for September with all predictors
  dataYearly2_trS<-dataYearly2[!is.na(dataYearly2$minSepFlow7),]
  globalmodelSep <- lm(log(minSepFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJAS+Precip_JAS+Precip_AS+Precip_A+Precip_S+
                         Temp_MJJAS+Temp_JAS+Temp_AS+Temp_A+Temp_S+
                         September.cms,
                       data= dataYearly2_trS,
                       na.action = "na.fail")
  
  # Create model for October with all predictors
  dataYearly2_trO<-dataYearly2[!is.na(dataYearly2$minOctFlow7),]
  globalmodelOct <- lm(log(minOctFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJASO+Precip_ASO+Precip_SO+Precip_S+Precip_O+
                         Temp_MJJASO+Temp_ASO+Temp_SO+Temp_S+Temp_O+
                         October.cms,
                       data= dataYearly2_trO,
                       na.action = "na.fail")
  if(!any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)){
    globalmodelAug<-update(globalmodelAug,.~.-August.cms)
  } 
  if(!any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)){
    globalmodelSep<-update(globalmodelSep,.~.-September.cms)
  }
  if(!any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE)){
    globalmodelOct<-update(globalmodelOct,.~.-October.cms)
  }
  
  # Make all combinations of predictor variables
  combinationsAug <- dredge(globalmodelAug,evaluate = FALSE)
  combinationsSep <- dredge(globalmodelSep,evaluate = FALSE)
  combinationsOct <- dredge(globalmodelOct,evaluate = FALSE)
  
  for(it_cv in 1:5){
    # Divide training and testing data
    dataYearly2_tr<-dataYearly2[-which(sets%in%it_cv),]
    dataYearly2_te<-dataYearly2[which(sets%in%it_cv),]
    
    
    dataYearly2_trA<-dataYearly2_tr[!is.na(dataYearly2_tr$minAugFlow7),]
    dataYearly2_trS<-dataYearly2_tr[!is.na(dataYearly2_tr$minSepFlow7),]
    dataYearly2_trO<-dataYearly2_tr[!is.na(dataYearly2_tr$minOctFlow7),]
    
    # Take surface water diversions out if they are small (less than 5% of the min flow in any given year)
    
    lm_test<-function(model_x){
      predict(model_x,dataYearly2_te)
    }    
    
    
    # evaluate models
    modelsAug<-lapply(combinationsAug,eval)
    modelsSep<-lapply(combinationsSep,eval)
    modelsOct<-lapply(combinationsOct,eval)
    
    # extract predictions for each model
    resAug<-sapply(modelsAug,FUN = lm_test)
    resSep<-sapply(modelsSep,FUN = lm_test)
    resOct<-sapply(modelsOct,FUN = lm_test)
    
    # Bind predicted low flows
    data_testedAug<-rbind(data_testedAug,resAug)
    data_testedSep<-rbind(data_testedSep,resSep)
    data_testedOct<-rbind(data_testedOct,resOct)
    
    #Bind measured low flows
    data_tested_meas<-rbind(data_tested_meas,dataYearly2_te)
    
  }
  
  
  KGEs_Aug<-apply(data_testedAug,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minAugFlow7))})
  KGEs_Sep<-apply(data_testedSep,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minSepFlow7))})
  KGEs_Oct<-apply(data_testedOct,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minOctFlow7))})
  
  # allAugModels[[it_stn]]<-combinationsAug
  allAugModels_KGEs[[it_stn]]<-KGEs_Aug
  # allSepModels[[it_stn]]<-combinationsSep
  allSepModels_KGEs[[it_stn]]<-KGEs_Sep
  # allOctModels[[it_stn]]<-combinationsOct
  allOctModels_KGEs[[it_stn]]<-KGEs_Oct
  
  AugustBestModel[[it_stn]]<-combinationsAug[which.max(KGEs_Aug)]
  SeptemberBestModel[[it_stn]]<-combinationsSep[which.max(KGEs_Sep)]
  OctoberBestModel[[it_stn]]<-combinationsOct[which.max(KGEs_Oct)]
  
  
  tictoc::toc()
  print(sprintf("Done %d catchments",it_stn))
  
  
}

names(allAugModels_KGEs)<-stations$ID
names(allSepModels_KGEs)<-stations$ID
names(allOctModels_KGEs)<-stations$ID

names(AugustBestModel)<-stations$ID
names(SeptemberBestModel)<-stations$ID
names(OctoberBestModel)<-stations$ID

# allAugModels_KGEs2<-allAugModels_KGEs
# allSepModels_KGEs2<-allSepModels_KGEs
# allOctModels_KGEs2<-allOctModels_KGEs
# AugustBestModel2<-AugustBestModel
# SeptemberBestModel2<-SeptemberBestModel
# OctoberBestModel2<-OctoberBestModel
# 
# load("2.data/2.working/RegressionOptimization/RegressionOptimization.RData")
# allAugModels_KGEs<-c(allAugModels_KGEs,allAugModels_KGEs2)
# allSepModels_KGEs<-c(allSepModels_KGEs,allSepModels_KGEs2)
# allOctModels_KGEs<-c(allOctModels_KGEs,allOctModels_KGEs2)


save(allAugModels_KGEs,AugustBestModel,
     allSepModels_KGEs,SeptemberBestModel,
     allOctModels_KGEs,OctoberBestModel,
     file = "2.data/2.working/RegressionOptimization/RegressionOptimization.RData"
)

(lapply(allAugModels_KGEs,max))%>%
  unlist()%>%
  quantile()
(lapply(allSepModels_KGEs,max))%>%
  unlist()%>%
  quantile()
(lapply(allOctModels_KGEs,max))%>%
  unlist()%>%
  quantile()

## Second cross-validation
load("2.data/2.working/RegressionOptimization/RegressionOptimization.RData")


whichpart <- function(x, n=30) {
  nx <- length(x)
  # p <- nx-n
  xp <- -sort(-x, partial=n)[n]
  
  inds <- which(x >= xp)
  if(length(inds)>n){
    inds_rm <- which(x == xp)[1:(length(inds)-n)]
    inds <- inds[!inds%in%inds_rm]
  }
  return(inds)
}



allAugModels<-list()
allAugModels_KGEs_2<-list()
allSepModels<-list()
allSepModels_KGEs_2<-list()
allOctModels<-list()
allOctModels_KGEs_2<-list()


AugustBestModel<-list()
SeptemberBestModel<-list()
OctoberBestModel<-list()


for(it_stn in 1:length(stations$ID)){
  tictoc::tic()
  # it_stn = which(stations$ID=="08HA003")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$Discharge30 = zoo::rollmean(streamData$Discharge,30,fill = NA)
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     minFlow30CP= min(Discharge30[month%in%c(7,8,9)]),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     
                     MAD = mean(Discharge,na.rm = TRUE)
                     
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
    streamDataYrly$minSumFlow<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  
  ## Weather data
  
  data<-WeatherData[WeatherData$StationNum == stations$ID[it_stn],]
  
  data$WaterYear<-data$Year
  data$WaterYear[data$Month%in%c(11,12)] <- data$WaterYear[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(WaterYear)%>%
    dplyr::summarize(winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     
                     Precip_O = sum(Total.Precip..mm.[Month%in%c(10)],na.rm = TRUE),
                     Precip_S = sum(Total.Precip..mm.[Month%in%c(9)],na.rm = TRUE),
                     Precip_A = sum(Total.Precip..mm.[Month%in%c(8)],na.rm = TRUE),
                     Precip_Jl = sum(Total.Precip..mm.[Month%in%c(7)],na.rm = TRUE),
                     Precip_Jn =sum(Total.Precip..mm.[Month%in%c(6)],na.rm = TRUE),
                     Precip_M =sum(Total.Precip..mm.[Month%in%c(5)],na.rm = TRUE),
                     
                     Temp_O =  mean(Mean.Temp..C.[Month%in%c(10)],na.rm = TRUE),
                     Temp_S =  mean(Mean.Temp..C.[Month%in%c(9)],na.rm = TRUE),
                     Temp_A =  mean(Mean.Temp..C.[Month%in%c(8)],na.rm = TRUE),
                     Temp_Jl =  mean(Mean.Temp..C.[Month%in%c(7)],na.rm = TRUE),
                     Temp_Jn =mean(Mean.Temp..C.[Month%in%c(6)],na.rm = TRUE),
                     Temp_M =mean(Mean.Temp..C.[Month%in%c(5)],na.rm = TRUE),
                     
                     #August
                     
                     Precip_MJJ = sum(Total.Precip..mm.[Month%in%c(5,6,7)],na.rm = TRUE),
                     Precip_JJ = sum(Total.Precip..mm.[Month%in%c(6,7)],na.rm = TRUE),
                     Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Precip_JJA = sum(Total.Precip..mm.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     Temp_MJJ =  mean(Mean.Temp..C.[Month%in%c(5,6,7)],na.rm = TRUE),
                     Temp_JJ =  mean(Mean.Temp..C.[Month%in%c(6,7)],na.rm = TRUE),
                     Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Temp_JJA =  mean(Mean.Temp..C.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     # September
                     
                     Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Precip_JJAS = sum(Total.Precip..mm.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Precip_JAS = sum(Total.Precip..mm.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Temp_JJAS =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Temp_JAS =  mean(Mean.Temp..C.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     # October
                     
                     Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JJASO = sum(Total.Precip..mm.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JASO = sum(Total.Precip..mm.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Precip_ASO = sum(Total.Precip..mm.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JJASO =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JASO =  mean(Mean.Temp..C.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Temp_ASO =  mean(Mean.Temp..C.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     
                     
    )
  
  
  
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
  dataYearly2<-dataYearly2[dataYearly$WaterYear<2024,]
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/",stations$ID[it_stn],".csv"))
  Div<-Div[,c("year","Total.cms","August.cms","September.cms","October.cms")]
  
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  
  
  
  dataYearly2<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  #initialize model
  dataYearly2_trA<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  globalmodelAug <- lm(log(minAugFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJA+Precip_JJA+Precip_JA+Precip_A+Precip_Jl+
                         Temp_MJJA+Temp_JJA+Temp_JA+Temp_A+Temp_Jl+
                         August.cms,
                       data= dataYearly2_trA,
                       na.action = "na.fail")
  
  dataYearly2_trS<-dataYearly2[!is.na(dataYearly2$minSepFlow7),]
  globalmodelSep <- lm(log(minSepFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJAS+Precip_JAS+Precip_AS+Precip_A+Precip_S+
                         Temp_MJJAS+Temp_JAS+Temp_AS+Temp_A+Temp_S+
                         September.cms,
                       data= dataYearly2_trS,
                       na.action = "na.fail")
  
  summary(globalmodelSep)
  
  # Create model for October with all predictors
  dataYearly2_trO<-dataYearly2[!is.na(dataYearly2$minOctFlow7),]
  globalmodelOct <- lm(log(minOctFlow7)~
                         winterPrecip+meanWinterTemp+
                         Precip_MJJASO+Precip_ASO+Precip_SO+Precip_S+Precip_O+
                         Temp_MJJASO+Temp_ASO+Temp_SO+Temp_S+Temp_O+
                         October.cms,
                       data= dataYearly2_trO,
                       na.action = "na.fail")
  
  
  # Take surface water diversions out if they are small (less than 5% of the min flow in any given year)
  if(!any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)){
    globalmodelAug<-update(globalmodelAug,.~.-August.cms)
  } 
  if(!any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)){
    globalmodelSep<-update(globalmodelSep,.~.-September.cms)
  }
  if(!any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE)){
    globalmodelOct<-update(globalmodelOct,.~.-October.cms)
  }
  
  
  # Make all combinations of predictor variables
  combinationsAug <- dredge(globalmodelAug,evaluate = FALSE)
  combinationsSep <- dredge(globalmodelSep,evaluate = FALSE)
  combinationsOct <- dredge(globalmodelOct,evaluate = FALSE)
  
  bestAugMods<-whichpart(allAugModels_KGEs[[stations$ID[it_stn]]],n=50)
  bestSepMods<-whichpart(allSepModels_KGEs[[stations$ID[it_stn]]],n=50)
  bestOctMods<-whichpart(allOctModels_KGEs[[stations$ID[it_stn]]],n=50)
  
  combinationsAug<-combinationsAug[bestAugMods]
  combinationsSep<-combinationsSep[bestSepMods]
  combinationsOct<-combinationsOct[bestOctMods]
  
  KGEs_Aug<-data.frame()
  KGEs_Sep<-data.frame()
  KGEs_Oct<-data.frame()
  for(it_seed in 1:10){
    set.seed(it_seed)
    
    sets<-sample(1:nrow(dataYearly2)%%5+1,nrow(dataYearly2),replace = FALSE)
    
    data_testedAug = data.frame()
    data_testedSep = data.frame()
    data_testedOct = data.frame()
    data_tested_meas<-data.frame()
    
    
    
    for(it_cv in 1:5){
      # Divide training and testing data
      dataYearly2_tr<-dataYearly2[-which(sets%in%it_cv),]
      dataYearly2_te<-dataYearly2[which(sets%in%it_cv),]
      dataYearly2_trA<-dataYearly2_tr[!is.na(dataYearly2_tr$minAugFlow7),]
      dataYearly2_trS<-dataYearly2_tr[!is.na(dataYearly2_tr$minSepFlow7),]
      
      dataYearly2_trO<-dataYearly2_tr[!is.na(dataYearly2_tr$minOctFlow7),]
      
      
      lm_test<-function(model_x){
        predict(model_x,dataYearly2_te)
      }    
      
      
      # evaluate models
      modelsAug<-lapply(combinationsAug,eval)
      modelsSep<-lapply(combinationsSep,eval)
      modelsOct<-lapply(combinationsOct,eval)
      
      # extract predictions for each model
      resAug<-sapply(modelsAug,FUN = lm_test)
      resSep<-sapply(modelsSep,FUN = lm_test)
      resOct<-sapply(modelsOct,FUN = lm_test)
      
      # Bind predicted low flows
      data_testedAug<-rbind(data_testedAug,resAug)
      data_testedSep<-rbind(data_testedSep,resSep)
      data_testedOct<-rbind(data_testedOct,resOct)
      
      #Bind measured low flows
      data_tested_meas<-rbind(data_tested_meas,dataYearly2_te)
      
    }
    KGEs_Aug[it_seed,1:50]<-apply(data_testedAug,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minAugFlow7))})
    KGEs_Sep[it_seed,1:50]<-apply(data_testedSep,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minSepFlow7))})
    KGEs_Oct[it_seed,1:50]<-apply(data_testedOct,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minOctFlow7))})
    
    
    # KGEs_Aug[it_seed,1:50]<-apply(data_testedAug,2,FUN = function(x){KGE(x,log(data_tested_meas$minAugFlow7))})
    # KGEs_Sep[it_seed,1:50]<-apply(data_testedSep,2,FUN = function(x){KGE(x,log(data_tested_meas$minSepFlow7))})
    # KGEs_Oct[it_seed,1:50]<-apply(data_testedOct,2,FUN = function(x){KGE(x,log(data_tested_meas$minOctFlow7))})
    
    
  }
  
  
  
  KGEs_Aug<-apply(KGEs_Aug,2,mean)
  KGEs_Sep<-apply(KGEs_Sep,2,mean)
  KGEs_Oct<-apply(KGEs_Oct,2,mean)
  
  allAugModels[[it_stn]]<-combinationsAug
  allAugModels_KGEs_2[[it_stn]]<-KGEs_Aug
  allSepModels[[it_stn]]<-combinationsSep
  allSepModels_KGEs_2[[it_stn]]<-KGEs_Sep
  allOctModels[[it_stn]]<-combinationsOct
  allOctModels_KGEs_2[[it_stn]]<-KGEs_Oct
  
  AugustBestModel[[it_stn]]<-combinationsAug[which.max(KGEs_Aug)]
  SeptemberBestModel[[it_stn]]<-combinationsSep[which.max(KGEs_Sep)]
  OctoberBestModel[[it_stn]]<-combinationsOct[which.max(KGEs_Oct)]
  
  tictoc::toc()
  print(sprintf("Done %d catchments",it_stn))
}

names(AugustBestModel)<-stations$ID
names(SeptemberBestModel)<-stations$ID
names(OctoberBestModel)<-stations$ID





save(AugustBestModel,
     SeptemberBestModel,
     OctoberBestModel,
     file = "2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData"
)


## Analyse results
load("2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData")

stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")



# Use best models to cross-validate overall summer model

UsedCoefficientsAug<-data.frame(ID=stations$ID,
                                winterPrecip = NA,
                                meanWinterTemp =NA,
                                Precip_A = NA,
                                Precip_Jl = NA,
                                Temp_A =  NA,
                                Temp_Jl =  NA,
                                Precip_MJJA = NA,
                                Precip_JJA = NA,
                                Precip_JA = NA,
                                Temp_MJJA = NA,
                                Temp_JJA = NA,
                                Temp_JA = NA,
                                August.cms = NA
                                
)

UsedCoefficientsSep<-data.frame(ID=stations$ID,
                                winterPrecip = NA,
                                meanWinterTemp =NA,
                                Precip_S = NA,
                                Precip_A = NA,
                                Temp_S =  NA,
                                Temp_A =  NA,
                                Precip_MJJAS = NA,
                                Precip_JAS = NA,
                                Precip_AS = NA,
                                Temp_MJJAS = NA,
                                Temp_JAS = NA,
                                Temp_AS = NA,
                                September.cms = NA
                                
)

UsedCoefficientsOct<-data.frame(ID=stations$ID,
                                winterPrecip = NA,
                                meanWinterTemp =NA,
                                Precip_O = NA,
                                Precip_S = NA,
                                Temp_O =  NA,
                                Temp_S =  NA,
                                Precip_MJJASO = NA,
                                Precip_ASO = NA,
                                Precip_SO = NA,
                                Temp_MJJASO = NA,
                                Temp_ASO = NA,
                                Temp_SO = NA,
                                October.cms = NA
                                
)


for(it_stn in 71:length(stations$ID)){
  tictoc::tic()
  # it_stn = which(stations$ID=="08GA075")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$Discharge30 = zoo::rollmean(streamData$Discharge,30,fill = NA)
  
  streamData$DayOfYear<-lubridate::yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     minFlow30CP= min(Discharge30[month%in%c(7,8,9)]),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     MAD = mean(Discharge,na.rm = TRUE)
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
    streamDataYrly$minSumFlow<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  
  ## Weather data
  
  data<-WeatherData[WeatherData$StationNum == stations$ID[it_stn],]
  
  data$WaterYear<-data$Year
  data$WaterYear[data$Month%in%c(11,12)] <- data$WaterYear[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(WaterYear)%>%
    dplyr::summarize(winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     
                     Precip_O = sum(Total.Precip..mm.[Month%in%c(10)],na.rm = TRUE),
                     Precip_S = sum(Total.Precip..mm.[Month%in%c(9)],na.rm = TRUE),
                     Precip_A = sum(Total.Precip..mm.[Month%in%c(8)],na.rm = TRUE),
                     Precip_Jl = sum(Total.Precip..mm.[Month%in%c(7)],na.rm = TRUE),
                     Precip_Jn =sum(Total.Precip..mm.[Month%in%c(6)],na.rm = TRUE),
                     Precip_M =sum(Total.Precip..mm.[Month%in%c(5)],na.rm = TRUE),
                     
                     Temp_O =  mean(Mean.Temp..C.[Month%in%c(10)],na.rm = TRUE),
                     Temp_S =  mean(Mean.Temp..C.[Month%in%c(9)],na.rm = TRUE),
                     Temp_A =  mean(Mean.Temp..C.[Month%in%c(8)],na.rm = TRUE),
                     Temp_Jl =  mean(Mean.Temp..C.[Month%in%c(7)],na.rm = TRUE),
                     Temp_Jn =mean(Mean.Temp..C.[Month%in%c(6)],na.rm = TRUE),
                     Temp_M =mean(Mean.Temp..C.[Month%in%c(5)],na.rm = TRUE),
                     
                     #August
                     
                     # Precip_MJJ = sum(Total.Precip..mm.[Month%in%c(5,6,7)],na.rm = TRUE),
                     # Precip_JJ = sum(Total.Precip..mm.[Month%in%c(6,7)],na.rm = TRUE),
                     Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Precip_JJA = sum(Total.Precip..mm.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     # Temp_MJJ =  mean(Mean.Temp..C.[Month%in%c(5,6,7)],na.rm = TRUE),
                     # Temp_JJ =  mean(Mean.Temp..C.[Month%in%c(6,7)],na.rm = TRUE),
                     Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),
                     Temp_JJA =  mean(Mean.Temp..C.[Month%in%c(6,7,8)],na.rm = TRUE),
                     Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),
                     
                     
                     # September
                     
                     Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Precip_JJAS = sum(Total.Precip..mm.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Precip_JAS = sum(Total.Precip..mm.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
                     # Temp_JJAS =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9)],na.rm = TRUE),
                     Temp_JAS =  mean(Mean.Temp..C.[Month%in%c(7,8,9)],na.rm = TRUE),
                     Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),
                     
                     # October
                     
                     Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JJASO = sum(Total.Precip..mm.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Precip_JASO = sum(Total.Precip..mm.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Precip_ASO = sum(Total.Precip..mm.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JJASO =  mean(Mean.Temp..C.[Month%in%c(6,7,8,9,10)],na.rm = TRUE),
                     # Temp_JASO =  mean(Mean.Temp..C.[Month%in%c(7,8,9,10)],na.rm = TRUE),
                     Temp_ASO =  mean(Mean.Temp..C.[Month%in%c(8,9,10)],na.rm = TRUE),
                     Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),
                     
                     
                     
    )
  
  
  
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
  dataYearly2<-dataYearly2[dataYearly$WaterYear<2024,]
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))
  Div<-Div[,c("year","Total.cms","August.cms","September.cms","October.cms")]
  
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  
  
  
  dataYearly2<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  
  tempRes<-data.frame(it_seed = 1:10)
  for(it_seed in 1:10){
    set.seed(it_seed+100)
    
    
    data_tested<-data.frame()
    sets<-sample(1:nrow(dataYearly2)%%5+1,nrow(dataYearly2),replace = FALSE)
    
    for(it_cv in 1:5){
      # Divide training and testing data
      dataYearly2_tr<-dataYearly2[-which(sets%in%it_cv),]
      dataYearly2_te<-dataYearly2[which(sets%in%it_cv),]
      
      
      dataYearly2_trA<-dataYearly2_tr[!is.na(dataYearly2_tr$minAugFlow7),]
      
      dataYearly2_trS<-dataYearly2_tr[!is.na(dataYearly2_tr$minSepFlow7),]
      
      dataYearly2_trO<-dataYearly2_tr[!is.na(dataYearly2_tr$minOctFlow7),]
      
      
      
      modelAug<-eval(AugustBestModel[[stations$ID[it_stn]]][[1]])
      modelAug<-lm(log(minAugFlow7)~
                     winterPrecip+meanWinterTemp+
                     Precip_MJJA+Precip_JA+
                     Temp_MJJA+Temp_JA+
                     August.cms,
                   data= dataYearly2_trO,
                   na.action = "na.omit")
      
      
      if(!any(dataYearly2$August.cms>dataYearly2$minAugFlow7*0.05,na.rm = TRUE)){
        modelAug<-update(modelAug,.~.-August.cms)
      } 
      
      
      
      modelSep<-eval(SeptemberBestModel[[stations$ID[it_stn]]][[1]])
      modelSep<-lm(log(minSepFlow7)~
                     winterPrecip+meanWinterTemp+
                     Precip_MJJAS+Precip_AS+
                     Temp_MJJAS+Temp_AS+
                     September.cms,
                   data= dataYearly2_trO,
                   na.action = "na.omit")
      
      if(!any(dataYearly2$September.cms>dataYearly2$minSepFlow7*0.05,na.rm = TRUE)){
        modelSep<-update(modelSep,.~.-September.cms)
      }
      
      dataYearly2_te$minAugFlow7_pr<-predict(modelAug,dataYearly2_te)
      dataYearly2_te$minSepFlow7_pr<-predict(modelSep,dataYearly2_te)
      
      if(!stations$ID[it_stn]%in%c("08OA004","08OA005")){
        modelOct<-eval(OctoberBestModel[[stations$ID[it_stn]]][[1]])
        
        modelOct<-lm(log(minOctFlow7)~
                       winterPrecip+meanWinterTemp+
                       Precip_MJJASO+Precip_SO+
                       Temp_MJJASO+Temp_SO+
                       October.cms,
                     data= dataYearly2_trO,
                     na.action = "na.omit")
        if(!any(dataYearly2$October.cms>dataYearly2$minOctFlow7*0.05,na.rm = TRUE)){
          modelOct<-update(modelOct,.~.-October.cms)
        }
        
        dataYearly2_te$minOctFlow7_pr<-predict(modelOct,dataYearly2_te)
      }else{
        dataYearly2_te$minOctFlow7_pr<-NA_integer_
      }
      
      
      
      data_tested<-rbind(data_tested,dataYearly2_te)
      summary(modelOct)
    }
    
    ggplot(data_tested)+geom_point(aes(x = sqrt(exp(data_tested$minOctFlow7_pr)),y =sqrt(data_tested$minOctFlow7) ))
    data_tested$minSumFlow_pr_log<-pmin(data_tested$minAugFlow7_pr,data_tested$minSepFlow7_pr,data_tested$minOctFlow7_pr,na.rm = TRUE)
    
    if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
      data_tested$minSumFlow_pr_log<-pmin(data_tested$minAugFlow7_pr,data_tested$minSepFlow7_pr,na.rm = TRUE)
      tempRes$KGE_Oct[it_seed]<-NA
      tempRes$NSE_Oct[it_seed]<-NA
      tempRes$R2_Oct[it_seed]<-NA
    }else{
      tempRes$KGE_Oct[it_seed]<-KGE(sqrt(exp(data_tested$minOctFlow7_pr)),sqrt(data_tested$minOctFlow7))
      tempRes$NSE_Oct[it_seed]<-NSE(data_tested$minOctFlow7_pr,log(data_tested$minOctFlow7))
      tempRes$R2_Oct[it_seed]<-cor(data_tested$minOctFlow7_pr,log(data_tested$minOctFlow7),use = "complete.obs")^2
      
    }
    
    tempRes$KGE_Summer[it_seed]<-KGE(sqrt(exp(data_tested$minSumFlow_pr_log)),sqrt(data_tested$minSumFlow))
    tempRes$KGE_Aug[it_seed]<-KGE(sqrt(exp(data_tested$minAugFlow7_pr)),sqrt(data_tested$minAugFlow7))
    tempRes$KGE_Sep[it_seed]<-KGE(sqrt(exp(data_tested$minSepFlow7_pr)),sqrt(data_tested$minSepFlow7))
    
    
    tempRes$NSE_Summer[it_seed]<-NSE(data_tested$minSumFlow_pr_log,log(data_tested$minSumFlow))
    tempRes$NSE_Aug[it_seed]<-NSE(data_tested$minAugFlow7_pr,log(data_tested$minAugFlow7))
    tempRes$NSE_Sep[it_seed]<-NSE(data_tested$minSepFlow7_pr,log(data_tested$minSepFlow7))
    
    
    tempRes$R2_Summer[it_seed]<-cor(data_tested$minSumFlow_pr_log,log(data_tested$minSumFlow),use = "complete.obs")^2
    tempRes$R2_Aug[it_seed]<-cor(data_tested$minAugFlow7_pr,log(data_tested$minAugFlow7),use = "complete.obs")^2
    tempRes$R2_Sep[it_seed]<-cor(data_tested$minSepFlow7_pr,log(data_tested$minSepFlow7),use = "complete.obs")^2
    
    
  }
  stations$KGE_Summer[it_stn]<-mean(tempRes$KGE_Summer)
  stations$KGE_Aug[it_stn]<-mean(tempRes$KGE_Aug)
  stations$KGE_Sep[it_stn]<-mean(tempRes$KGE_Sep)
  stations$KGE_Oct[it_stn]<-mean(tempRes$KGE_Oct)
  
  stations$NSE_Summer[it_stn]<-mean(tempRes$NSE_Summer)
  stations$NSE_Aug[it_stn]<-mean(tempRes$NSE_Aug)
  stations$NSE_Sep[it_stn]<-mean(tempRes$NSE_Sep)
  stations$NSE_Oct[it_stn]<-mean(tempRes$NSE_Oct)
  
  stations$R2_Summer[it_stn]<-mean(tempRes$R2_Summer)
  stations$R2_Aug[it_stn]<-mean(tempRes$R2_Aug)
  stations$R2_Sep[it_stn]<-mean(tempRes$R2_Sep)
  stations$R2_Oct[it_stn]<-mean(tempRes$R2_Oct)
  
  
  
  modelAug<-eval(AugustBestModel[[stations$ID[it_stn]]][[1]])%>%
    update(data= dataYearly2,na.action = "na.omit")
  modelAug<-eval(modelAug)%>%
    update(data= dataYearly2,na.action = "na.omit")
  
  # x<-coef(modelAug)
  x<-anova(modelAug)
  UsedCoefficientsAug[it_stn,rownames(x[-which(rownames(x)=="Residuals"),])]<-
    x$`Pr(>F)`[-which(rownames(x)=="Residuals")]
  
  modelSep<-eval(SeptemberBestModel[[stations$ID[it_stn]]][[1]])%>%
    update(data= dataYearly2,na.action = "na.omit")
  modelSep<-eval(modelSep)%>%
    update(data= dataYearly2,na.action = "na.omit")
  # x<-coef(modelAug)
  x<-anova(modelSep)
  UsedCoefficientsSep[it_stn,rownames(x[-which(rownames(x)=="Residuals"),])]<-
    x$`Pr(>F)`[-which(rownames(x)=="Residuals")]
  
  modelOct<-eval(OctoberBestModel[[stations$ID[it_stn]]][[1]])%>%
    update(data= dataYearly2,na.action = "na.omit")
  modelOct<-eval(modelOct)%>%
    update(data= dataYearly2,na.action = "na.omit")
  
  # x<-coef(modelAug)
  x<-anova(modelOct)
  UsedCoefficientsOct[it_stn,rownames(x[-which(rownames(x)=="Residuals"),])]<-
    x$`Pr(>F)`[-which(rownames(x)=="Residuals")]
  
  stations$R2_Summer_ip[it_stn]<-cor(pmin(predict(modelAug,dataYearly2),
                                          predict(modelSep,dataYearly2),
                                          predict(modelOct,dataYearly2)),
                                     log(dataYearly2$minSumFlow),use = "complete.obs")^2
  
  stations$R2_Aug_ip[it_stn]<-cor(predict(modelAug,dataYearly2),log(dataYearly2$minAugFlow7),use = "complete.obs")^2
  stations$R2_Sep_ip[it_stn]<-cor(predict(modelSep,dataYearly2),log(dataYearly2$minSepFlow7),use = "complete.obs")^2
  # stations$R2_Oct_ip[it_stn]<-cor(predict(modelOct,dataYearly2),log(dataYearly2$minOctFlow7),use = "complete.obs")^2
  
  if(!stations$ID[it_stn]%in%c("08OA004","08OA005")){
    stations$R2_Oct_ip[it_stn]<-cor(predict(modelOct,dataYearly2),log(dataYearly2$minOctFlow7),use = "complete.obs")^2    
  }else{
    stations$R2_Oct_ip[it_stn]<-NA
    stations$R2_Summer_ip[it_stn]<-cor(pmin(predict(modelAug,dataYearly2),
                                            predict(modelSep,dataYearly2),
                                            predict(modelOct,dataYearly2)),
                                       log(dataYearly2$minSumFlow),use = "complete.obs")^2
  }
  
  stations$nAug[it_stn]<-sum(!is.na(dataYearly2$minAugFlow7))
  stations$nSep[it_stn]<-sum(!is.na(dataYearly2$minSepFlow7))
  stations$nOct[it_stn]<-sum(!is.na(dataYearly2$minOctFlow7))
  
}

write.csv(stations,"2.data/2.working/StationMetadata/stations_performance_fixedModels.csv",row.names = FALSE)

# stations$R2_Aug_ip[129]<-0

UsedCoefficientsAug$SummerPrecipAny<-apply(UsedCoefficientsAug[,c("Precip_A",
                                                                  "Precip_Jl",
                                                                  "Precip_JA",
                                                                  "Precip_JJA",
                                                                  "Precip_MJJA")],1,min,na.rm = TRUE)
UsedCoefficientsAug$SummerPrecipAny[is.infinite(UsedCoefficientsAug$SummerPrecipAny)]<-NA

UsedCoefficientsAug$SummerTempAny<-apply(UsedCoefficientsAug[,c("Temp_A",
                                                                "Temp_Jl",
                                                                "Temp_JA",
                                                                "Temp_JJA",
                                                                "Temp_MJJA")],1,min,na.rm = TRUE)
UsedCoefficientsAug$SummerTempAny[is.infinite(UsedCoefficientsAug$SummerTempAny)]<-NA



UsedCoefficientsSep$SummerPrecipAny<-apply(UsedCoefficientsSep[,c("Precip_S",
                                                                  "Precip_A",
                                                                  "Precip_AS",
                                                                  "Precip_JAS",
                                                                  "Precip_MJJAS")],1,min,na.rm = TRUE)
UsedCoefficientsSep$SummerPrecipAny[is.infinite(UsedCoefficientsSep$SummerPrecipAny)]<-NA

UsedCoefficientsSep$SummerTempAny<-apply(UsedCoefficientsSep[,c("Temp_S",
                                                                "Temp_A",
                                                                "Temp_AS",
                                                                "Temp_JAS",
                                                                "Temp_MJJAS")],1,min,na.rm = TRUE)
UsedCoefficientsSep$SummerTempAny[is.infinite(UsedCoefficientsSep$SummerTempAny)]<-NA


UsedCoefficientsOct$SummerPrecipAny<-apply(UsedCoefficientsOct[,c("Precip_O",
                                                                  "Precip_S",
                                                                  "Precip_SO",
                                                                  "Precip_ASO",
                                                                  "Precip_MJJASO")],1,min,na.rm = TRUE)
UsedCoefficientsOct$SummerPrecipAny[is.infinite(UsedCoefficientsOct$SummerPrecipAny)]<-NA

UsedCoefficientsOct$SummerTempAny<-apply(UsedCoefficientsOct[,c("Temp_O",
                                                                "Temp_S",
                                                                "Temp_SO",
                                                                "Temp_ASO",
                                                                "Temp_MJJASO")],1,min,na.rm = TRUE)


UsedCoefficientsOct$SummerTempAny[is.infinite(UsedCoefficientsOct$SummerTempAny)]<-NA

d1<-UsedCoefficientsAug%>%
  summarise_if(is.numeric,  function(x){sum(!is.na(x))})
d1$month = "August"
d1$var = "all"
d2<-UsedCoefficientsAug%>%
  summarise_if(is.numeric,  function(x){sum(x<0.05,na.rm = TRUE)})
d2$month = "August"
d2$var = "p<0.05"

d1.2<-rbind(d1,d2)%>%
  rename("Precip_k0" = "Precip_A",
         "Precip_k1" = "Precip_Jl",
         "Temp_k0" = "Temp_A",
         "Temp_k1" = "Temp_Jl",
         "Precip_summer" = "Precip_MJJA",
         "Precip_k2..k0" = "Precip_JJA",
         "Precip_k1..k0" = "Precip_JA",
         "Temp_summer" = "Temp_MJJA",
         "Temp_k2..k0" = "Temp_JJA",
         "Temp_k1..k0" = "Temp_JA",
         "lcd_SW_div" = "August.cms"
  )


d3<-UsedCoefficientsSep%>%
  summarise_if(is.numeric,  function(x){sum(!is.na(x))})
d3$month = "September"
d3$var = "all"
d4<-UsedCoefficientsSep%>%
  summarise_if(is.numeric,  function(x){sum(x<0.05,na.rm = TRUE)})
d4$month = "September"
d4$var = "p<0.05"

d3.4<-rbind(d3,d4)%>%
  rename("Precip_k0" = "Precip_S",
         "Precip_k1" = "Precip_A",
         "Temp_k0" = "Temp_S",
         "Temp_k1" = "Temp_A",
         "Precip_summer" = "Precip_MJJAS",
         "Precip_k2..k0" = "Precip_JAS",
         "Precip_k1..k0" = "Precip_AS",
         "Temp_summer" = "Temp_MJJAS",
         "Temp_k2..k0" = "Temp_JAS",
         "Temp_k1..k0" = "Temp_AS",
         "lcd_SW_div" = "September.cms"
  )

d5<-UsedCoefficientsOct%>%
  summarise_if(is.numeric,  function(x){sum(!is.na(x))})
d5$month = "October"
d5$var = "all"
d6<-UsedCoefficientsOct%>%
  summarise_if(is.numeric,  function(x){sum(x<0.05,na.rm = TRUE)})
d6$month = "October"
d6$var = "p<0.05"


d5.6<-rbind(d5,d6)%>%
  rename("Precip_k0" = "Precip_O",
         "Precip_k1" = "Precip_S",
         "Temp_k0" = "Temp_O",
         "Temp_k1" = "Temp_S",
         "Precip_summer" = "Precip_MJJASO",
         "Precip_k2..k0" = "Precip_ASO",
         "Precip_k1..k0" = "Precip_SO",
         "Temp_summer" = "Temp_MJJASO",
         "Temp_k2..k0" = "Temp_ASO",
         "Temp_k1..k0" = "Temp_SO",
         "lcd_SW_div" = "October.cms"
  )


variableInclusion<-rbind(d1.2,d3.4,d5.6)%>%
  reshape2::melt(id.vars = c("month","var"))
variableInclusion$variable<-factor(variableInclusion$variable,
                                   levels = c(
                                     "meanWinterTemp",
                                     "SummerTempAny",
                                     "Temp_summer",
                                     "Temp_k2..k0",
                                     "Temp_k1..k0",
                                     "Temp_k1",
                                     "Temp_k0",
                                     
                                     "winterPrecip",
                                     "SummerPrecipAny",
                                     "Precip_summer",
                                     "Precip_k2..k0",
                                     "Precip_k1..k0",
                                     "Precip_k1",
                                     "Precip_k0",
                                     "lcd_SW_div"
                                     
                                   ),
                                   ordered = TRUE
)

variableInclusion$month<-factor(variableInclusion$month,levels = c("October","September","August"),
                                ordered = TRUE)

ggplot(variableInclusion, aes(x = variable,y = value,fill = month))+
  geom_col(data = . %>% filter( var=="p<0.05"),position = "dodge", alpha = 1)+
  geom_col(data = . %>% filter( var=="all"),position = "dodge", alpha = 0.4)+
  geom_tile(aes(y=NA_integer_, alpha = factor(var))) + 
  scale_alpha_manual(values = c(1,0.7),name = "Significance",labels = c("p<0.05","p≥0.05"))+
  scale_fill_brewer(name = "Month",
                    palette = "Dark2",
                    guide = guide_legend(reverse = TRUE) )+
  geom_hline(aes(yintercept =172),col = "grey",linewidth = 1)+
  coord_flip()+
  scale_x_discrete(limits = rev,
                   name = NULL,
                   labels = c(expression(SW.Abs[k]),
                              expression(P[k]),
                              expression(P[k-1]),
                              expression(P["[k-1,k]"]),
                              expression(P["[k-2,k]"]),
                              expression(P[summer]),
                              expression(P[any~summer~var.]),
                              expression(P[winter]),
                              expression(T[k]),
                              expression(T[k-1]),
                              expression(T["[k-1,k]"]),
                              expression(T["[k-2,k]"]),
                              expression(T[summer]),
                              expression(T[any~summer~var.]),
                              expression(T[winter])))+
  
  scale_y_continuous(name = "# Catchment Models Including Variable")+
  theme_bw()

ggsave("Figures/SelectedVariables.png",width = 6,height = 4.5,dpi = 600)

hist(rowSums(!is.na(UsedCoefficientsAug[,2:14])))
quantile(rowSums(!is.na(UsedCoefficientsAug[,2:14])))
hist(rowSums(!is.na(UsedCoefficientsSep[,2:14])))
quantile(rowSums(!is.na(UsedCoefficientsSep[,2:14])))
hist(rowSums(!is.na(UsedCoefficientsOct[,2:14])))
quantile(rowSums(!is.na(UsedCoefficientsOct[,2:14])))

watersheds_KGE2<-left_join(watersheds,stations[,c("ID",
                                                  "KGE_Summer","KGE_Aug","KGE_Sep","KGE_Oct",
                                                  "NSE_Summer","NSE_Aug","NSE_Sep","NSE_Oct",
                                                  "R2_Summer","R2_Aug","R2_Sep","R2_Oct",
                                                  "R2_Summer_ip","R2_Aug_ip","R2_Sep_ip","R2_Oct_ip")],
                           by = c("StationNum" = "ID"))

ggplot(watersheds_KGE2,aes(KGE_Summer))+geom_histogram()
tmap_mode("view")
tm_shape(watersheds_KGE2)+tm_polygons(alpha = 1,col = "KGE_Summer",
                                      breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                                      palette = "RdPu",
                                      # palette = scico(10,palette = "batlow",direction = -1),
                                      title = "KGE (Summer)")

summarizeFunc<-function(x){
  medx<-median(x,na.rm = TRUE)%>%round(2)
  minx = min(x,na.rm = TRUE)%>%round(2)
  maxx = max(x,na.rm = TRUE)%>%round(2)
  return(paste0(medx," (",minx,", ",maxx,")"))
}
stations$regime<-factor(stations$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"))
summaryTable<-stations%>%
  dplyr::select(Station.Name,ID,regime, Dates,KGE_Summer:R2_Oct_ip)%>%
  group_by(regime)%>%
  summarise(across(KGE_Summer:R2_Oct_ip,~summarizeFunc(.x)))%>%
  melt(id.vars = "regime")
summaryTable$stat<-paste0(str_split_fixed(summaryTable$variable%>%as.character(),"_",n = 3)[,1],
                          str_split_fixed(summaryTable$variable%>%as.character(),"_",n = 3)[,3])
summaryTable$month<-str_split_fixed(summaryTable$variable%>%as.character(),"_",n = 3)[,2]

summaryTable2<-summaryTable%>%
  tidyr::pivot_wider(id_cols = c(regime,stat),
                     names_from = month,
                     values_from = value)

quantile(stations$KGE_Aug)
quantile(stations$KGE_Sep)
quantile(stations$KGE_Oct,na.rm = TRUE)
quantile(stations$KGE_Summer)

quantile(stations$NSE_Aug)
quantile(stations$NSE_Sep)
quantile(stations$NSE_Oct,na.rm = TRUE)
quantile(stations$NSE_Summer)


quantile(stations$R2_Aug)
quantile(stations$R2_Sep)
quantile(stations$R2_Oct,na.rm = TRUE)
quantile(stations$R2_Summer)

quantile(stations$R2_Aug_ip,na.rm = TRUE)
quantile(stations$R2_Sep_ip)
quantile(stations$R2_Oct_ip,na.rm = TRUE)
quantile(stations$R2_Summer_ip)



closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(rgdal)
library(terra)
library(sp)
library(sf)
library(tmap)
library(scico)
library(raster)
library(zyp)
library(stringr)
library(sp)
library(scico)
library(reshape2)
library(zoo)
library(hydroGOF)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/data/BC_watersheds/")) #Set the working directory

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_wSnow.gpkg")
stations<-read.csv("2.data/2.working/StationMetadata/stations_final_wSnow.csv",fileEncoding = "UTF-8-BOM")
stations<-stations[stations$ID%in%watersheds$StationNum,]
# load("Output/RegressionOptimization_small.RData")

# names(AugustBestModel)<-stations$ID
# names(SeptemberBestModel)<-stations$ID
# names(OctoberBestModel)<-stations$ID

streamDataAll<-read.csv("2.data/2.working/StreamflowData/streamDataFinal_wSnow.csv")

pcic_models<-list.files("PCIC_station_hydro_model_out/",pattern = ".ascii")%>%str_remove(".ascii")
pcic_models<-pcic_models[tolower(pcic_models)%in%tolower(watersheds$NameNom)]

watersheds<-watersheds[tolower(watersheds$NameNom)%in%tolower(pcic_models),]
stations<-stations[stations$ID%in%watersheds$StationNum,]





# WeatherData<-read.csv("WeatherDataANUSPLIN/dataMonthly.csv")
WeatherData<-read.csv("WeatherDataPWNAmet/dataMonthly.csv")

for(it in 1:56){
  streamData<-streamDataAll[streamDataAll$ID==watersheds$StationNum[it],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     # minFlow = min(Discharge,na.rm = TRUE),
                     # minFlow7 = min(Discharge7,na.rm = TRUE),
                     
                     # minAugFlow = min(Discharge[month==8],na.rm = TRUE),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE)
    )
  
  
  sum(!is.na(streamDataYrly$minAugFlow7))
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  # streamDataYrly$minSumFlow[is.na(streamDataYrly$minAugFlow7)|is.na(streamDataYrly$minSepFlow7)]<-NA
  
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  
  
  
  sum(tolower(watersheds$NameNom)%in%tolower(pcic_models))
  # pcic_models[tolower(pcic_models)%in%tolower(watersheds$NameNom)]
  
  PCIC_model<-read.csv(paste0("PCIC_station_hydro_model_out/",watersheds$NameNom[it],".ascii"),skip = 1)
  PCIC_model$Date<-ymd(PCIC_model$Date)
  PCIC_model$month = month(PCIC_model$Date)
  
  PCIC_model$year = year(PCIC_model$Date)
  PCIC_model$Discharge<-PCIC_model$PNWNAmet
  PCIC_model$Discharge7<-zoo::rollmean(PCIC_model$PNWNAmet,k=7,align = "right",fill = NA)
  PCIC_model$Discharge7Summer<-PCIC_model$Discharge7
  PCIC_model$Discharge7Summer[!PCIC_model$month%in%c(8,9,10)]<-NA
  
  
  
  
  PCIC_modelYrly<-PCIC_model%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     # minFlow = min(Discharge,na.rm = TRUE),
                     # minFlow7 = min(Discharge7,na.rm = TRUE),
                     
                     # minAugFlow = min(Discharge[month==8],na.rm = TRUE),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE)
    )
  
  PCIC_modelYrly$minAugFlow7[is.infinite(PCIC_modelYrly$minAugFlow7)]<-NA
  PCIC_modelYrly$minSepFlow7[is.infinite(PCIC_modelYrly$minSepFlow7)]<-NA
  PCIC_modelYrly$minOctFlow7[is.infinite(PCIC_modelYrly$minOctFlow7)]<-NA
  PCIC_modelYrly$minSumFlow[is.infinite(PCIC_modelYrly$minSumFlow)]<-NA
  data<-inner_join(streamDataYrly,PCIC_modelYrly,by = "year")
  
  
  # ggplot(data,aes(minSumFlow.x,minSumFlow.y))+geom_point()
  
  watersheds$PCIC_R2[it]<-cor(log(data$minSumFlow.x),log(data$minSumFlow.y),use = "complete")^2
  watersheds$PCIC_R2Aug[it]<-cor(log(data$minAugFlow7.x),log(data$minAugFlow7.y),use = "complete")^2
  watersheds$PCIC_R2Sep[it]<-cor(log(data$minSepFlow7.x),log(data$minSepFlow7.y),use = "complete")^2
  watersheds$PCIC_R2Oct[it]<-cor(log(data$minOctFlow7.x),log(data$minOctFlow7.y),use = "complete")^2
  
  watersheds$PCIC_RMSE[it]<-(data$minSumFlow.x-data$minSumFlow.y)^2%>%
    mean(na.rm = TRUE)%>%
    sqrt()
  
  watersheds$PCIC_KGE_sqrt[it]<-KGE(sqrt(data$minSumFlow.y),sqrt(data$minSumFlow.x))
  watersheds$PCIC_NSE_log[it]<-NSE(log(data$minSumFlow.y),log(data$minSumFlow.x))
  watersheds$PCIC_KGE[it]<-KGE((data$minSumFlow.y),(data$minSumFlow.x))
  watersheds$PCIC_NSE[it]<-NSE((data$minSumFlow.y),(data$minSumFlow.x))
  
}

tmap_mode("view")
tm_shape(watersheds)+tm_polygons(alpha = 1,col = "PCIC_R2",
                                 breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),
                                 # palette = "RdPu",
                                 palette = scico(10,palette = "acton",direction = -1),
                                 title = expression("PCIC Multiple Regression R"^2))
median(watersheds$PCIC_R2)
mean(watersheds$PCIC_R2)


PCIC_R2<- watersheds[,c("NameNom","StationNum","PCIC_R2","PCIC_R2Aug","PCIC_R2Sep","PCIC_R2Oct","PCIC_RMSE","PCIC_KGE","PCIC_NSE","PCIC_KGE_sqrt","PCIC_NSE_log")]%>%
  st_drop_geometry()
write.csv(PCIC_R2,"PCIC_station_hydro_model_out/PCIC_R2.csv")

PCIC_R2<-read.csv("PCIC_station_hydro_model_out/PCIC_R2.csv")

stations$RMSE<-NA

stations$KGE<-NA

stations$NSE<-NA

load("2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData")


for(it_stn in 1:length(stations$ID)){
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     # minFlow = min(Discharge,na.rm = TRUE),
                     # minFlow7 = min(Discharge7,na.rm = TRUE),
                     # minFlow30CP= min(Discharge30[month%in%c(7,8,9)]),
                     # minDate = Date[which.min(Discharge)],
                     # minAugFlow = min(Discharge[month==8],na.rm = TRUE),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     # minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     # minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     # lowFlowStandard = 0.02*mean(Discharge),
                     # MAD = mean(Discharge,na.rm = TRUE)
                     
    )
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  # streamDataYrly$minSumFlowDay[streamDataYrly$numQDaysSum<77]<-NA
  
  fullYearMask<-streamData$year%in%streamDataYrly$year[streamDataYrly$numQnotNAN>=350]&
    streamData$year>2000
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  
  data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
  
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
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/",stations$ID[it_stn],".csv"))
  
  # numWells<-read.csv(paste0("wellRecords/",stations$StationNum[it_stn],".csv"))
  # names(numWells)[2]<-"constructedWells"
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  
  
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  dataYearly2_trA<-dataYearly2%>%
    filter(!is.na(minAugFlow7)&!is.na(meanWinterTemp)&!is.na(Temp_MJJA))
  dataYearly2_trS<-dataYearly2%>%
    filter(!is.na(minSepFlow7)&!is.na(meanWinterTemp)&!is.na(Temp_MJJAS))
  dataYearly2_trO<-dataYearly2%>%
    filter(!is.na(minOctFlow7)&!is.na(meanWinterTemp)&!is.na(Temp_MJJASO))
  
  
  
  my_lmAug<-eval(AugustBestModel[[stations$ID[it_stn]]][[1]])
  
  my_lmSep<-eval(SeptemberBestModel[[stations$ID[it_stn]]][[1]])
  
  my_lmOct<-eval(OctoberBestModel[[stations$ID[it_stn]]][[1]])
  
  
  
  predMin<-pmin(predict(my_lmAug,dataYearly2,se.fit = TRUE)$fit,
                predict(my_lmSep,dataYearly2,se.fit = TRUE)$fit,
                predict(my_lmOct,dataYearly2,se.fit = TRUE)$fit
  )
  
  measMin<-dataYearly2$minSumFlow%>%log()
  
  cvDat<-data.frame(predMin = predMin,
                    measMin = measMin)
  cvDat<-cvDat[rowSums(is.na(cvDat))==0,]
  
  stations$RMSE[it_stn]<-(exp(cvDat$predMin)-exp(cvDat$measMin))^2%>%
    mean(na.rm = TRUE)%>%
    sqrt()  
  
  stations$KGE[it_stn]<-KGE(exp(cvDat$predMin),exp(cvDat$measMin))
  
  stations$NSE[it_stn]<-NSE(exp(cvDat$predMin),exp(cvDat$measMin))
  
  stations$KGE_sqrt[it_stn]<-KGE(sqrt(exp(cvDat$predMin)),sqrt(exp(cvDat$measMin)))
  
  stations$NSE_log[it_stn]<-NSE(cvDat$predMin,cvDat$measMin)
  
}

PCIC_compare<-left_join(stations[,c("Station.Name","ID","regime","RMSE","KGE","NSE","KGE_sqrt","NSE_log")],
                        PCIC_R2[,c("StationNum","PCIC_RMSE","PCIC_KGE","PCIC_NSE","PCIC_KGE_sqrt","PCIC_NSE_log")],
                        by = c("ID" = "StationNum"))

ggplot(PCIC_compare,aes(KGE,PCIC_KGE,color = regime))+geom_point()+geom_abline()
ggplot(PCIC_compare,aes(NSE_log,PCIC_NSE_log,color = regime))+geom_point()+geom_abline()
ggplot(PCIC_compare,aes(NSE,PCIC_NSE,color = regime))+geom_point()+geom_abline()

ggplot(PCIC_compare,aes(RMSE,PCIC_RMSE))+geom_point()+geom_abline()

sum(PCIC_compare$KGE>PCIC_compare$PCIC_KGE) # KGE is better in 52/56 catchments
sum((PCIC_compare$KGE>PCIC_compare$PCIC_KGE)[!PCIC_compare$regime%in%"Glacial"]) # KGE is better in 44/47 nonglacial catchments
sum(!PCIC_compare$regime%in%"Glacial")
sum((PCIC_compare$KGE>PCIC_compare$PCIC_KGE)[PCIC_compare$regime%in%"Glacial"]) # KGE is better in 8/9 glacial catchments
sum(PCIC_compare$regime%in%"Glacial")


sum(PCIC_compare$KGE_sqrt>PCIC_compare$PCIC_KGE_sqrt) # KGE is better in 47/56 catchments

ggplot(PCIC_compare,aes(KGE_sqrt,PCIC_KGE_sqrt))+geom_point()+geom_abline()

sum(PCIC_compare$RMSE<PCIC_compare$PCIC_RMSE)
sum(PCIC_compare$NSE>PCIC_compare$PCIC_NSE) #NSE is better in 55/56 catchments
sum(PCIC_compare$NSE<PCIC_compare$PCIC_NSE) #NSE is better in 55/56 catchments
sum(PCIC_compare$NSE_log>PCIC_compare$PCIC_NSE_log) #NSE is better in 55/56 catchments



PCIC_compare$RMSE_ratio<-PCIC_compare$PCIC_RMSE/PCIC_compare$RMSE
PCIC_compare$RMSE_ratio<-PCIC_compare$PCIC_RMSE/PCIC_compare$RMSE


sum(PCIC_compare$PCIC_KGE< -0.41)
sum(PCIC_compare$PCIC_NSE<0)
sum(PCIC_compare$PCIC_NSE_log<0)
sum(PCIC_compare$NSE<0)
min(PCIC_compare$KGE)

quantile(PCIC_compare$KGE)
quantile(PCIC_compare$PCIC_KGE)
quantile(PCIC_compare$NSE)
quantile(PCIC_compare$PCIC_NSE)

quantile(PCIC_compare$KGE_sqrt)
quantile(PCIC_compare$PCIC_KGE_sqrt)
quantile(PCIC_compare$NSE_log)
quantile(PCIC_compare$PCIC_NSE_log)

sum(PCIC_compare$NSE_log<0)
sum(PCIC_compare$PCIC_NSE_log<0)


summarizeFunc<-function(x){
  medx<-median(x,na.rm = TRUE)%>%round(2)
  minx = min(x,na.rm = TRUE)%>%round(2)
  maxx = max(x,na.rm = TRUE)%>%round(2)
  return(paste0(medx," (",minx,", ",maxx,")"))
}
PCIC_compare$regime<-factor(PCIC_compare$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"))
summaryTable<-PCIC_compare%>%
  dplyr::select(Station.Name,ID,regime,KGE:NSE_log,PCIC_KGE:PCIC_NSE_log)%>%
  # group_by(regime)%>%
  summarise(across(c(KGE:NSE_log,PCIC_KGE:PCIC_NSE_log),~summarizeFunc(.x)))%>%
  # melt(id.vars = "regime")
  melt(id.vars = NULL)

summaryTable$stat<-str_remove(summaryTable$variable,"PCIC_")
summaryTable$model<-str_detect(summaryTable$variable,"PCIC")%>%
  plyr::mapvalues(from = c(0,1),to = c("Regression","VIC-GL"))

summaryTable2<-summaryTable%>%
  tidyr::pivot_wider(id_cols = c(stat),
                     names_from = model,
                     values_from = value)

summary(PCIC_compare$regime)


stations_cv<-read.csv("2.data/2.working/StationMetadata/stations_performance.csv")

PCIC_compare_2<-inner_join(PCIC_R2%>%dplyr::select(NameNom,StationNum,PCIC_R2,PCIC_KGE_sqrt,PCIC_NSE_log),
                           stations_cv%>%dplyr::select(ID,R2_Summer,KGE_Summer,NSE_Summer),
                           by = c("StationNum" = "ID"))

sum(PCIC_compare_2$KGE_Summer>PCIC_compare_2$PCIC_KGE_sqrt)
sum(PCIC_compare_2$NSE_Summer>PCIC_compare_2$PCIC_NSE_log)

## Compare ANUSPLIN and PWNAmet
WeatherData<-read.csv("WeatherDataANUSPLIN/dataMonthly.csv")
WeatherData<-filter(WeatherData,Year>=1945&Year<=2012)
stations2<-stations


stations2$RMSE<-NA

stations2$KGE<-NA

stations2$NSE<-NA


for(it_stn in 1:length(stations2$ID)){
  streamData<-streamDataAll[streamDataAll$ID==stations2$ID[it_stn],]
  
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     # minFlow = min(Discharge,na.rm = TRUE),
                     # minFlow7 = min(Discharge7,na.rm = TRUE),
                     # minFlow30CP= min(Discharge30[month%in%c(7,8,9)]),
                     # minDate = Date[which.min(Discharge)],
                     # minAugFlow = min(Discharge[month==8],na.rm = TRUE),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     # minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     # minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     # lowFlowStandard = 0.02*mean(Discharge),
                     # MAD = mean(Discharge,na.rm = TRUE)
                     
    )
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  # streamDataYrly$minSumFlowDay[streamDataYrly$numQDaysSum<77]<-NA
  
  fullYearMask<-streamData$year%in%streamDataYrly$year[streamDataYrly$numQnotNAN>=350]&
    streamData$year>2000
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  
  data<-WeatherData[WeatherData$StationNum == stations2$ID[it_stn],]
  
  data$WaterYear<-data$Year
  data$WaterYear[data$Month%in%c(11,12)] <- data$WaterYear[data$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data%>%
    group_by(WaterYear)%>%
    dplyr::summarize(winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
                     PrecipMay = sum(Total.Precip..mm.[Month%in%c(5)],na.rm = TRUE),
                     PrecipJun = sum(Total.Precip..mm.[Month%in%c(6)],na.rm = TRUE),
                     PrecipJul = sum(Total.Precip..mm.[Month%in%c(7)],na.rm = TRUE),
                     PrecipAug = sum(Total.Precip..mm.[Month%in%c(8)],na.rm = TRUE),
                     PrecipSep = sum(Total.Precip..mm.[Month%in%c(9)],na.rm = TRUE),
                     PrecipOct = sum(Total.Precip..mm.[Month%in%c(10)],na.rm = TRUE),
                     TempMay= mean(Mean.Temp..C.[Month%in%c(5)],na.rm = TRUE),
                     TempJun= mean(Mean.Temp..C.[Month%in%c(6)],na.rm = TRUE),
                     TempJul= mean(Mean.Temp..C.[Month%in%c(7)],na.rm = TRUE),
                     TempAug= mean(Mean.Temp..C.[Month%in%c(8)],na.rm = TRUE),
                     TempSep= mean(Mean.Temp..C.[Month%in%c(9)],na.rm = TRUE),
                     TempOct= mean(Mean.Temp..C.[Month%in%c(10)],na.rm = TRUE)
    )
  
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
  dataYearly2<-dataYearly2[dataYearly$WaterYear<2024,]
  
  licensedDiv<-read.csv(paste0("SurfaceWaterLicenses/",stations2$ID[it_stn],".csv"))
  names(licensedDiv)[2]<-"lcd_SW_div"
  licensedDiv$lcd_SW_div.cms<-licensedDiv$lcd_SW_div/(365*24*3600)# convert to cms
  # numWells<-read.csv(paste0("wellRecords/",stations2$StationNum[it_stn],".csv"))
  # names(numWells)[2]<-"constructedWells"
  
  dataYearly2<-left_join(dataYearly2,licensedDiv,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  if(!any(dataYearly2$lcd_SW_div.cms>(0.05*mean(dataYearly2$minAugFlow7,na.rm = TRUE)))){
    
    
    xAug<-summary(my_lmAug<-lm(log(minAugFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug)/4)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun))+
                                 PrecipJul+
                                 PrecipAug+
                                 winterPrecip,
                               data= dataYearly2,na.action = na.exclude))
    xSep<-summary(my_lmSep<-lm(log(minSepFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug+TempSep)/5)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun+PrecipJul))+
                                 PrecipAug+
                                 PrecipSep+
                                 winterPrecip,
                               data= dataYearly2,na.action = na.exclude))
    
    xOct<-summary(my_lmOct<-lm(log(minOctFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug+TempSep+TempOct)/6)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun+PrecipJul+PrecipAug))+
                                 PrecipSep+
                                 PrecipOct+
                                 winterPrecip,
                               data= dataYearly2,na.action = na.exclude))
    
    
  }else{
    
    xAug<-summary(my_lmAug<-lm(log(minAugFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug)/4)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun))+
                                 PrecipJul+
                                 PrecipAug+
                                 winterPrecip+
                                 lcd_SW_div.cms,
                               data= dataYearly2,na.action = na.exclude))
    xSep<-summary(my_lmSep<-lm(log(minSepFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug+TempSep)/5)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun+PrecipJul))+
                                 PrecipAug+
                                 PrecipSep+
                                 winterPrecip+
                                 lcd_SW_div.cms,
                               data= dataYearly2,na.action = na.exclude))
    
    xOct<-summary(my_lmOct<-lm(log(minOctFlow7)~
                                 I((TempMay+TempJun+TempJul+TempAug+TempSep+TempOct)/6)+
                                 meanWinterTemp+
                                 I((PrecipMay+PrecipJun+PrecipJul+PrecipAug))+
                                 PrecipSep+
                                 PrecipOct+
                                 winterPrecip+
                                 lcd_SW_div.cms,
                               data= dataYearly2,na.action = na.exclude))
    
  }
  
  dataYearly2$residsAug<-residuals(my_lmAug)
  dataYearly2$residsSep<-residuals(my_lmSep)
  dataYearly2$residsOct<-residuals(my_lmOct)
  
  predMin<-pmin(predict(my_lmAug,dataYearly2,se.fit = TRUE)$fit,
                predict(my_lmSep,dataYearly2,se.fit = TRUE)$fit,
                predict(my_lmOct,dataYearly2,se.fit = TRUE)$fit
  )
  
  measMin<-dataYearly2$minSumFlow%>%log()
  
  cvDat<-data.frame(predMin = predMin,
                    measMin = measMin)
  cvDat<-cvDat[rowSums(is.na(cvDat))==0,]
  
  stations2$RMSE[it_stn]<-(exp(cvDat$predMin)-exp(cvDat$measMin))^2%>%
    mean(na.rm = TRUE)%>%
    sqrt()  
  
  stations2$KGE[it_stn]<-KGE(cvDat$predMin,cvDat$measMin)
  
  stations2$NSE[it_stn]<-NSE(cvDat$predMin,cvDat$measMin)
  
}

stations3<-left_join(stations[,c("Station.Name","ID","RMSE","KGE","NSE")],stations2[,c("Station.Name","ID","RMSE","KGE","NSE")],by = c("Station.Name","ID"),
                     suffix  = c(".PWNAmet",".ANUSPLIN"))

ggplot(stations3,aes(KGE.PWNAmet,KGE.ANUSPLIN))+geom_point()+geom_abline()

sum(stations3$KGE.PWNAmet<stations3$KGE.ANUSPLIN)
18/34
sum(stations3$NSE.PWNAmet<stations3$NSE.ANUSPLIN)
ggplot(stations3,aes(NSE.PWNAmet,NSE.ANUSPLIN))+geom_point()+geom_abline()


PCIC_eff_overall<-watersheds[,c("NameNom","StationNum")]%>%
  st_drop_geometry()
PCIC_eff_overall$KGE<-NA
PCIC_eff_overall$NSE<-NA

for(it in 1:56){
  streamData<-streamDataAll[streamDataAll$ID==watersheds$StationNum[it],]
  streamData$dt<-ymd(streamData$Date)
  PCIC_model<-read.csv(paste0("PCIC_station_hydro_model_out/",watersheds$NameNom[it],".ascii"),skip = 1)
  PCIC_model$dt<-ymd(PCIC_model$Date)
  model_compare<-inner_join(streamData[,c("dt","Discharge")],PCIC_model[,c("dt","PNWNAmet")])
  PCIC_eff_overall$KGE[it]<-KGE(model_compare$PNWNAmet,
                                model_compare$Discharge) 
  PCIC_eff_overall$KGE_sqrt[it]<-KGE(sqrt(model_compare$PNWNAmet),
                                     sqrt(model_compare$Discharge))  
  PCIC_eff_overall$NSE[it]<-NSE(model_compare$PNWNAmet,
                                model_compare$Discharge)
  PCIC_eff_overall$NSE_log[it]<-NSE(log(model_compare$PNWNAmet),
                                    log(model_compare$Discharge))
  PCIC_eff_overall$NSE_sqrt[it]<-NSE(sqrt(model_compare$PNWNAmet),
                                     sqrt(model_compare$Discharge))
  
}

median(PCIC_eff_overall$KGE)
median(PCIC_eff_overall$NSE)
median(PCIC_eff_overall$NSE_log)
median(PCIC_eff_overall$KGE_sqrt)
min(PCIC_eff_overall$KGE_sqrt)
min(PCIC_eff_overall$NSE_log)
sum(PCIC_eff_overall$NSE_log<0)
sum(PCIC_eff_overall$NSE<0)
PCIC_eff_overall$KGE/PCIC_eff_overall



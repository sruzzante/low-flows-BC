## what if I try using model residuals?
closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(tidyr)

library("tseries")
library(forecast)

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

# maskPrivateBC<-crop(maskPrivate)

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

stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(StationNum,Area_km2),
                    by = c("ID" = "StationNum"))


stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)

stn_MC<-stn_MC[order(stn_MC$ID),]


# stns_with_brks<-c()

stn_MC_ls<-split(stn_MC,stn_MC$ID)

ECA<-ECA%>%
  filter(StationNum%in%stn_MC$ID)
ECA<-ECA[order(ECA$StationNum),]
ECA_ls<-split(ECA,ECA$StationNum)

streamDataAll$Date<-ymd(streamDataAll$Date)


streamDataAll$DayOfYear<-yday(ymd(streamDataAll$Date))
streamDataAll$Discharge7Summer<-streamDataAll$Discharge7
streamDataAll$Discharge7Summer[!streamDataAll$month%in%c(8,9,10)]<-NA



streamDataAll<-streamDataAll%>%
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



streamDataMonthly<-streamDataAll%>%
  
  dplyr::group_by(ID,NovWaterYear)%>%
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


streamDataMonthly$minAugFlow7[streamDataMonthly$numAugQnonNans<26]<-NA
streamDataMonthly$minSepFlow7[streamDataMonthly$numSepQnonNans<25]<-NA
streamDataMonthly$minOctFlow7[streamDataMonthly$numOctQnonNans<26]<-NA

# streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA

streamDataMonthly$minAugFlow7[is.infinite(streamDataMonthly$minAugFlow7)]<-NA
streamDataMonthly$minSepFlow7[is.infinite(streamDataMonthly$minSepFlow7)]<-NA
streamDataMonthly$minOctFlow7[is.infinite(streamDataMonthly$minOctFlow7)]<-NA

streamDataMonthly$minSumFlow7[streamDataMonthly$numQDaysSum<77]<-NA

streamDataMonthly$minSumFlow7[streamDataMonthly$ID%in%c("08OA004","08OA005","08HD023")]<-
  pmin( streamDataMonthly$minAugFlow7[streamDataMonthly$ID%in%c("08OA004","08OA005","08HD023")],
        streamDataMonthly$minSepFlow7[streamDataMonthly$ID%in%c("08OA004","08OA005","08HD023")])



streamDataMonthly<-left_join(streamDataMonthly,ECA,by = c("NovWaterYear" = "year","ID" = "StationNum"))
streamDataMonthly<-dplyr::filter(streamDataMonthly,!is.na(minSumFlow7)&!is.na(ECA_60)&
                                   ID%in%stn_MC$ID)

streamDataMonthly<-streamDataMonthly[order(streamDataMonthly$ID),]
streamDataMonthly_ls<-split(streamDataMonthly,streamDataMonthly$ID)

WeatherData<-WeatherData[(WeatherData$StationNum%in%stn_MC$ID),]
WeatherData<-WeatherData[order(WeatherData$StationNum),]
WeatherData_ls<-split(WeatherData,WeatherData$StationNum )

bestModels<-load("../DATA/BC_Watersheds/2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData")
AugustBestModel<-AugustBestModel[stn_MC$ID]
SeptemberBestModel<-SeptemberBestModel[stn_MC$ID]
OctoberBestModel<-OctoberBestModel[stn_MC$ID]



library(foreach)
library(doParallel)
# registerDoParallel(cores=6)

# cl <- makeCluster(22)
#Register cluster
# registerDoParallel(cl)
stnRs<-data.frame()
stn_MC_result<-foreach(stn_MC_x = stn_MC_ls,
                       streamDataYrly = streamDataMonthly_ls,
                       data_x = WeatherData_ls,
                       AugustBestModel_x = AugustBestModel,
                       SeptemberBestModel_x = SeptemberBestModel,
                       OctoberBestModel_x = OctoberBestModel,
                       # fctrs = fctrs,
                       .packages=c("dplyr","forecast")
)%do%{
  # stn_MC_x = stn_MC_ls[[1]]
  # streamDataYrly = streamDataMonthly_ls[[1]]
  # data_x = WeatherData_ls[[1]]
  # AugustBestModel_x = AugustBestModel[[1]]
  # SeptemberBestModel_x = SeptemberBestModel[[1]]
  # OctoberBestModel_x = OctoberBestModel[[1]]
  
  
  
  data_x$WaterYear<-data_x$Year
  data_x$WaterYear[data_x$Month%in%c(11,12)] <- data_x$WaterYear[data_x$Month%in%c(11,12)]+1
  
  
  
  dataYearly<-data_x%>%
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
  
  
  
  
  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "NovWaterYear"))
  dataYearly2<-dataYearly2[dataYearly$WaterYear<2024,]
  
  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stn_MC_x$ID,".csv"))
  Div<-Div[,c("year","Total.cms","August.cms","September.cms","October.cms")]
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
  dataYearly2<-filter(dataYearly2,!is.na(minSumFlow7))
  dataYearly2_trA<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  dataYearly2_trS<-dataYearly2[!is.na(dataYearly2$minSepFlow7),]
  dataYearly2_trO<-dataYearly2[!is.na(dataYearly2$minOctFlow7),]
  
  mdlAug<-eval(AugustBestModel_x[[1]])
  mdlSep<-eval(SeptemberBestModel_x[[1]])
  mdlOct<-eval(OctoberBestModel_x[[1]])
  
  predictMins<-pmin(predict(mdlAug,data = dataYearly2),
                    predict(mdlSep,data = dataYearly2),
                    predict(mdlOct,data = dataYearly2)
  )
  resids<-log(dataYearly2$minSumFlow7)-predictMins
  
  # fitQ7<- auto.arima(resids)
  dat<-data.frame(resids=resids,ECA_I_9 = dataYearly2$ECA_I_9,ECA_III_24 = dataYearly2$ECA_III_24)
  
  if(max(dat$ECA_III_24>0.05)){
    mylm<-lm(resids~ECA_I_9+ECA_III_24,data = dat)
    lm_results<-data.frame( b_I_9 = mylm$coefficients[2],
                            b_I_9.p = summary(mylm)$coefficients[2,4],
                            
                            b_III_24 = mylm$coefficients[3],
                            b_III_24.p = summary(mylm)$coefficients[3,4])
  }else{
    mylm<-lm(resids~ECA_I_9,data = dat)
    lm_results<-data.frame( b_I_9 = mylm$coefficients[2],
                            b_I_9.p = summary(mylm)$coefficients[2,4],
                            
                            b_III_24 = NA,
                            b_III_24.p = NA)
  }
  
  
  
  # fitECA5<- auto.arima(dataYearly2$ECA_5)
  # fitECA10<-auto.arima(dataYearly2$ECA_10)
  # fitECA20<-auto.arima(dataYearly2$ECA_20)
  # fitECA60<-auto.arima(dataYearly2$ECA_60)
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  c0.60<-cor.test(resids,dataYearly2$ECA_60,method = "spearman")
  # ccf(logQ7.60,dataYearly2$ECA_60,na.action=na.omit)
  c1.60<-cor.test(resids[2:length(resids)],
                  dataYearly2$ECA_60[1:(length(resids)-1)],method = "spearman")
  c2.60<-cor.test(resids[3:length(resids)],
                  dataYearly2$ECA_60[1:(length(resids)-2)],method = "spearman")
  
  c0.20<-cor.test(resids,dataYearly2$ECA_20,method = "spearman")
  
  c1.20<-cor.test(resids[2:length(resids)],
                  dataYearly2$ECA_20[1:(length(resids)-1)],method = "spearman")
  c2.20<-cor.test(resids[3:length(resids)],
                  dataYearly2$ECA_20[1:(length(resids)-2)],method = "spearman")
  
  
  # ccf(logQ7.60,dataYearly2$ECA_60,na.action=na.omit)
  
  
  c0.10<-cor.test(resids,dataYearly2$ECA_10,method = "spearman")
  
  c1.10<-cor.test(resids[2:length(resids)],
                  dataYearly2$ECA_10[1:(length(resids)-1)],method = "spearman")
  c2.10<-cor.test(resids[3:length(resids)],
                  dataYearly2$ECA_10[1:(length(resids)-2)],method = "spearman")
  
  
  
  c0.5<-cor.test(resids,dataYearly2$ECA_5,method = "spearman")
  
  c1.5<-cor.test(resids[2:length(resids)],
                 dataYearly2$ECA_5[1:(length(resids)-1)],method = "spearman")
  c2.5<-cor.test(resids[3:length(resids)],
                 dataYearly2$ECA_5[1:(length(resids)-2)],method = "spearman")
  
  
  
  
  stnRs<-rbind(stnRs,
               data.frame(
                 ID = stn_MC_x$ID,
                 regime = stn_MC_x$regime,
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
                 c2.5.p =c2.5$p.value,
                 
                 # c2.60 =c2$estimate,
                 # c2.60.p =c2$p.values
                 b_I_9 = lm_results$b_I_9,
                 b_I_9.p =  lm_results$b_I_9.p,
                 b_III_24 =  lm_results$b_III_24,
                 b_III_24.p = lm_results$b_III_24.p
                 
               ))
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
}


stnRs<-left_join(watersheds,stnRs,by = c( "StationNum" = "ID"))

stnRs<-left_join(stnRs,stations)
stnRs<-filter(stnRs,!is.na(c0.60))

saveRDS(stnRs,"4.output/ECA_analysis_C_stnRs.rds")
stnRs<-readRDS("4.output/ECA_analysis_C_stnRs.rds")

###
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

ggsave("3.figures/ECA_vs_TotalCut_C.png",width =7,height = 4)

stnRs_long%>%
  group_by(regime,name)%>%
  summarize(H1.p.pos =  binom.test(sum(p.val<0.05&estimate>0),n(),p=0.05,alternative = "greater")$p.value,
            H1.p.neg =  binom.test(sum(p.val<0.05&estimate<0),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       digits = 2,
                       out = "4.output/ECA_analysis_C.doc")


## For b_I_9 and b_III_24


stnRs_long<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,b_I_9:b_III_24.p)%>%
  pivot_longer(cols =b_I_9:b_III_24.p)
stnRs_long$meas<-str_detect(stnRs_long$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long$name<-str_remove(stnRs_long$name,"\\.p")%>%factor()
# stnRs_long$varbl<-str_remove(stnRs_long$name,"\\.p")%>%factor()
stnRs_long<-pivot_wider(stnRs_long,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long<-filter(stnRs_long,!regime=="Glacial")
stnRs_long$regime<-factor(stnRs_long$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                          labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long<-filter(stnRs_long,!is.na(estimate))

ggplot(stnRs_long,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r"
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())


stnRs_long%>%
  group_by(regime,name)%>%
  dplyr::summarize(
    H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
    H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
    H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
    H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
    n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       out = "4.output/ECA_analysis_I_III.doc")


Clim<-WeatherData%>%
  group_by(StationNum)%>%
  summarize(meanAnnualTemp = mean(Mean.Temp..C.))

stnRs<-left_join(stnRs,Clim)


stnRs_long<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,meanAnnualTemp,c0.60:c0.5.p)%>%
  pivot_longer(cols = c0.60:c0.5.p)
stnRs_long$meas<-str_detect(stnRs_long$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long$name<-str_remove(stnRs_long$name,"\\.p")
stnRs_long<-pivot_wider(stnRs_long,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name,meanAnnualTemp),names_from = meas,values_from = value)


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

ggplot(stnRs_long,aes(x = meanAnnualTemp,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Mean Annual Temperature")+
  scale_y_continuous(name = "Spearman r",
                     # limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())


stnRs_long%>%
  group_by(regime,name)%>%
  dplyr::summarize(
    H4.b = summary(lm(estimate~meanAnnualTemp))$coefficients[2,1],
    H4.p = summary(lm(estimate~meanAnnualTemp))$coefficients[2,4],
    n = n())

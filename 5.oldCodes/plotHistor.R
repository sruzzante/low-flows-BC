
closeAllConnections()
rm(list=ls())
graphics.off()

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
library(terra)
library(cowplot)
library(scico)
library(scales)
library(tictoc)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC")) #Set the working directory

streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")

stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")

load("2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData")

# Stations<-Stations[Stations$]
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")




WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")


findMin<-function(x){
  minInd<-which.min(x)
  if(length(minInd)==0){minInd = 1}
  return(minInd)}


DAT<-data.frame()
tic.clear()
it_stn = 79

for(it_stn in 1:length(stations$ID)){
  tic(sprintf("Station %d",it_stn))
  # it_stn = which(stations$ID=="08HA003")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08LB020",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$Discharge30 = zoo::rollmean(streamData$Discharge,30,fill = NA)
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    dplyr::group_by(year)%>%
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
                     minSumFlowDay = DayOfYear[findMin(Discharge7Summer)],
                     minSumFlowMonth = month[findMin(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     MAD = mean(Discharge,na.rm = TRUE),
                     minFlow = min(Discharge7,na.rm = TRUE)
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  streamDataYrly$minFlow[streamDataYrly$numQNans>30]<-NA
  
  Q_filled<-streamData[streamData$year>=1993,c("year","month","DayOfYear","Discharge7")]
  
  
  Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]<-
    Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]-1
  set.seed(123)
  for(it in 1:365){
    sum_notNA<-sum(!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
    if(sum_notNA>0){
      sum_NA<-sum(is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
      Q_filled$Discharge7[is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it]<-
        sample(Q_filled$Discharge7[!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it],size = sum_NA,replace = TRUE)
    }
    
  }
  if(sum(is.na(Q_filled$Discharge7))>0){
    sprintf("%d of 365 days with no filled Q for station %s - %s",sum(is.na(Q_filled$Discharge7[1:365])),stations$ID[it_stn],stations$Station.Name[it_stn])
  }
  xtrm_val_data<-Q_filled%>%
      group_by(year)%>%
      summarize(minFlow = min(Discharge7),
                minAugFlow7 = min(Discharge7[month == 8]),
                minSepFlow7 = min(Discharge7[month == 9]),
                minOctFlow7 = min(Discharge7[month == 10]))
  
  
  
  DroughtIndic5 = quantile(Q_filled$Discharge7,0.02,type =5,na.rm =TRUE)
  
  DroughtIndic5_Aug = quantile(log(xtrm_val_data$minAugFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  DroughtIndic5_Sep = quantile(log(xtrm_val_data$minSepFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  DroughtIndic5_Oct = quantile(log(xtrm_val_data$minOctFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  
  MAD = mean(Q_filled$Discharge7,na.rm = TRUE)
  

 



  
  CEFT = max(c(min(streamDataYrly$minFlow30CP,na.rm = TRUE),0.05*MAD))
  if(stations$regime[it_stn]=="Rainfall"){
    CEFT = max(c(min(streamDataYrly$minFlow30CP,na.rm = TRUE),0.02*MAD))
    
  }
 
  
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
  # names(licensedDiv)[2]<-"lcd_SW_div"
  # licensedDiv$lcd_SW_div.cms<-licensedDiv$lcd_SW_div/(365*24*3600)# convert to cms
  # 
  # licensedDiv$lcd_SW_div.August.cms<-licensedDiv$August/(31*24*3600)
  # licensedDiv$lcd_SW_div.September.cms<-licensedDiv$September/(30*24*3600)
  # licensedDiv$lcd_SW_div.October.cms<-licensedDiv$October/(31*24*3600)
  # numWells<-read.csv(paste0("wellRecords/",stations$StationNum[it_stn],".csv"))
  # names(numWells)[2]<-"constructedWells"
  
  dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
  
  dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1901&dataYearly2$WaterYear<2023,]
  # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
  
  
  dataYearly2_trA<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
  dataYearly2_trS<-dataYearly2[!is.na(dataYearly2$minSepFlow7),]
  
  dataYearly2_trO<-dataYearly2[!is.na(dataYearly2$minOctFlow7),]
  
  modelAug<-eval(AugustBestModel[[stations$ID[1]]][[1]])%>%
    update(data = dataYearly2,na.action = "na.exclude")
  
  modelSep<-eval(SeptemberBestModel[[stations$ID[1]]][[1]])%>%
    update(data = dataYearly2,na.action = "na.exclude")
  dataYearly2$residsAug<-residuals(modelAug)
  dataYearly2$residsSep<-residuals(modelSep)
  
  predMinAug<-predict(modelAug,dataYearly2,se.fit = TRUE)$fit
  predMinSep<-predict(modelSep,dataYearly2,se.fit = TRUE)$fit
  
  if(!stations$ID[it_stn]%in%c("08OA004","08OA005")){
    modelOct<-eval(OctoberBestModel[[stations$ID[1]]][[1]])%>%
      update(data = dataYearly2,na.action = "na.exclude")
    dataYearly2$residsOct<-residuals(modelOct)
    
    predMin<-pmin(predict(modelAug,dataYearly2,se.fit = TRUE)$fit,
                  predict(modelSep,dataYearly2,se.fit = TRUE)$fit,
                  predict(modelOct,dataYearly2,se.fit = TRUE)$fit
    )
    
   
    predMinOct<-predict(modelOct,dataYearly2,se.fit = TRUE)$fit
    
  }else{
    predMin<-pmin(predict(modelAug,dataYearly2,se.fit = TRUE)$fit,
                  predict(modelSep,dataYearly2,se.fit = TRUE)$fit)
    predMinOct<-NA
  }
  
  # Predictions
  dat<-data.frame(ID = stations$ID[it_stn],
                  Station.Name = stations$Station.Name[it_stn],
                  WaterYear =1901:2022,
                  predMin = predMin,
                  predMinAug = predMinAug,
                  predMinSep = predMinSep,
                  predMinOct = predMinOct,
                  CEFT= CEFT,
                  D5 = DroughtIndic5,
                  D5Aug = DroughtIndic5_Aug,
                  D5Sep = DroughtIndic5_Sep,
                  D5Oct = DroughtIndic5_Oct
  )
  dat$predMin.m3s<-exp(dat$predMin)
    
  
  DAT<-rbind(DAT,dat)
  
  resids<-left_join(streamDataYrly[,c("year","minSumFlow")],dat[,c("WaterYear","predMin.m3s")],
                    by = c("year" = "WaterYear"))
  
  mkTest<-modifiedmk::mmkh3lag((resids$minSumFlow-resids$predMin.m3s))
  stations$senSlope[it_stn] <-mkTest[7]
  stations$MK.p[it_stn] <-mkTest[2]
  
  toc()
  naInds<-which(is.na(streamDataYrly$minSumFlow))
  naInds<-naInds[!naInds%in%c(1,dim(streamDataYrly)[1])]
  streamDataYrly$minSumFlow_filled<-streamDataYrly$minSumFlow
  if(length(naInds)>0){
    streamDataYrly$minSumFlow_filled[naInds]<-
      (streamDataYrly$minSumFlow_filled[naInds+1]+
         streamDataYrly$minSumFlow_filled[naInds-1])/2
    
  }
  streamDataYrly$minSumFlow.10<-zoo::rollmean(streamDataYrly$minSumFlow_filled,
                                              k=10,
                                              align = "right",
                                              fill = NA)
  
    dat$predMin.m3s.10<-zoo::rollmean(dat$predMin.m3s,
                                    k=10,
                                    align = "right",
                                    fill = NA)
  
  
  p1<-ggplot(streamDataYrly)+
    geom_line(aes(x = year,y = minSumFlow.10,colour = "measured",linetype = "measured"))+
    geom_line(data = dat,aes(x = WaterYear,y = predMin.m3s.10,colour = "predicted",linetype = "predicted"))+
    geom_hline(aes(yintercept = CEFT,colour = "CEFT",linetype = "CEFT"))+
    geom_hline(aes(yintercept = DroughtIndic5,colour = "Drought 5",linetype = "Drought 5"))+
    scale_y_continuous(name = expression(Q7[min]~(m^3/s)~10~year~mean))+
    scale_x_continuous(limits = c(range(streamDataYrly$year)))+
    scale_colour_manual(breaks = c("measured","predicted","CEFT","Drought 5"),
                        values = c("black","#1B9E77","#D95F02","#7570B3"),
                        name = "")+
    scale_linetype_manual(breaks = c("measured","predicted","CEFT","Drought 5"),
                          values = c(1,1,2,3),
                          name = "")+
    ggtitle(label = paste(stations$Station.Name[it_stn],", ", stations$ID[it_stn],", ", stations$regime[it_stn]))+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.03, vjust=-8,size = 8))
  p1
  # ggsave(plot = p1,filename = paste0("Figures/SimulatedHistorical/",stations$Station.Name[it_stn], "-",stations$ID[it_stn],".png"),
         # width = 6,height = 4)
  
}

stations%>%group_by(regime)%>%
  summarize(percSigResidTrends = sum(MK.p<0.05)/n(),
            percPosResidTrends = sum(senSlope>0)/n(),
            percPosSigResidTrends = sum(senSlope>0&MK.p<0.05)/n(),
            percNegSigResidTrends = sum(senSlope<0&MK.p<0.05)/n(),
  )

stations%>%
  summarize(percSigResidTrends = sum(MK.p<0.05)/n(),
            percPosResidTrends = sum(senSlope>0)/n(),
            percPosSigResidTrends = sum(senSlope>0&MK.p<0.05)/n(),
            percNegSigResidTrends = sum(senSlope<0&MK.p<0.05)/n(),
  )
# Hybrid and snowfall regimes - 18% negative and significant 
# Residuals are becoming more negative; we are overpredicting low flows
# Projections are likely to be overpredicted; transgressions underpredicted;
# change underpredicted.

# DAT$predMin.m3s<-exp(DAT$predMin)

DAT$predMinAug.m3s<-exp(DAT$predMinAug)
DAT$predMinSep.m3s<-exp(DAT$predMinSep)
DAT$predMinOct.m3s<-exp(DAT$predMinOct)

DAT$underCEFT<-DAT$predMin.m3s<DAT$CEFT
DAT$underD5<-DAT$predMin.m3s<DAT$D5 # this is not really meaningful

DAT$underD5Aug<-DAT$predMinAug.m3s<DAT$D5Aug
DAT$underD5Sep<-DAT$predMinSep.m3s<DAT$D5Sep
DAT$underD5Oct<-DAT$predMinOct.m3s<DAT$D5Oct


DAT<-left_join(DAT,stations[,c("ID","regime")])
# mean.begin<-DAT%>%
#   group_by(ID)%>%
#   summarize(
#     startMean.log = mean(predMin[WaterYear<=1930]),
#     startMean.m3s = mean(predMin.m3s[WaterYear<=1930])
#   )
# 
# DAT2<-left_join(DAT,mean.begin)
# DAT2$predMin.m3s.normed<-(DAT2$predMin.m3s)/DAT2$startMean.m3s
# DAT2$predMin.normed<-(DAT2$predMin)/DAT2$startMean.log
# 
# 
# ggplot(DAT2%>%filter(str_detect(ID,"08H")),aes(x = WaterYear,y = predMin.m3s.normed,colour = ID))+
#   geom_smooth(method = "loess",
#               span = 0.1,se = FALSE)+
#   theme(legend.position = "none")+
#   scale_y_continuous(
#     # limits = c(0,5),
#   )
# 
# # ggplot(DAT2,aes(x = WaterYear,y = predMin.m3s.normed))+
# #   geom_smooth(method = "loess",
# #               span = 0.1,se = TRUE)+
# #   theme(legend.position = "none")+
# #   scale_y_continuous(
# #     # limits = c(0,5),
# #   )
# 
# DAT2.b<-DAT2%>%
#   group_by(WaterYear)%>%
#   summarise(mean.predMin.m3s.normed = mean(predMin.m3s.normed))
# 
# DAT2.b$mean.predMin.m3s.normed.roll<-zoo::rollmean(DAT2.b$mean.predMin.m3s.normed,k=10,align = "right",fill=NA)
# ggplot(DAT2.b,aes(x = WaterYear,y = mean.predMin.m3s.normed.roll))+
#   geom_line()
# geom_smooth(method = "loess",
#             span = 0.1,se = TRUE)+
#   theme(legend.position = "none")+
#   scale_y_continuous(
#     # limits = c(0,5),
#   )
DAT3<-DAT%>%
  # filter(str_detect(ID,"08H"))%>%
  group_by(WaterYear)%>%
  summarize(numExceeds = sum(underCEFT)/n(),
            numExceedsD5 = sum(underD5)/n())

DAT3<-DAT%>%
  # filter(str_detect(ID,"08H"))%>%
  group_by(regime,WaterYear)%>%
  summarize(numExceeds = sum(underCEFT)/n(),
            numExceedsD5 = sum(underD5)/n(),
            numExceedsD5Aug = sum(underD5Aug)/n(),
            numExceedsD5Sep = sum(underD5Sep)/n(),
            numExceedsD5Oct = sum(underD5Oct,na.rm = TRUE)/sum(!is.na(underD5Oct)),
  )

# 
# DAT3$numExceeds.10 <- zoo::rollmean(DAT3$numExceeds, k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
# DAT3$numExceedsD5.10 <- zoo::rollmean(DAT3$numExceedsD5,k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
rollFunc<-function(yr,val){
  
  tibble(
    val = zoo::rollmean(val, k=10, align = "right", fill=NA),
    yr = yr
  )
}

DAT4<-DAT3%>%
  group_by(regime)%>%
  reframe(numExceeds.10 = rollFunc(WaterYear,numExceeds),
          numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5),
          numExceedsD5Aug.10 = rollFunc(WaterYear,numExceedsD5Aug),
          numExceedsD5Sep.10 = rollFunc(WaterYear,numExceedsD5Sep),
          numExceedsD5Oct.10 = rollFunc(WaterYear,numExceedsD5Oct))%>%
  tidyr::unnest(cols = c(numExceeds.10, numExceedsD5.10,numExceedsD5Aug.10,numExceedsD5Sep.10,numExceedsD5Oct.10),names_sep = "_")%>%
  select(c("regime","numExceeds.10_yr","numExceeds.10_val",
           "numExceedsD5.10_val",
           "numExceedsD5Aug.10_val",
           "numExceedsD5Sep.10_val",
           "numExceedsD5Oct.10_val"))%>%
  dplyr::rename(c(WaterYear = numExceeds.10_yr,
                  numExceeds.10 = numExceeds.10_val,
                  numExceedsD5.10 = numExceedsD5.10_val,
                  numExceedsD5Aug.10 = numExceedsD5Aug.10_val,
                  numExceedsD5Sep.10 = numExceedsD5Sep.10_val,
                  numExceedsD5Oct.10 = numExceedsD5Oct.10_val))




# p1<-ggplot(DAT4,aes(x = WaterYear,y = numExceeds.10))+
#   geom_line()+
#   scale_x_continuous(name = "year", limits = c(1910,2022))+
#   scale_y_continuous(name= "% years transgressing CEFT")+
#   theme_classic()
# # ggplotly()
# p1
library(plotly)
ggplotly(p1)
# DAT3<-DAT%>%
#   # filter(str_detect(ID,"08H"))%>%
#   group_by(WaterYear,regime)%>%
#   summarize(numExceeds = sum(underCEFT)/n())
# 
# DAT3.hyb<-DAT3%>%filter(regime == "Hybrid")
# DAT3.rain<-DAT3%>%filter(regime == "Rainfall")
# 
# DAT3.hyb$numExceeds.10<-zoo::rollmean(DAT3.hyb$numExceeds,k=10,align = "right",fill=NA)
# DAT3.rain$numExceeds.10<-zoo::rollmean(DAT3.rain$numExceeds,k=10,align = "right",fill=NA)
# 
# DAT3<-rbind(DAT3.hyb,DAT3.rain)
# ggplot(DAT3,aes(x = WaterYear,y = numExceeds.10,colour = regime))+
#   geom_line()+
#   scale_x_continuous(name = "year", limits = c(1910,2022))+
#   scale_y_continuous(name= "% years transgressing CEFT")+
#   theme_classic()



stations.exceed<-read.csv("2.data/2.working/ClimateChangeProjections/stations_exceed_all.csv")



quantile_df <- function(x, probs = c(0,0.05,0.1, 0.5, 0.9,0.95,1)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}


# d<-stations.exceed%>%
#   group_by(model)%>%
#   # summarize(x = mean(pr_CEFT_ssp126_mid))
#   dplyr::summarize(across("pr_CEFT_ssp126_mid":"pr_D5_ssp585_end", 
#                           # ~quantile(.x,probs = c(0.1,0.5,0.9)),
#                           ~mean(.x)
#   ))%>%
#   dplyr::reframe(across("pr_CEFT_ssp126_mid":"pr_D5_ssp585_end", 
#                           # ~quantile(.x,probs = c(0.1,0.5,0.9)),
#                           quantile_df,.unpack = TRUE
#   ))%>%
#   select(!ends_with("quant")|pr_CEFT_ssp126_mid_quant)
# 
# names(d)[names(d)=="pr_CEFT_ssp126_mid_quant"] <- "quant"
# 
# d.2<-d%>%
#   reshape2::melt(id.vars = "quant",variable.name = "vbl")
# 
# 
#   
#   
# 
# d.2$year<-substr(d.2$vbl,16,18)%>%
#   plyr::mapvalues(from = c("mid","end"),
#                   to = c(2050,2085))
# d.2$ssp<-substr(d.2$vbl,9,14)
# 
# d.2.CEFT<-d.2[base::substr(d.2$vbl,1,7)=="pr_CEFT",]
# d.2.D5<-d.2[base::substr(d.2$vbl,1,5)=="pr_D5",]
# 
#   
# d.3.CEFT<-d.2.CEFT%>%
#   tidyr::pivot_wider(names_from = quant,values_from = value,
#                      id_cols = c(year,ssp),
#                      names_prefix = "x")
db<-stations.exceed%>%
  group_by(model,regime)%>%
  # summarize(x = mean(pr_CEFT_ssp126_mid))
  dplyr::summarize(across("pr_D5Aug_ssp126_mid":"pr_D5_ssp585_end", 
                          # ~quantile(.x,probs = c(0.1,0.5,0.9)),
                          ~mean(.x)
  ))%>%
  reshape2::melt(id.vars = c("regime","model"))

db.CEFT<-db[base::substr(db$variable,1,7)=="pr_CEFT",]
db.CEFT$year<-substr(db.CEFT$variable,16,18)%>%
  plyr::mapvalues(from = c("mid","end"),
                  to = c(2050,2085))
db.CEFT$ssp<-substr(db.CEFT$variable,9,14)


db.D5<-db[base::substr(db$variable,1,8)%in%c("pr_D5Aug","pr_D5Sep","pr_D5Oct"),]
db.D5$year<-substr(db.D5$variable,17,19)%>%
  plyr::mapvalues(from = c("mid","end"),
                  to = c(2050,2085))
db.D5$ssp<-substr(db.D5$variable,10,15)
db.D5$month<-substr(db.D5$variable,6,8)

ggplot()+
  geom_boxplot(data = db.CEFT,aes(x = year,y = value,colour = ssp),
               inherit.aes = FALSE,
               # position = 
               #   position_dodge2(
               #     width = 5
               #   )
  )
db.CEFT$year<-as.numeric(db.CEFT$year)
db.D5$year<-as.numeric(db.D5$year)


DAT4$regime<-factor(DAT4$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                    labels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))
db.CEFT$regime<-factor(db.CEFT$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                       labels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))

ggplot(DAT4%>%filter(!regime == "Glacial"),aes(x = WaterYear,y = numExceeds.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_boxplot(data = db.CEFT%>%filter(!regime=="Glacial"),aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name= "Annual CEFT Transgressions\n(% Catchments)",
                     labels = function(x){x*100},
                     limits= c(0,1),
                     breaks = c(.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  scale_linetype(name = "",
                 labels = "Simulated\nhistorical",
                 guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regime"))})+
  
  guides(fill = guide_legend(title = "Scenario", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("Figures/ExceedPlot_hist_future.png",
       width = 5.5,height = 6,dpi = 600)
summary(stations$regime%>%factor()
)


## Compare with all together
DAT5<-DAT%>%
  filter(regime!="Glacial")%>%
  group_by(WaterYear)%>%
  summarize(numExceeds = sum(underCEFT)/n(),
            numExceedsD5 = sum(underD5)/n())

DAT6<-DAT5%>%
  reframe(numExceeds.10 = rollFunc(WaterYear,numExceeds),
          numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5),
          numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5),
          numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5))%>%
  tidyr::unnest(cols = c(numExceeds.10, numExceedsD5.10),names_sep = "_")%>%
  select(c("numExceeds.10_yr","numExceeds.10_val","numExceedsD5.10_val"))%>%
  dplyr::rename(c(WaterYear = numExceeds.10_yr,numExceeds.10 = numExceeds.10_val,numExceedsD5.10 = numExceedsD5.10_val))


DAT5_decade<-DAT%>%
  filter(regime!="Glacial")%>%
  group_by(floor((WaterYear)/10)*10)%>%
  summarize(numExceeds = sum(underCEFT)/n(),
            numExceedsD5Aug = sum(underD5Aug)/n(),
            numExceedsD5Sep = sum(underD5Sep)/n(),
            numExceedsD5Oct = sum(underD5Oct,na.rm = TRUE)/sum(!is.na(underD5Oct)))



db.D5$regime<-factor(db.D5  $regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                     labels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))


DAT4%>%
  filter(WaterYear<2000)%>%
  group_by(regime)%>%
  summarize(meanAugD5 = mean(numExceedsD5Aug.10,na.rm = TRUE),
            meanSepD5 = mean(numExceedsD5Sep.10,na.rm = TRUE),
            meanOctD5 = mean(numExceedsD5Oct.10,na.rm = TRUE),
  )
DAT4%>%
  filter(regime!="Glacial")%>%
  group_by()%>%
  summarize(meanAugD5 = mean(numExceedsD5Aug.10,na.rm = TRUE),
            meanSepD5 = mean(numExceedsD5Sep.10,na.rm = TRUE),
            meanOctD5 = mean(numExceedsD5Oct.10,na.rm = TRUE),
  )

db.D5%>%
  group_by(regime,ssp, year,month)%>%
  summarize(median(value))%>%print(n=100)

ggplot(DAT4%>%filter(!regime == "Glacial"),aes(x = WaterYear,y = numExceedsD5Aug.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_vline(xintercept = c(2035,2065,2070,2100), color = "grey50")+
  geom_boxplot(data = db.D5%>%filter(!regime=="Glacial"&month=="Aug"),aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name= "August Exceptional Drought\n(% Catchments)",
                     # labels = function(x){x*100},
                     labels = c("","25","50","75","100"),
                     limits= c(-0.01,1),
                     breaks = c(0,.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  scale_linetype(name = "",
                 labels = "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("Figures/ExceedPlot_hist_future_D5Aug.png",
       width = 5.5,height = 6,dpi = 600)


ggplot(DAT4%>%filter(!regime == "Glacial"),aes(x = WaterYear,y = numExceedsD5Aug.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_vline(xintercept = c(2035,2065,2070,2100), color = "grey50")+
  geom_boxplot(data = db.D5%>%filter(!regime=="Glacial"&month=="Aug"),aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name= "Level 5 Streamflow Drought (1-in-50-year low flow)\nAugust, % Catchments",
                     # labels = function(x){x*100},
                     labels = c("","25","50","75","100"),
                     limits= c(-0.01,1),
                     breaks = c(0,.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  scale_linetype(name = "",
                 labels = "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("Figures/ExceedPlot_hist_future_D5Aug_var.png",
       width = 5.5,height = 6,dpi = 1200)


ggplot(DAT4%>%filter(!regime == "Glacial"),aes(x = WaterYear,y = numExceedsD5Sep.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_vline(xintercept = c(2035,2065,2070,2100), color = "grey50")+
  geom_boxplot(data = db.D5%>%filter(!regime=="Glacial"&month == "Sep"),aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name= "September Exceptional Drought\n(% Catchments)",
                     # labels = function(x){x*100},
                     labels = c("","25","50","75","100"),
                     limits= c(-0.01,1),
                     breaks = c(0,.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  scale_linetype(name = "",
                 labels = "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("Figures/ExceedPlot_hist_future_D5Sep.png",
       width = 5.5,height = 6,dpi = 600)


ggplot(DAT4%>%filter(!regime == "Glacial"),aes(x = WaterYear,y = numExceedsD5Oct.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_vline(xintercept = c(2035,2065,2070,2100), color = "grey50")+
  geom_boxplot(data = db.D5%>%filter(!regime=="Glacial"&month == "Oct"),aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name= "October Exceptional Drought\n(% Catchments)",
                     # labels = function(x){x*100},
                     labels = c("","25","50","75","100"),
                     limits= c(-0.01,1),
                     breaks = c(0,.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  scale_linetype(name = "",
                 labels = "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("Figures/ExceedPlot_hist_future_D5Oct.png",
       width = 5.5,height = 6,dpi = 600)
## Look at some real data


##### Vancouver Island ################

DAT3<-DAT%>%
  filter(str_detect(ID,"08H"))%>%
  group_by(WaterYear)%>%
  summarize(numExceeds = sum(underCEFT)/n(),
            numExceedsD5 = sum(underD5)/n(),
            numExceedsD5Aug = sum(underD5Aug)/n(),
            numExceedsD5Sep = sum(underD5Sep)/n(),
            numExceedsD5Oct = sum(underD5Oct,na.rm = TRUE)/sum(!is.na(underD5Oct)),
  )

# 


db<-stations.exceed%>%
  filter(substr(ID,1,3)=="08H")%>%
  group_by(model)%>%
  # summarize(x = mean(pr_CEFT_ssp126_mid))
  dplyr::summarize(across("pr_D5Aug_ssp126_mid":"pr_D5_ssp585_end", 
                          # ~quantile(.x,probs = c(0.1,0.5,0.9)),
                          ~mean(.x)
  ))%>%
  reshape2::melt(id.vars = c("regime","model"))


db.D5<-db[base::substr(db$variable,1,8)%in%c("pr_D5Aug","pr_D5Sep","pr_D5Oct"),]
db.D5$year<-substr(db.D5$variable,17,19)%>%
  plyr::mapvalues(from = c("mid","end"),
                  to = c(2050,2085))
db.D5$ssp<-substr(db.D5$variable,10,15)
db.D5$month<-substr(db.D5$variable,6,8)


db.D5$year<-as.numeric(db.D5$year)
# DAT3$numExceeds.10 <- zoo::rollmean(DAT3$numExceeds, k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
# DAT3$numExceedsD5.10 <- zoo::rollmean(DAT3$numExceedsD5,k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
rollFunc<-function(yr,val){
  
  tibble(
    val = zoo::rollmean(val, k=10, align = "right", fill=NA),
    yr = yr
  )
}

DAT4<-DAT3%>%
  group_by()%>%
  reframe(numExceeds.10 = rollFunc(WaterYear,numExceeds),
          numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5),
          numExceedsD5Aug.10 = rollFunc(WaterYear,numExceedsD5Aug),
          numExceedsD5Sep.10 = rollFunc(WaterYear,numExceedsD5Sep),
          numExceedsD5Oct.10 = rollFunc(WaterYear,numExceedsD5Oct))%>%
  tidyr::unnest(cols = c(numExceeds.10, numExceedsD5.10,numExceedsD5Aug.10,numExceedsD5Sep.10,numExceedsD5Oct.10),names_sep = "_")%>%
  select(c("numExceeds.10_yr","numExceeds.10_val",
           "numExceedsD5.10_val",
           "numExceedsD5Aug.10_val",
           "numExceedsD5Sep.10_val",
           "numExceedsD5Oct.10_val"))%>%
  dplyr::rename(c(WaterYear = numExceeds.10_yr,
                  numExceeds.10 = numExceeds.10_val,
                  numExceedsD5.10 = numExceedsD5.10_val,
                  numExceedsD5Aug.10 = numExceedsD5Aug.10_val,
                  numExceedsD5Sep.10 = numExceedsD5Sep.10_val,
                  numExceedsD5Oct.10 = numExceedsD5Oct.10_val))



p1<-
  ggplot(DAT4,aes(x = WaterYear,y = numExceedsD5Aug.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  geom_vline(xintercept = 2050,linewidth= .5,color = "white")+
  geom_vline(xintercept = 2085,linewidth= .5,color = "white")+
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
  geom_ribbon(data = data.frame(x = c(2035,2065),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_ribbon(data = data.frame(x = c(2070,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
              inherit.aes = FALSE,
              fill = "grey90",
              alpha = 1)+
  geom_boxplot(data = db.D5,aes(x = year,y = value,fill = ssp ,group = interaction(year, ssp)),
               width = 25,
               inherit.aes = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     minor_breaks = c(1925,1975,2025))+
  scale_y_continuous(name=  "August Level 5 Drought\n(% Catchments)",
                     labels = function(x){x*100},
                     limits= c(0,1),
                     breaks = c(.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+

  scale_linetype(name = "",
                 labels =  "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  # facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
  #            labeller = function(variable, value){return(paste(value,"Regime"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

p1


ggsave(p1,filename = "3.Figures/ExceedPlot_hist_future_VanIsland.png",
       width = 5.5,height = 3.5,dpi = 600)

p2<-
  
  ggplot(DAT4,aes(x = WaterYear,y = numExceedsD5Aug.10))+
  geom_line(aes(linetype = "SimHist"),linewidth = 1)+
  
  # geom_blank()
  # geom_ribbon(data = data.frame(x = c(2025,2100),ymax = c(1,1)),aes(x =x,ymin = 0,ymax = ymax),
  #             inherit.aes = FALSE,
  #             fill = "white")+
 
  
  scale_x_continuous(name = NULL,expand = c(0,0),limits = c(1900,2100),
                     breaks = c(1900,1950,2000,2050,2085),
                     labels = c("1900","1950","2000","",""),
                     minor_breaks = c(1925,1975,2025))+
 
  scale_fill_brewer(palette = "BuPu",
                    # values =  c("#27AAE1","#006838","#BE1E2D"),
                    # labels = c("1-2.6","2-4.5","5-8.5"),
                    labels = c("1-2.6:\nSustainability","2-4.5:\nMiddle of\nthe Road","5-8.5:\nFossil-Fueled\nDevelopment"),
                    
                    name = "SSP",
                    direction = 1,
                    guide = guide_legend())+
  
  scale_linetype(name = "",
                 labels =  "Simulated\nhistorical\n(10-year mean)",
                 guide = guide_legend())+  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  # facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
  #            labeller = function(variable, value){return(paste(value,"Regime"))})+
  
  guides(
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))+
  scale_y_continuous(name=  "August Level 5 Drought\n(% Catchments)",
                     labels = function(x){x*100},
                     breaks = c(0,0.01,0.02,0.03,0.04),
                     expand = c(0,0),
                     # oob = squish,
                     limits = c(-0.002,0.03))


p2
ggsave(p2,filename = "3.Figures/ExceedPlot_hist_future_VanIsland_hist.png",
       width = 5.5,height = 3.5,dpi = 600)


# what is causing the increase in minimum flow in snowmelt catchments?###########















stations$ID[stations$regime%in%"Snowfall"]
stations_sno<-stations[stations$regime%in%"Snowfall",]


stations_sno$overallChange<-NA
stations_sno$SummerPChange<-NA
stations_sno$SummerTChange<-NA
stations_sno$WinterPChange<-NA
stations_sno$WinterTChange<-NA
stations_sno$WinterConditionsOverall<-NA
stations_sno$summerTempIncrease<-NA

for(it_sno in 1:45){
  it_stn = which(stations$ID==stations_sno$ID[it_sno])
  

    streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
    # streamData<-streamDataAll[streamDataAll$ID=="08LB020",]
    # streamData$Date<-streamData$Date%>%ymd()
    
    
    streamData$Discharge30 = zoo::rollmean(streamData$Discharge,30,fill = NA)
    
    streamData$DayOfYear<-yday(ymd(streamData$Date))
    streamData$Discharge7Summer<-streamData$Discharge7
    streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
    
    
    streamDataYrly<-streamData%>%
      dplyr::group_by(year)%>%
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
                       minSumFlowDay = DayOfYear[findMin(Discharge7Summer)],
                       minSumFlowMonth = month[findMin(Discharge7Summer)],
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
    
    if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
      streamDataYrly$minSumFlow<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
    }
    
    
    Q_filled<-streamData[streamData$year>=1993,c("year","DayOfYear","Discharge7")]
    
    
    Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]<-
      Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]-1
    set.seed(123)
    for(it in 1:365){
      sum_notNA<-sum(!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
      if(sum_notNA>0){
        sum_NA<-sum(is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
        Q_filled$Discharge7[is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it]<-
          sample(Q_filled$Discharge7[!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it],size = sum_NA,replace = TRUE)
      }
      
    }
    if(sum(is.na(Q_filled$Discharge7))>0){
      sprintf("%d of 365 days with no filled Q for station %s - %s",sum(is.na(Q_filled$Discharge7[1:365])),stations$ID[it_stn],stations$Station.Name[it_stn])
    }
    
    DroughtIndic1 = quantile(Q_filled$Discharge7,0.3,type =5,na.rm =TRUE)
    DroughtIndic2 = quantile(Q_filled$Discharge7,0.2,type =5,na.rm =TRUE)
    DroughtIndic3 = quantile(Q_filled$Discharge7,0.1,type =5,na.rm =TRUE)
    DroughtIndic4 = quantile(Q_filled$Discharge7,0.05,type =5,na.rm =TRUE)
    DroughtIndic5 = quantile(Q_filled$Discharge7,0.02,type =5,na.rm =TRUE)
    
    MAD = mean(Q_filled$Discharge7,na.rm = TRUE)
    
    
    
    CEFT = max(c(min(streamDataYrly$minFlow30CP,na.rm = TRUE),0.05*MAD))
    if(stations$regime[it_stn]=="Rainfall"){
      CEFT = max(c(min(streamDataYrly$minFlow30CP,na.rm = TRUE),0.02*MAD))
      
    }
    
    
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
    
    Div<-read.csv(paste0("2.data/2.working/WaterUse/",stations$ID[it_stn],".csv"))
    # names(licensedDiv)[2]<-"lcd_SW_div"
    # licensedDiv$lcd_SW_div.cms<-licensedDiv$lcd_SW_div/(365*24*3600)# convert to cms
    #
    # licensedDiv$lcd_SW_div.August.cms<-licensedDiv$August/(31*24*3600)
    # licensedDiv$lcd_SW_div.September.cms<-licensedDiv$September/(30*24*3600)
    # licensedDiv$lcd_SW_div.October.cms<-licensedDiv$October/(31*24*3600)
    # numWells<-read.csv(paste0("wellRecords/",stations$StationNum[it_stn],".csv"))
    # names(numWells)[2]<-"constructedWells"
    
    dataYearly2<-left_join(dataYearly2,Div,by = c("WaterYear"= "year"))
    
    dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1901&dataYearly2$WaterYear<2023,]
    # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
    
    
    dataYearly2_trA<-dataYearly2[!is.na(dataYearly2$minAugFlow7),]
    dataYearly2_trS<-dataYearly2[!is.na(dataYearly2$minSepFlow7),]
    
    dataYearly2_trO<-dataYearly2[!is.na(dataYearly2$minOctFlow7),]
    
    modelAug<-eval(AugustBestModel[[stations$ID[1]]][[1]])%>%
      update(data = dataYearly2,na.action = "na.exclude")
    
    modelSep<-eval(SeptemberBestModel[[stations$ID[1]]][[1]])%>%
      update(data = dataYearly2,na.action = "na.exclude")
    dataYearly2$residsAug<-residuals(modelAug)
    dataYearly2$residsSep<-residuals(modelSep)
    
    if(!stations$ID[it_stn]%in%c("08OA004","08OA005")){
      modelOct<-eval(OctoberBestModel[[stations$ID[1]]][[1]])%>%
        update(data = dataYearly2,na.action = "na.exclude")
      dataYearly2$residsOct<-residuals(modelOct)
      
      predMin<-pmin(predict(modelAug,dataYearly2,se.fit = TRUE)$fit,
                    predict(modelSep,dataYearly2,se.fit = TRUE)$fit,
                    predict(modelOct,dataYearly2,se.fit = TRUE)$fit
      )
      
    }else{
      predMin<-pmin(predict(modelAug,dataYearly2,se.fit = TRUE)$fit,
                    predict(modelSep,dataYearly2,se.fit = TRUE)$fit)
    }
    
    
    dataYearly2_comp<-filter(dataYearly2,WaterYear<=1930|WaterYear>=1993)%>%
      group_by(WaterYear>1950)%>%
      summarise(across(where(is.numeric),~mean(.x)))
    
    
    stations_sno$summerTempIncrease[it_sno]<-
      dataYearly2_comp$Temp_MJJASO[2]-
      dataYearly2_comp$Temp_MJJASO[1]
    
    stations_sno$OctoberTempIncrease[it_sno]<-dataYearly2_comp$Temp_O[2]-
      dataYearly2_comp$Temp_O[1]
    
    dataYearly2_comp<-rbind(dataYearly2_comp,dataYearly2_comp[2,]-dataYearly2_comp[1,])%>%
      select(names(modelOct$coefficients)[-1])
    deltas<-dataYearly2_comp[3,]*modelOct$coefficients[-1]
    
    summary(modelOct)
    paste("Overall change is ",deltas%>%sum()%>%round(3))%>%print()
    
    paste("Summer Precip accounts for ",deltas%>%select(starts_with("Precip"))%>%sum()%>%round(3))%>%print()
    paste("Summer temperature accounts for ",deltas%>%select(starts_with("Temp"))%>%sum()%>%round(3))%>%print()
    paste("Winter Precip accounts for ",deltas%>%select(contains("winterPrecip"))%>%sum()%>%round(3))%>%print()
    paste("Winter Temperature accounts for ",deltas%>%select(contains("meanWinterTemp"))%>%sum()%>%round(3))%>%print()
    paste("Winter conditions overall account for ",deltas%>%select(contains("winter"))%>%sum()%>%round(3))%>%print()
    
    stations_sno$overallChange[it_sno]<-deltas%>%sum()
    stations_sno$SummerPChange[it_sno]<-deltas%>%select(starts_with("Precip"))%>%sum()
    stations_sno$SummerTChange[it_sno]<-deltas%>%select(starts_with("Temp"))%>%sum()
    stations_sno$WinterPChange[it_sno]<-deltas%>%select(contains("winterPrecip"))%>%sum()
    stations_sno$WinterTChange[it_sno]<-deltas%>%select(contains("meanWinterTemp"))%>%sum()
    stations_sno$WinterConditionsOverall[it_sno]<-deltas%>%select(contains("winter"))%>%sum()
    stations_sno$waterUseChange[it_sno]<-deltas%>%select(contains("October.cms"))%>%sum()
    
    # stations_sno$summerTempIncrease[it_sno]<-
    
    # predict(modelOct,dataYearly2_comp,se.fit = TRUE)$fit
    # predict()
    # Predictions
    dat<-data.frame(ID = stations$ID[it_stn],
                    Station.Name = stations$Station.Name[it_stn],
                    WaterYear =1901:2022,
                    predMin = predMin,
                    CEFT= CEFT,
                    D5 = DroughtIndic5)
    
    
    toc()
    naInds<-which(is.na(streamDataYrly$minSumFlow))
    naInds<-naInds[!naInds%in%c(1,dim(streamDataYrly)[1])]
    streamDataYrly$minSumFlow_filled<-streamDataYrly$minSumFlow
    if(length(naInds)>0){
      streamDataYrly$minSumFlow_filled[naInds]<-
        (streamDataYrly$minSumFlow_filled[naInds+1]+
           streamDataYrly$minSumFlow_filled[naInds-1])/2
      
    }
    streamDataYrly$minSumFlow.10<-zoo::rollmean(streamDataYrly$minSumFlow_filled,
                                                k=10,
                                                align = "right",
                                                fill = NA)
    dat$predMin.m3s<-exp(dat$predMin)
    dat$predMin.m3s.10<-zoo::rollmean(dat$predMin.m3s,
                                      k=10,
                                      align = "right",
                                      fill = NA)
    p1<-ggplot(streamDataYrly)+
      geom_line(aes(x = year,y = minSumFlow.10,colour = "measured",linetype = "measured"))+
      geom_line(data = dat,aes(x = WaterYear,y = predMin.m3s.10,colour = "predicted",linetype = "predicted"))+
      geom_hline(aes(yintercept = CEFT,colour = "CEFT",linetype = "CEFT"))+
      geom_hline(aes(yintercept = DroughtIndic5,colour = "Drought 5",linetype = "Drought 5"))+
      scale_y_continuous(name = expression(Q7[min]~(m^3/s)~10~year~mean))+
      scale_x_continuous(limits = c(1900,2022))+
      scale_colour_manual(breaks = c("measured","predicted","CEFT","Drought 5"),
                          values = c("black","#1B9E77","#D95F02","#7570B3"),
                          name = "")+
      scale_linetype_manual(breaks = c("measured","predicted","CEFT","Drought 5"),
                            values = c(1,1,2,3),
                            name = "")+
      ggtitle(label = paste(stations$Station.Name[it_stn],", ", stations$ID[it_stn],", ", stations$regime[it_stn]))+
      theme_bw()+
      theme(plot.title = element_text(hjust=0.03, vjust=-8,size = 8))
    # print(p1)
    
  
}

stations_sno%>%
  summarize(across(overallChange:waterUseChange,~mean(.x)))
# ggsave(plot = p1,filename = paste0("Figures/SimulatedHistorical/",stations$Station.Name[it_stn], "-",stations$ID[it_stn],".png"),
#        width = 6,height = 4)
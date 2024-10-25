
# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-01


# This script evaluates residual autocorrelation in the regression models
# It also runs the trend analysis on the residuals to evaluate stationarity

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
library(dplyr)

getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")
Div<-readRDS("2.data/2.working/WaterUse/estimates_all.RDS")
streamDataMonthly<-left_join(streamDataMonthly,Div[,c("ID","year","Total.cms")])

WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")

autoCorr<-function(dataYearly2){
  dat<-data.frame(WaterYear = seq(min(dataYearly2$WaterYear),max(dataYearly2$WaterYear)))
  dat<-left_join(dat,dataYearly2[,c("WaterYear","resids")])
  dat$residsLagged<-c(NA,dat$resids[1:(nrow(dat)-1)])
  lmSum<-summary(lm(resids~residsLagged,data = dat))
  x<-lmtest::bgtest(dat$resids)
  return(lmSum$coefficients[2,4])
}

# Breusch-Godfrey test (programmed to not use NA, only lag-1)
BG.Test<-function(model_x,dataYearly2){
  
  
  model_x2<-update(model_x,data= dataYearly2,na.action = "na.exclude")
  dataYearly2$resids<-residuals(model_x2)
  
  dataYearly2$resids.1<-c(0,dataYearly2$resids[1:(nrow(dataYearly2)-1)])
  
  bgModel<-update(model_x2,resids~.+resids.1)
  bg.stat<-nobs(bgModel)*summary(bgModel)$r.squared
  # p.val<-1-pchisq(bg.stat,df = 1)
  return(bg.stat)
}
miniFunc<-function(it_seed,model_x2,dataYearly2,resids.1){
  set.seed(it_seed)
  dataYearly2$resids.1[!is.na(resids.1)]<-base::sample(resids.1,sum(!is.na(resids.1)),replace = FALSE)
  
  bgModel<-update(model_x2,resids~.+resids.1)
  return(nobs(bgModel)*summary(bgModel)$r.squared)
}
BG.Test.reorder<-function(model_x,dataYearly2){
  
  
  model_x2<-update(model_x,data= dataYearly2,na.action = "na.exclude")
  dataYearly2$resids<-residuals(model_x2)
  
  resids.1<-c(0,dataYearly2$resids[1:(nrow(dataYearly2)-1)])
  dataYearly2$resids.1<-NA
  
  dataYearly2<-dataYearly2[!is.na(resids.1),]
  resids.1<-resids.1[!is.na(resids.1)]
  bg.stat<-c()
  # it_seed_list<-as.list( 1:10000)
  # names(it_seed_list)[1:10000]<-"it_seed"
  # tictoc::tic()
  # bg.stat<-lapply(X = it_seed_list,FUN= miniFunc,model_x2 = model_x2,dataYearly2 = dataYearly2,resids.1=resids.1)
  # 
  # quantile(bg.stat%>%unlist(),0.95)
  # tictoc::toc()
  tictoc::tic()
  for(it_reorder in 1:10000){
    
    
    dataYearly2$resids.1[!is.na(resids.1)]<-base::sample(resids.1,sum(!is.na(resids.1)),replace = FALSE)
    
    bgModel<-update(model_x2,resids~.+resids.1)
    bg.stat[it_reorder]<-nobs(bgModel)*summary(bgModel)$r.squared
    # p.val<-1-pchisq(bg.stat,df = 1)
  }
  tictoc::toc()
  bg.stat.crit<-quantile(bg.stat,0.95,)
  return(bg.stat.crit)
}




stations$AugLag_BG<-NA
stations$AugLag_BG.crit<-NA
stations$SepLag_BG<-NA
stations$SepLag_BG.crit<-NA
stations$OctLag_BG<-NA
stations$OctLag_BG.crit<-NA


## Analyse autocorrelation of relationships

BG_result<-data.frame()
for(it_stn in 1:length(stations$ID)){
  tictoc::tic()
  station_x = stations[it_stn,]
  streamData_x = streamDataMonthly%>%filter(ID %in% station_x$ID)
  data_x = WeatherData%>%filter(ID %in% station_x$ID)
  
  
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",station_x$ID,".rds"))
  
  # MonModels_KGEs<-readRDS(paste0("2.data/2.working/RegressionOptimization/catchment_KGEs/step1_",station_x$ID,".rds"))
  
  month_bgn<-max(station_x$minSumFlowMonth-1,station_x$SDD)
  
  month_end<-min(station_x$minSumFlowMonth+1,station_x$SAD)
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  data_tested_meas<-data.frame()
  
  
  
  
  for(it_mn in month_rng){
    
    streamDataYrly<-streamData_x%>%
      filter(month==it_mn)
    
    
    
    
    
    
    ## Weather data
    
    # data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
    
    # MonBestModel<-list()
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
        Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
        Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
        Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        
        
      )
    dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
                             !is.na(dataYearly$Temp_1),]
    
    
    
    dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
    dataYearly2<-dataYearly2[dataYearly2$WaterYear<2012,]
    
    
    
    dataYearly2<-dataYearly2[dataYearly2$WaterYear>1945,]
    
    
    
    dataYearly2<-dataYearly2[!is.na(dataYearly2$minMonFlow7)&
                               !is.na(dataYearly2$Precip_1)&
                               !is.na(dataYearly2$Temp_1),]
    
    dataYearly2_trX<-dataYearly2
    
    modelsMon<-update(MonBestModel[[it_mn]],data = dataYearly2_trX)
    
    
    dataYearly2_trX$resids<-modelsMon$residuals
    
    BG_result<-rbind(BG_result,
                     data.frame(ID = station_x$ID,
                                Month = it_mn,
                                BG.Test = BG.Test(modelsMon,dataYearly2%>%
                                                    filter(WaterYear>=min(dataYearly2_trX$WaterYear)&
                                                             WaterYear<=max(dataYearly2_trX$WaterYear))),
                                BG.Test.crit =  BG.Test.reorder(modelsMon,dataYearly2%>%
                                                                  filter(WaterYear>=min(dataYearly2_trX$WaterYear)&
                                                                           WaterYear<=max(dataYearly2_trX$WaterYear))))
    )
    
    
    
    
    
  }
  
  
  
  
  
  tictoc::toc()
  print(sprintf("Done %d catchments",it_stn))
}
BG_result<-BG_result%>%select(!regime)
BG_result<-left_join(BG_result,stations[,c("ID","regime","minSumFlowMonth")])
sum(BG_result$BG.Test>BG_result$BG.Test.crit)/nrow(BG_result)
sum((BG_result$BG.Test>BG_result$BG.Test.crit)[BG_result$Month==BG_result$minSumFlowMonth])/nrow(BG_result[BG_result$Month==BG_result$minSumFlowMonth,])

BG_result%>%
  filter(Month==minSumFlowMonth)%>%
  group_by(regime)%>%
  dplyr::summarize(frac = mean(BG.Test>BG.Test.crit),
            bt = binom.test(x=sum(BG.Test>BG.Test.crit),n(),p=0.05,alternative = "greater")$p.value)


write.csv(BG_result,"4.output/stations_autocorrelation.csv",row.names = FALSE)
BG_result<-read.csv("4.output/stations_autocorrelation.csv")
BG_result2<-BG_result%>%
  group_by(ID)%>%
  dplyr::summarize(numSignif = sum(BG.Test>BG.Test.crit)/n())
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")
watersheds<-left_join(watersheds,BG_result2)

tmap_mode("view")
tm_shape(watersheds)+tm_polygons(col = "numSignif")


## Check stationarity of residuals


# MK_test_reorder<-function(x){
#   
#   
#   tictoc::tic()
#   MK.Tau<-c()
#   for(it_reorder in 1:10000){
#     set.seed(it_reorder)
#     resids<-sample(x,length(x),replace = FALSE)
#     
#     MK_test<-modifiedmk::mmkh3lag(resids)
#     
#     MK.Tau[it_reorder]<-MK_test[6]
#     # p.val<-1-pchisq(bg.stat,df = 1)
#   }
#   tictoc::toc()
#  
#   return(quantile(MK.Tau,0.95))
# }
stationarity_result<-data.frame()

for(it_stn in 1:length(stations$ID)){
  tictoc::tic()
  # station_x =
  
  
  
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",stations$ID[it_stn],".rds"))
  
  # MonModels_KGEs<-readRDS(paste0("2.data/2.working/RegressionOptimization/catchment_KGEs/step1_",station_x$ID,".rds"))
  
  month_bgn<-max(stations$minSumFlowMonth[it_stn]-1,stations$SDD[it_stn])
  
  month_end<-min(stations$minSumFlowMonth[it_stn]+1,stations$SAD[it_stn])
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  
  for(it_mn in month_rng){
    
    # for(it_mn in(month_end)){
    
    MK_test<-modifiedmk::mmkh3lag(residuals(MonBestModel[[it_mn]]))
    
    
    
    stationarity_result<-rbind(stationarity_result,
                               data.frame(ID = stations$ID[it_stn],
                                          Month = it_mn,
                                          MK.p = MK_test[2],
                                          sen.slope = MK_test[7],
                                          
                                          MK.tau = MK_test[6]
                                          # MK.tau.crit =  MK_test_reorder(residuals(MonBestModel[[it_mn]]))
                                          
                               ))
  }
  
  tictoc::toc()
  print(sprintf("Done %d catchments",it_stn))
}
stationarity_result<-left_join(stationarity_result,stations[,c("ID","regime")])
stationarity_result%>%
  # group_by(regime)%>%
  dplyr::summarize(n=n(),
            frac = mean(MK.p<0.05),
            bt = binom.test(x=sum(MK.p<0.05),n(),p=0.05,alternative = "greater")$p.value,
            fracNeg = sum(sen.slope<0)/n(),
            bt.Neg = binom.test(sum(sen.slope<0),n(),p=0.5)$p.value
  )

stationarity_result%>%
  group_by(regime)%>%
  dplyr::summarize(n=n(),
            frac = sum(MK.p<0.05)/n(),
            bt = binom.test(x=sum(MK.p<0.05),n(),p=0.05,alternative = "greater")$p.value,
            fracNeg = sum(sen.slope<0)/n(),
            bt.Neg = binom.test(sum(sen.slope<0),n(),p=0.5)$p.value
  )


# examine stationarity but use overall model residuals


stationarity<-stations[,c("ID","regime")]
for(it_stn in 1:length(stations$ID)){
  station_x = stations[it_stn,]
  streamData_x = streamDataMonthly%>%filter(ID %in% station_x$ID)
  data_x = WeatherData%>%filter(ID %in% station_x$ID)
  
  
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",station_x$ID,".rds"))
  
  # MonModels_KGEs<-readRDS(paste0("2.data/2.working/RegressionOptimization/catchment_KGEs/step1_",station_x$ID,".rds"))
  
  month_bgn<-max(station_x$minSumFlowMonth-1,station_x$SDD)
  
  month_end<-min(station_x$minSumFlowMonth+1,station_x$SAD)
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  data_tested_meas<-data.frame()
  
  
  for(it_mn in month_rng){
    
    streamDataYrly<-streamData_x%>%
      filter(month==it_mn)
    
    
    
    
    
    
    ## Weather data
    
    # data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
    
    # MonBestModel<-list()
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
        Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
        Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
        Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        
        
      )
    dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
                             !is.na(dataYearly$Temp_1),]
    
    #dataYearly$Temp_1<-residuals(lm(Temp_1~Precip_1,data = dataYearly))
    #dataYearly$Temp_2<-residuals(lm(Temp_2~Precip_2,data = dataYearly))
    #dataYearly$Temp_3<-residuals(lm(Temp_3~Precip_3,data = dataYearly))
    #dataYearly$Temp_4<-residuals(lm(Temp_4~Precip_4,data = dataYearly))
    #dataYearly$Temp_6<-residuals(lm(Temp_6~Precip_6,data = dataYearly))
    #dataYearly$Temp_8<-residuals(lm(Temp_8~Precip_8,data = dataYearly))
    #dataYearly$Temp_12<-residuals(lm(Temp_12~Precip_12,data = dataYearly))
    
    
    
    
    dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
    dataYearly2<-dataYearly2[dataYearly2$WaterYear<2012,]
    
    
    
    dataYearly2<-dataYearly2[dataYearly2$WaterYear>1945,]
    
    
    
    dataYearly2<-dataYearly2[!is.na(dataYearly2$minMonFlow7)&
                               !is.na(dataYearly2$Precip_1)&
                               !is.na(dataYearly2$Temp_1),]
    
    dataYearly2_trX<-dataYearly2
    
    modelsMon<-update(MonBestModel[[it_mn]],data = dataYearly2_trX)
    
    dataYearly2_trX$resMon<-(predict(modelsMon))
    
    data_tested_meas<-rbind(data_tested_meas,dataYearly2_trX[,c("WaterYear","resMon")])
    
    
    # modifiedmk::mmkh3lag(residuals(modelsMon))
    # modifiedmk::mmkh3lag(dataYearly2_trX$resMon-log(dataYearly2_trX$minMonFlow7))
    # modifiedmk::mmkh3lag(exp(dataYearly2_trX$resMon)-(dataYearly2_trX$minMonFlow7))
  }
  
  streamDataYrly<-streamData_x%>%
    filter(month%in%month_rng)%>%
    group_by(year)%>%
    dplyr::summarise(minSumFlow.log = log(min(minMonFlow7)))
  
  cvDat<-data_tested_meas%>%
    group_by(WaterYear)%>%
    dplyr::summarise(predMin = min(resMon))%>%
    inner_join(streamDataYrly,by = c("WaterYear" = "year"))
  
  cvDat<-cvDat%>%filter(!is.na(minSumFlow.log))
  
  
  MK_test<-modifiedmk::mmkh3lag(cvDat$minSumFlow.log-cvDat$predMin)
  
  
  stationarity$MK.p[it_stn]<-MK_test[2]
  
  stationarity$sen.slope[it_stn]<-MK_test[7]
  
}

stationarity%>%
  group_by(regime)%>%
  dplyr::summarize(frac = mean(MK.p<0.05),
            bt = binom.test(x=sum(MK.p<0.05),n(),p=0.05,alternative = "greater")$p.value,
            fracNeg = sum(sen.slope<0)/n(),
            bt.Neg = binom.test(sum(sen.slope<0),n(),p=0.5)$p.value,
            n=n()
  )

stationarity%>%
  # group_by(regime)%>%
  dplyr::summarize(frac = mean(MK.p<0.05),
            bt = binom.test(x=sum(MK.p<0.05),n(),p=0.05,alternative = "greater")$p.value,
            fracNeg = sum(sen.slope<0)/n(),
            bt.Neg = binom.test(sum(sen.slope<0),n(),p=0.5)$p.value
  )

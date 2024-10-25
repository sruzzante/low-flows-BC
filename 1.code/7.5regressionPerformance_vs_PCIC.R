# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-08


# This script reruns the model fitting procedure using the PWNAmet data, and compares the performance to PCIC's VIC-GL models 


closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(scico)
library(stringr)
library(sf)
library(scico)
library(hydroGOF)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")
Div<-readRDS("2.data/2.working/WaterUse/estimates_all.RDS")
streamDataMonthly<-left_join(streamDataMonthly,Div[,c("ID","year","Total.cms")])


pcic_models<-list.files("2.data/1.original/PCIC_station_hydro_model_out/",pattern = ".ascii")%>%str_remove(".ascii")
pcic_models[!(tolower(pcic_models)%in%tolower(stations$Station.Name))]
pcic_models<-pcic_models[tolower(pcic_models)%in%tolower(stations$Station.Name)]

stations<-stations[stations$Station.Name%in%pcic_models,]%>%
  filter(!ID=="08ME028")




# WeatherData<-read.csv("WeatherDataANUSPLIN/dataMonthly.csv")
WeatherData<-readRDS("2.data/2.working/WeatherDataPWNAmet/dataMonthly.RDS")

GoF_PCIC <- stations[,c("ID","Station.Name","regime")]
GoF_PCIC_all <- data.frame()

GoF_log_PCIC_all<- data.frame()
GoF_sqrt_PCIC_all<- data.frame()
for(it in 1:55){
  
  streamData_x = streamDataMonthly%>%filter(ID %in% stations$ID[it])
  
  month_bgn<-max(stations$minSumFlowMonth[it]-1,stations$SDD[it])
  
  month_end<-min(stations$minSumFlowMonth[it]+1,stations$SAD[it])
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  
  
  streamDataYrly<-streamData_x%>%
    filter(month%in%month_rng)%>%
    dplyr::summarise(minSumFlow = min(minMonFlow7))
  
  
  # pcic_models[tolower(pcic_models)%in%tolower(watersheds$NameNom)]
  
  PCIC_model<-read.csv(paste0("2.data/1.original/PCIC_station_hydro_model_out/",stations$Station.Name[it],".ascii"),skip = 1)
  PCIC_model$Date<-ymd(PCIC_model$Date)
  PCIC_model$month = month(PCIC_model$Date)
  
  PCIC_model$year = year(PCIC_model$Date)
  PCIC_model$Discharge<-PCIC_model$PNWNAmet
  PCIC_model$Discharge7<-RcppRoll::roll_meanr (PCIC_model$PNWNAmet,n=7,fill = NA)
  PCIC_model$Discharge7Summer<-PCIC_model$Discharge7
  PCIC_model$Discharge7Summer[!PCIC_model$month%in%month_rng]<-NA
  
  
  
  
  PCIC_modelYrly<-PCIC_model%>%
    filter(year%in%(1946:2012))%>%
    group_by(year)%>%
    dplyr::summarize(
      
      minSumFlow = min(Discharge7Summer,na.rm = TRUE)
    )
  
  
  
  data<-inner_join(streamDataYrly,PCIC_modelYrly,by = "year")
  # ggplot(data,aes(minSumFlow.x,minSumFlow.y))+geom_point()
  
  data<-filter(data,!is.na(data$minSumFlow.x))
  GoF_PCIC$R2[it]<-hydroGOF::R2(log(data$minSumFlow.x),log(data$minSumFlow.y),use = "complete")
  
  
  GoF_PCIC$RMSE[it]<-(data$minSumFlow.x-data$minSumFlow.y)^2%>%
    mean(na.rm = TRUE)%>%
    sqrt()
  
  GoF_PCIC$KGE_sqrt[it] <- KGE(sqrt(data$minSumFlow.y),sqrt(data$minSumFlow.x))
  GoF_PCIC$NSE_log[it] <- NSE(log(data$minSumFlow.y),log(data$minSumFlow.x))
  GoF_PCIC$KGE[it] <- KGE((data$minSumFlow.y),(data$minSumFlow.x))
  GoF_PCIC$NSE[it] <- NSE((data$minSumFlow.y),(data$minSumFlow.x))
  
  GoF_PCIC$pbias[it] <- hydroGOF::pbias(data$minSumFlow.y,data$minSumFlow.x)
  
  GoF_PCIC$r[it] <- cor(data$minSumFlow.y,data$minSumFlow.x)
  
  
  
  GOF2<-cbind(stations[it,c("ID","Station.Name","regime")],
              gof(sim = data$minSumFlow.y,obs = data$minSumFlow.x,  epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  GoF_PCIC_all<-rbind(GoF_PCIC_all,GOF2)
  
  
  
  
  GOF3<-cbind(stations[it,c("ID","Station.Name","regime")],
              gof(sim = data$minSumFlow.y,obs = data$minSumFlow.x, fun = sqrt, epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  
  GoF_sqrt_PCIC_all<-rbind(GoF_sqrt_PCIC_all,GOF3)
  
  GOF4<-cbind(stations[it,c("ID","Station.Name","regime")],
              gof(sim = data$minSumFlow.y,obs = data$minSumFlow.x, fun = log, epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  GoF_log_PCIC_all<-rbind(GoF_log_PCIC_all,GOF4)
}


write.csv(GoF_PCIC,"4.output/GoF_PCIC.csv")

GoF_PCIC<-read.csv("4.output/GoF_PCIC.csv")

GoF_Regression<-stations[,c("ID","Station.Name","regime")]%>%
  filter(!ID=="08ME028")
GoF_Regression_all<-data.frame()
GoF_sqrt_Regression_all<-data.frame()
GoF_log_Regression_all<-data.frame()
for(it_stn in 1:length(stations$ID)){
  station_x = stations[it_stn,]
  streamData_x = streamDataMonthly%>%filter(ID %in% station_x$ID)
  data_x = WeatherData%>%filter(ID %in% station_x$ID)
  
  # if(!file.exists(paste0("2.data/2.working/RegressionOptimization/BestModels/PNWNAmet/step2_lm_",station_x$ID,".rds"))){next}
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/PNWNAmet/step2_lm_",station_x$ID,".rds"))
  
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
    dataYearly2<-dataYearly2[dataYearly2$WaterYear<=2012,]
    
    
    
    dataYearly2<-dataYearly2[dataYearly2$WaterYear>1945,]
    
    
    
    dataYearly2<-dataYearly2[!is.na(dataYearly2$minMonFlow7)&
                               !is.na(dataYearly2$Precip_1)&
                               !is.na(dataYearly2$Temp_1),]
    
    dataYearly2_trX<-dataYearly2
    
    modelsMon<-update(MonBestModel[[it_mn]],data = dataYearly2_trX)
    
    dataYearly2_trX$resMon<-exp(predict(modelsMon))
    
    data_tested_meas<-rbind(data_tested_meas,dataYearly2_trX[,c("WaterYear","resMon")])
    
  }
  
  streamDataYrly<-streamData_x%>%
    filter(month%in%month_rng)%>%
    group_by(year)%>%
    dplyr::summarise(minSumFlow = min(minMonFlow7))
  
  cvDat<-data_tested_meas%>%
    group_by(WaterYear)%>%
    dplyr::summarise(predMin = min(resMon))%>%
    inner_join(streamDataYrly,by = c("WaterYear" = "year"))
  
  
  GoF_Regression$RMSE[it_stn]<-((cvDat$predMin)-(cvDat$minSumFlow))^2%>%
    mean(na.rm = TRUE)%>%
    sqrt()  
  
  GoF_Regression$KGE[it_stn]<-KGE(cvDat$predMin,cvDat$minSumFlow)
  
  GoF_Regression$NSE[it_stn]<-NSE(cvDat$predMin,cvDat$minSumFlow)
  
  GoF_Regression$KGE_sqrt[it_stn]<-KGE(sqrt(cvDat$predMin),sqrt(cvDat$minSumFlow))
  
  GoF_Regression$NSE_log[it_stn]<-NSE(log(cvDat$predMin),log(cvDat$minSumFlow))
  
  GoF_Regression$pbias[it_stn] <- hydroGOF::pbias(cvDat$predMin,cvDat$minSumFlow)
  GoF_Regression$r[it_stn] <- cor(cvDat$predMin,cvDat$minSumFlow)
  
  # GoF_Regression$me[it_stn] <- hydroGOF::me(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$mae[it_stn] <- hydroGOF::mae(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$mse[it_stn] <- hydroGOF::mse(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rmse[it_stn] <- hydroGOF::rmse(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$ubRMSE[it_stn] <- hydroGOF::ubRMSE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$nrmse[it_stn] <- hydroGOF::nrmse(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rsr[it_stn] <- hydroGOF::rsr(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rSD[it_stn] <- hydroGOF::rSD(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$mNSE[it_stn] <- hydroGOF::mNSE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rNSE[it_stn] <- hydroGOF::rNSE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$wNSE[it_stn] <- hydroGOF::wNSE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$d[it_stn] <- hydroGOF::d(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$dr[it_stn] <- hydroGOF::dr(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$md[it_stn] <- hydroGOF::md(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rd[it_stn] <- hydroGOF::rd(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$cp[it_stn] <- hydroGOF::cp(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rPearson[it_stn] <- hydroGOF::rPearson(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$R2[it_stn] <- hydroGOF::R2(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$br2[it_stn] <- hydroGOF::br2(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$KGElf[it_stn] <- hydroGOF::KGElf(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$KGEnp[it_stn] <- hydroGOF::KGEnp(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$sKGE[it_stn] <- hydroGOF::sKGE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$VE[it_stn] <- hydroGOF::VE(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$rSpearman[it_stn] <- hydroGOF::rSpearmanpbiasfdc(cvDat$predMin,cvDat$minSumFlow)
  # GoF_Regression$pbiasfdc[it] <- hydroGOF::pbiasfdc(cvDat$predMin,cvDat$minSumFlow)
  GOF2<-cbind(stations[it_stn,c("ID","Station.Name","regime")],
              gof(sim = cvDat$predMin,obs = cvDat$minSumFlow,  epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  GoF_Regression_all<-rbind(GoF_Regression_all,GOF2)
  
  GOF3<-cbind(stations[it_stn,c("ID","Station.Name","regime")],
              gof(sim = cvDat$predMin,obs = cvDat$minSumFlow, fun = sqrt, epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  GoF_sqrt_Regression_all<-rbind(GoF_sqrt_Regression_all,GOF3)
  GOF4<-cbind(stations[it_stn,c("ID","Station.Name","regime")],
              gof(sim = cvDat$predMin,obs = cvDat$minSumFlow, fun = log, epsilon.type="Pushpalatha2012", do.spearman=TRUE, do.pbfdc=TRUE)%>%
                t()%>%
                data.frame())
  
  GoF_log_Regression_all<-rbind(GoF_log_Regression_all,GOF4)
  
}

PCIC_compare<-left_join(GoF_Regression,
                        GoF_PCIC,
                        by = c("ID","Station.Name","regime"),
                        suffix = c(".reg",".VIC"))

ggplot(PCIC_compare,aes(KGE.reg,KGE.VIC,color = regime))+geom_point()+geom_abline()
ggplot(PCIC_compare,aes(NSE.reg,NSE.VIC,color = regime))+geom_point()+geom_abline()

ggplot(PCIC_compare,aes(KGE_sqrt.reg,KGE_sqrt.VIC,color = regime))+geom_point()+geom_abline()
ggplot(PCIC_compare,aes(NSE_log.reg,NSE_log.VIC,color = regime))+geom_point()+geom_abline()



sum(PCIC_compare$KGE.reg>PCIC_compare$KGE.VIC) # KGE is better in 53/55 catchments
sum(PCIC_compare$KGE_sqrt.reg>PCIC_compare$KGE_sqrt.VIC) # KGE_sqrt is better in 53/55 catchments

sum(PCIC_compare$NSE.reg>PCIC_compare$NSE.VIC) #NSE is better in 54/55 catchments
sum(PCIC_compare$NSE_log.reg>PCIC_compare$NSE_log.VIC) #NSE_log is better in 55/55 catchments

sum(abs(PCIC_compare$pbias.reg)<abs(PCIC_compare$pbias.VIC)) #PBIAS is better in 53/55 catchments
sum(PCIC_compare$RMSE.reg<PCIC_compare$RMSE.VIC) #RMSE is smaller in 55/55 catchments





sum(PCIC_compare$KGE.VIC< -0.41)
sum(PCIC_compare$KGE.reg< -0.41)

sum(PCIC_compare$KGE_sqrt.VIC< -0.41)
sum(PCIC_compare$KGE_sqrt.reg< -0.41)

sum(PCIC_compare$NSE.reg<0)
sum(PCIC_compare$NSE.VIC<0)
sum(PCIC_compare$NSE_log.reg<0)
sum(PCIC_compare$NSE_log.VIC<0)




summarizeFunc<-function(x){
  medx<-median(x,na.rm = TRUE)%>%round(2)
  minx = min(x,na.rm = TRUE)%>%round(2)
  maxx = max(x,na.rm = TRUE)%>%round(2)
  return(paste0(medx," (",minx,", ",maxx,")"))
}
PCIC_compare$regime<-factor(PCIC_compare$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"))

summaryTable<-PCIC_compare%>%
  
  dplyr::select(Station.Name,ID,regime,KGE.reg:pbias.reg,KGE_sqrt.VIC:pbias.VIC)%>%
  # group_by(regime)%>%
  dplyr::summarise(across(c(KGE.reg:pbias.reg,KGE_sqrt.VIC:pbias.VIC),~summarizeFunc(.x)))%>%
  # melt(id.vars = "regime")
  reshape2::melt(id.vars = NULL)

summaryTable$stat<-str_remove(summaryTable$variable,".reg|.VIC")
summaryTable$model<-str_detect(summaryTable$variable,"VIC")%>%
  plyr::mapvalues(from = c(0,1),to = c("Regression","VIC-GL"))

summaryTable2<-summaryTable%>%
  tidyr::pivot_wider(id_cols = c(stat),
                     names_from = model,
                     values_from = value)
summaryTable2
summary(PCIC_compare$regime)

funcBetter = function(name,fracGreater){
  
  fracBetter = fracGreater
  msk<-name%in%c("MAE","ME","MSE","NRMSE..","PBIAS..",
                 "RMSE","RSR","pbiasFDC..",
                 "rSD","ubRMSE")
  fracBetter[msk] = 1-fracGreater[msk]
  fracBetter
}

# untransformed
x1<-rbind(GoF_Regression_all%>%mutate(model = "reg"),
         GoF_PCIC_all%>%mutate(model = "VIC")%>%
           filter(ID%in%GoF_log_Regression_all$ID))%>%
  mutate(PBIAS..=abs(PBIAS..),
         pbiasFDC..=abs(pbiasFDC..),
         rSD=abs(log(rSD)),
         ME = abs(ME),
         r = abs(r))%>%
  
  tidyr::pivot_longer(cols = c(ME:pbiasFDC..))%>%
  
  tidyr::pivot_wider(id_cols = c(ID,Station.Name,regime,name),
                     values_from = value,
                     names_from = c(model))%>%
  mutate(regGreater = reg>VIC)%>%
  group_by(name)%>%
  dplyr::summarise(fracGreater = mean(regGreater))%>%
  dplyr::mutate(fracBetter = funcBetter(name,fracGreater))%>%print(n=100)
mean(x1$fracBetter)

# sqrt
x2<-rbind(GoF_sqrt_Regression_all%>%mutate(model = "reg"),
      GoF_sqrt_PCIC_all%>%mutate(model = "VIC")%>%
        filter(ID%in%GoF_log_Regression_all$ID))%>%
  mutate(PBIAS..=abs(PBIAS..),
         pbiasFDC..=abs(pbiasFDC..),
         rSD=abs(log(rSD)),
         ME = abs(ME),
         r = abs(r))%>%
  
  tidyr::pivot_longer(cols = c(ME:pbiasFDC..))%>%
  
  tidyr::pivot_wider(id_cols = c(ID,Station.Name,regime,name),
                     values_from = value,
                     names_from = c(model))%>%
  mutate(regGreater = reg>VIC)%>%
  group_by(name)%>%
  dplyr::summarise(fracGreater = mean(regGreater))%>%
  dplyr::mutate(fracBetter = funcBetter(name,fracGreater))%>%print(n=100)
mean(x2$fracBetter)


## log
x3<-rbind(GoF_log_Regression_all%>%mutate(model = "reg"),
      GoF_log_PCIC_all%>%mutate(model = "VIC")%>%
        filter(ID%in%GoF_log_Regression_all$ID))%>%
  mutate(PBIAS..=abs(PBIAS..),
         pbiasFDC..=abs(pbiasFDC..),
         rSD=abs(log(rSD)),
         ME = abs(ME),
         r = abs(r))%>%
  
  tidyr::pivot_longer(cols = c(ME:pbiasFDC..))%>%
  
  tidyr::pivot_wider(id_cols = c(ID,Station.Name,regime,name),
                     values_from = value,
                     names_from = c(model))%>%
  mutate(regGreater = reg>VIC)%>%
  group_by(name)%>%
  dplyr::summarise(fracGreater = mean(regGreater,na.rm = TRUE))%>%
  dplyr::mutate(fracBetter = funcBetter(name,fracGreater))%>%print(n=100)
mean(x3$fracBetter)

rbind(x1,x2,x3)%>%dplyr::summarize(mean(fracBetter),
                                   min(fracBetter
                                       ))
# PBIASfdc can't really be calculated

dplyr::summarise(across(ME:pbiasFDC..,function(x,model){x[model=="reg"]>x[model=="VIC"]}))




PCIC_compare_all<- left_join(GoF_Regression_all,
                             GoF_PCIC_all,
                             by = c("ID","Station.Name","regime"),
                             suffix = c(".reg",".VIC"))




stations_cv<-read.csv("2.data/2.working/StationMetadata/stations_performance.csv")

PCIC_compare_2<-inner_join(GoF_PCIC,
                           stations_cv%>%select(ID,KGE:pbias)%>%dplyr::rename(KGE_sqrt = KGE.sqrt,NSE_log = NSE.log),
                           by = c("ID"),
                           suffix = c(".VIC",".reg"))


summaryTable<-PCIC_compare_2%>%
  
  dplyr::select(Station.Name,ID,regime,KGE.reg:pbias.reg,KGE_sqrt.VIC:pbias.VIC)%>%
  # group_by(regime)%>%
  dplyr::summarise(across(c(KGE.reg:pbias.reg,KGE_sqrt.VIC:pbias.VIC),~summarizeFunc(.x)))%>%
  # melt(id.vars = "regime")
  reshape2::melt(id.vars = NULL)

summaryTable$stat<-str_remove(summaryTable$variable,".reg|.VIC")
summaryTable$model<-str_detect(summaryTable$variable,"VIC")%>%
  plyr::mapvalues(from = c(0,1),to = c("Regression","VIC-GL"))

summaryTable2<-summaryTable%>%
  tidyr::pivot_wider(id_cols = c(stat),
                     names_from = model,
                     values_from = value)

sum(PCIC_compare_2$KGE.reg>PCIC_compare_2$KGE.VIC)
sum(PCIC_compare_2$KGE_sqrt.reg>PCIC_compare_2$KGE_sqrt.VIC)
sum(PCIC_compare_2$NSE.reg>PCIC_compare_2$NSE.VIC)
sum(PCIC_compare_2$NSE_log.reg>PCIC_compare_2$NSE_log.VIC)

summary(PCIC_compare$regime)

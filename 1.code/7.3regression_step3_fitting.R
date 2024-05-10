
library(dplyr)

library(lubridate)
library(tictoc)
# library(MuMIn)
# library(hydroGOF)
#setwd("/project/def-tgleeson/ruzzante/low-flows-WNA/regressionOptimization_1/") #Set the working directory


streamDataMonthly<-readRDS("../2.data/2.working/Discharge/streamDataMonthly_2.RDS")
stations<-readRDS("../2.data/2.working/stationMetadata/stations_final_2.RDS")
WeatherData<-readRDS("../2.data/2.working/WeatherDataANUSPLIN/dataMonthly_2.RDS")
Div<-readRDS("../2.data/2.working/WaterUse/estimates_all.RDS")

streamDataMonthly<-left_join(streamDataMonthly,Div[,c("ID","year","Total.cms")])


print("loaded data")
#for(it in 1:nrow(stations)){
for(it in c(1:230)){
  station_x = stations[it,]
  streamData_x = streamDataMonthly%>%filter(ID %in% station_x$ID)
  data_x = WeatherData%>%filter(ID %in% station_x$ID)
  
  tictoc::tic()
  
  
  MonBestModel<-readRDS(paste0("../2.data/2.working/RegressionOptimization/BestModels/step1_",station_x$ID,".rds"))
  
  
  # MonBestModel<-list()
  MonBestModel_fitted<-list()
  
  month_bgn<-max(station_x$minSumFlowMonth-1,station_x$SDD)
  
  month_end<-min(station_x$minSumFlowMonth+1,station_x$SAD)
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  
  data_tested_meas_3<-data.frame()
  
  for(it_mn in month_rng){
    
    streamDataYrly<-streamData_x%>%
      filter(month==it_mn)
    
    
    
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    # print("here-2")
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
        # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        
        Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
        Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
        
        Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
        Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
        
        
        
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
    dataYearly2<-dataYearly2[dataYearly2$WaterYear<2023,]
    
    
    
    dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1900,]
    
    
    
    dataYearly2<-dataYearly2[!is.na(dataYearly2$minMonFlow7)&
                               !is.na(dataYearly2$Precip_1)&
                               !is.na(dataYearly2$Temp_1),]
    # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
    
    if(nrow(dataYearly2)<20){print(it_mn);next}
    
    
    # Fit the model to all the data
    
    dataYearly2_trX<-dataYearly2
    MonBestModel_fitted[[it_mn]]<-eval(MonBestModel[[it_mn]][[1]])
    
  }
  
  
  saveRDS(MonBestModel_fitted,paste0("../2.data/2.working/RegressionOptimization/BestModels/step2_lm_",station_x$ID,".rds"))
  
  
  print(paste("Done",it,"catchments"))
  tictoc::toc()
  
}




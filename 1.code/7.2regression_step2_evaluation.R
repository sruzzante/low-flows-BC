# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-09-30


# This script evaluates the 'best' regression models chosen in the script 7.1regression_step1_optimization.R
# It runs a 10 x 5-fold cross-validation.
# It saves the performance data to 2.data/2.working/stationMetadata/stations_performance.csv

library(dplyr)

library(lubridate)
library(MuMIn)
library(hydroGOF)

#setwd(getSrcDirectory(function(){})[1])
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory
# setwd("projects/def-tgleeson/ruzzante/low-flows-WNA/regressionOptimization_1/")


streamDataMonthly<-readRDS("./2.data/2.working/Discharge/streamDataMonthly.RDS")
stations<-readRDS("./2.data/2.working/stationMetadata/stations_final.RDS")
WeatherData<-readRDS("./2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")
Div<-readRDS("./2.data/2.working/WaterUse/estimates_all.RDS")

streamDataMonthly<-left_join(streamDataMonthly,Div[,c("ID","year","Total.cms")])


print("loaded data")
allMonModels_KGEs<-list()
allBestModels<-list()


lm_test<-function(model_x){
  predict(model_x,dataYearly2_te)
}    

KGE_func<-function(y,x){
  res<-hydroGOF::KGE(y,x,na.rm = TRUE,out.type = "full")$KGE.elements
  
  if(is.na(res[1])){res[1]<-0}
  
  ED<-
    sqrt(
      (res[1]-1)^2 +
        (res[2]-1)^2 +
        (res[3]-1)^2
    )
  return(1-ED)
  
  
}


# 
# streamData_ls<-split(streamDataMonthly,streamDataMonthly$ID )
# # streamData_ls<-streamData_ls[inds]
# WeatherData<-WeatherData[order(WeatherData$ID),]
# WeatherData_ls<-split(WeatherData,WeatherData$ID )
# # WeatherData_ls<-WeatherData_ls[inds]
# stations<-stations[order(stations$ID),]
# stations_ls<-split(stations,stations$ID,)




print("starting loop")

for(it in 1:nrow(stations)){
  station_x = stations[it,]
  streamData_x = streamDataMonthly%>%filter(ID %in% station_x$ID)
  data_x = WeatherData%>%filter(ID %in% station_x$ID)
  
  tictoc::tic()
  # tictoc::tic()
  # it_stn = which(stations$ID=="08HA003")
  # streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  ## Weather data
  
  # data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
  # station_x = stations_ls[[1]]
  # streamData_x = streamData_ls[[1]]
  # data_x = WeatherData_ls[[1]]
  
  MonModels_KGEs<-list()
  
  print("here-1")
  # MonBestModel<-list()
  MonBestModel<-readRDS(paste0("./2.data/2.working/RegressionOptimization/BestModels/step1_",station_x$ID,".rds"))
  
  # MonModels_KGEs<-readRDS(paste0("2.data/2.working/RegressionOptimization/catchment_KGEs/step1_",station_x$ID,".rds"))
  
  month_bgn<-max(station_x$minSumFlowMonth-1,station_x$SDD)
  
  month_end<-min(station_x$minSumFlowMonth+1,station_x$SAD)
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  data_tested_meas_3<-data.frame()
  

  for(it_mn in month_rng){
    
    # if(it_mn<=length(MonModels_KGEs)){
    #   if(!is.null(MonModels_KGEs[[it_mn]])){next}
    # }
    
    
    
    
    
    streamDataYrly<-streamData_x%>%
      filter(month==it_mn)
    
    
    
    
    
    
    ## Weather data
    
    # data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
    
    # MonBestModel<-list()
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    # print("here-2")
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
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
    
    if(nrow(dataYearly2)<20){next}
    
    
    # August Regressions ####
    # combinationsMon<-list()
    # combinationsSep<-list()
    # combinationsOct<-list()
    
    data_testedMon = data.frame()
    # data_testedSep = data.frame()
    # data_testedOct = data.frame()
    data_tested_meas<-data.frame()
    
    
    # Create model for Month with all predictors
    dataYearly2_trX<-dataYearly2
    
    .GlobalEnv$dataYearly2_trX <- dataYearly2_trX
       #combinationsMon <- dredge(globalmodelMon,evaluate = FALSE,subset = Precip_1|Precip_2)
    
    data_tested_meas_2<-data.frame()
    for(it_seed in 1:10){
      set.seed(it_seed+100)
      
      sets<-sample(1:nrow(dataYearly2)%%5+1,nrow(dataYearly2),replace = FALSE)
      
      data_tested_meas<-data.frame()
      for(it_cv in 1:5){
        # Divide training and testing data
        dataYearly2_trX<-dataYearly2[-which(sets%in%it_cv),] 
        dataYearly2_te<-dataYearly2[which(sets%in%it_cv),]
        
        .GlobalEnv$dataYearly2_trX <- dataYearly2_trX
        .GlobalEnv$dataYearly2_te <- dataYearly2_te
        # dataYearly2_trX<-dataYearly2_tr[!is.na(dataYearly2_tr$minAugFlow7),]
        
        
        # Take surface water diversions out if they are small (less than 5% of the min flow in any given year)
        
        
        
        
        # evaluate models
        modelsMon<-eval(MonBestModel[[it_mn]][[1]],dataYearly2_trX)
        
        
        # extract predictions for each model
        resMon<-predict(modelsMon,dataYearly2_te)
        
        
        # Bind predicted low flows
        # data_testedMon<-rbind(data_testedMon,resMon)
        
        dataYearly2_te$predict<-exp(resMon)
        # data_testedSep<-rbind(data_testedSep,resSep)
        # data_testedOct<-rbind(data_testedOct,resOct)
        
        #Bind measured low flows
        data_tested_meas<-rbind(data_tested_meas,dataYearly2_te)
        
        
      }
      
      data_tested_meas$seed<-it_seed
      # KGEs_Mon[it_seed,1:50]<-apply(data_testedMon,2,FUN = function(x){KGE(sqrt(exp(x)),sqrt(data_tested_meas$minMonFlow7))})
      data_tested_meas_2<-rbind(data_tested_meas_2,data_tested_meas)
      
      
    }
    data_tested_meas_2$it_month<-it_mn
    data_tested_meas_3<-rbind(data_tested_meas_3,data_tested_meas_2)
    
  }
  
  data_tested_meas_4<-data_tested_meas_3%>%
    group_by(WaterYear,seed)%>%
    dplyr::summarize(minMonFlow7 = min(minMonFlow7),
              minMonFlow7.predict = min(predict),
              n_ = n())
  
  perform_data<-data_tested_meas_4%>%
    group_by(seed)%>%
    dplyr::summarize(
      KGE_ = KGE_func(minMonFlow7.predict,minMonFlow7),
      KGE_sqrt = KGE_func(sqrt(minMonFlow7.predict),sqrt(minMonFlow7)),
      NSE_ = NSE(minMonFlow7.predict,minMonFlow7),
      NSE_log = NSE(log(minMonFlow7.predict),log(minMonFlow7)),
      R2 = hydroGOF::R2(minMonFlow7.predict,minMonFlow7),
      pbias = hydroGOF::pbias(minMonFlow7.predict,minMonFlow7),
      RMSE = ((minMonFlow7.predict-minMonFlow7)^2)%>%
        mean()%>%
        sqrt()
    )
  
  stations$KGE[it]<-mean(perform_data$KGE_)
  stations$KGE.sqrt[it]<-mean(perform_data$KGE_sqrt)
  stations$NSE[it]<-mean(perform_data$NSE_)
  stations$NSE.log[it]<-mean(perform_data$NSE_log)
  stations$R2[it]<-mean(perform_data$R2)
  stations$RMSE[it]<-mean(perform_data$RMSE)
  stations$pbias[it]<-mean(perform_data$pbias)
  
  
  print(paste("Done",it,"catchments"))
  tictoc::toc()  # MonModels_KGEs
}

write.csv(stations,"./2.data/2.working/stationMetadata/stations_performance.csv",row.names = FALSE)

#

# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script optimizes the regressions using 10 x 5-fold cross-validation with all subsets of variables.
# It saves the best model to a file named step1_XXXXXXX_.rds, where XXXXXXX is the station ID
# the script takes a while and was run on a high performance cluster

args = commandArgs(trailingOnly=TRUE)

# test if there is at least two arguments: if not, return an error
if (length(args)<1) {
  stop("At least 1 arguments must be supplied (partition)", call.=FALSE)
}

partition    <- as.integer( args[1] )  # read first argument as integer

print(paste("Processing with partition:", partition))

library(dplyr)

library(lubridate)
library(MuMIn)
library(hydroGOF)
#library(foreach)
#library(doParallel)

#setwd(getSrcDirectory(function(){})[1])
#setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/1.code")) #Set the working directory
# setwd("/project/def-tgleeson/ruzzante/low-flows-WNA/regressionOptimization_1/") #Set the working directory

streamDataMonthly<-readRDS("../2.data/2.working/Discharge/streamDataMonthly_2.RDS")
stations<-readRDS("../2.data/2.working/stationMetadata/stations_final_2.RDS")
WeatherData<-readRDS("../2.data/2.working/WeatherDataANUSPLIN/dataMonthly_2.RDS")
Div<-readRDS("../2.data/2.working/WaterUse/estimates_all.RDS")

print("loaded data")
allMonModels_KGEs<-list()
allBestModels<-list()

streamDataMonthly<-left_join(streamDataMonthly,Div[,c("ID","year","Total.cms")])


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


# registerDoParallel(cores=6)
# 
# cl <- makeCluster(48)
# #Register cluster
# registerDoParallel(cl)

# stations<-stations%>%filter(ID=="11143000")
# streamDataMonthly<-streamDataMonthly%>%filter(ID=="11143000")
# WeatherData<-WeatherData%>%filter(ID=="11143000")
streamDataMonthly<-streamDataMonthly[order(streamDataMonthly$ID),]
streamData_ls<-split(streamDataMonthly,streamDataMonthly$ID )
# streamData_ls<-streamData_ls[inds]
WeatherData<-WeatherData[order(WeatherData$ID),]
WeatherData_ls<-split(WeatherData,WeatherData$ID )
# WeatherData_ls<-WeatherData_ls[inds]
stations<-stations[order(stations$ID),]
stations_ls<-split(stations,stations$ID,)


streamdata_x<-streamData_ls[[partition]]

station_x<-stations_ls[[partition]]

data_x<-WeatherData_ls[[partition]]



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
MonBestModel<-list()

# MonModels_KGEs<-readRDS(paste0("2.data/2.working/RegressionOptimization/catchment_KGEs/step1_",station_x$ID,".rds"))

month_bgn<-max(station_x$minSumFlowMonth-1,station_x$SDD)

month_end<-min(station_x$minSumFlowMonth+1,station_x$SAD)

# month_end<-min(month_end,length(MonBestModel))
month_rng<-((month_bgn:month_end)-1)%%12+1

tryWaterUse = FALSE
if(any(streamdata_x$Total.cms>streamdata_x$minMonFlow7*0.1,na.rm = TRUE)){
  tryWaterUse = TRUE
} 

for(it_mn in month_rng){
  
  # if(it_mn<=length(MonModels_KGEs)){
  #   if(!is.null(MonModels_KGEs[[it_mn]])){next}
  # }
  
  
  
  
  
  streamDataYrly<-streamdata_x%>%
    filter(month==it_mn)
  
  
  
  
  
  
  ## Weather data
  
  # data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]
  
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
  #dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
  #                        !is.na(dataYearly$Temp_1),]
  
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
  combinationsMon<-list()
  # combinationsSep<-list()
  # combinationsOct<-list()
  
  data_testedMon = data.frame()
  # data_testedSep = data.frame()
  # data_testedOct = data.frame()
  data_tested_meas<-data.frame()
  
  
  # Create model for Month with all predictors
  dataYearly2_trX<-dataYearly2
  
  .GlobalEnv$dataYearly2_trX <- dataYearly2_trX
  globalmodelMon <- lm(log(minMonFlow7)~
                         Temp_1+Temp_2+Temp_3+Temp_4+Temp_6+Temp_8+Temp_12+
                         Precip_1+Precip_2+Precip_3+Precip_4+Precip_6+Precip_8+Precip_12,
                       data= dataYearly2_trX,
                       na.action = "na.fail")
  
  
  if(tryWaterUse){
    globalmodelMon<-update(globalmodelMon,~.+Total.cms)
  }
  
  
  
  # Make all combinations of predictor variables
  # combinationsMon <- dredge(globalmodelMon,evaluate = FALSE,subset = Precip_1|Precip_2|Precip_3|Precip_4|Precip_6|Precip_8|Precip_12)
  combinationsMon <- dredge(globalmodelMon,evaluate = FALSE,subset = Precip_1|Precip_2)
  
  KGEs_Mon<-data.frame()
  for(it_seed in 1:10){
    set.seed(it_seed)
    
    sets<-sample(1:nrow(dataYearly2)%%5+1,nrow(dataYearly2),replace = FALSE)
    
    
    for(it_cv in 1:5){
      
      tictoc::tic()
      # Divide training and testing data
      dataYearly2_trX<-dataYearly2[-which(sets%in%it_cv),] 
      dataYearly2_te<-dataYearly2[which(sets%in%it_cv),]
      
      .GlobalEnv$dataYearly2_trX <- dataYearly2_trX
      .GlobalEnv$dataYearly2_te <- dataYearly2_te
      # dataYearly2_trX<-dataYearly2_tr[!is.na(dataYearly2_tr$minAugFlow7),]
      
      
      # Take surface water diversions out if they are small (less than 5% of the min flow in any given year)
      
      
      
      
      # evaluate models
      modelsMon<-lapply(combinationsMon,eval)
      
      
      # extract predictions for each model
      resMon<-sapply(modelsMon,FUN = lm_test)
      
      
      # Bind predicted low flows
      data_testedMon<-rbind(data_testedMon,resMon)
      # data_testedSep<-rbind(data_testedSep,resSep)
      # data_testedOct<-rbind(data_testedOct,resOct)
      
      #Bind measured low flows
      data_tested_meas<-rbind(data_tested_meas,dataYearly2_te)
      print(sprintf("Done it_cv %d of it_seed %d",it_cv,it_seed))
      tictoc::toc()
    }
    
    
    KGEs_Mon[it_seed,1:length(combinationsMon)]<-apply(data_testedMon,2,FUN = function(x){KGE_func(sqrt(exp(x)),sqrt(data_tested_meas$minMonFlow7))})
    
    
  }
  KGEs_Mon2<-apply(KGEs_Mon,2,mean)
  
  
  MonModels_KGEs[[it_mn]]<-KGEs_Mon2
  
  
  MonBestModel[[it_mn]]<-combinationsMon[which.max(KGEs_Mon2)]
  
}

# allMonModels_KGEs[[it_stn]]<-MonModels_KGEs
# allBestModels[[it_stn]]<-MonBestModel
# tictoc::toc()
# print(sprintf("Done %d catchments",it_stn))
names(MonModels_KGEs)<-paste0(station_x$ID,"_",1:length(MonModels_KGEs))
saveRDS(MonModels_KGEs,paste0("../2.data/2.working/RegressionOptimization/Catchment_KGEs/step1_",station_x$ID,".rds"))
names(MonBestModel)<-paste0(station_x$ID,"_",1:length(MonBestModel))
saveRDS(MonBestModel,paste0("../2.data/2.working/RegressionOptimization/BestModels/step1_",station_x$ID,".rds"))
# MonModels_KGEs



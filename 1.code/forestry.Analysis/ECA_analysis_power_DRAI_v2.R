# 
# save(list = c("AugustBestModel","SeptemberBestModel","OctoberBestModel","stn_MC_ls",
#               "streamDataMonthly_ls","WeatherData_ls"),
#      file = "4.output/ECA_power_analysis.RData"
# )

args = commandArgs(trailingOnly=TRUE)

# test if there is at least two arguments: if not, return an error
if (length(args)<2) {
  stop("At least 2 arguments must be supplied (ECA, routine)", call.=FALSE)
}

ECA    <- as.character(args[1])  # read first argument as integer
routine <- as.character(args[2])

load("../2.data/2.working/ECA/ECA_power_analysis.RData")

library(foreach)
library(doParallel)
library(dplyr)
library(tidyr)
library(forecast)
# registerDoParallel(cores=6)

cl <- makeCluster(48)
#Register cluster
registerDoParallel(cl)

stn_MC_result<-foreach(stn_MC_x = stn_MC_ls,
                       streamDataYrly = streamDataMonthly_ls,
                       data_x = WeatherData_ls,
                       bestModel_x = bestModels,
                       # fctrs = fctrs,
                       .packages=c("dplyr","forecast")
)%dopar%{
  # stn_MC_x = stn_MC_ls[[1]]
  # streamDataYrly = streamDataMonthly_ls[[1]]
  # data_x = WeatherData_ls[[1]]
  # AugustBestModel_x = AugustBestModel[[1]]
  # SeptemberBestModel_x = SeptemberBestModel[[1]]
  # OctoberBestModel_x = OctoberBestModel[[1]]
  
  
  fctrs <- c(0,1,2,5,10,20,50,100)
  # fctrs <- c(0,1)
  stn_MC_x[,paste0("fracSignif_",fctrs)]<-NA
  stn_MC_x[,paste0("fracNeg_",fctrs)]<-NA
  
  
  
  month_rng<-lapply(bestModel_x,function(x){!is.null(x)})%>%unlist()%>%which()
  
  
  dat_predict<-data.frame()
  
  for(it_mn in month_rng){
    
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
        Total.cms = Total.cms[1]
        
        
        
      )
    dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
                             !is.na(dataYearly$Temp_1),]
    
    dat_predict<-rbind(dat_predict,
                       data.frame(year = dataYearly$WaterYear,
                                  month = it_mn,
                                  predMin = predict(bestModel_x[[it_mn]],newdata = dataYearly)))
    
    
  }
  
  
  predictMins<-dat_predict%>%
    group_by(year)%>%
    dplyr::summarise(predMin = (min(predMin)))
  dataYearly2<-left_join(streamDataYrly,predictMins,by = c("NovWaterYear" = "year"))
  
  resids<-log(dataYearly2$minSumFlow7)-dataYearly2$predMin
  # for(it_stn in 1:nrow(stn_MC)){
  # tictoc::tic()
  # ECA_x<-dplyr::filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
  # streamData<-streamDataAll[streamDataAll$ID==stn_MC$ID[it_stn],]
  # streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stn_MC$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  # 
  # if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
  #          streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
  #   print(stn_MC$ID[it_stn])
  #   stns_with_brks = c(stns_with_brks,stn_MC$ID[it_stn])
  # }
  
  
  
  # fitECA60<-auto.arima(streamDataYrly$ECA_60,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # 
  # 
  # fitECA20<-auto.arima(streamDataYrly$ECA_20,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA10<-auto.arima(streamDataYrly$ECA_10,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA5<-auto.arima(streamDataYrly$ECA_5,trace = TRUE,
  #                     max.p = 1,max.d=1,max.q = 0,
  #                     allowdrift = FALSE,
  #                     stepwise=FALSE , approximation=FALSE)
  
  # fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  # fitECA60<-sarima(streamDataYrly$ECA_60,1,1,0)
  # fitECA20<-arima(streamDataYrly$ECA_20,c(1,1,0))
  # fitECA10<-arima(streamDataYrly$ECA_10,c(1,1,0))
  # fitECA5<-arima(streamDataYrly$ECA_5,c(1,1,0))

  if(routine == "A"){
    if(ECA=="ECA_60"){
      fitECA<-auto.arima(dataYearly2$ECA_60)
    }else if(ECA == "ECA_5"){
      fitECA<-auto.arima(dataYearly2$ECA_5)
    }
    
  }else if(routine == "B"){
    if(ECA=="ECA_60"){
      fitECA<-auto.arima(dataYearly2$ECA_60)
    }else if(ECA == "ECA_5"){
      fitECA<-auto.arima(dataYearly2$ECA_5)
    }
    
  }else if(routine=="C"){
    if(ECA=="ECA_60"){
      fitECA<-arima(dataYearly2$ECA_60,c(0,0,0))
    }else if(ECA == "ECA_5"){
      fitECA<-arima(dataYearly2$ECA_5,c(0,0,0))
    }
    
  }
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  
  # medianQ7<-median(streamDataYrly$minSumFlow7)
  sd.log.Q7<-sd(log(streamDataYrly$minSumFlow7))
  
  # fctrs <- c(0, 0.1, 0.25,0.5,1)
  
  for(it_fctr in 1:length(fctrs)){
    p_vals<-c()
    neg<-c()
    # tictoc::tic()
    for(it_MC in 1:1000){
      set.seed(it_MC)
      if(routine=="A"){
        Q7_i.log<-log(dataYearly2$minSumFlow7[sample(1:nrow(dataYearly2),nrow(dataYearly2),replace = FALSE)])
      }else if(routine%in%c("B","C")){
        Q7_i.log<-resids[sample(1:length(resids),length(resids),replace = FALSE)]
      }
      # 
      
      
      if(ECA=="ECA_60"){
        Q7_i.log_shift<-Q7_i.log - (dataYearly2$ECA_60*fctrs[it_fctr]*sd.log.Q7)
      }else if(ECA == "ECA_5"){
        Q7_i.log_shift<-Q7_i.log - (dataYearly2$ECA_5*fctrs[it_fctr]*sd.log.Q7)
      }
      
      if(routine == "A"){
        fitQ7<- auto.arima(Q7_i.log_shift)
        
      }else if(routine == "B"){
        fitQ7<- auto.arima(Q7_i.log_shift)
        
      }else if(routine=="C"){
        fitQ7<- arima(Q7_i.log_shift,c(0,0,0))
        
      }
      
      
      # logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(residuals(fitECA), residuals(fitQ7),method = "spearman")
      # c0.60<-cor.test(streamDataYrly$ECA_60, Q7_i.log_shift,method = "spearman")
      # c0.60
      # plot(as.numeric(streamDataYrly$ECA_60),Q7_i.log_shift)
      # cor.test(streamDataYrly$ECA_60, logQ7.60,method = "spearman")
      p_vals[it_MC]<-c0.60$p.value
      
      # plot(Q7_i.log_shift,logQ7.60)
      # 
      # 
      # plot(as.numeric(residuals(fitECA60)),Q7_i.log)
      # plot(as.numeric(residuals(fitECA60)),Q7_i.log_shift)
      # plot(as.numeric(streamDataYrly$ECA_60),Q7_i.log_shift)
      # plot(as.numeric(streamDataYrly$ECA_60),((streamDataYrly$ECA_60*fctrs[it_fctr])*sd.log.Q7))
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i_shift))
      # plot(streamDataYrly$ECA_5,log(Q7_i_shift))
      
      neg[it_MC]<-c0.60$estimate<0
      # ccf(resid(fitECA5$fit), logQ7.60,na.action = na.omit)
      
      
    }
    # tictoc::toc()
    stn_MC_x[paste0("fracNeg_",fctrs[it_fctr])] <- sum(neg)/1000
    stn_MC_x[paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05&neg)/1000
  }
  
  # tictoc::toc()
  return(stn_MC_x)
}
stn_MC<-stn_MC_result%>%bind_rows()

fctrs <- c(0,1,2,5,10,20,50,100)

rbinomfunc <- function(prob){rbinom(10000,1,prob)}
binom.testfunc<-function(x,p_exp){binom.test(sum(x),length(x),p=p_exp)$p.value}


stn_MC<-dplyr::filter(stn_MC,!is.na(fracSignif_0))
type2Errors = data.frame(fctrs,
                         H1_rate_rain = NA,
                         H1_rate_hybr = NA,
                         H1_rate_snow = NA,
                         H2_rate_rain = NA,
                         H2_rate_hybr = NA,
                         H2_rate_snow = NA)


for(it_fctr in 1:length(fctrs)){
  set.seed(1)
  x<-sapply(stn_MC[,paste0("fracSignif_",fctrs[it_fctr])],rbinomfunc)
  
  x2_rain<-apply(x[,stn_MC$regime=="Rainfall"],1,binom.testfunc,0.05)
  x2_hybr<-apply(x[,stn_MC$regime=="Hybrid"  ],1,binom.testfunc,0.05)
  x2_snow<-apply(x[,stn_MC$regime=="Snowfall"],1,binom.testfunc,0.05)
  
  # type2Errors$H1_rate[it_fctr]<- 1-(sum(x2<0.05)/10000)
  type2Errors$H1_rate_rain[it_fctr]<- 1-(sum(x2_rain<0.05)/10000)
  type2Errors$H1_rate_hybr[it_fctr]<- 1-(sum(x2_hybr<0.05)/10000)
  type2Errors$H1_rate_snow[it_fctr]<- 1-(sum(x2_snow<0.05)/10000)
  
  x<-sapply(stn_MC[,paste0("fracNeg_",fctrs[it_fctr])],rbinomfunc)
  x2_rain<-apply(x[,stn_MC$regime=="Rainfall"],1,binom.testfunc,0.5)
  x2_hybr<-apply(x[,stn_MC$regime=="Hybrid"  ],1,binom.testfunc,0.5)
  x2_snow<-apply(x[,stn_MC$regime=="Snowfall"],1,binom.testfunc,0.5)
  type2Errors$H2_rate_rain[it_fctr]<- 1-(sum(x2_rain<0.05)/10000)
  type2Errors$H2_rate_hybr[it_fctr]<- 1-(sum(x2_hybr<0.05)/10000)
  type2Errors$H2_rate_snow[it_fctr]<- 1-(sum(x2_snow<0.05)/10000)
  
}



type2Errors_2<-pivot_longer(type2Errors,cols = H1_rate_rain:H2_rate_snow)
type2Errors_2$hypo<-substr(type2Errors_2$name,1,2)
type2Errors_2$regime<-substr(type2Errors_2$name,9,12)%>%
  factor(levels = c("rain","hybr","snow"),
         labels = c("Rainfall","Hybrid","Snowmelt"))

saveRDS(type2Errors_2,file = paste0("../4.output/ECA_power_",routine,"_",ECA,".rds"))

closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(sf)
library(tmap)
library(lubridate)
library(bcmaps)
library(stringr)
library(terra)
library(cowplot)
library(scico)
library(grwat)
library(reshape2)
library(ggplot2)
library(ggpattern)
library(lm.beta)
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


library(foreach)
library(doParallel)
# registerDoParallel(cores=6)

cl <- makeCluster(22)
#Register cluster
registerDoParallel(cl)

stn_MC_result<-foreach(stn_MC_x = stn_MC_ls,
                       streamDataYrly = streamDataMonthly_ls,
                       # fctrs = fctrs,
                       .packages=c("dplyr","forecast")
)%dopar%{
  # stn_MC_x = stn_MC_ls[[1]]
  # streamDataYrly = streamDataMonthly_ls[[1]]
  
  fctrs <- c(0,1,2,5,10,20,50,100)
  # fctrs <- c(0,1)
  stn_MC_x[,paste0("fracSignif_",fctrs)]<-NA
  stn_MC_x[,paste0("fracNeg_",fctrs)]<-NA

  
  
  # for(it_stn in 1:nrow(stn_MC)){
  # tictoc::tic()
  # ECA_x<-filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
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
  fitECA5<-auto.arima(streamDataYrly$ECA_5)
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
      Q7_i.log<-log(streamDataYrly$minSumFlow7[sample(1:nrow(streamDataYrly),nrow(streamDataYrly),replace = FALSE)])
      Q7_i.log_shift<-Q7_i.log - (streamDataYrly$ECA_5*fctrs[it_fctr]*sd.log.Q7)
      fitQ7<- auto.arima(Q7_i.log_shift)
      
      # logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(residuals(fitECA5), residuals(fitQ7),method = "spearman")
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
    stn_MC_x[paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05)/1000
  }
  
  # tictoc::toc()
  return(stn_MC_x)
}
stn_MC<-stn_MC_result%>%bind_rows()


rbinomfunc <- function(prob){rbinom(10000,1,prob)}
binom.testfunc<-function(x,p_exp){binom.test(sum(x),length(x),p=p_exp)$p.value}


stn_MC<-filter(stn_MC,!is.na(fracSignif_0))
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

ggplot(type2Errors_2,aes(x = (fctrs+1)))+
  geom_line(aes(y = value,col = regime,linetype = hypo),linewidth =1)+
  # geom_line(aes(y = H2_rate,col = "H2"))+
  scale_x_log10(name = "Harvest Effect (# standard deviations)",labels = function(x){x-1},
                # breaks = c(1,2,4,11,51,101)
                breaks = unique(fctrs)+1,
                minor_breaks = NULL
                )+
  scale_y_continuous(name = "Type II Error Rate")+
  scale_color_manual(values = c("#005804","#599D7A","#B2E3F0"),
                     name = "Regime")+
  scale_linetype(name = "Hypothesis")+
  theme_bw()
ggsave("3.figures/ECA_power_Zhang_Wei.png",width=5,height = 3)

## what if I try using model residuals?



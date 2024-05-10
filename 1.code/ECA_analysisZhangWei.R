
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
## Loop through stations and extract variables ################



stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(StationNum,Area_km2),
                    by = c("ID" = "StationNum"))





# Zhang and Wei 2012 analysis ###########
# 08KE016


library("tseries")
library(forecast)
stnRs<-data.frame()
stns_with_brks<-c()
sum(stations$PrivateForestry>0.1)
for(it_stn in 1:nrow(stations)){
  # it_stn = which(stations$ID=="08HA003")
  if(stations$PrivateForestry[it_stn]>0.1|stations$Total.Cut.Area[it_stn]<=0.1){next}
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
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
  
  
  
  streamDataYrly<-streamData%>%
    
    dplyr::group_by(NovWaterYear)%>%
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
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stations$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stations$ID[it_stn])
  }
  
  
  
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
  
  fitECA60<-auto.arima(streamDataYrly$ECA_60)
  fitECA20<-auto.arima(streamDataYrly$ECA_20)
  fitECA10<-auto.arima(streamDataYrly$ECA_10)
  fitECA5<-auto.arima(streamDataYrly$ECA_5)
  
  
  
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  logQ7<-auto.arima(log(streamDataYrly$minSumFlow7))
  
  
  # logQ7.60<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA60$coef,fitECA60$coef),sides = 1)
  # logQ7.20<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA20$coef,fitECA20$coef),sides = 1)
  # logQ7.10<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA10$coef,fitECA10$coef),sides = 1)
  # logQ7.5<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA5$coef,fitECA5$coef),sides = 1)
  
  
  # ccf(fitECA60$residuals,logQ7.60,na.action=na.omit,lag.max=10 )
  # ccf(fitECA20$residuals,logQ7.20,na.action=na.omit)
  # ccf(fitECA10$residuals,logQ7.10,na.action=na.omit)
  # ccf(fitECA5$residuals,logQ7.5,na.action=na.omit)
  
  # 
  # acf(streamDataYrly$ECA_60)
  # pacf(streamDataYrly$ECA_60)
  # diff1x=diff(streamDataYrly$ECA_60,1)
  # acf(diff1x)
  # pacf(diff1x)
  # plot(logQ7.60)
  c0.60<-cor.test(logQ7$residuals,fitECA60$residuals,method = "spearman")
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  c1.60<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA60$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.60<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA60$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  c0.20<-cor.test(logQ7$residuals,fitECA20$residuals,method = "spearman")
  
  c1.20<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA20$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.20<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA20$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  
  
  c0.10<-cor.test(logQ7$residuals,fitECA10$residuals,method = "spearman")
  
  c1.10<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA10$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.10<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA10$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  
  c0.5<-cor.test(logQ7$residuals,fitECA5$residuals,method = "spearman")
  
  c1.5<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                 fitECA5$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.5<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                 fitECA5$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-2)])
  # 
  # 
  # c1.20<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.20<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA$residuals)-2)])
  # 
  # c0.60<-cor.test(fitQ$residuals,fitECA60$residuals)
  # 
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-2)])
  stnRs<-rbind(stnRs,
               data.frame(
                 ID = stations$ID[it_stn],
                 regime = stations$regime[it_stn],
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
                 c2.5.p =c2.5$p.value
                 
                 # c2.60 =c2$estimate,
                 # c2.60.p =c2$p.value
               ))
  
}

# stnRs$maxR<-
sum(stnRs$c0.60>0)

stnRs<-left_join(watersheds,stnRs,by = c( "StationNum" = "ID"))

stnRs<-left_join(stnRs,stations)
stnRs<-filter(stnRs,!is.na(c0.60))

saveRDS(stnRs,"4.output/ECA_analysis_A_stnRs.rds")
# tmap_mode("view")
# tm_shape(stnRs%>%filter(!is.na(c0.60)))+tm_dots(col = "c0.60")
# 
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Area_km2,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)+
#   scale_x_log10()
# 
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c1.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.10))+geom_point(aes(color = c0.10.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.5))+geom_point(aes(color = c0.5.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# 

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
ggsave("3.figures/ECA_vs_TotalCut_A.png",width =7,height = 4)


stnRs_long%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       out = "4.output/ECA_analysis_A.doc")



stnRs_long_lag1<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c1.60:c1.5.p)%>%
  pivot_longer(cols = c1.60:c1.5.p)
stnRs_long_lag1$meas<-str_detect(stnRs_long_lag1$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag1$name<-str_remove(stnRs_long_lag1$name,"\\.p")
stnRs_long_lag1<-pivot_wider(stnRs_long_lag1,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long_lag1<-filter(stnRs_long_lag1,!regime=="Glacial")
stnRs_long_lag1$regime<-factor(stnRs_long_lag1$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                               labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long_lag1$name<-factor(stnRs_long_lag1$name,levels = c("c1.60","c1.20","c1.10","c1.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag1%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())

ggplot(stnRs_long_lag1,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r"
                     # limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut_A_lag1.png",width =7,height = 4)



stnRs_long_lag2<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c2.60:c2.5.p)%>%
  pivot_longer(cols = c2.60:c2.5.p)
stnRs_long_lag2$meas<-str_detect(stnRs_long_lag2$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag2$name<-str_remove(stnRs_long_lag2$name,"\\.p")
stnRs_long_lag2<-pivot_wider(stnRs_long_lag2,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long_lag2<-filter(stnRs_long_lag2,!regime=="Glacial")
stnRs_long_lag2$regime<-factor(stnRs_long_lag2$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                               labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long_lag2$name<-factor(stnRs_long_lag2$name,levels = c("c2.60","c2.20","c2.10","c2.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag2%>%
  group_by(regime,name)%>%
  summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())

ggplot(stnRs_long_lag2,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
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
ggsave("3.figures/ECA_vs_TotalCut_A_lag2.png",width =7,height = 4)





stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)%>%
  mutate(
    fracSignif_0 = NA,
    # fracSignif_0.1 = NA,
    # fracSignif_0.25 = NA,
    # fracSignif_0.5 = NA,
    fracSignif_1 = NA,
    fracSignif_2 = NA,
    fracSignif_3 = NA,
    fracNeg_0 = NA,
    # fracNeg_0.1 = NA,
    # fracNeg_0.25 = NA,
    # fracNeg_0.5 = NA,
    fracNeg_1 = NA,
    fracNeg_2 = NA,
    fracNeg_3 = NA,
    
    
  )

for(it_stn in 1:nrow(stn_MC)){
  tictoc::tic(stn_MC$ID[it_stn])
  ECA_x<-filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stn_MC$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stn_MC$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
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
  
  
  
  streamDataYrly<-streamData%>%
    
    dplyr::group_by(NovWaterYear)%>%
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
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stn_MC$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stn_MC$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stn_MC$ID[it_stn])
  }
  
  
  
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
  fitECA10<-sarima(streamDataYrly$ECA_10,1,1,0,details = FALSE)
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  
  medianQ7<-median(streamDataYrly$minSumFlow7)
  sd.log.Q7<-sd(log(streamDataYrly$minSumFlow7))
  
  fctrs <- c(0, 0.1, 0.25,0.5,1)
  fctrs <- c(0,1,2,3)
  for(it_fctr in 1:4){
    p_vals<-c()
    neg<-c()
    for(it_MC in 1:1000){
      set.seed(it_MC)
      Q7_i.log<-log(streamDataYrly$minSumFlow7[sample(1:nrow(streamDataYrly),nrow(streamDataYrly),replace = FALSE)])
      Q7_i.log_shift<-Q7_i.log - (streamDataYrly$ECA_10*fctrs[it_fctr])*sd.log.Q7
      logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(resid(fitECA10$fit), logQ7.60,method = "spearman")
      p_vals[it_MC]<-c0.60$p.value
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i))
      # plot(as.numeric(resid(fitECA5$fit)),log(Q7_i_shift))
      # plot(streamDataYrly$ECA_5,log(Q7_i_shift))
      
      neg[it_MC]<-c0.60$estimate<0
      # ccf(resid(fitECA5$fit), logQ7.60,na.action = na.omit)
      
      
    }
    stn_MC[it_stn,paste0("fracNeg_",fctrs[it_fctr])] <- sum(neg)/1000
    stn_MC[it_stn,paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05)/1000
  }
  
  tictoc::toc()
}


stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)%>%
  mutate(
    fracSignif_0 = NA,
    # fracSignif_0.1 = NA,
    # fracSignif_0.25 = NA,
    # fracSignif_0.5 = NA,
    fracSignif_1 = NA,
    # fracSignif_2 = NA,
    # fracSignif_3 = NA,
    fracSignif_5 = NA,
    fracSignif_10 = NA,
    fracSignif_100 = NA,
    fracNeg_0 = NA,
    # fracNeg_0.1 = NA,
    # fracNeg_0.25 = NA,
    # fracNeg_0.5 = NA,
    fracNeg_1 = NA,
    # fracNeg_2 = NA,
    # fracNeg_3 = NA,
    fracNeg_5 = NA,
    fracNeg_10 = NA,
    fracNeg_100 = NA,
    
    
  )

for(it_stn in 1:nrow(stn_MC)){
  tictoc::tic(stn_MC$ID[it_stn])
  ECA_x<-filter(ECA,StationNum%in%stn_MC$ID[it_stn])
  
  streamData<-streamDataAll[streamDataAll$ID==stn_MC$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stn_MC$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
  
  streamData$Date<-ymd(streamData$Date)
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  
  streamData<-streamData%>%
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
  
  
  
  streamDataYrly<-streamData%>%
    
    dplyr::group_by(NovWaterYear)%>%
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
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stn_MC$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  }
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stn_MC$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stn_MC$ID[it_stn])
  }
  
  
  
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
  fitECA60<-arima(streamDataYrly$ECA_60,c(1,1,0))
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
  
  # medianQ7<-median(streamDataYrly$minSumFlow7)
  sd.log.Q7<-sd(log(streamDataYrly$minSumFlow7))
  
  # fctrs <- c(0, 0.1, 0.25,0.5,1)
  fctrs <- c(0,1,5,10,100)
  for(it_fctr in 1:5){
    p_vals<-c()
    neg<-c()
    # tictoc::tic()
    for(it_MC in 1:100){
      set.seed(it_MC)
      Q7_i.log<-log(streamDataYrly$minSumFlow7[sample(1:nrow(streamDataYrly),nrow(streamDataYrly),replace = FALSE)])
      Q7_i.log_shift<-Q7_i.log - (streamDataYrly$ECA_60*fctrs[it_fctr]*sd.log.Q7)
      fitQ7<- arima(Q7_i.log_shift,c(1,1,0))
      logQ7.60 <-residuals(fitQ7)
      # logQ7.60<-stats::filter(Q7_i.log_shift,filter = c(1,-(1+fitECA10$fit$coef[1]),fitECA10$fit$coef[1]),sides = 1)
      # c0.60<-cor.test(logQ7.60,fitECA60$residuals,method = "spearman")
      c0.60<-cor.test(residuals(fitECA60), logQ7.60,method = "spearman")
      c0.60<-cor.test(streamDataYrly$ECA_60, Q7_i.log_shift,method = "spearman")
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
    (stn_MC[it_stn,paste0("fracNeg_",fctrs[it_fctr])] <- sum(neg)/100)
    (stn_MC[it_stn,paste0("fracSignif_",fctrs[it_fctr])] <- sum(p_vals<0.05)/100)
  }
  
  tictoc::toc()
}


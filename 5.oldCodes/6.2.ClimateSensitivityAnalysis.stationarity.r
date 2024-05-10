
closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(lubridate)
library(sf)
library(terra)
library(stringr)
library(scico)
library(ggplot2)
library(lm.beta)
library(tidyr)
library(Hmisc) 
library(grwat)
library(tictoc)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal_2.rds")
# streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly_2.RDS")

# Load Station metadata
# stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv",fileEncoding = "UTF-8-BOM")
stations<-readRDS("2.data/2.working/StationMetadata/stations_final_2.RDS")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")

# Load monthly weather data
WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")%>%
  filter(StationNum%in%unique(streamDataAll$ID))
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly_2.RDS")

# Load daily weather data
WeatherDataDaily<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataDaily.csv")%>%
  filter(ID%in%unique(stations$ID))

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



streamDataAll<-dplyr::left_join(streamDataAll,data_SWE_dly,by = c("ID","Date"))
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
maskPrivate_bc<-terra::mask(maskPrivate,bc)
summary(maskPrivate_bc)
# terra::global(maskPrivate_bc,mean,na.rm = TRUE)
plot(maskPrivate_bc)

# maskPrivateBC<-crop(maskPrivate)

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")%>%
  st_transform(st_crs(maskPrivate))

watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private[,"Canada_MFv2020"])
watersheds$PrivateForestry<-watersheds$watersheds_Private....Canada_MFv2020..
stations<-left_join(stations,st_drop_geometry(watersheds[,c("ID","PrivateForestry")]),by = c("ID" ))

TCA<-readRDS("2.data/2.working/ECA/TCA_2022.rds")
names(TCA)[3]<-"Total.Cut.Area"
stations<-left_join(stations,TCA%>%select(StationNum,Total.Cut.Area),by = c("ID" = "StationNum"))

sum(stations$PrivateForestry<0.1)
sum(stations$PrivateForestry<0.1&stations$Total.Cut.Area>0.1)
## Loop through stations and extract variables ################

watersheds<-st_transform(watersheds,st_crs("+proj=aea +lat_0=40 +lon_0=-96 +lat_1=50 +lat_2=70 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
watersheds$Area_km2<-(st_area(watersheds)/10^6)%>%as.numeric()

stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(ID,Area_km2)%>%
                      st_drop_geometry(),
                    by = c("ID" ))

stnCoeffs<-data.frame()
stnRs<-data.frame()

stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)

stations[,c("rBF_30day_Eckhardt0.97.diff",
            # "rTotal.cms.diff",
            "rmaxSWEdly.diff",
            "rTempSummer.diff","rPcpSummer.diff","rSumT7.diff",
            "rBF_30day_Eckhardt0.97.diff.p",
            # "rTotal.cms.diff.p",
            "rmaxSWEdly.diff.p",
            "rTempSummer.diff.p","rPcpSummer.diff.p","rSumT7.diff.p"
            # "rECAI.diff","rECAI.diff.p","rECAIII.diff","rECAIII.diff.p"
            )]<-NA

n_shuffles<-10000

for(it_stn in 1:length(stations$ID)){
  tic()
  it_stn = which(stations$ID=="08LG056")

  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])


  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  streamData$Discharge7<-streamData$Discharge7*(24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d

  streamData$Date<-ymd(streamData$Date)


  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(stations$month_bgn[it_stn]:stations$month_end[it_stn])]<-NA


  # streamData$BaseFlow_lynehollick<-gr_baseflow(streamData$Discharge, method = 'lynehollick', a = 0.925, passes = 3)
  # streamData$BaseFlow_boughton<-gr_baseflow(streamData$Discharge, method = 'boughton', k = 0.975, passes = 3)
  # streamData$BaseFlow_jakeman<-gr_baseflow(streamData$Discharge, method = 'jakeman', a = 0.925, passes = 3,aq = -0.5)
  # streamData$BaseFlow_maxwell<-gr_baseflow(streamData$Discharge, method = 'maxwell', k = 0.975, passes = 3)
  # streamData$BaseFlow_chapman<-gr_baseflow(streamData$Discharge, method = 'chapman', a = 0.925, passes = 3)
  streamData$BaseFlow_Eckhardt0.97<-NA
  # streamData$BaseFlow_Eckhardt0.995<-NA
  streamData$BaseFlow_Eckhardt0.97[!is.na(streamData$Discharge)]<-
    FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.97, 0.8)
  # streamData$BaseFlow_Eckhardt0.995[!is.na(streamData$Discharge)]<-
  #   FlowScreen::bf_eckhardt(streamData$Discharge[!is.na(streamData$Discharge)], 0.995, 0.8)



  streamData<-streamData%>%
    filter(year>=1950)%>%
    filter(Date>=Date[which(month==1&DayOfYear==1)[1]])

  streamData$T7<-zoo::rollmean(streamData$Mean.Temp..C.,k=7,align = "right",fill = NA)


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



  ## Add in SWE Data
  snowData<-data_SWE[data_SWE$ID==stations$ID[it_stn],]%>%
    group_by(WaterYear)%>%
    dplyr::summarize(SWE = mean(value),
                     maxMonth = maxMonth[1])

  fixed_BF_month<-snowData$maxMonth[1]%>%
    pmax(3)%>%
    pmin(5)


  streamData<-left_join(streamData,stations[,c("ID","month_bgn","month_end")])
  streamDataYrly<-streamData%>%

    dplyr::group_by(NovWaterYear)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),

                     numJunQnonNans = sum(!is.na(Discharge[month ==6])),
                     numJulQnonNans = sum(!is.na(Discharge[month ==7])),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),

                     minJunFlow7 = min(Discharge7[month==6],na.rm = TRUE),
                     minJunFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,6),
                     minJulFlow7 = min(Discharge7[month==7],na.rm = TRUE),
                     minJulFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,7),
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minAugFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,8),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minSepFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,9),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     minOctFlowDay = findMinFlowDay(DayOfYear,Discharge7,month,10),

                     minSumFlow7 = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     # minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     lengthSum = sum(month%in%c(month_bgn:month_end)),

                     maxSWEdly = max(SWE,na.rm = TRUE),
                     maxSWEdly_day = DayOfYear[which.max(SWE)],
                     SumT7=T7[DayOfYear=minSumFlowDay],
                     #
                     # BF_30day_lynehollick = mean(BaseFlow_lynehollick[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_boughton = mean(BaseFlow_boughton[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_jakeman = mean(BaseFlow_jakeman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_maxwell = mean(BaseFlow_maxwell[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_chapman = mean(BaseFlow_chapman[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),

                     BF_30day_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),
                     # BF_30day_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[DayOfYear%in%c((maxSWEdly_day-30):maxSWEdly_day)]),


                     # winterBF_lynehollick = mean(BaseFlow_lynehollick[month==fixed_BF_month]),
                     # winterBF_boughton = mean(BaseFlow_boughton[month==fixed_BF_month]),
                     # winterBF_jakeman = mean(BaseFlow_jakeman[month==fixed_BF_month]),
                     # winterBF_maxwell = mean(BaseFlow_maxwell[month==fixed_BF_month]),
                     # winterBF_chapman = mean(BaseFlow_chapman[month==fixed_BF_month]),
                     # winterBF_Eckhardt0.995 = mean(BaseFlow_Eckhardt0.995[month==fixed_BF_month]),
                     winterBF_Eckhardt0.97 = mean(BaseFlow_Eckhardt0.97[month==fixed_BF_month])


    )


  streamDataYrly$minJunFlow7[streamDataYrly$numJunQnonNans<26]<-NA
  streamDataYrly$minJulFlow7[streamDataYrly$numJulQnonNans<26]<-NA
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA

  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA

  streamDataYrly$minJulFlow7[is.infinite(streamDataYrly$minJulFlow7)]<-NA
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA

  streamDataYrly$minSumFlow7[(streamDataYrly$lengthSum-streamDataYrly$numQDaysSum)>5]<-NA

  # if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
  #   streamDataYrly$minSumFlow7<-pmin( streamDataYrly$minJulFlow7,streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
  # }




  streamDataYrly<-left_join(streamDataYrly,snowData,by = c("NovWaterYear" = "WaterYear"))


  # stations$minFlowMonth[it_stn]<-getMode(streamDataYrly$minSumFlowMonth)

  # if(stations$minFlowMonth[it_stn]==8){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minAugFlow7
  # }else if(stations$minFlowMonth[it_stn]==9){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minSepFlow7
  # }else if(stations$minFlowMonth[it_stn]==10){
  #   streamDataYrly$minMonFlow7<-streamDataYrly$minOctFlow7
  # }

  ## add in ECA data
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))


  data<-WeatherData[WeatherData$ID == stations$ID[it_stn],]

  data$NovWaterYear<-data$Year
  data$NovWaterYear[data$Month%in%c(11,12)] <- data$Year[data$Month%in%c(11,12)]+1



  dataYearly<-data%>%
    group_by(NovWaterYear)%>%
    dplyr::summarize(

      winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),

      summerPrecip = sum(Total.Precip..mm.[Month%in%c(5:stations$minSumFlowMonth[it_stn])],na.rm = TRUE),
      meanSummerTemp = mean(Mean.Temp..C.[Month%in%c(5:stations$minSumFlowMonth[it_stn])],na.rm = TRUE),

      Precip_MJJA = sum(Total.Precip..mm.[Month%in%c(5,6,7,8)],na.rm = TRUE),
      Temp_MJJA =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8)],na.rm = TRUE),

      Precip_JA = sum(Total.Precip..mm.[Month%in%c(7,8)],na.rm = TRUE),
      Temp_JA =  mean(Mean.Temp..C.[Month%in%c(7,8)],na.rm = TRUE),

      Precip_MJJAS = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),
      Temp_MJJAS =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9)],na.rm = TRUE),

      Precip_AS = sum(Total.Precip..mm.[Month%in%c(8,9)],na.rm = TRUE),
      Temp_AS =  mean(Mean.Temp..C.[Month%in%c(8,9)],na.rm = TRUE),

      Precip_MJJASO = sum(Total.Precip..mm.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),
      Temp_MJJASO =  mean(Mean.Temp..C.[Month%in%c(5,6,7,8,9,10)],na.rm = TRUE),

      Precip_SO = sum(Total.Precip..mm.[Month%in%c(9,10)],na.rm = TRUE),
      Temp_SO =  mean(Mean.Temp..C.[Month%in%c(9,10)],na.rm = TRUE),




    )

  dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("NovWaterYear"))

  Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stations$ID[it_stn],".csv"))


  dataYearly2<-left_join(dataYearly2,Div,by = c("NovWaterYear"= "year"))



  # stations$meanSummerTemp[it_stn]<-mean(dataYearly2$TempSummer,na.rm = TRUE)
  dataYearly2$minSumFlow7.log<-log(dataYearly2$minSumFlow7)


  dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                      "BF_30day_Eckhardt0.97","ECA_I_9","ECA_III_24","Total.cms",
                      "August.cms","September.cms","October.cms","NovWaterYear")]%>%
    na.omit()
  dat<-cbind(dat%>%
               scale(scale = TRUE,center = TRUE)%>%
               data.frame()%>%
               select(!NovWaterYear),
             dat[,"NovWaterYear"])




  if(stations$ID[it_stn] %in% c("08OA004","08OA005","08HD023")){
    dat<-dataYearly2[,c("minSumFlow7.log","summerPrecip","meanSummerTemp","SumT7","maxSWEdly",
                        "ECA_I_9","ECA_III_24","Total.cms",
                        "August.cms","September.cms","October.cms","NovWaterYear")]%>%
      na.omit()
    dat<-cbind(dat%>%
                 scale(scale = TRUE,center = TRUE)%>%
                 data.frame()%>%
                 select(!NovWaterYear),
               dat[,"NovWaterYear"])
    dat$BF_30day_Eckhardt0.97<-0}


  if(!(sum(dat$NovWaterYear<=1997)>=20&sum(dat$NovWaterYear>1997)>=20)){next}




  # tryWaterUse = FALSE
  # if(any(dataYearly2$Total.cms>dataYearly2$minSumFlow7*0.1,na.rm = TRUE)&
  #    any(!is.na(dat$Total.cms))){
  #   tryWaterUse = TRUE
  # }
  # 
  # tryECAI = FALSE
  # tryECAIII = FALSE
  # if(stations$PrivateForestry[it_stn]<=0.1&stations$Total.Cut.Area[it_stn]>0.1){
  # 
  #   tryECAI = TRUE
  #   if(any(!is.na(dat$ECA_III_24))){
  #     tryECAIII=TRUE
  # 
  #   }
  # }


  msk<-dat$NovWaterYear<=1997



  # x1<-cor(dat[msk,]$minSumFlow7.log,
  #         dat[msk,]$maxSWEdly,use = "complete")
  # x2<-cor(dat[!msk,]$minSumFlow7.log,
  #         dat[!msk,]$maxSWEdly,use = "complete")

  x1<-lm(minSumFlow7.log~maxSWEdly,data = dat[msk,])$coefficients[2]
  x2<-lm(minSumFlow7.log~maxSWEdly,data = dat[!msk,])$coefficients[2]

  stations$rmaxSWEdly.diff[it_stn]<-x2-x1


  # x1<-cor(dat[msk,]$minSumFlow7.log,
  #         dat[msk,]$meanSummerTemp,use = "complete")
  # x2<-cor(dat[!msk,]$minSumFlow7.log,
  #         dat[!msk,]$meanSummerTemp,use = "complete")
  x1<-lm(minSumFlow7.log~meanSummerTemp,data = dat[msk,])$coefficients[2]
  x2<-lm(minSumFlow7.log~meanSummerTemp,data = dat[!msk,])$coefficients[2]

  stations$rTempSummer.diff[it_stn]<-x2-x1


  # x1<-cor(dat[msk,]$minSumFlow7.log,
  #         dat[msk,]$summerPrecip,use = "complete")
  # x2<-cor(dat[!msk,]$minSumFlow7.log,
  #         dat[!msk,]$summerPrecip,use = "complete")
  x1<-lm(minSumFlow7.log~summerPrecip,data = dat[msk,])$coefficients[2]
  x2<-lm(minSumFlow7.log~summerPrecip,data = dat[!msk,])$coefficients[2]

  stations$rPcpSummer.diff[it_stn]<-x2-x1

  # x1<-cor(dat[msk,]$minSumFlow7.log,
  #         dat[msk,]$SumT7,use = "complete")
  # x2<-cor(dat[!msk,]$minSumFlow7.log,
  #         dat[!msk,]$SumT7,use = "complete")
  x1<-lm(minSumFlow7.log~SumT7,data = dat[msk,])$coefficients[2]
  x2<-lm(minSumFlow7.log~SumT7,data = dat[!msk,])$coefficients[2]

  stations$rSumT7.diff[it_stn]<-x2-x1



  nyr<-length(msk)
  set.seed(1)

  # diffR<-c()
  diffR.SWE<-c()
  diffR.TempSummer<-c()
  diffR.PcpSummer<-c()
  diffR.SumT7<-c()
  diffR.BF<-c()
  diffR.WaterUse<-c()
  diffR.ECAI<-c()
  diffR.ECAIII<-c()



  for(it_r in 1:n_shuffles){
    set.seed(it_r)
    mskRand<-sample(msk,size = nyr,replace = FALSE)
    dat1<-dat[mskRand,]
    dat2<-dat[!mskRand,]

    # x1<-cor(dat1$minSumFlow7.log,
    #         dat1$maxSWEdly,use = "complete")
    # x2<-cor(dat2$minSumFlow7.log,
    #         dat2$maxSWEdly,use = "complete")
    x1<-lm(minSumFlow7.log~maxSWEdly,data = dat1)$coefficients[2]
    x2<-lm(minSumFlow7.log~maxSWEdly,data = dat2)$coefficients[2]

    diffR.SWE[it_r]<-x1-x2

    # x1<-cor(dat1$minSumFlow7.log,
    #         dat1$meanSummerTemp,use = "complete")
    # x2<-cor(dat2$minSumFlow7.log,
    #         dat2$meanSummerTemp,use = "complete")
    x1<-lm(minSumFlow7.log~meanSummerTemp,data = dat1)$coefficients[2]
    x2<-lm(minSumFlow7.log~meanSummerTemp,data = dat2)$coefficients[2]

    diffR.TempSummer[it_r]<-x1-x2

    # x1<-cor(dat1$minSumFlow7.log,
    #         dat1$summerPrecip,use = "complete")
    # x2<-cor(dat2$minSumFlow7.log,
    #         dat2$summerPrecip,use = "complete")
    x1<-lm(minSumFlow7.log~summerPrecip,data = dat1)$coefficients[2]
    x2<-lm(minSumFlow7.log~summerPrecip,data = dat2)$coefficients[2]

    diffR.PcpSummer[it_r]<-x1-x2


    # x1<-cor(dat1$minSumFlow7.log,
    #         dat1$SumT7,use = "complete")
    # x2<-cor(dat2$minSumFlow7.log,
    #         dat2$SumT7,use = "complete")

    x1<-lm(minSumFlow7.log~SumT7,data = dat1)$coefficients[2]
    x2<-lm(minSumFlow7.log~SumT7,data = dat2)$coefficients[2]
    diffR.SumT7[it_r]<-x1-x2


  }

  stations$rmaxSWEdly.diff.p[it_stn]<-sum(abs(diffR.SWE)>abs(stations$rmaxSWEdly.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.SWE))

  stations$rTempSummer.diff.p[it_stn]<-sum(abs(diffR.TempSummer)>abs(stations$rTempSummer.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.TempSummer))
  stations$rPcpSummer.diff.p[it_stn]<-sum(abs(diffR.PcpSummer)>abs(stations$rPcpSummer.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.PcpSummer))
  stations$rSumT7.diff.p[it_stn]<-sum(abs(diffR.SumT7)>abs(stations$rSumT7.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.SumT7))




  if(!stations$ID[it_stn]%in%c("08ED004","08OA004")){
    # x1<-cor(dat[msk,]$minSumFlow7.log,
    #         dat[msk,]$BF_30day_Eckhardt0.97,use = "complete")
    # x2<-cor(dat[!msk,]$minSumFlow7.log,
    #         dat[!msk,]$BF_30day_Eckhardt0.97,use = "complete")


    x1<-lm(minSumFlow7.log~BF_30day_Eckhardt0.97,data = dat[msk,])$coefficients[2]

    x2<-lm(minSumFlow7.log~BF_30day_Eckhardt0.97,data = dat[!msk,])$coefficients[2]


    stations$rBF_30day_Eckhardt0.97.diff[it_stn]<-x2-x1

    for(it_r in 1:n_shuffles){
      set.seed(it_r)
      mskRand<-sample(msk,size = nyr,replace = FALSE)
      dat1<-dat[mskRand,]
      dat2<-dat[!mskRand,]


      # x1<-cor(dat1$minSumFlow7.log,
      #         dat1$BF_30day_Eckhardt0.97,use = "complete")
      # x2<-cor(dat2$minSumFlow7.log,
      #         dat2$BF_30day_Eckhardt0.97,use = "complete")

      x1<-lm(minSumFlow7.log~BF_30day_Eckhardt0.97,data = dat1)$coefficients[2]

      x2<-lm(minSumFlow7.log~BF_30day_Eckhardt0.97,data = dat2)$coefficients[2]

      diffR.BF[it_r]<-x1-x2}

    stations$rBF_30day_Eckhardt0.97.diff.p[it_stn]<-sum(abs(diffR.BF)>abs(stations$rBF_30day_Eckhardt0.97.diff[it_stn]))/sum(!is.na(diffR.BF))


  }

  # if(tryWaterUse){
  #
  #   x1<-cor(dat[msk,]$minSumFlow7.log,
  #           dat[msk,]$Total.cms,use = "complete")
  #   x2<-cor(dat[!msk,]$minSumFlow7.log,
  #           dat[!msk,]$Total.cms,use = "complete")
  #   stations$rTotal.cms.diff[it_stn]<-x2-x1
  #
  #   for(it_r in 1:n_shuffles){
  #     set.seed(it_r)
  #     mskRand<-sample(msk,size = nyr,replace = FALSE)
  #
  #     dat1<-dat[mskRand,]
  #     dat2<-dat[!mskRand,]
  #     x1<-cor(dat1$minSumFlow7.log,
  #             dat1$Total.cms,use = "complete")
  #     x2<-cor(dat2$minSumFlow7.log,
  #             dat2$Total.cms,use = "complete")
  #     diffR.WaterUse[it_r]<-x1-x2
  #   }
  #
  #   stations$rTotal.cms.diff.p[it_stn]<-sum(abs(diffR.WaterUse)>abs(stations$rTotal.cms.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.WaterUse))
  #
  # }
  #
  # if(tryECAI){
  #
  #   x1<-cor(dat[msk,]$minSumFlow7.log,
  #           dat[msk,]$ECA_I_9,use = "complete")
  #   x2<-cor(dat[!msk,]$minSumFlow7.log,
  #           dat[!msk,]$ECA_I_9,use = "complete")
  #   stations$rECAI.diff[it_stn]<-x2-x1
  #
  #   for(it_r in 1:n_shuffles){
  #     set.seed(it_r)
  #     mskRand<-sample(msk,size = nyr,replace = FALSE)
  #
  #     dat1<-dat[mskRand,]
  #     dat2<-dat[!mskRand,]
  #     x1<-cor(dat1$minSumFlow7.log,
  #             dat1$ECA_I_9,use = "complete")
  #     x2<-cor(dat2$minSumFlow7.log,
  #             dat2$ECA_I_9,use = "complete")
  #     diffR.ECAI[it_r]<-x1-x2
  #   }
  #
  #   stations$rECAI.diff.p[it_stn]<-sum(abs(diffR.ECAI)>abs(stations$rECAI.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.ECAI))
  #
  # }
  #
  # if(tryECAIII){
  #
  #   x1<-cor(dat[msk,]$minSumFlow7.log,
  #           dat[msk,]$ECA_III_24,use = "complete")
  #   x2<-cor(dat[!msk,]$minSumFlow7.log,
  #           dat[!msk,]$ECA_III_24,use = "complete")
  #   stations$rECAIII.diff[it_stn]<-x2-x1
  #
  #   for(it_r in 1:n_shuffles){
  #     set.seed(it_r)
  #     mskRand<-sample(msk,size = nyr,replace = FALSE)
  #
  #     dat1<-dat[mskRand,]
  #     dat2<-dat[!mskRand,]
  #     x1<-cor(dat1$minSumFlow7.log,
  #             dat1$ECA_III_24,use = "complete")
  #     x2<-cor(dat2$minSumFlow7.log,
  #             dat2$ECA_III_24,use = "complete")
  #     diffR.ECAIII[it_r]<-x1-x2
  #   }
  #
  #
  #
  #
  #   stations$rECAIII.diff.p[it_stn]<-sum(abs(diffR.ECAIII)>abs(stations$rECAIII.diff[it_stn]),na.rm = TRUE)/sum(!is.na(diffR.ECAIII))
  #
  #
# }

toc()
}



stations%>%dplyr::summarize(across(ends_with(".p"),~sum(.x<0.05,na.rm = TRUE)/sum(!is.na(.x))))

stations%>%
  group_by(regime)%>%
  dplyr::summarize(N=n(),
    across(ends_with(".p"),~sum(.x<0.05,na.rm = TRUE)/sum(!is.na(.x))))



Coeffs<-stations%>%
  select(ID,regime,rBF_30day_Eckhardt0.97.diff:rSumT7.diff.p)%>%
  pivot_longer(cols = rBF_30day_Eckhardt0.97.diff:rSumT7.diff.p)%>%
  mutate(meas = str_detect(name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("r","p.val")),
         variable = str_remove(name,"\\.p"))%>%
  pivot_wider(id_cols = c(ID,regime,variable),values_from = value,
              names_from = meas)%>%
  filter(!is.na(p.val)&!is.na(r))


Coeffs$regime<-factor(Coeffs$regime,
                      levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                      labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))


Coeffs$variable<-factor(Coeffs$variable,levels =c("rmaxSWEdly.diff","rBF_30day_Eckhardt0.97.diff",
                                                  "rPcpSummer.diff","rTempSummer.diff","rSumT7.diff"#,
                                                  # "rTotal.cms.diff","rECAI.diff","rECAIII.diff"
                                                  ) ,
                        labels = c("SWE.max","BF.winter","P.summer","T.summer","T.7"#,"Abstraction","ECA.I", "ECA.III"
                                   ))


(testTable<-
    Coeffs%>%
    group_by(variable,regime)%>%
    dplyr::summarize(num = sum(!is.na(r)),
                     nIncrease = paste0(round(sum(r>0,na.rm = TRUE)/num*100),"%"),
                     
                     p = binom.test(sum(r>0,na.rm = TRUE),num,p=0.5,alternative = c("two.sided"))$p.value,
                     
                     
                     sigInc.frac = paste0(round(sum(r>0&p.val<0.05,na.rm = TRUE)/n()*100),"%"),
                     sigInc.pval = binom.test(sum(r>0&p.val<0.05,na.rm = TRUE),n(),p=0.05,alternative = "greater")$p.value,
                     sigDec.frac = paste0( round(sum(r<0&p.val<0.05,na.rm = TRUE)/n()*100),"%"),
                     sigDec.pval = binom.test(sum(r<0&p.val<0.05,na.rm = TRUE),n(),p=0.05,alternative = "greater")$p.value
                     
                     
    ))%>%
  print(n=100)



k<-rank(testTable$p, ties.method = "first")

m_prime<-20+1-k

testTable$T1.sig<-""
sigFunc<-function(x1,m_prime){
  if(x1<(0.001/m_prime)){return("***")}
  if(x1<(0.01/m_prime)){return("**")}
  if(x1<(0.05/m_prime)){return("*")}
  return("")
}

for(it in 1:20){
  it_row<-which(k==it)
  testTable$T1.sig[it_row]<-sigFunc(testTable$p[it_row],m_prime[it_row])
  if(testTable$T1.sig[it_row]==""){break}
}

k<-rank(testTable$sigInc.pval, ties.method = "first")
m_prime<-20+1-k

testTable$T2.Inc.sig<-""
for(it in 1:20){
  it_row<-which(k==it)
  testTable$T2.Inc.sig[it_row]<-sigFunc(testTable$sigInc.pval[it_row],m_prime[it_row])
  if(testTable$T2.Inc.sig[it_row]==""){break}
}



k<-rank(testTable$sigDec.pval, ties.method = "first")
m_prime<-20+1-k

testTable$T2.Dec.sig<-""
for(it in 1:20){
  it_row<-which(k==it)
  testTable$T2.Dec.sig[it_row]<-sigFunc(testTable$sigDec.pval[it_row],m_prime[it_row])
  if(testTable$T2.Dec.sig[it_row]==""){break}
}

testTable.df<-
  testTable%>%data.frame()%>%
  mutate(variable=as.character(variable),
         regime = as.character(regime),
         # p=round(p,3),
         # sigInc.pval = round(sigInc.pval,3),
         # sigDec.pval = round(sigDec.pval,3)
         
  )

testTable.df<-testTable.df%>%select(variable,regime,num,nIncrease,p,T1.sig,sigInc.frac,sigInc.pval,T2.Inc.sig,sigDec.frac,sigDec.pval,T2.Dec.sig)

testTable.df

stargazer::stargazer(testTable.df,
                     type = "html",
                     out = "4.output/stationarity_test_univariate.doc",
                     summary = FALSE,
                     digits = 2,
                     digits.extra = 2,
                     rownames = FALSE
)

write.csv(testTable.df,"4.output/stationarity_test_univariate.csv")


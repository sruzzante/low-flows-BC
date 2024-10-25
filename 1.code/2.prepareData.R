# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-09-09

# This script performs a number of tasks:
# prepare the streamflow data and save it to an RDS file
# filter to stations with enough data (20 years of summer data, at least one winter)
# calculate glacial percentage
# remove intermittent streams
# remove catchments with >20% urban land
# generate example hydrographs for two stations


closeAllConnections()
rm(list=ls())
graphics.off()

library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
library(sf)
library(tmap)
library(reshape2)
library(ggplot2)
library(cowplot)
library("bcmaps")
library(terra)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory



bc <- (bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province
# First open all files in notepad++ and remove all zero-width no-break spaces

fls<-list.files("2.data/1.original/Discharge_natural/",full.names = TRUE,pattern = "daily.*csv")
streamData<-data.frame()
for(it in 1:length(fls)){
  dat<-read.csv(fls[it],skip = 1,fileEncoding = "UTF-8-BOM")
  streamData<-rbind(streamData,dat)
}

# streamData$ID <-  str_remove(streamData$ID,"\ufeff")

streamData<-streamData[streamData$PARAM==1,]
streamData$Date<-streamData$Date%>%ymd()
streamData$year<-year(streamData$Date)
streamData$month<-month(streamData$Date)
streamData$day<-day(streamData$Date)
streamData<-streamData[!duplicated(streamData),]

names(streamData)[4]<-"Discharge"

streamData$Discharge[streamData$Discharge==0]<-0.0005
unique(streamData$ID[streamData$Discharge==0.0005])


streamData$Discharge[streamData$ID=="08JB003"&
                       streamData$month==9&
                       streamData$year==1961]<-NA

streamData<-filter(streamData,substr(ID,1,2)%in%c("08","09"))
stns<-unique(streamData$ID)

# streamData$dischargeSmooth<-stat_smooth(data=streamData$Discarge,)
# streamData$dischargeSmooth<-zoo::rollmean(streamData$Discharge,k = 7,align = "right",fill = NA)

# streamData$qi_diff<-c(NA,diff(streamData$Discharge))

# x<-duplicated(streamData[,c("ID","Date")])

streamData_cast<-dcast(streamData,Date~ID,value.var = "Discharge")
streamData_cast2<-streamData_cast

# streamData_cast2<-apply(streamData_cast,2,FUN = function(x){})

myrollmean<-function(x){zoo::rollmean(x,k = 7,align = "right",fill = NA)}

streamData_cast2[,2:dim(streamData_cast2)[2]]<-apply(streamData_cast2[,2:dim(streamData_cast2)[2]],2,myrollmean)

streamData_7<-melt(streamData_cast2,id.vars = "Date",value.name = "Discharge7",variable.name = "ID")
streamData_7$year<-year(streamData_7$Date)
streamData_7$month<-month(streamData_7$Date)
streamData_7$day<-day(streamData_7$Date)
# streamData_7<-streamData_7[!is.na(streamData_7$Discharge7),]
# names()
streamData<-left_join(streamData,streamData_7[,c("ID","year","month","day","Discharge7")],by = c("ID","year","month","day"))


rm(streamData_cast,streamData_7,streamData_cast2)

streamData$DayOfYear<-yday(ymd(streamData$Date))
streamData$Discharge7Summer<-streamData$Discharge7
streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA

streamDataYrly<-streamData%>%
  group_by(ID,year)%>%
  dplyr::summarize(
    numQdays = sum(!is.na(Discharge)),
    numAugQnonNans = sum(!is.na(Discharge[month ==8])),
    numSepQnonNans = sum(!is.na(Discharge[month ==9])),
    numOctQnonNans = sum(!is.na(Discharge[month ==10])),
    minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
    minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
    minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
    minSumFlow7 = min(Discharge7Summer,na.rm = TRUE),
    minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
    minSumFlowMonth = month[which.min(Discharge7Summer)],
    numQDaysSum = sum(!is.na(Discharge7Summer)),
    minSumFlow1 = min(Discharge[month%in%c(8,9,10)],na.rm = TRUE),
    minFlowMonth = month[which.min(Discharge7)]
  )


streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA


streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA

streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<77&!streamDataYrly$ID%in%c("08OA004","08OA005","08HD023")]<-NA
streamDataYrly$minSumFlow1[streamDataYrly$numQDaysSum<77&!streamDataYrly$ID%in%c("08OA004","08OA005","08HD023")]<-NA

streamDataYrly$minSumFlow7[streamDataYrly$numQDaysSum<51&streamDataYrly$ID%in%c("08OA004","08OA005","08HD023")]<-NA
streamDataYrly$minSumFlow1[streamDataYrly$numQDaysSum<51&streamDataYrly$ID%in%c("08OA004","08OA005","08HD023")]<-NA


# streamDataYrly<-streamDataYrly[streamDataYrly$numQdays>350,]

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

stns<-streamDataYrly%>%
  group_by(ID)%>%
  dplyr::summarise(strtYr = min(year[!is.na(minSumFlow7)]),
                   endYr = max(year[!is.na(minSumFlow7)]),
                   nYr = sum(!is.na(minSumFlow7)),
                   nFullYear = sum(numQdays>350),
                   minFlowSummerN = sum(minFlowMonth%in%c(7,8,9,10))/n(),
                   minFlowMonth = getmode(minFlowMonth)
                   
                   
  )



stnsAll<-stns[stns$nYr>=20&
                stns$endYr>=2000&
                stns$nFullYear>=1,]



streamData<-streamData[streamData$ID%in%stnsAll$ID,c("ID","Date","Discharge","year","month","day","Discharge7")]
write.csv(streamData,"2.data/2.working/Discharge/streamDataNaturalRecent20yr.csv",row.names = FALSE) # Save as csv
saveRDS(streamData,file = "2.data/2.working/Discharge/streamDataNaturalRecent20yr.rds") # save as rds (smaller, manageable on GitHub)

# Hydrograph Selection ####
# Select only stations with a local minimum between August 1 and October 31st
# Also plot hydrographs

rm(list = ls())
# streamDataAll<-read.csv("2.data/2.working/Discharge/streamDataNaturalRecent20yr.csv")
streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataNaturalRecent20yr.rds")

stations<-read.csv("2.data/1.original/Discharge_natural/stationList.csv",fileEncoding = "UTF-8-BOM")
stations<-stations[stations$ID%in%unique(streamDataAll$ID),]
# start with 282 stations

watersheds<-st_read("2.data/2.working/CatchmentPolygons/Watersheds_natural_BC.gpkg")%>%
  filter(StationNum%in%stations$ID)%>%
  st_simplify(dTolerance = 50)%>%
  st_make_valid()%>%
  st_buffer(dist = 0)


bc <- (bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province




COT_func<-function(x){
  AD=sum(x)
  cumQ<-cumsum(x)
  ind = which(cumQ>AD/2)[1]
  return(ind)
}

streamDataAll$wateryear<-streamDataAll$year
streamDataAll$wateryear[streamDataAll$month%in%c(10,11,12)]<-streamDataAll$year[streamDataAll$month%in%c(10,11,12)]+1


# Find glacial percentage
glaciers<-st_read("2.data/1.original/Glaciers/FWA_GLACIERS_POLY/FWGLCRSPL_polygon.shp")%>%
  # st_simplify(dTolerance = 50)%>%
  st_make_valid()

watersheds_combine<-st_union(watersheds)

glaciers<-glaciers[st_intersects(glaciers,watersheds_combine,sparse = FALSE),]
# glaciers2<-glaciers[st_area(glaciers)>100,]
# glaciers2<-glaciers%>%st_simplify(dTolerance = 50)
# glaciers2<-glaciers2[st_area(glaciers2)>units::set_units(0, m^2),]

x<-st_union(glaciers)
watersheds_glaciated<-st_intersection(watersheds,x)

watersheds_glaciated$Gl_Area<-st_area(watersheds_glaciated)

watersheds2<-left_join(watersheds,st_drop_geometry(watersheds_glaciated))
watersheds2$Gl_Area[is.na(watersheds2$Gl_Area)]<-0
watersheds2$perc_Gl<-as.numeric(watersheds2$Gl_Area/st_area(watersheds2))

stations<-left_join(stations, watersheds2[,c("StationNum","perc_Gl")],by = c("ID" = "StationNum"))

# 
# for(it_stn in 1:length(stations$ID)){
#   streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
#   
#   streamData$DayOfYear<-yday(ymd(streamData$Date))
#   streamData$DayOfYear[streamData$DayOfYear==366]<-365
#   
#   streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
#   streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
#   
#   
#   dataSmooth2<-streamData%>%group_by(DateFake,DayOfYear)%>%
#     dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE))
#  
#   dataSmooth2$month<-month(dataSmooth2$DateFake)
#   dataSmooth2Summer<-dataSmooth2[dataSmooth2$DateFake>=as.Date("1999-08-01")&
#                                    dataSmooth2$DateFake<=as.Date("1999-10-31"),]
#   
#   minDischargeDate<-dataSmooth2$DateFake[which.min(dataSmooth2$Discharge)]
#   # stations$minFlowDate[it_stn]<-minDischargeDate
#   # minSumDischargeDate<-dataSmooth2Summer$DateFake[which.min(dataSmooth2Summer$Discharge)]
#   # stations$minSumFlowDate[it_stn]<-minSumDischargeDate
#   
#   
#   # Classify as snow-affected if there is a freshet
#   
#   stations$minWinterQ[it_stn]<-min(dataSmooth2$Discharge[dataSmooth2$DayOfYear%in%46:120])
#   ind_min<-which(dataSmooth2$Discharge==stations$minWinterQ[it_stn]&dataSmooth2$DayOfYear%in%46:120 )
#   
#   stations$maxFreshetQ[it_stn]<-max(dataSmooth2$Discharge[dataSmooth2$month%in%c(4,5,6,7)&
#                                                             dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
#   
#   stations$freshetFactor[it_stn]<-stations$maxFreshetQ[it_stn]/stations$minWinterQ[it_stn]
#   
#   stations$minSummerQ[it_stn]<-min(dataSmooth2$Discharge[dataSmooth2$month%in%c(8,9,10)])
#   ind_min<-which(dataSmooth2$Discharge==stations$minSummerQ[it_stn]&dataSmooth2$month%in%c(8,9,10) )
#   
#   stations$maxLateSumQ[it_stn]<-max(dataSmooth2$Discharge[dataSmooth2$month%in%c(10,11,12)&
#                                                             dataSmooth2$DateFake>dataSmooth2$DateFake[ind_min]])
#   
#   stations$summerRainFactor[it_stn]<-stations$maxLateSumQ[it_stn]/stations$minSummerQ[it_stn]
#   
#   # 
#   # snow = FALSE
#   # rain = FALSE
#   # 
#   # if(stations$freshetFactor[it_stn]>1.05){
#   #   snow = TRUE
#   # }
#   # if(stations$summerRainFactor[it_stn]>1.05){
#   #   rain = TRUE
#   # }
#   # 
#   # if(rain&snow){
#   #   stations$regime[it_stn]<-"Hybrid"
#   # }else if(rain){
#   #   stations$regime[it_stn]<-"Rainfall"
#   # }else if(snow){
#   #   stations$regime[it_stn]<-"Snowfall"
#   # }else{
#   #   stations$regime[it_stn]<-NA
#   # }
#   # 
#   # 
#   # 
#   # 
#   # if(stations$perc_Gl[it_stn] >0.05){
#   #   stations$regime[it_stn]<-"Glacial"
#   # }
#   # 
#   # # Reclassify two Tarundl Creek and Honna River
#   # if(stations$ID[it_stn]%in%c("08OA004","08OA005")){
#   #   stations$regime[it_stn]<-"Rainfall"
#   # }
#   # 
#   # 
#   # 
#   # p1<-ggplot(streamData,aes(DateFake,Discharge))+
#   #   geom_line(col = "gray50",alpha = 0.1,aes(group = year))+
#   #   
#   #   geom_line(data = dataSmooth2,aes(col = "Average Q30"),linewidth = 1)+
#   #   annotate("text",label = paste("Summer-Autumn Minimum Flow Date: ",substr(minSumDischargeDate,6,10),
#   #                                 "\nregime: ",stations$regime[it_stn]),
#   #            x= as.Date("1999-01-01"),y =max(streamData$Discharge,na.rm = TRUE),hjust = 0,
#   #            size = 5)+
#   #  
#   #   geom_vline(xintercept = as.Date(minSumDischargeDate))+
#   #   
#   #   scale_x_date(date_labels = "%b",name = "Date")+
#   #   scale_y_log10(name = "Discharge (cms)")+
#   #   scale_color_manual(name = NULL,
#   #                      values = c("blue","red"))+
#   #   theme_bw()
#   # p1
#   # 
#   # 
#   # watershed<-watersheds[watersheds$StationNum==stations$ID[it_stn],]
#   # p_map<-
#   #   ggplot()+geom_sf(data = bc)+
#   #   geom_sf(data = watershed,col = "red",fill = "pink")+
#   #   theme_void()
#   # 
#   # plot.with.inset <-
#   #   ggdraw() +
#   #   draw_plot(p1) +
#   #   draw_plot(p_map, x = .8, y = 0.8, width = .2, height = .2)
#   # ggsave(paste0("3.figures/Hydrographs/Yearly-",stations$Station.Name[it_stn],"-",stations$ID[it_stn],"v2.png"),plot.with.inset,width = 10,height = 6)
#   # 
#   
#   streamDataYrly<-streamData%>%
#     group_by(wateryear)%>%
#     dplyr::summarize(COT = COT_func(Discharge),
#                      numQDays = sum(!is.na(Discharge)))
#   
#   streamDataYrly<-streamDataYrly[streamDataYrly$numQDays>=365,]
#   
#   stations$COT[it_stn]<-median(streamDataYrly$COT)
# }
# 
# 
# 
# 
# 
# summary(factor(stations$regime))
# 
# stations$regime_Wenger<-NA
# 
# stations$regime_Wenger[stations$COT<150]<-"Rainfall"
# stations$regime_Wenger[stations$COT>200]<-"Snowfall"
# stations$regime_Wenger[stations$COT>=150&stations$COT<=200]<-"Hybrid"
# 
# summary(factor(stations$regime_Wenger))




for(it_stn in 1:length(stations$ID)){
  # it_stn = which(stations$ID=="08OA005")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08HA003",]
  # streamData$Date<-streamData$Date%>%ymd()
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  
  streamDataYrly<-streamData%>%
    group_by(year)%>%
    dplyr::summarize(numQNans = sum(is.na(Discharge)),
                     numQnotNAN = sum(!is.na(Discharge)),
                     numAugQnonNans = sum(!is.na(Discharge[month ==8])),
                     numSepQnonNans = sum(!is.na(Discharge[month ==9])),
                     numOctQnonNans = sum(!is.na(Discharge[month ==10])),
                     
                     minAugFlow7 = min(Discharge7[month==8],na.rm = TRUE),
                     minSepFlow7 = min(Discharge7[month==9],na.rm = TRUE),
                     minOctFlow7 = min(Discharge7[month==10],na.rm = TRUE),
                     minSumFlow = min(Discharge7Summer,na.rm = TRUE),
                     minSumFlowDay = DayOfYear[which.min(Discharge7Summer)],
                     minSumFlowMonth = month[which.min(Discharge7Summer)],
                     numQDaysSum = sum(!is.na(Discharge7Summer)),
                     MAD = mean(Discharge,na.rm = TRUE),
                     minSumFlow1 = min(Discharge[month%in%c(8,9,10)],na.rm = TRUE)
                     
    )
  
  
  streamDataYrly$minAugFlow7[streamDataYrly$numAugQnonNans<26]<-NA
  streamDataYrly$minSepFlow7[streamDataYrly$numSepQnonNans<25]<-NA
  streamDataYrly$minOctFlow7[streamDataYrly$numOctQnonNans<26]<-NA
  
  # streamDataYrly$minAugFlow[streamDataYrly$numAugQNans>0]<-NA
  
  streamDataYrly$minAugFlow7[is.infinite(streamDataYrly$minAugFlow7)]<-NA
  streamDataYrly$minSepFlow7[is.infinite(streamDataYrly$minSepFlow7)]<-NA
  streamDataYrly$minOctFlow7[is.infinite(streamDataYrly$minOctFlow7)]<-NA
  
  tempminSumFlow1<-streamDataYrly$minSumFlow1
  streamDataYrly$minSumFlow[streamDataYrly$numQDaysSum<77]<-NA
  streamDataYrly$minSumFlow1[streamDataYrly$numQDaysSum<77]<-NA
  
  if(stations$ID[it_stn]%in%c("08OA004","08OA005","08HD023")){
    streamDataYrly$minSumFlow<-pmin( streamDataYrly$minAugFlow7,streamDataYrly$minSepFlow7)
    tempminSumFlow1[is.na(streamDataYrly$minAugFlow7)|is.na(streamDataYrly$minSepFlow7)]<-NA
    streamDataYrly$minSumFlow1<-tempminSumFlow1
  }
  
  
  
  stations$nZero[it_stn]<-sum(streamDataYrly$minSumFlow1==0.0005,na.rm = TRUE)
  stations$nYears[it_stn]<-sum(!is.na(streamDataYrly$minSumFlow1))
  
  # stations$nAug[it_stn]<-sum(!is.na(streamDataYrly$minAugFlow7))
  # stations$nSep[it_stn]<-sum(!is.na(streamDataYrly$minSepFlow7))
  # stations$nOCt[it_stn]<-sum(!is.na(streamDataYrly$minOctFlow7))
  
  # stations$covMinSumFlow7[it_stn]<-sd(streamDataYrly$minSumFlow,na.rm = TRUE)/
  #   mean(streamDataYrly$minSumFlow,na.rm = TRUE)
}



# stations$intermittent<-(stations$nZero/stations$nYears)>0.05
stations$intermittent<-(stations$nZero)>1
sum(stations$intermittent)

stations<-stations%>%
  filter(!intermittent)

# Remove stations with >20% urban land use
stations<-filter(stations,!ID%in%c("08MH155","08GA061"))

stations_x<-stations[,-which(names(stations)=="geom")]
# Save final versions of raw data
write.csv(stations_x,"2.data/2.working/StationMetadata/stations_final.csv",row.names = FALSE)

watersheds<-watersheds[watersheds$StationNum%in%stations$ID ,]
watersheds<-watersheds%>%
  st_simplify(dTolerance = 50)
st_write(watersheds,"2.data/2.working/CatchmentPolygons/watersheds_final.gpkg",delete_dsn = TRUE)

streamDataFinal<-streamDataAll[streamDataAll$ID%in%stations$ID,]
write.csv(streamDataFinal,"2.data/2.working/Discharge/streamDataFinal.csv",row.names = FALSE)
saveRDS(streamDataFinal,"2.data/2.working/Discharge/streamDataFinal.rds")

## Glaciers 1985-2021
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")


glaciersYrly<-st_read("../DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/lulc1_glaciers_Bevington/bca_glaciers_1984-2021.gpkg")
  # st_simplify(dTolerance = 50)%>%
  st_make_valid()
  
glaciersYr<-glaciersYrly%>%filter(year==2009)

glaciersYr<-st_union(glaciersYr)
glaciersYr<-st_make_valid(glaciersYr)
st_write(glaciersYr,"2.data/2.working/GlacierRasters/glacier2009.gpkg")

watersheds_combine<-st_union(watersheds)%>%
  st_transform(st_crs(glaciersYrly))



glaciersYrly_fltr<-st_filter(glaciersYrly,watersheds_combine, .predicate = st_intersects)

terra::rasterize()
r<-terra::rast(bc, resolution = 20)

for(it_yr in 1984:2021){
  glaciersYr<-glaciersYrly%>%filter(year==it_yr)
  terra::rasterize(glaciersYr[1:10,],r,values = 1,touches = TRUE,
                                 filename = paste0("2.data/2.working/GlacierRasters/","glaciers",it_yr,".tif"),
                   overwrite = TRUE)
}

plot(r)
plot(glaciersYr[1:10,],add = TRUE)
plot(glaciersYr_r)
#Moyie River at Eastport

streamDataAll<-read.csv("2.data/2.working/streamFlowData/streamDataFinal.csv")

streamData<-streamDataAll[streamDataAll$ID=="08NH006",]

streamData$DayOfYear<-yday(ymd(streamData$Date))
streamData$DayOfYear[streamData$DayOfYear==366]<-365
streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")

streamData1998<-streamData
streamData1998$DateFake<-as.Date(streamData1998$DayOfYear-1,origin = "1998-01-01")
streamData1998$DayOfYear<- streamData1998$DayOfYear-365
streamData2000<-streamData
streamData2000$DateFake<-as.Date(streamData2000$DayOfYear-1,origin = "2000-01-01")
streamData2000$DayOfYear<- streamData1998$DayOfYear+365

streamData_x<-rbind(streamData1998,streamData,streamData2000)

# streamDataNA<-streamData[streamData$month==1&streamData$day==1,]
# streamDataNA$Discharge<-NA
# streamData<-rbind(streamData,streamData,streamData)

dataSmooth2<-streamData%>%group_by(DateFake,DayOfYear)%>%
  dplyr::summarize(Discharge = mean(Discharge7,na.rm = TRUE))

loessmdl<-loess(log10(Discharge)~DayOfYear,data = streamData_x[!is.na(streamData$Discharge),],span = 0.1667/3,family = "gaussian",na.action = "na.exclude")

dataSmooth2$Discharge_smooth<-  10^predict(loessmdl,newdata = dataSmooth2)

dataSmooth2$Discharge_smooth<-stat_smooth(data = dataSmooth2)

streamData$YR<-streamData$year%>%as.factor()
m_labels<-c("J","F","M","A","M","J","J","A","S","O","N","D")

dte_formatter <- function(x) { 
  #formatter for axis labels: J,F,M,... etc 
   substr(format(x, "%b"),1,1) 
 
} 

p1<-ggplot(streamData,aes(x = DateFake,y = Discharge))+
  geom_vline(xintercept = ymd(c(19990401,19990701,19991001)),col = "grey95")+
  geom_line(linewidth= 1,col = "gray50",alpha = 0.1,aes(group = YR))+
  geom_line(data = dataSmooth2,aes(y = Discharge_smooth),linewidth = 2)+
  # geom_smooth(data = dataSmooth2,linewidth = 2)+
  scale_color_manual(name = NULL,values = "grey20")+
  
  
  scale_x_date(name = "Date",
               # date_minor_breaks = "10 year",
               limits = ymd(c(19990101,20000101)),
               expand = c(0,0),
               date_breaks = "1 month",
                
               # breaks  = seq(from=as.Date("1999-01-01"),to=as.Date("2000-01-01"),by="month"),
               labels = dte_formatter)+
  scale_y_log10(name = expression(Discharge~(m^3*s^-1)))+
  
  theme_bw()+
  theme(legend.position = c(0.2,0.9),
        legend.background = element_rect(fill=alpha('white', 0.7)),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank())
p1


ggsave("Figures/HydrographExample_MoyieRiver.png",p1,width = 7.5,height = 5,dpi = 600)
ggsave("Figures/HydrographExample_MoyieRiver.pdf",p1,width = 7.5,height = 5,dpi = 600)



streamDataAll<-read.csv("2.data/2.working/Discharge/streamDataFinal.csv")

streamData<-streamDataAll[streamDataAll$ID=="08HA003",]

streamData$DayOfYear<-yday(ymd(streamData$Date))
streamData$DayOfYear[streamData$DayOfYear==366]<-365
streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")

streamData1998<-streamData
streamData1998$DateFake<-as.Date(streamData1998$DayOfYear-1,origin = "1998-01-01")
streamData1998$DayOfYear<- streamData1998$DayOfYear-365
streamData2000<-streamData
streamData2000$DateFake<-as.Date(streamData2000$DayOfYear-1,origin = "2000-01-01")
streamData2000$DayOfYear<- streamData1998$DayOfYear+365

streamData_x<-rbind(streamData1998,streamData,streamData2000)

# streamDataNA<-streamData[streamData$month==1&streamData$day==1,]
# streamDataNA$Discharge<-NA
# streamData<-rbind(streamData,streamData,streamData)

dataSmooth2<-streamData%>%group_by(DateFake,DayOfYear)%>%
  dplyr::summarize(Discharge = mean(Discharge7,na.rm = TRUE))

loessmdl<-loess(log10(Discharge)~DayOfYear,data = streamData_x[!is.na(streamData$Discharge),],span = 0.1667/3,family = "gaussian",na.action = "na.exclude")

dataSmooth2$Discharge_smooth<-  10^predict(loessmdl,newdata = dataSmooth2)


# dataSmooth2$Discharge_smooth<-stat_smooth(data = dataSmooth2)

streamData$YR<-streamData$year%>%as.factor()
m_labels<-c("J","F","M","A","M","J","J","A","S","O","N","D")

dte_formatter <- function(x) { 
  #formatter for axis labels: J,F,M,... etc 
  substr(format(x, "%b"),1,1) 
  
} 

p1<-ggplot(streamData,aes(x = DateFake,y = Discharge))+
  geom_vline(xintercept = ymd(c(19990401,19990701,19991001)),col = "grey95")+
  geom_line(linewidth= 1,col = "gray50",alpha = 0.1,aes(group = YR))+
  geom_line(data = dataSmooth2,aes(y = Discharge_smooth),linewidth = 2)+
  # geom_smooth(data = dataSmooth2,linewidth = 2)+
  scale_color_manual(name = NULL,values = "grey20")+
  
  
  scale_x_date(name = "Date",
               # date_minor_breaks = "10 year",
               limits = ymd(c(19990101,20000101)),
               expand = c(0,0),
               date_breaks = "1 month",
               
               # breaks  = seq(from=as.Date("1999-01-01"),to=as.Date("2000-01-01"),by="month"),
               labels = dte_formatter)+
  scale_y_log10(name = expression(Discharge~(m^3*s^-1)))+
  
  theme_bw()+
  theme(legend.position = c(0.2,0.9),
        legend.background = element_rect(fill=alpha('white', 0.7)),
        panel.grid.major.x  = element_blank(),
        panel.grid.minor.x  = element_blank())
p1


ggsave("3.Figures/HydrographExample_Koksilah.png",p1,width = 5,height = 3,dpi = 600)




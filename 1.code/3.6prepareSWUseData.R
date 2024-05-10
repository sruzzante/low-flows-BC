
closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(terra)
library(sf)
library(tmap)
library(scico)
library(raster)
library(reshape2)
library(zoo)
library(stringr)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")
watershedsAll<-st_union(watersheds)

licenses<-st_read("2.data/1.original/WaterUse/SurfaceWaterLicenses/WLS_WATER_RIGHTS_LICENCES_SV/WLS_WRL_P_point.shp")%>%
  st_transform(st_crs(watersheds))


licenses<-licenses[st_intersects(licenses,watershedsAll,sparse = FALSE),]

tmap_mode("view")
# tm_shape(watersheds)+tm_borders()+
#   tm_shape(licenses)+tm_dots()




parcels<-st_read("2.data/1.original/WaterUse/ParcelMap/pmbc_parcel_poly_sv.gdb")

summary(factor(licenses$PRPS_SE))
# tm_shape(watersheds)+tm_borders()+
#   tm_shape(licenses[licenses$PRPS_S_CD%in%c("03A","03B"),])+tm_dots()



AgWaterUse<-read.csv("2.data/1.original/Wateruse/AgWaterUse.csv",fileEncoding = "UTF-8-BOM")
AgWaterUse<-AgWaterUse[,!str_detect(names(AgWaterUse),"X\\.")]
AgWaterUse<-melt(AgWaterUse,id.vars = "X",variable.name = "PID")
AgWaterUse$PID<-str_remove(AgWaterUse$PID,"X")%>%
  str_pad(width = 9,pad = "0",side = "left")
AgWaterUse$value<-str_remove(AgWaterUse$value,"\u00A0m3")%>%as.numeric()
AgWaterUse$PID[AgWaterUse$PID=="0WLS.3143"]<-"WLS.3143"
AgWaterUse2<-AgWaterUse%>%
  group_by(PID)%>%
  dplyr::summarize(AugustPerc = value[X=="August"]/sum(value),
                   SeptemberPerc = value[X=="September"]/sum(value),
                   OctoberPerc = value[X=="October"]/sum(value)
  )



# parcelCentroids<-st_centroid(parcels)
names(parcels)
head(parcels)
# parcels_x<-parcels[parcels$PID%in%AgWaterUse2$PID,]
parcels$PID[parcels$PARCEL_POLY_ID=="8173096"]<-"PARCEL_POLY_ID..8173096"

# parcels2<-rbind(parcels,st_buffer(licen))
parcel_add<-st_buffer(licenses[licenses$WLS_WRL_SD%in%c(28720,84118,94236,3143,47763),],1000)
parcel_add[,names(parcels)]<-NA
parcel_add$PID<-paste0("WLS.",parcel_add$WLS_WRL_SD)
parcel_add<-parcel_add[,-which(names(parcel_add)%in%"SHAPE")]
st_geometry(parcel_add)<-"SHAPE"
# parcel_add$PID<-c("WLS.28720","WLS.84118","WLS.94236","WLS.3143","WLS.47763")
parcel_add<-parcel_add[,names(parcels)]
parcels2<-rbind(parcels,parcel_add)

parcels_x<-inner_join(parcels2,AgWaterUse2)

sum(AgWaterUse2$PID%in%parcels2$PID)

AgWaterUse2[!AgWaterUse2$PID%in%parcels2$PID,]
sum(parcels2$PID%in%AgWaterUse2$PID)


parcels_x_centroids<-st_centroid(parcels_x)


licenses$minDist<-apply(st_distance(licenses,parcels_x_centroids),1,FUN = min)


minInd<-apply(st_distance(licenses,parcels_x_centroids),1,FUN = which.min)

licenses$AugustPercUse<-parcels_x_centroids$AugustPerc[minInd]

licenses$SeptembertPercUse<-parcels_x_centroids$SeptemberPerc[minInd]

licenses$OctoberPercUse<-parcels_x_centroids$OctoberPerc[minInd]


ggplot(licenses[licenses$PRPS_S_CD%in%c("03A","03B"),],aes(x = minDist))+geom_histogram()
tmap_mode("view")
tm_shape(licenses[licenses$PRPS_S_CD%in%c("03A","03B"),])+
  tm_dots(col = "minDist",
          breaks = c(0,1e4,Inf))+
  tm_shape(parcel_add)+tm_dots()
tm_shape(licenses[licenses$PRPS_S_CD%in%c("04A"),])+
  tm_dots()

x<-licenses[licenses$PRPS_S_CD%in%c("04A")&licenses$YrlyDiversion>0,]
tm_shape(parcels_x_centroids)+tm_dots(col = "AugustPerc")

tm_shape(parcels_x)+tm_polygons(col = "AugustPerc")
licenses<-licenses[!duplicated(licenses[,-which(names(licenses)%in%c("WLS_WRL_SD","OBJECTID"))]),]

lcncNums<-licenses%>%
  as.data.frame()%>%
  group_by(LCNC_NMBR,PRPS_SE)%>%
  dplyr::summarize(N = n())

licenses<-left_join(licenses,lcncNums)


licenses$date<-ymd(licenses$PRRTY_DTE)

# licenses$QUANTITY
licenses$YrlyDiversion<-NA
summary(factor(licenses$QNTTY_NTS))


licenses$QNTTY_NTS[licenses$LCNC_NMBR=="504109"]<-"m3/year"
licenses$QNTTY_NTS[licenses$LCNC_NMBR=="504068"]<-"m3/year"

licenses$YrlyDiversion[licenses$QNTTY_NTS%in%c("m3/year","Total Flow")]<-(licenses$QUANTITY/licenses$N)[licenses$QNTTY_NTS%in%c("m3/year","Total Flow")]
licenses$YrlyDiversion[licenses$QNTTY_NTS=="m3/day"]<-(licenses$QUANTITY/licenses$N)[licenses$QNTTY_NTS=="m3/day"]*365

licenses$endDate<-as.Date("2025-01-01")
licenses$endDate[licenses$LCNC_STTS%in%c("Cancelled","Abandoned")]<-ymd(substr(licenses$LCNC_STTSE,1,8))[licenses$LCNC_STTS%in%c("Cancelled","Abandoned")]
summary(factor(licenses$PRPS_SE))



licenses[licenses$PRPS_S_CD=="02I01",]
licenses[licenses$PRPS_S_CD=="07A",]
licenses[licenses$PRPS_S_CD=="11A",]
licenses[licenses$PRPS_S_CD=="11B",]
licenses[licenses$PRPS_S_CD=="11C",]
licenses[licenses$PRPS_S_CD=="WSA01",] 


licenses$PRPS_SE[licenses$PRPS_SE%in%c("01A01 - Incidental - Domestic",
                                       'WSA07 - Misc Indust',
                                       "07A - Power: Residential",
                                       "02I06 - Misc Ind'l: Dewatering",
                                       "12A - Stream Storage: Power",
                                       "08A - Stream Storage: Non-Power",
                                       "02I30 - Ice & Snow Making: Snow",
                                       "02I14 - Crops: Frost Protection",
                                       "07A - Power: Residential",
                                       "07B - Power: Commercial",
                                       "07C - Power: General",
                                       "08B - Aquifer Storage: NP",
                                       "11A - Conservation: Storage",
                                       "11B - Conservation: Use of Water",
                                       "11C - Conservation: Construct Works",
                                       "WSA06 - Ice & Snow Making",
                                       "04A - Land Improve: General"
)]%>%factor()%>%table()%>%data.frame()

x<-licenses%>%
  filter(!duplicated(LCNC_NMBR))%>%
  mutate(exclude =  PRPS_SE%in%c("01A01 - Incidental - Domestic",
                                 'WSA07 - Misc Indust',
                                 "07A - Power: Residential",
                                 "02I06 - Misc Ind'l: Dewatering",
                                 "12A - Stream Storage: Power",
                                 "08A - Stream Storage: Non-Power",
                                 "02I30 - Ice & Snow Making: Snow",
                                 "02I14 - Crops: Frost Protection",
                                 "07A - Power: Residential",
                                 "07B - Power: Commercial",
                                 "07C - Power: General",
                                 "08B - Aquifer Storage: NP",
                                 "11A - Conservation: Storage",
                                 "11B - Conservation: Use of Water",
                                 "11C - Conservation: Construct Works",
                                 "WSA06 - Ice & Snow Making",
                                 "04A - Land Improve: General"
  ))%>%
  group_by(PRPS_S_CD,PRPS_SE)%>%
  summarise(nLicenses = n(),
            include = (!exclude[1])%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("NO","YES")))%>%
  st_drop_geometry()

for(it in 1:nrow(x)){
  x$PRPS_SE[it]<-str_remove(x$PRPS_SE[it],paste0(x$PRPS_S_CD[it]," - "))
}
write.csv(x,"4.output/licenseSummary.csv")


licenses<-licenses[!licenses$PRPS_SE%in%c("01A01 - Incidental - Domestic",
                                          'WSA07 - Misc Indust',
                                          "07A - Power: Residential",
                                          "02I06 - Misc Ind'l: Dewatering",
                                          "12A - Stream Storage: Power",
                                          "08A - Stream Storage: Non-Power",
                                          "02I30 - Ice & Snow Making: Snow",
                                          "02I14 - Crops: Frost Protection",
                                          "07A - Power: Residential",
                                          "07B - Power: Commercial",
                                          "07C - Power: General",
                                          "08B - Aquifer Storage: NP",
                                          "11A - Conservation: Storage",
                                          "11B - Conservation: Use of Water",
                                          "11C - Conservation: Construct Works",
                                          "WSA06 - Ice & Snow Making",
                                          "04A - Land Improve: General"
),]



summary(factor(licenses$PRPS_SE[licenses$QNTTY_NTS=="m3/year"]))
summary(factor(licenses$PRPS_SE[licenses$QNTTY_NTS=="m3/sec"]))




summary(factor(licenses$PRPS_SE))

monthlyMask = licenses$QNTTY_NTS=="m3/year"& licenses$PRPS_SE%in%c(
  "02F - Lwn, Fairway & Grdn: Watering",
  "02I13 - Crops: Flood Harvesting",
  "02I17 - Grnhouse & Nursery: Grnhouse",
  "02I22 - Grnhouse & Nursery: Nursery",
  "02I42 - Lwn, Fairway & Grdn: Res L/G",
  "03A - Irrigation: Local Provider",
  "03B - Irrigation: Private",
  "WSA05 - Greenhouse & Nursery",
  "WSA11 - Lawn, Fairway & Garden"
)




licenses$AugDiversion[monthlyMask]<-(licenses$YrlyDiversion*licenses$AugustPercUse)[monthlyMask]
licenses$SepDiversion[monthlyMask]<-(licenses$YrlyDiversion*licenses$SeptembertPercUse)[monthlyMask]
licenses$OctDiversion[monthlyMask]<-(licenses$YrlyDiversion*licenses$OctoberPercUse)[monthlyMask]

licenses$AugDiversion[!monthlyMask]<-licenses$YrlyDiversion[!monthlyMask]*31/365.25
licenses$SepDiversion[!monthlyMask]<-licenses$YrlyDiversion[!monthlyMask]*30/365.25
licenses$OctDiversion[!monthlyMask]<-licenses$YrlyDiversion[!monthlyMask]*31/365.25


st_write(licenses,"2.data/2.working/WaterUse/LicensedUse.gpkg")

# 
# licenses<-licenses%>%
#   filter(PODSUBTYPE=="POD"&
#            !is.na(YrlyDiversion))
# 
# for(it_wtrshd in 1:length(watersheds$StationNum)){
#   shp_wtrshd<-watersheds[it_wtrshd,]
#   
#   licenses_wtrshd<-st_intersection(licenses,shp_wtrshd)
#   tm_shape(shp_wtrshd)+tm_borders()+
#   tm_shape(licenses_wtrshd)+tm_dots(col = "black")
#   
#   
#   licensed_Div<-data.frame(year = 1900:2023,Total = NA,August = NA,September = NA,October = NA)
#   licenses_wtrshd<-licenses_wtrshd%>%filter(!is.na(YrlyDiversion))
#   for(it in 1:length(licensed_Div$year)){
#     licensed_Div$Total[it] <- sum(licenses_wtrshd$YrlyDiversion[year(licenses_wtrshd$date)<=licensed_Div$year[it]&
#                                                                   year(licenses_wtrshd$endDate)>licensed_Div$year[it]])  
#     licensed_Div$August[it] <- sum(licenses_wtrshd$AugDiversion[year(licenses_wtrshd$date)<=licensed_Div$year[it]&
#                                                                   year(licenses_wtrshd$endDate)>licensed_Div$year[it]])
#     licensed_Div$September[it] <- sum(licenses_wtrshd$SepDiversion[year(licenses_wtrshd$date)<=licensed_Div$year[it]&
#                                                                      year(licenses_wtrshd$endDate)>licensed_Div$year[it]])
#     licensed_Div$October[it] <- sum(licenses_wtrshd$OctDiversion[year(licenses_wtrshd$date)<=licensed_Div$year[it]&
#                                                                    year(licenses_wtrshd$endDate)>licensed_Div$year[it]])
#   }  
#   
#   write.csv(licensed_Div,paste0("2.data/2.working/WaterUse/",watersheds$StationNum[it_wtrshd],".csv"),row.names = FALSE)
#   datPlot<-licensed_Div
#   datPlot$Total<-datPlot$Total/(365.25*24*3600)
#   datPlot$August<-datPlot$August/(31*24*3600)
#   datPlot$September<-datPlot$September/(30*24*3600)
#   datPlot$October<-datPlot$October/(31*24*3600)
#   datPlot<-melt(datPlot,id.vars = "year")
#   p<-ggplot(datPlot,aes(year,value,color = variable))+geom_line()+
#     scale_y_continuous(name = "Water Diversion (cms)")+
#     ggtitle(watersheds$Station.Name[it_wtrshd])
#   
#   ggsave(paste0("Figures/WaterUse/Licensed-",watersheds$NameNom[it_wtrshd],"-",watersheds$StationNum[it_wtrshd],".png"),p,
#          width = 4,height = 3)
#   
#   
# }




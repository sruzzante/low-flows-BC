# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script estimates ground water use for each catchment, using:
# BC water license data (https://catalogue.data.gov.bc.ca/dataset/water-rights-licences-public)
# Well data https://catalogue.data.gov.bc.ca/dataset/groundwater-wells
# BC Assessment Folio data. This is the only dataset used in this paper that is not fully open-source, though it is available for universities in British Columbia  (https://databases.lib.sfu.ca/record/61430687690003610/BC-Assessment-Data-Advice)
# BC parcel map https://catalogue.data.gov.bc.ca/dataset/parcelmap-bc-parcel-polygons-ogl

# Method is described in Appendix B


closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(rgdal)
library(terra)
library(sp)
library(sf)
library(tmap)
library(scico)
library(raster)
library(zyp)
library(stringr)
library(sp)
library(scico)
library(reshape2)
library(zoo)
library(ggmap)
library(stringr)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory



# stations<-read_excel("stations3.xlsx")[1:27,1:7]
# 
# stations<-stations[!duplicated(stations$`Gauging station #`),]


watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")
# stations<-read.csv("2.data/2.working/StationMetadata/stations_final_wSnow.csv")
# watersheds<-watersheds[watersheds$StationNum%in%stations$ID,]
# watersheds <- watersheds[watersheds$Station%in%stations$`Gauging station #`,]
# st_crs(watersheds)<-"EPSG:3857"

watersheds_all<-st_union(watersheds)

# 
# licenses<-st_read("SurfaceWaterLicenses/BCGW_7113060B_1689278538004_4780/WLS_WATER_RIGHTS_LICENCES_SV/WLS_WRL_P_point.shp")%>%
#   st_transform(st_crs(watersheds))
# licenses<-st_intersection(licenses,watersheds)

licenses<-st_read("2.data/1.original/WaterUse/SurfaceWaterLicences_02022024/WLS_WATER_RIGHTS_LICENCES_SV/WLS_WRL_P_point.shp")%>%
  st_transform(st_crs(watersheds))



lcncNums<-licenses%>%
  as.data.frame()%>%
  group_by(LCNC_NMBR,PRPS_SE)%>%
  dplyr::summarize(N = n())

licenses<-left_join(licenses,lcncNums)

licenses$YrlyDiversion[licenses$QNTTY_NTS%in%c("m3/year","Total Flow")]<-(licenses$QUANTITY/licenses$N)[licenses$QNTTY_NTS%in%c("m3/year","Total Flow")]
licenses$YrlyDiversion[licenses$QNTTY_NTS=="m3/day"]<-(licenses$QUANTITY/licenses$N)[licenses$QNTTY_NTS=="m3/day"]*365

# WA_licenses<-st_read("SurfaceWaterLicenses/GWIS_SDEexport.gdb")%>%
#   st_transform(st_crs(watersheds))
# 
# WA_licenses2<-st_read("SurfaceWaterLicenses/WR_GEO_WaterDiversions_ECY_NHD.gdb")%>%
#   st_transform(st_crs(watersheds))

# wells<-st_read("2.data/1.original/WaterUse/wellRecords/GW_WATER_WELLS_WRBC_SVW/GW_WW_WRBC_point.shp")
wells<-st_read("2.data/1.original/WaterUse/wellRecords/GW_WATER_WELLS_WRBC_SVW/GW_WW_WRBC_point.shp")

sum(licenses$WLL_TG_NMR%in%wells$WELL_TAG)
licensedWells<-inner_join(wells,licenses%>%st_drop_geometry(),by =c("WELL_TAG" = "WLL_TG_NMR" ) )

sum(!is.na(licenses$WLL_TG_NMR))
sum(licenses$PODSUBTYPE=="PWD")
sum(wells$LIC_STATUS=="Licensed")
wells<-wells[!wells$WELL_TAG%in%licenses$WLL_TG_NMR,]


# licenses<-st_intersection(licenses,watersheds)
licenses<-licenses[st_intersects(licenses,watersheds_all,sparse = FALSE),]
wells<-wells[st_intersects(wells,watersheds_all,sparse = FALSE),]%>%
  filter(!is.na(SYSID))

# WA_licenses<-WA_licenses[st_intersects(WA_licenses,watersheds_all,sparse = FALSE),]
# 
# WA_licenses2<-WA_licenses2[st_intersects(WA_licenses2,watersheds_all,sparse = FALSE),]

summary(factor(wells$LIC_STATUS))
summary(factor(wells$INTEND_USE))
summary(factor(wells$WELL_CLASS))



tm_shape(watersheds)+tm_borders()+
  tm_shape(licenses)+tm_dots()+
  tm_shape(wells)+tm_dots(col = "red")
# tm_shape(WA_licenses2)+tm_dots(col = "green")


wells<-filter(wells,WELL_CLASS%in%c("Unknown","Water Supply")& 
                !INTEND_USE%in%c("Observation Well","Test","Open LP Geoexchange"))

summary(factor(wells$LIC_STATUS))
summary(factor(wells$INTEND_USE))
summary(factor(wells$WELL_CLASS))


# These data cannot be shared 
BCA_FOLIO<-st_read("../1.Spatial_data/regional/BC/soc_population_gov_mgmt_conflict_health/soc1_BCA_FOLIO/BCA_FOLIO_DESCRIPTIONS.gpkg")
BCA_FOLIO_centroids<-st_centroid(BCA_FOLIO)

watersheds_all_buff<-st_buffer(watersheds_all,3000)

BCA_mask1<-st_intersects(BCA_FOLIO_centroids, watersheds_all_buff,sparse = FALSE)

BCA_FOLIO2<-BCA_FOLIO[BCA_mask1,]

BCA_FOLIO_centroids<-BCA_FOLIO_centroids[BCA_mask1,]

quantile(st_coordinates(BCA_FOLIO_centroids)[,2])
BCA_lats<-st_coordinates(BCA_FOLIO_centroids)[,2]

BCA_FOLIO2.a<-BCA_FOLIO2[BCA_lats<468000,]
BCA_FOLIO2.b<-BCA_FOLIO2[BCA_lats>=468000&BCA_lats<571000,]
BCA_FOLIO2.c<-BCA_FOLIO2[BCA_lats>=571000&BCA_lats<680000,]
BCA_FOLIO2.d<-BCA_FOLIO2[BCA_lats>=680000,]

well_lats<-st_coordinates(wells)[,2]
wells.a<-wells[well_lats<471000,]%>%st_combine()
wells.b<-wells[well_lats>=465000&well_lats<574000,]%>%st_combine()
wells.c<-wells[well_lats>=568000&well_lats<683000,]%>%st_combine()
wells.d<-wells[well_lats>=677000,]%>%st_combine()

mask.a<-st_intersects(BCA_FOLIO2.a,wells.a,sparse = FALSE)
mask.b<-st_intersects(BCA_FOLIO2.b,wells.b,sparse = FALSE)
mask.c<-st_intersects(BCA_FOLIO2.c,wells.c,sparse = FALSE)
mask.d<-st_intersects(BCA_FOLIO2.d,wells.d,sparse = FALSE)

BCA_FOLIO2.a<-BCA_FOLIO2.a[mask.a,]
BCA_FOLIO2.b<-BCA_FOLIO2.b[mask.b,]
BCA_FOLIO2.c<-BCA_FOLIO2.c[mask.c,]
BCA_FOLIO2.d<-BCA_FOLIO2.d[mask.d,]


BCA_FOLIO3<-rbind(BCA_FOLIO2.a,BCA_FOLIO2.b,BCA_FOLIO2.c,BCA_FOLIO2.d)




BCA_FOLIO3<-BCA_FOLIO3[!duplicated(BCA_FOLIO3[,-c(1,2,3,4,36)]),]


well_parcel<-st_intersection(wells,BCA_FOLIO3)
length(unique(well_parcel$id))
length(unique(well_parcel$id))

#Jitter points to attribute them to nearby parcels
wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(10,0)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(-10,0)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(0,10)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(0,-10)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)


#Jitter points to attribute them to nearby parcels - 20 m
wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(20,0)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(-20,0)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(0,20)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
wells_notFound$geometry<-wells_notFound$geometry+c(0,-20)
st_crs(wells_notFound)<-"EPSG:3005"
well_parcel2<-st_intersection(wells_notFound,BCA_FOLIO2)
well_parcel<-rbind(well_parcel,well_parcel2)

wells_notFound<-wells[!wells$WELL_TAG%in%well_parcel$WELL_TAG,]
st_crs(wells_notFound)<-"EPSG:3005"
tmap_mode("view")
tm_shape(watersheds)+tm_polygons()+
  tm_shape(wells_notFound)+tm_dots()


# x<-BCA_FOLIO2[st_intersects(BCA_FOLIO_centroids,st_buffer(wells_notFound,100),sparse = FALSE),]
# tm_shape(watersheds)+tm_polygons()+

tm_shape(wells_notFound)+tm_dots()+
  tm_shape(x)+tm_polygons(col = "red",alpha = 0.5)


length(unique(well_parcel$WELL_TAG ))

summary(factor(wells_notFound$INTEND_USE))
summary(factor(wells_notFound$WELL_CLASS))

st_write(well_parcel,"2.data/2.working/WaterUse/Groundwater/ParcelData/wellParcels.gpkg",append=FALSE)

wells2<-left_join(wells[,c("WELL_TAG","ADDRESS","CITY","LEGAL_LOT",  "LEGAL_PLAN",
                           "LGL_DSTRCT","LEGAL_PID","LOCN_DESCR","WELLSTATUS","COORDAQ",
                           "WELL_CLASS", "WLL_SBCLSS", "INTEND_USE", "WELL_URL" ,
                           "LIC_STATUS", "AQUIFER_ID","PERSON", "FNSH_DEPTH", "TTL_DEPTH",
                           "BED_DEPTH" , "YIELD" ,"YIELD_DUR",  "ARTSN_CND" ,
                           "ARTSN_FLW","DIAMETER","CNSTRCT_ST","CNSTRCT_EN",
                           "DECOM_ST" ,  "DECOM_EN" ,  "DECOM_REAS","AQFR_MTRL")],
                 st_drop_geometry( well_parcel[,c("WELL_TAG","FOLIO_ID","ACTUAL_USE_CODE","ACTUAL_USE_DESCRIPTION",
                                 "FOLIO_STATUS","MANUAL_CLASS_CODE","TENURE_CODE",
                                 "TENURE_DESCRIPTION","FEATURE_AREA_SQM")]),
                 )
# duplicated parcels for some reason
wells2<-wells2[!duplicated(wells2$WELL_TAG),]
st_write(wells2,"2.data/2.working/WaterUse/Groundwater/ParcelData/wells.gpkg",append=FALSE)

wells2<-st_read("2.data/2.working/WaterUse/Groundwater/ParcelData/wells.gpkg")
# Water use estimates
summary(factor(wells2$INTEND_USE))

wells2$WaterUseYrly<-NA
wells2$WaterUseYrly[wells2$INTEND_USE%in%"Private Domestic"]<-1.75*365

wells2$WaterUseYrly[wells2$INTEND_USE%in%"Irrigation"]<- 0.3048*
  wells2$FEATURE_AREA_SQM[wells2$INTEND_USE%in%"Irrigation"]/2 # 1 acre-foot per acre per year, half the size of the lot


# Check licensed values and make some guesses about water use at unlicensed wells

summary(licensedWells$INTEND_USE%>%factor())
summary(licensedWells$YrlyDiversion[licensedWells$INTEND_USE=="Water Supply System"])
x<-licensedWells[licensedWells$INTEND_USE=="Water Supply System",]
y<-wells2[wells2$INTEND_USE=="Water Supply System",]

paste(wells2$ACTUAL_USE_CODE, wells2$ACTUAL_USE_DESCRIPTION)[is.na(wells2$WaterUseYrly)]%>%factor()%>%unique()%>%sort()
paste(wells2$ACTUAL_USE_CODE, wells2$ACTUAL_USE_DESCRIPTION)[is.na(wells2$WaterUseYrly)]%>%factor()%>%summary()

#assume domestic use
wells2$WaterUseYrly[(wells2$INTEND_USE%in%c("Water Supply System","Not Applicable","Unknown Well Use","Other"))&
                      wells2$ACTUAL_USE_CODE%>%as.numeric()<=70]<-1.75*365
summary(factor(wells2$INTEND_USE[is.na(wells2$WaterUseYrly)]))
summary(factor(wells2$ACTUAL_USE_DESCRIPTION[is.na(wells2$WaterUseYrly)]))

summary(factor(wells2$ACTUAL_USE_DESCRIPTION[wells2$INTEND_USE==""]))

wells2$WaterUseYrly[wells2$ACTUAL_USE_CODE=="610"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # campgrounds

wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Seasonal Resort"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Automobile Paint Shop, Garages, Etc."&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Campgrounds (Includes Government Campgrounds, Ymca &"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Campground (Commercial)"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Motel & Auto Court"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Churches & Bible Schools"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Hall (Community, Lodge, Club, Etc.)"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Hotel"&is.na(wells2$WaterUseYrly)]<-10*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Neighbourhood Pub"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Restaurant Only"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Fast Food Restaurants"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Automobile Dealership"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Bed & Breakfast Operation Less Than 4 Units"&is.na(wells2$WaterUseYrly)]<-3*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Individual Strata Lot (Hotel/Motel)"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Multi-Family (Residential Hotel)"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 #  
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Convenience Store/Service Station"&is.na(wells2$WaterUseYrly)]<-2*1.75*365 #  
wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Self-Serve Service Station"&is.na(wells2$WaterUseYrly)]<-2*1.75*365 #  


# wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION=="Hotel"&is.na(wells2$WaterUseYrly)]<-5*1.75*365 # 

wells2$WaterUseYrly[wells2$ACTUAL_USE_DESCRIPTION%in%"Golf Courses (Includes Public & Private)"&is.na(wells2$WaterUseYrly)]<-
  wells2$FEATURE_AREA_SQM[wells2$ACTUAL_USE_DESCRIPTION%in%"Golf Courses (Includes Public & Private)"&is.na(wells2$WaterUseYrly)]*0.3048 # assume 1 acre-foot per acre
  # 
pulpParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Pulp & Paper Mills (Incl Fine Paper, Tissue & Asphalt Roof)",]%>%st_union()
pulpLicenses<-licenses[st_intersects(licenses,pulpParcels,sparse = FALSE),]

pulpLicenses2<-licenses[licenses$PRPS_SE=="02A - Pulp Mill",]
# Ok so pulp mills apparently are not licensed, and mostly they return the water they use

x<-wells2%>%filter(is.na(WaterUseYrly))
paste(x$ACTUAL_USE_CODE, x$ACTUAL_USE_DESCRIPTION)[is.na(x$WaterUseYrly)]%>%factor()%>%summary()

#
wells2$INTEND_USE2<-wells2$INTEND_USE

wells2$INTEND_USE2[wells2$ACTUAL_USE_DESCRIPTION%in%c("Grain & Forage","Grain & Forage (Vacant)",
                                                      "Vegetable & Truck","Vegetable & Truck (Vacant)",
                                                      "Government Research Centres (Includes Nurseries &",
                                                      "Land Classified Recreational Used For",
                                                      "Mixed","Other","Mixed (Vacant)","Other (Vacant)")&
                     is.na(wells2$WaterUseYrly)]<-"Irrigation"

wells2$WaterUseYrly[wells2$INTEND_USE2%in%"Irrigation"]<- 0.3048*
  wells2$FEATURE_AREA_SQM[wells2$INTEND_USE2%in%"Irrigation"]/2 # 1 acre-foot per acre per year, half the size of the lot


# assume 1 acre per cow
# 0.05 m^3 per cow
msk<-is.na(wells2$WaterUseYrly)&
  wells2$ACTUAL_USE_DESCRIPTION%in%c("Beef","Beef (Vacant)","Dairy","Dairy (Vacant)",
                                     "Poultry","Poultry (Vacant)","Meat & Poultry")
x<-wells2[msk,]
wells2$WaterUseYrly[msk]<-wells2$FEATURE_AREA_SQM[msk]/4046.86*0.05*365

# assume irrigation duty of wineries and fruits is 0.5 acre-feet per acre

xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Winery",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Winery",],xLicenses)
summary(xLicParcels$YrlyDiversion/xLicParcels$FEATURE_AREA_SQM) # mean is 0.11669, half an acre-foot seems reasonable

mskOrchards<-is.na(wells2$WaterUseYrly)&
  wells2$ACTUAL_USE_DESCRIPTION%in%c("Winery","Small Fruits","Small fruits (Vacant)","Tree Fruits")

wells2$WaterUseYrly[mskOrchards]<-wells2$FEATURE_AREA_SQM[mskOrchards]*0.3048*0.5/2

# tm_shape(x[x$ACTUAL_USE_CODE=="650",])+tm_dots()

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Schools & Universities, College Or Technical Schools",
                                                                      "Civic, Institutional & Recreational (Vacant)",
                                                                      "Government Buildings (Includes Courthouse, Post Office",
                                                                      "Recreational & Cultural Buildings (Includes Curling",
                                                                      "Recreational Clubs, Ski Hills",
                                                                      "Office Building (Primary Use)",
                                                                      "Store(S) And Offices",
                                                                      "Shopping Centre (Community)",
                                                                      "Retail Strip",
                                                                      "Food Market",
                                                                      "Cemeteries (Includes Public Or Private).",
                                                                      "Seniors Licensed Care",
                                                                      "Store(S) And Living Quarters",
                                                                      "Store(S) And Service Commercial",
                                                                      "Stores And/Or Offices With Apartments",
                                                                      "Shopping Centre (Neighbourhood)",
                                                                      "Seniors Independent & Assisted Living",
                                                                      "Big Box"
                                                                      
                                                                      
                                                                      
)]<-10*1.75*365 #
wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Multi-Family (Conversion)",
                                                                      "Service Station"
                                                                      
)]<-5*1.75*365 #

x<-wells2%>%filter(is.na(WaterUseYrly))
paste(x$ACTUAL_USE_CODE, x$ACTUAL_USE_DESCRIPTION)[is.na(x$WaterUseYrly)]%>%factor()%>%summary()

xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Car Wash",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Car Wash",],xLicenses)
summary(xLicParcels$YrlyDiversion/xLicParcels$FEATURE_AREA_SQM) # mean is 0.11669, half an acre-foot seems reasonable

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Car Wash")]<-9125 #based on license PW194189

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Cement Plants")]<-40880.0000 #based on license PW190798


xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Cement Plants",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Sawmills",],xLicenses)
summary(xLicParcels$YrlyDiversion/xLicParcels$FEATURE_AREA_SQM) #



wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Sawmills",
                                                                                   "Planer Mills (When Separate From Sawmill)",
                                                                                   "Plywood Mills",
                                                                                   "Shingle Mills",
                                                                                   "Lumber Remanufacturing (When Separate From Sawmill)"
                                                                                   
                                                                                   )]<-1584.1    #based on median of licenses


x<-wells2%>%filter(is.na(WaterUseYrly))
x$ACTUAL_USE_DESCRIPTION[is.na(x$WaterUseYrly)]%>%factor()%>%summary()

xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Mining (Coal)",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Mining (Coal)",],xLicenses)
summary(xLicParcels$YrlyDiversion/xLicParcels$FEATURE_AREA_SQM) #

ggplot(xLicParcels,aes(x = FEATURE_AREA_SQM,y =  YrlyDiversion))+geom_point()
summary(xLicenses$YrlyDiversion[xLicenses$PRPS_SE=="05B - Mining: Washing Coal"])
summary(licenses$YrlyDiversion[licenses$PRPS_SE=="05B - Mining: Washing Coal"])
hist(licenses$YrlyDiversion[licenses$PRPS_SE=="05B - Mining: Washing Coal"])
unique(licenses$FL_NMBR[licenses$PRPS_SE=="05B - Mining: Washing Coal"])

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Mining (Coal)"
                                                                                   
)]<-829970      #based on median of licenses


xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Mining & Milling (Metallic)",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Mining & Milling (Metallic)",],xLicenses)

summary(licenses$YrlyDiversion[licenses$PRPS_SE=="05C - Mining: Processing Ore"])
sum(licenses$PRPS_SE=="05C - Mining: Processing Ore")

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Mining & Milling (Metallic)",
                                                                                   "Mining & Milling Non-Metallic (Including Asbestos,"
                                                                                   
)]<-182526         #based on median of licenses




xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Metal Fabricating Industries",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Metal Fabricating Industries",],xLicenses)



xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Airports, Heliports, Etc.",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Airports, Heliports, Etc.",],xLicenses) # based on median licensed 
summary(xLicParcels$YrlyDiversion)

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Airports, Heliports, Etc."
                                                                                   
)]<-7378         #based on median of licenses

# Logging Operations, Incl Log Storage # -not relevant
# Managed Forest (Improved)
# Miscellaneous (Forest And Allied Industry)
# Petroleum Bulk Plants
# Oil & Gas Pumping & Compressor Stations
# Miscellaneous (Petroleum Industry)
# Sand & Gravel (Vacant and Improved)
# Concrete Mixing Plants
# Railway



xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Railway",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(xLicenses$YrlyDiversion)
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Airports, Heliports, Etc.",],xLicenses) # based on median licensed 

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Petroleum Bulk Plants")]<-6784.14         #based on license # 0368895
wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Oil & Gas Pumping & Compressor Stations")]<-182.5            #based on license # 0368895




xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Sand & Gravel (Vacant and Improved)",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(factor(xLicenses$PRPS_SE))
summary(xLicenses$YrlyDiversion[xLicenses$PRPS_SE%in%c("WSA09 - Processing & Manufacturing","02B - Processing & Mfg: Processing")])
xLicParcels<-st_intersection(BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Airports, Heliports, Etc.",],xLicenses) # based on median licensed 

wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Sand & Gravel (Vacant and Improved)")]<-43983              #based on license # 0368895



xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Concrete Mixing Plants",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(factor(xLicenses$PRPS_SE))
summary(xLicenses$YrlyDiversion[xLicenses$PRPS_SE%in%c("WSA09 - Processing & Manufacturing","02B - Processing & Mfg: Processing",
                                                       "WSA07 - Misc Indust",
                                                       "WSA03 - Commercial Enterprise")])
wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Concrete Mixing Plants")]<-11680              #based on median


x<-wells2%>%filter(is.na(WaterUseYrly))
paste(x$ACTUAL_USE_CODE, x$ACTUAL_USE_DESCRIPTION)[is.na(x$WaterUseYrly)]%>%factor()%>%summary()



xParcels<-BCA_FOLIO[BCA_FOLIO$ACTUAL_USE_DESCRIPTION =="Water Distribution Systems",]%>%st_union()
xLicenses<-licenses[st_intersects(licenses,xParcels,sparse = FALSE),]
summary(factor(xLicenses$PRPS_SE))
summary(xLicenses$YrlyDiversion[xLicenses$PRPS_SE%in%c("00A - Waterworks: Local Provider")])

tm_shape(wells2[is.na(wells2$WaterUseYrly)& wells2$ACTUAL_USE_DESCRIPTION%in%c("Water Distribution Systems"),])+tm_dots()
wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)&( wells2$ACTUAL_USE_DESCRIPTION%in%c("Water Distribution Systems")|wells2$INTEND_USE%in%"Water Supply System")]  <-165932   #based on median

x<-wells2%>%filter(is.na(WaterUseYrly))
wells2$WaterUseYrly[is.na(wells2$WaterUseYrly)]<-1.75*365

folio_IDs<-wells2%>%
  st_drop_geometry()%>%
  group_by(FOLIO_ID,INTEND_USE2)%>%
  dplyr::summarize(n=n())


wells2<-left_join(wells2,folio_IDs)

wells2$YrlyDiversion<-wells2$WaterUseYrly/wells2$n

wells2$date<-ymd(wells2$CNSTRCT_ST)




summary(factor(licenses$PRPS_SE))

monthlyMask = wells2$INTEND_USE2%in%c("Irrigation")|mskOrchards






parcels<-st_read("2.data/1.original/WaterUse/ParcelMap/pmbc_parcel_poly_sv.gdb")


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


wells2$minDist<-apply(st_distance(wells2,parcels_x_centroids),1,FUN = min)


minInd<-apply(st_distance(wells2,parcels_x_centroids),1,FUN = which.min)

wells2$AugustPercUse<-parcels_x_centroids$AugustPerc[minInd]

wells2$SeptembertPercUse<-parcels_x_centroids$SeptemberPerc[minInd]

wells2$OctoberPercUse<-parcels_x_centroids$OctoberPerc[minInd]


ggplot(wells2[monthlyMask,],aes(x = minDist))+geom_histogram()
tmap_mode("view")
tm_shape(wells2[monthlyMask,])+
  tm_dots(col = "minDist",
          breaks = c(0,1e4,Inf))




wells2$AugDiversion[monthlyMask]<-(wells2$YrlyDiversion*wells2$AugustPercUse)[monthlyMask]
wells2$SepDiversion[monthlyMask]<-(wells2$YrlyDiversion*wells2$SeptembertPercUse)[monthlyMask]
wells2$OctDiversion[monthlyMask]<-(wells2$YrlyDiversion*wells2$OctoberPercUse)[monthlyMask]

wells2$AugDiversion[!monthlyMask]<-wells2$YrlyDiversion[!monthlyMask]*31/365.25
wells2$SepDiversion[!monthlyMask]<-wells2$YrlyDiversion[!monthlyMask]*30/365.25
wells2$OctDiversion[!monthlyMask]<-wells2$YrlyDiversion[!monthlyMask]*31/365.25


wells2<-wells2%>%select(!c(ACTUAL_USE_CODE,ACTUAL_USE_DESCRIPTION,FOLIO_ID,FOLIO_STATUS,
                           MANUAL_CLASS_CODE,TENURE_CODE,TENURE_DESCRIPTION,
                           FEATURE_AREA_SQM))

st_write(wells2,"2.data/2.working/WaterUse/UnlicensedWellsUse.gpkg",delete_dsn = TRUE)


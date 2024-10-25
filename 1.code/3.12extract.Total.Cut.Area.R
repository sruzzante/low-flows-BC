# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24

# This script calculates the total cut/burned fraction of each watershed, since 1900

#library(sf)
library(terra)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/"))
# setwd(paste0("projects/def-tgleeson/ruzzante/low-flows-BC/1.code/"))


TCA_fun<-function(xAge){
  
  
  totalCutArea<- mean(xAge<=150,na.rm = TRUE)
  # return(list(ECA_5,ECA_10,ECA_20,ECA_60))
  
}



watersheds<-vect("./2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

forestAge_x<-terra::rast(paste0("../DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/lulc2_forestAge/forestAge_2022.tif"))

x<-terra::extract(forestAge_x,watersheds,fun = TCA_fun)


# x<- unlist(x)
x<-cbind(data.frame(watersheds[,c("StationNum")]),data.frame(x))

#head(x)
saveRDS(x,file = paste0("../2.data/2.working/ECA/TCA_2022.rds"))


# load a raster of private forestry land across Canada
# Map of Forest Management in Canada - Map of Forest Management in Canada, 2020 Version, Gridded Raster
# https://open.canada.ca/data/en/dataset/d8fa9a38-c4df-442a-8319-9bbcbdc29060/resource/b90b0339-1d99-4103-8a40-c3dce9f019de

x<-rast("../DATA/1.Spatial_data/regional/Canada/lulc_landuse_landcover/lulc1_ForestManagement/Canada_MFv2020.tif")

# 50 is the 'private land' class
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

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  st_transform(st_crs(maskPrivate))
stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")


watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private[,"Canada_MFv2020"])
watersheds$PrivateForestry<-watersheds$watersheds_Private....Canada_MFv2020..
stations<-left_join(stations,st_drop_geometry(watersheds[,c("ID","PrivateForestry")]),by = c("ID" ))

TCA<-readRDS("2.data/2.working/ECA/TCA_2022.rds")
names(TCA)[3]<-"Total.Cut.Area"
stations<-left_join(stations,TCA%>%select(StationNum,Total.Cut.Area),by = c("ID" = "StationNum"))

# overwrite the 'stations' file with the new information
saveRDS(stations,"2.data/2.working/StationMetadata/stations_final.RDS")

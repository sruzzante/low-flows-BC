
library(sf)
library(terra)
library(doParallel)
library(foreach)
# cutblocks<-vect("/lulc1_Consolidated_Cut_Block/Consolidated_Cut_Block.gdb")
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria./DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/"))

cutblocks<-st_read("lulc1_Consolidated_Cut_Block/Consolidated_Cut_Block.gdb")

cutblocks_ls<-split(cutblocks,cutblocks$HARVEST_YEAR)

# d<-cutblocks%>%
#   st_drop_geometry()%>%
#   group_by(HARVEST_YEAR)%>%
#   summarize(cut_area = sum(Shape_Area))

# ggplot(d)+geom_line(aes(x = HARVEST_YEAR,y = cut_area))


(er <- rast(ext( c(xmin = 275000,xmax = 1871000 ,ymin = 367000,ymax = 1736000 )), resolution=30) ) 

# plot()
crs(er) <- crs(cutblocks)

# cutblocks_x<-cutblocks_ls[1]


# cl <- makeCluster(48)
#Register cluster
# registerDoParallel(cl)
foreach(cutblocks_x = cutblocks_ls,
        .packages=c("sf","terra")
)%do%{
  x<-terra::rasterize(cutblocks_x,er)
  writeRaster(x,paste0("lulc2_Consolidated_Cut_Block/cutblocks_",cutblocks_x$HARVEST_YEAR[1],".tif"),
              gdal=c("COMPRESS=DEFLATE", "TFW=YES","SPARSE_OK=TRUE"),
              overwrite = TRUE)
}


forestAge<-rast(vals = 200,xmin = 275000,xmax = 1871000 ,ymin = 367000,ymax = 1736000, resolution=30) 
crs(forestAge) <- crs("EPSG:3005")

bc<-bcmaps::bc_neighbours()
library(tmap)
tmap_mode("plot")
tm_shape(x,raster.downsample = TRUE)+tm_raster()+
  tm_shape(bc)+tm_borders()
plot(x)
plot(bc,add = TRUE,alpha = 0)
plot(forestAge)

forestAge_x<-forestAge
# xCut<-terra::rast(paste0("lulc2_Consolidated_Cut_Block/cutblocks_",it_yr,".tif"))
# xFire<-terra::rast(paste0("lulc2_Consolidated_Cut_Block/cutblocks_",it_yr,".tif"))
# forestAge_x<-terra::rast("lulc2_forestAge/forestAge_1902.tif")
for(it_yr in 1904:2022){
  tictoc::tic()
  forestAge_x<-forestAge_x+1
  if(!it_yr%in%c(1903,1906,1907,1911,1914,1916)){
    xCut<-terra::rast(paste0("lulc2_Consolidated_Cut_Block/cutblocks_",it_yr,".tif"))
    forestAge_x[xCut]<-0
  }
  
  if(it_yr>=1917){
    xFire<-terra::rast(paste0("lulc2_Fire_perimeters/firePerims_",it_yr,".tif"))
    forestAge_x[xFire]<-0
  }
   
  writeRaster(forestAge_x,paste0("lulc2_forestAge/forestAge_",it_yr,".tif"),
              gdal=c("COMPRESS=DEFLATE", "TFW=NO","SPARSE_OK=TRUE"),
              overwrite = TRUE)
  print(it_yr)
  tictoc::toc()
}


### Create ECA time series


library(sf)
library(terra)
library(doParallel)
library(foreach)
# setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria./low-flows-BC/"))

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")
ECA_fun<-function(xAge){
  ECA_5 <-mean(pmax((1-xAge/5 ),0),na.rm = TRUE)
  ECA_10<-mean(pmax((1-xAge/10),0),na.rm = TRUE)
  ECA_20<-mean(pmax((1-xAge/20),0),na.rm = TRUE)
  ECA_60<-mean(pmax((1-xAge/60),0),na.rm = TRUE)
  return(data.frame(ECA_5=ECA_5,ECA_10=ECA_10,ECA_20=ECA_20,ECA_60=ECA_60))
  # return(list(ECA_5,ECA_10,ECA_20,ECA_60))
  
}
watersheds<-vect(watersheds)
watersheds_ls<-rep(list(watersheds),123)
foreach(it_yr = 1900:2022,
        .packages=c("terra")){
  forestAge_x<-terra::rast(paste0("../DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/lulc2_forestAge/forestAge_",it_yr,".tif"))
 
    x<-extract(forestAge_x,watersheds[115:116,],fun = ECA_fun)
     # x<- unlist(x)
    x<-cbind(data.frame(watersheds[115:116,c("StationNum")]),data.frame(x))
    names(x)[3:6]<-c("ECA_5","ECA_10","ECA_20","ECA_60")
    saveRDS(x,file = paste0("../2.data/2.working/ECA/ECA_",it_yr,".rds"))
  
}

x0<-terra::rast("lulc2_forestAge/withoutFire/forestAge_1965.tif")
x1<-terra::rast("lulc2_forestAge/forestAge_1965.tif")
plot(x0)

tm_shape(x0,raster.downsample = TRUE)+tm_raster()+
  tm_shape(bc)+tm_borders()

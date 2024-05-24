
library(sf)
library(terra)
library(doParallel)
library(foreach)
#setwd("/home/ruzzante/projects/def-tgleeson/ruzzante/DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/")


forestAge<-rast(vals = 200,xmin = 275000,xmax = 1871000 ,ymin = 367000,ymax = 1736000, resolution=30) 
crs(forestAge) <- crs("EPSG:3005")

#forestAge_x<-forestAge
forestAge_x<-terra::rast("lulc2_forestAge/forestAge_1956.tif")
for(it_yr in 1957:2022){
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


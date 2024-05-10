
library(sf)
library(terra)
library(doParallel)
library(foreach)

# cutblocks<-vect("/lulc1_Consolidated_Cut_Block/Consolidated_Cut_Block.gdb")
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria./DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/"))

cutblocks<-st_read("lulc1_Consolidated_Cut_Block/Consolidated_Cut_Block.gdb")

cutblocks_ls<-split(cutblocks,cutblocks$HARVEST_YEAR)

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




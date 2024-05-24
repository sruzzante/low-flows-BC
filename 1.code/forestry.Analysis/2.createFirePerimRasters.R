
library(sf)
library(terra)
library(doParallel)
library(foreach)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria./DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/"))

firePerims<-st_read("lulc1_Fire_perimeters/PROT_HISTORICAL_FIRE_POLYS_SP/H_FIRE_PLY_polygon.shp")
firePerims<-firePerims%>%filter(FIRE_YEAR>1976)
firePerims_ls<-split(firePerims,firePerims$FIRE_YEAR)



(er <- rast(ext( c(xmin = 275000,xmax = 1871000 ,ymin = 367000,ymax = 1736000 )), resolution=30) ) 
crs(er) <- crs(firePerims)

firePerims_x<-firePerims_ls[[1]]


cl <- makeCluster(22)
# Register cluster
registerDoParallel(cl)

foreach(firePerims_x = firePerims_ls,
        # er = er,
        .packages=c("sf","terra")
)%do%{
  tictoc::tic()
  x<-terra::rasterize(firePerims_x,er)
  writeRaster(x,paste0("lulc2_Fire_perimeters/firePerims_",firePerims_x$FIRE_YEAR[1],".tif"),
              gdal=c("COMPRESS=DEFLATE", "TFW=NO","SPARSE_OK=TRUE"),
              overwrite = TRUE)
  tictoc::toc()
}



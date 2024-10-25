# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-25

## This script extracts compares the annual mean precipitation for 1900-1949 and 1950-2015 for ANUSPLIN and ANUSPLIN-adjusted (Figure G11 and G12)

closeAllConnections()
rm(list=ls())
graphics.off()
library(terra)
library(tictoc)
library(stringr)
library(dplyr)
library(sf)
library(tmap)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory
#1900-1949
pcp_adj<-c()
for(it_year in 1900:1949){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_adj<-c(pcp_adj,
               rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",it_year,"/pcp_",it_month_pad,".asc/pcp_",it_month_pad,".asc"))
               )
   
    toc()
    
  }
}

pcp_adj_rast<-rast(pcp_adj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
# pcp_adj_rast<-terra::crop(pcp_adj_rast,bc)
plot(pcp_adj_rast[[1]])
pcp_adj_mean<-sum(pcp_adj_rast)/50
plot(pcp_adj_mean)

writeRaster(pcp_adj_rast,"C:/Users/Sacha Ruzzante/Desktop/pcp_adj_rast.tif")



pcp_unadj<-c()
for(it_year in 1900:1949){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_unadj<-c(pcp_unadj,
               rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",it_year,"/pcp60_",it_month_pad,".tif"))
    )
    
    toc()
    
  }
}

pcp_unadj_rast<-rast(pcp_unadj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
# pcp_unadj_rast<-terra::crop(pcp_unadj_rast,bc)
pcp_unadj_mean<-sum(pcp_unadj_rast)/50
plot(pcp_unadj_mean)

# 
# pcp_unadj_mean_ds<-terra::project(pcp_unadj_mean, (pcp_adj_mean),method = "bilinear" )
# 
# plot(pcp_unadj_mean_ds)
# 
# pcp_ratio<-pcp_adj_mean/pcp_unadj_mean_ds
# plot(pcp_ratio)


pcp_adj_mean_up<-terra::project(pcp_adj_mean, (pcp_unadj_mean),method = "bilinear" )
plot(pcp_adj_mean_up)

pcp_ratio<-pcp_adj_mean_up/pcp_unadj_mean
plot(pcp_ratio)

writeRaster(pcp_ratio,"2.data/2.working/WeatherDataANUSPLIN/pcp_ratio_1900_1949.tif",overwrite = TRUE)
pcp_ratio<-rast("2.data/2.working/WeatherDataANUSPLIN/pcp_ratio_1900_1949.tif")
stns_AdjDlyRS<-xlsx::read.xlsx("C:/Users/Sacha Ruzzante/Desktop/StationSIS_3346.xlsx",1)
stns_AdjDlyRS<-filter(stns_AdjDlyRS,Start.year<195000&End.year>189900)%>%
  st_as_sf(coords = c("Longitude","Latitude"),crs = "EPSG:4326")%>%
  mutate(lbl = "Adjusted P stations operating between 1900 and 1949")

tm<-
  tm_shape(st_as_sf(bc))+tm_borders()+
  tm_shape(pcp_ratio,raster.downsample = FALSE)+tm_raster(breaks = c(-Inf,seq(0.5,1.5,0.1),Inf),
                              palette = "PiYG",
                              title = "Adjusted/Unadjusted\nAnnual Precipitation\n(1900-1949)")+
  tm_shape(st_as_sf(bc))+tm_borders()+
  tm_shape(stns_AdjDlyRS)+tm_dots(col="lbl",title = "",palette = "black")+
  tm_layout(legend.position = c("left","bottom"))
tmap_save(tm,filename = "3.figures/ANUSPLIN_ratio_1900_1949.png",dpi = 600,
          width = 6)


# 1950-2015
rm(list=ls())
pcp_adj<-c()
for(it_year in 1950:2015){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_adj<-c(pcp_adj,
               rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",it_year,"/pcp_",it_month_pad,".asc/pcp_",it_month_pad,".asc"))
    )
    
    toc()
    
  }
}

pcp_adj_rast<-rast(pcp_adj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
# pcp_adj_rast<-terra::crop(pcp_adj_rast,bc)
plot(pcp_adj_rast[[1]])
pcp_adj_mean<-sum(pcp_adj_rast)/66
plot(pcp_adj_mean)

writeRaster(pcp_adj_rast,"C:/Users/Sacha Ruzzante/Desktop/pcp_adj_rast_1950_2015.tif")



pcp_unadj<-c()
for(it_year in 1950:2015){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_unadj<-c(pcp_unadj,
                 rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",it_year,"/pcp60_",it_month_pad,".tif"))
    )
    
    toc()
    
  }
}

pcp_unadj_rast<-rast(pcp_unadj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
# pcp_unadj_rast<-terra::crop(pcp_unadj_rast,bc)
pcp_unadj_mean<-sum(pcp_unadj_rast)/66
plot(pcp_unadj_mean)


# pcp_unadj_mean_ds<-terra::project(pcp_unadj_mean, (pcp_adj_mean),method = "bilinear" )
# 
# plot(pcp_unadj_mean_ds)
# 
# pcp_ratio<-pcp_adj_mean/pcp_unadj_mean_ds
# plot(pcp_ratio)


pcp_adj_mean_up<-terra::project(pcp_adj_mean, (pcp_unadj_mean),method = "bilinear" )
plot(pcp_adj_mean_up)

pcp_ratio<-pcp_adj_mean_up/pcp_unadj_mean
plot(pcp_ratio)

writeRaster(pcp_ratio,"2.data/2.working/WeatherDataANUSPLIN/pcp_ratio_1950_2015.tif",overwrite = TRUE)

pcp_ratio<-rast("2.data/2.working/WeatherDataANUSPLIN/pcp_ratio_1950_2015.tif")
plot(pcp_ratio
     )

stns_AdjDlyRS<-xlsx::read.xlsx("C:/Users/Sacha Ruzzante/Desktop/StationSIS_3346.xlsx",1)
stns_AdjDlyRS<-filter(stns_AdjDlyRS,Start.year<=201500&End.year>=195000)%>%
  st_as_sf(coords = c("Longitude","Latitude"),crs = "EPSG:4326")%>%
  mutate(lbl = "Adjusted P stations operating between 1950 and 2015")

tm<-
  tm_shape(st_as_sf(bc))+tm_borders()+
  
  tm_shape(pcp_ratio,raster.downsample = FALSE)+tm_raster(breaks = c(-Inf,seq(0.5,1.5,0.1),Inf),
                                                          palette = "PiYG",
                                                          title = "Adjusted/Unadjusted\nAnnual Precipitation\n(1950-2015)")+
  tm_shape(st_as_sf(bc))+tm_borders()+
  tm_shape(stns_AdjDlyRS)+tm_dots(col="lbl",title = "",palette = "black")+
  tm_layout(legend.position = c("left","bottom"))
tmap_save(tm,filename = "3.figures/ANUSPLIN_ratio_1950_2015.png",dpi = 600,
          width = 6)


# 2000-2015
rm(list=ls())
pcp_adj<-c()
for(it_year in 2000:2015){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_adj<-c(pcp_adj,
               rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",it_year,"/pcp_",it_month_pad,".asc/pcp_",it_month_pad,".asc"))
    )
    
    toc()
    
  }
}

pcp_adj_rast<-rast(pcp_adj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
pcp_adj_rast<-terra::crop(pcp_adj_rast,bc)
plot(pcp_adj_rast[[1]])
pcp_adj_mean<-sum(pcp_adj_rast)/50
plot(pcp_adj_mean)

writeRaster(pcp_adj_rast,"C:/Users/Sacha Ruzzante/Desktop/pcp_adj_rast_2000_2015.tif")



pcp_unadj<-c()
for(it_year in 2000:2015){
  for(it_month in 1:12){
    tic()
    it_month_pad<-str_pad(it_month,width=2,side = "left",pad=0)
    pcp_unadj<-c(pcp_unadj,
                 rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",it_year,"/pcp60_",it_month_pad,".tif"))
    )
    
    toc()
    
  }
}

pcp_unadj_rast<-rast(pcp_unadj)

bc<-bcmaps::bc_neighbours()%>%
  filter(name=="British Columbia")%>%st_transform(st_crs(pcp_adj_rast))%>%
  vect()
pcp_unadj_rast<-terra::crop(pcp_unadj_rast,bc)
pcp_unadj_mean<-sum(pcp_unadj_rast)/50
plot(pcp_unadj_mean)


# pcp_unadj_mean_ds<-terra::project(pcp_unadj_mean, (pcp_adj_mean),method = "bilinear" )
# 
# plot(pcp_unadj_mean_ds)
# 
# pcp_ratio<-pcp_adj_mean/pcp_unadj_mean_ds
# plot(pcp_ratio)


pcp_adj_mean_up<-terra::project(pcp_adj_mean, (pcp_unadj_mean),method = "bilinear" )
plot(pcp_adj_mean_up)

pcp_ratio<-pcp_adj_mean_up/pcp_unadj_mean
plot(pcp_ratio)

writeRaster(pcp_ratio,"2.data/2.working/WeatherDataANUSPLIN/pcp_ratio_2000_2015.tif")

stns_AdjDlyRS<-xlsx::read.xlsx("C:/Users/Sacha Ruzzante/Desktop/StationSIS_3346.xlsx",1)
stns_AdjDlyRS<-filter(stns_AdjDlyRS,Start.year<201500&End.year>=200000)%>%
  st_as_sf(coords = c("Longitude","Latitude"),crs = "EPSG:4326")%>%
  mutate(lbl = "Adjusted stations operating between 2000-2015")

tm<-
  tm_shape(pcp_ratio,raster.downsample = FALSE)+tm_raster(breaks = c(-Inf,seq(0.5,1.5,0.1),Inf),
                                                          palette = "PiYG",
                                                          title = "Adjusted/Unadjusted\nAnnual Precipitation\n(2000-2015)")+
  tm_shape(st_as_sf(bc))+tm_borders()+
  tm_shape(stns_AdjDlyRS)+tm_dots(col="lbl",title = "",palette = "black")+
  tm_layout(legend.position = c("left","bottom"))
tmap_save(tm,filename = "3.figures/ANUSPLIN_ratio_2000_2015.png",dpi = 600,
          width = 6)


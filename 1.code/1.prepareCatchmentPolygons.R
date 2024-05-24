# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10

# Prepare Catchment Polygon data
# From two sources: 
# Hydrometric Basin Network Polygons: https://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/HydrometricNetworkBasinPolygons/
# Delineation by Jeremy Krogh: https://www.arcgis.com/apps/mapviewer/index.html?layers=844c585dbe7a44e382564446e21690e4

# This script also creates a line for the continental divide in BC and polygons
# for the east and west of the province.

library(sf)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


stations<-read.csv("2.data/1.original/Discharge_natural/stationList.csv",fileEncoding = "UTF-8-BOM")

# Load in data from Hydrometric Basin
shapefileList<-list.files(path = "2.data/1.original/CatchmentPolygons/HydrometricNetworkBasinPolygons/data",
                          pattern = "_DrainageBasin_BassinDeDrainage.shp",recursive = TRUE)
shapefileList<-shapefileList[!str_detect(shapefileList,".xml")]
shapefileListStn<-substr(shapefileList,1,7)

shapefileList<-shapefileList[shapefileListStn%in%stations$ID]
shapefileList<-paste0("2.data/1.original/CatchmentPolygons/HydrometricNetworkBasinPolygons/data/",shapefileList)


watersheds<-st_read(shapefileList[1])
for(it in 2:length(shapefileList)){
  wtrshd<- st_read(shapefileList[it])%>%
    st_zm(drop = TRUE)
  watersheds<-rbind(watersheds,
                    wtrshd)
  
}


watersheds_valid<-watersheds%>%
  st_transform("EPSG:3005")%>%
  st_make_valid()
watersheds_valid<-watersheds_valid[order(watersheds_valid$Area_km2,decreasing = TRUE),]
tm_shape(watersheds_valid)+tm_polygons(alpha = 0.5)

x<-st_read("2.data/1.original/CatchmentPolygons/Jeremy_Krogh_delineation/Watersheds.shp")
names(watersheds_valid)
names(x)
names(x)[5]<-"StationNum"
names(x)[7]<-'Area_km2'
names(x)[6]<-'NameNom'
st_geometry(x)<-'geom'
x<-x[,c("StationNum","NameNom","Area_km2","geom")]
x<-x[!x$StationNum%in%watersheds_valid$StationNum,]
st_crs(x)<-"EPSG:3857"

x<-st_transform(x,st_crs(watersheds_valid))
# tm_shape(x)+tm_polygons()

watersheds_valid<-watersheds_valid[,c("StationNum","NameNom","Area_km2","geom")]
watersheds_valid2<-rbind(watersheds_valid,x)
# 
watersheds_valid2<-watersheds_valid2[watersheds_valid2$StationNum%in%stations$ID&
                                       !duplicated(watersheds_valid2),]

st_write(watersheds_valid2,"2.data/2.working/CatchmentPolygons/Watersheds_natural_BC.gpkg",delete_dsn = TRUE)


## prepare study area polygons and dividing line 

basins<-st_read("../1.Spatial_data/regional/Canada/dem_topography/dem1_drainageBasins/m-c-1.1/Drainage_regions_Regions_de_drainage.shp")
basins_east<-basins%>%filter(Code_RD%in%c("06","07"))%>%
  st_union()%>%
  st_transform(st_crs(bc))%>%
  st_simplify(dTolerance = 500)

basins_east<-st_difference(basins_east,plot_sf%>%st_union())


bc_west<-st_difference(bc, basins_east)

# basins_east<-basins_east%>%
#   st_buffer(dist = -4000)
bc_east<-st_intersection(basins_east,bc)
bc_east<-st_cast(bc_east, "POLYGON")
bc_east<-bc_east[st_area(bc_east)%>%as.numeric()>2E11]
plot(bc_east)
plot(bc_west)

cont_divide<-st_cast(bc_east,"LINESTRING")[[1]]
plot(st_linestring(cont_divide[c(6453:6456 ,1:6432),])%>%st_sfc())
cont_divide<-st_linestring(cont_divide[c(6453:6456 ,1:6432),])%>%st_sfc()

st_crs(cont_divide)<- st_crs(bc)

st_write(cont_divide,dsn = "2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")

st_write(bc_west,dsn = "2.data/2.working/CatchmentPolygons/bc_west.gpkg")
st_write(bc_east,dsn = "2.data/2.working/CatchmentPolygons/bc_east.gpkg")


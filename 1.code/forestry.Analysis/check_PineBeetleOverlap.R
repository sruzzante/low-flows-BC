library(sf)
library(terra)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC"))

#st_info("D:/Downloads/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2023.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2023.gdb")


st_layers("D:/Downloads/VRI_2023/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2023.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")
st_read("D:/Downloads/VRI_2023/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2023.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_2023/VEG_COMP_LYR_D_POLY_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_LYR_D_POLY_2023.gdb", 
        query="select * from \"VEG_COMP_LYR_D_POLY\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_2023/VEG_COMP_LYR_L2_POLY_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_LYR_L2_POLY_2023.gdb", 
        query="select * from \"VEG_COMP_LYR_L2_POLY\" limit 0")%>%names()%>%sort()
st_layers("D:/Downloads/VRI_2023/VEG_COMP_LYR_L1_POLY_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_LYR_L1_POLY_2023.gdb", 
        query="select * from \"VEG_COMP_LYR_L1_POLY\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_2023/VEG_COMP_POLY_AND_LAYER_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_POLY_AND_LAYER_2023.gdb", 
        query="select * from \"VEG_COMP_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_2023/VEG_COMP_POLY_AND_LAYER_2023.gdb", 
        query="select * from \"VEG_COMP_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_2023/VEG_COMP_LYR_R1_POLY_2023.gdb")
st_read("D:/Downloads/VRI_2023/VEG_COMP_LYR_R1_POLY_2023.gdb", 
        query="select * from \"VEG_COMP_LYR_R1_POLY\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2022.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2022.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2022.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VEG_COMP_VDYP_INPUT_2010.gdb/VEG_COMP_VDYP_INPUT.gdb")
st_read("D:/Downloads/VEG_COMP_VDYP_INPUT_2010.gdb/VEG_COMP_VDYP_INPUT.gdb", 
        query="select * from \"VEG_COMP_VDYP_INPUT\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2021.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2020.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2019.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2018.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2018.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2018.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2017.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2017.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2017.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()



st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2016.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2016.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2016.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2015.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2015.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_POLY\" limit 0")%>%names()%>%sort()
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER_2015.gdb/VEG_COMP_VDYP7_INPUT_POLY_AND_LAYER.gdb", 
        query="select * from \"VEG_COMP_VDYP7_INPUT_LAYER\" limit 0")%>%names()%>%sort()

st_layers("D:/Downloads/VRI_historical/VEG_COMP_VDYP_INPUT_2014.gdb/veg_comp_vdyp_input.gdb")
st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP_INPUT_2014.gdb/veg_comp_vdyp_input.gdb", 
        query="select * from \"VEG_COMP_VDYP_INPUT\" limit 0")%>%names()%>%sort()

X<-st_read("D:/Downloads/VRI_historical/VEG_COMP_VDYP_INPUT_2014.gdb/veg_comp_vdyp_input.gdb")

summary(factor(X$DISTURBANCE_TYPE))

head(X)

X_I<-dplyr::filter(X,DISTURBANCE_TYPE%in%c("I","IAG","IB","IBB","IBD",'IBI','IBL','IBM',
                                    'IBS','ID','IDF','IDH','IDI','IDL','IDT','IDW',
                                    'ISB','IW','IWS') )
st_layers("D:/Downloads/VRI_historical/VEG_COMP_POLY_AND_LAYER_2014.gdb")
X_poly<-st_read("D:/Downloads/VRI_historical/VEG_COMP_POLY_AND_LAYER_2014.gdb",layer = "VEG_COMP_POLY")
X_poly_I<-dplyr::filter(X_poly,FEATURE_ID%in%X_I$FEATURE_ID)

allInsect<-st_union(X_poly_I)

# allInsect<-st_combine(X_poly_I)

# allInsect_vc<-vect(allInsect)


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

watersheds_insects<-st_intersection(watersheds,allInsect)
hist(as.numeric(st_area(watersheds_insects)/(watersheds_insects$Area_km2*10^6)))

sdf<-watersheds_insects[as.numeric(st_area(watersheds_insects)/(watersheds_insects$Area_km2*10^6))>0.4,]
library(tmap)
tmap_mode("view")
tm_shape(sdf)+tm_polygons()+
  tm_shape(watersheds%>%dplyr::filter(StationNum%in%sdf$StationNum))+tm_borders()

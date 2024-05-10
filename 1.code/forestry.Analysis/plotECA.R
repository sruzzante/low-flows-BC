library(ggplot2)
library(dplyr)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC")) #Set the working directory


ECA<-readRDS("2.data/2.working/ECA/ECA.rds")

for(it in 2:7){ECA[,it]<-as.numeric(ECA[,it])}

stns<-unique(ECA$StationNum)

x<-rast("../DATA/1.Spatial_data/regional/Canada/lulc_landuse_landcover/lulc1_ForestManagement/Canada_MFv2020.tif")

maskPrivate<-x==50
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  st_transform(st_crs(maskPrivate))

watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private$Canada_MFv2020)

# tm_shape(watersheds)+tm_polygons(col = "watersheds_Private.Canada_MFv2020")
stations<-read.csv("2.data/2.working/StationMetadata/stations_final.csv")

for(it in 1:nrow(stations)){
  
  ECA_x<-ECA%>%filter(StationNum%in%stations$ID[it])%>%
    pivot_longer(cols = ECA_5:ECA_60)
  ECA_x$name<-factor(ECA_x$name,levels = c("ECA_5","ECA_10","ECA_20","ECA_60"))
  ggplot(ECA_x,aes(x= year,y = value,col = name))+
    geom_line()+
    scale_color_brewer(name = "",
                         palette = "Spectral")+
    scale_y_continuous(name = "ECA (%)",labels = function(x){x*100})+
    ggtitle(paste0(stations$Station.Name[it],", ",stations$ID[it],"\nPrivate Forest = ",
                   round(watersheds$watersheds_Private.Canada_MFv2020[watersheds$StationNum==stations$ID[it]],3)*100,
                   "%"))+
    
    theme_bw()
  ggsave(filename = paste0("3.figures/ECA_figs/ECA_",stations$ID[it],".png"),width = 8,height= 6,dpi = 150)
}



library(tidyr)
ECA_x<-ECA%>%filter(StationNum%in%"08HD007")%>%
  pivot_longer(cols = c(ECA_5,ECA_20,ECA_60))
ECA_x$name<-factor(ECA_x$name,levels = c("ECA_5","ECA_20","ECA_60"),
                   labels = c("5 year","20 year","60 year"))

ggplot(ECA_x,aes(x= year,y = value,col = name))+
  geom_line(linewidth = 1)+
  scale_color_brewer(palette = "Set2",
                     labels = c("5 year","20 year","60 year"),
                     name = "ECA")+
  scale_y_continuous(name = "ECA (%)",labels = function(x){x*100})+
  ggtitle(paste0("SALMON RIVER ABOVE MEMEKAY RIVER, 08HD007","\nPrivate Forest = ",
                 round(watersheds$PrivateForestry[watersheds$StationNum=="08HD007"],3)*100,
                 "%"))+
  
  theme_bw()
ggsave(filename = paste0("3.figures/ECA_figs/ECA_SalmonRiver.png"),width = 8,height= 6,dpi = 150)






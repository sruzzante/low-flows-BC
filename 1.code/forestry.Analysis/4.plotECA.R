# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10


# This script plots the estimated equivalent clearcut area for each station over time
# To generate these data run /1.code/extractECA.R

library(ggplot2)
library(dplyr)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC")) #Set the working directory


ECA<-readRDS("2.data/2.working/ECA/ECA.rds")


stns<-unique(ECA$StationNum)


# tm_shape(watersheds)+tm_polygons(col = "watersheds_Private.Canada_MFv2020")
stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

for(it in 1:nrow(stations)){
  
  ECA_x<-ECA%>%filter(StationNum%in%stations$ID[it])%>%
    pivot_longer(cols = c(ECA_5:ECA_60,ECA_I_9,ECA_III_24))
  
  ECA_x$name<-factor(ECA_x$name,levels = c("ECA_I_9","ECA_III_24", "ECA_5","ECA_10","ECA_20","ECA_60"),
                     ordered = TRUE)
  
  ggplot(ECA_x,aes(x= year,y = value,col = name,linetype = name,linewidth = name))+
    geom_line()+
    scale_linetype_manual(name = "",values = c(1,1,2,2,2,2))+
    scale_linewidth_manual(name = "",values = c(1,1,0.5,0.5,0.5,0.5))+
    scale_color_manual(name = "",
                         values = c("black","grey", "#D7191C", "#FDAE61", "#ABDDA4", "#2B83BA"))+
    scale_y_continuous(name = "ECA (%)",labels = function(x){x*100})+
    ggtitle(paste0(stations$Station.Name[it],", ",stations$ID[it],"\nPrivate Forest = ",
                   round(stations$PrivateForestry[it],2)*100,
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






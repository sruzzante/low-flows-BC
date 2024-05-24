# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24

# This script creates Figure 1 in the manuscript (regime classification map)

closeAllConnections()
rm(list=ls())
graphics.off()

library(lubridate)
library(plyr)
library(dplyr)
library(stringr)
library(sf)
library(tmap)
library(reshape2)
library(ggplot2)
library(cowplot)
library(bcmaps)
library(scico)
library(terra)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory



bc <- (bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province
# First open all files in notepad++ and remove all zero-width no-break spaces

stations<-readRDS("2.data/2.working/StationMetadata/stations_final_2.RDS")
# stations_reg<-read.csv("2.data/2.working/StationMetadata/Stations_regimeClass.csv",fileEncoding = "UTF-8")
stations$minFlowDate<-as.Date(stations$minFlowDate,origin = "1970-01-01")
## Regime Classification
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")%>%
  st_transform("EPSG:3005")

watersheds<-inner_join(watersheds,stations[,c("ID","regime","minFlowDate","Lat","Lon","Area_km2")],by = c("ID"))


watersheds$minMonth<-month(ymd(watersheds$minFlowDate),label = TRUE)

watersheds$minMonth<-factor(watersheds$minMonth,levels= c("Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct"),
                            ordered = TRUE)

watersheds$regime<-factor(watersheds$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                          ordered = TRUE)

watersheds<-watersheds[order(watersheds$Area_km2,decreasing = TRUE),]
tm_shape(watersheds)+tm_polygons(col = "regime")
tm_shape(watersheds)+tm_polygons(col = "minMonth")

# stations_reg$ratio<-stations_reg$maxFreshetQ/stations_reg$minWinterQ
# ggplot(stations_reg,aes(x = ratio))+geom_histogram()+
#   scale_x_log10()

stations_sf<-st_as_sf(st_drop_geometry(watersheds),coords = c("Lon","Lat"),crs = "EPSG:4326")%>%
  st_transform(st_crs(watersheds))

stations_sf2<-st_buffer(stations_sf,dist = 9000)
st_geometry(stations_sf2)<-"geom"

nms_both<-names(watersheds)[names(watersheds)%in%names(stations_sf2)]

plot_sf<-rbind(watersheds[watersheds$Area_km2>200,nms_both],
               stations_sf2[stations_sf2$Area_km2<=200,nms_both])

plot_sf<-plot_sf[order(plot_sf$Area_km2,decreasing = TRUE),]
plot_sf<-st_simplify(plot_sf,dTolerance = 250)

for(it in 1:nrow(plot_sf)){
  plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),]))
}

bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")

# bc_nghbrs<-st_crop(bc_nghbrs,xmin = 275942.4,ymin = 367537.4,xmax = 1810000,ymax = 1680000)

brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1872409.8,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()

p1<-ggplot()+
  geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
  geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
  geom_sf(data = bc_nghbrs,fill="grey90")+
  geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
  geom_sf(data = plot_sf,col = "white",lwd  =0.75)+
  # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
  geom_sf(data = watersheds,aes(fill = minMonth))+
  geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
          linewidth = 0.2,inherit.aes = FALSE)+
  scale_fill_scico_d(palette = "roma",
                     name = "minimum\nflow\nmonth",
                     breaks = c("0","1",levels(watersheds$minMonth)),
                     direction = -1,
                     # drop = FALSE
  )+
  geom_sf(data = brdr,
          col = "black",
          fill = NA)+
  scale_x_continuous(expand = c(0,0),limits = c(260942.4,1872409.8))+
  scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
  theme_void()+
  theme(legend.position = c(0.87,0.75),
        plot.title = element_text(hjust = 0,vjust=-20),
        legend.title = element_text(face = "bold",hjust = 0.5,size = 8),
        legend.background = element_rect(fill = "grey85",colour = "grey50"),
        legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
        plot.background = element_rect(fill = "white",colour = "black"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(8,"pt"))

ggsave("3.figures/maps/minFlowMonth.png",p1,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)

p2<-ggplot()+
  geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
  geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
  geom_sf(data = bc_nghbrs,fill="grey90")+
  geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
  geom_sf(data = plot_sf,col = "white",lwd  =0.75)+
  # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
  geom_sf(data = plot_sf,aes(fill = regime))+
  geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
          linewidth = 0.2,inherit.aes = FALSE)+
  geom_sf(data = brdr,
          col = "black",
          fill = NA)+
  scale_x_continuous(expand = c(0,0),limits = c(260942.4,1872409.8))+
  scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
  scale_fill_manual(
    # palette = colorRampPalette(colors = c("#FCF4D9","#00B9D2","#005804"))(n) ,
    values = c("#005804","#599D7A","#B2E3F0","#FCF4D9"),
    name = "regime",
    breaks = c("Rainfall","Hybrid","Snowfall","Glacial"),
    labels= c("Rainfall (34)","Hybrid (111)","Snowmelt (48)","Glacial (37)"),
  )+
  # scale_fill_scico_d(
  #   palette = "navia",
  #   name = "regime",
  #   breaks = c("Rainfall","Hybrid","Snowfall","Glacial"),
  #   labels= c("Rainfall","Hybrid","Snowmelt","Glacial"),
  #   # values = c("#33A02C","#1F78B4","#A6CEE3")
  #   direction = 1,
  #   # drop = FALSE
  # )+
  
  theme_void()+
  theme(legend.position = c(0.85,0.85),
        plot.title = element_text(hjust = 0,vjust=-20),
        legend.title = element_text(face = "bold",hjust = 0.5,size = 8),
        legend.background = element_rect(fill = "white",colour = "grey50"),
        legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
        plot.background = element_rect(fill = "white",colour = "grey50"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(8,"pt"))
p2
ggsave("3.Figures/maps/flowRegimes.png",p2,width = 3.75,height = 3.225,
       bg = "white",dpi = 600)


ggsave("3.Figures/maps/flowRegimes.svg",p2,width = 3.75,height = 3.225,
       bg = "white",dpi = 600,
       device = "svg")

# make 4 inset graphs for flow regime map

streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal_2.rds")

m_labels<-c("J","F","M","A","M","J","J","A","S","O","N","D")

dte_formatter <- function(x) { 
  #formatter for axis labels: J,F,M,... etc 
  substr(format(x, "%b"),1,1) 
  
} 

stnsPlot<-c("08JE001","08HA010","09AA006","08NH006")
streamData_x<-data.frame()
dataSmooth2_x<-data.frame()
for(it_stn in 1:length(stnsPlot)){
  streamData<-streamDataAll[streamDataAll$ID==stnsPlot[it_stn],]
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$DayOfYear[streamData$DayOfYear==366]<-365
  streamData$DateFake<-as.Date(streamData$DayOfYear-1,origin = "1999-01-01")
  streamDataNA<-streamData[streamData$month==1&streamData$day==1,]
  streamDataNA$Discharge<-NA
  streamData<-rbind(streamData,streamDataNA)
  # streamData$Discharge7[streamData$Discharge7==0]
  
  # stat_smooth(data = streamData, x = streamData$DateFake,y = streamData$Discharge)
  
  streamData$Discharge30<-zoo::rollmean(streamData$Discharge,k = 30,fill = NA,align = "right")
  
  
  dataSmooth2<-streamData%>%group_by(DateFake,ID)%>%
    dplyr::summarize(Discharge = mean(Discharge30,na.rm = TRUE))
  streamData$YR<-streamData$year%>%as.factor()
  streamData_x<-rbind(streamData_x,streamData)
  
  dataSmooth2_x<-rbind(dataSmooth2_x,dataSmooth2)
  
}

streamData_x$regimeLbl<-factor(streamData_x$ID,
                               levels = c("08HA010","08NH006","08JE001","09AA006"),
                               labels = c("A (Rainfall)","B (Hybrid)","C (Snowmelt)","D (Glacial)"))
dataSmooth2_x$regimeLbl<-factor(dataSmooth2_x$ID,
                               levels = c("08HA010","08NH006","08JE001","09AA006"),
                               labels = c("A (Rainfall)","B (Hybrid)","C (Snowmelt)","D (Glacial)"))

p1<-ggplot(streamData_x,aes(x = DateFake,y = Discharge))+
  geom_line(linewidth= 0.15,col = "gray50",alpha = 0.2,aes(group = YR))+
  # geom_smooth(span= 0.25)+
  geom_line(data = dataSmooth2_x,aes(col = "Average Q30"),linewidth = 0.3)+
  scale_color_manual(name = NULL,values = "grey20")+
  
  
  scale_x_date(name = "Date",
               # date_minor_breaks = "10 year",
               limits = ymd(c(19990101,20000101)),
               expand = c(0,0),
               date_breaks = "1 month",
               
               # breaks  = seq(from=as.Date("1999-01-01"),to=as.Date("2000-01-01"),by="month"),
               labels = dte_formatter)+
  scale_y_log10(name = expression(Q~(m^3*s^-1)))+
  
  theme_bw()+
  theme(legend.position = "none",
        # legend.position = c(0.25,0.9),
        legend.background = element_rect(fill=alpha('white', 0)),
        text = element_text(size = 8),
        # axis.ticks = element_line(linewidth = 0.1
        #                           ),
        # axis.ticks.length = unit(.02, "cm"),
        axis.ticks = element_blank(),
        legend.key.size = unit(0.08,"cm"),
        panel.grid = element_line(linewidth = 0.1),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(margin = margin(t=-2,r=0,b=0,l=0)),
        axis.title.x = element_text(margin = margin(t=0.5,r=0,b=0,l=0)),
        # axis.title.x = element_blank(),
        axis.text.y = element_text(margin = margin(t=0,r=-2,b=0,l=0)),
        axis.title.y = element_text(margin = margin(t=-0,r=-1,b=0,l=0),size = 8),
        plot.margin = margin(t=0.5,r=0.5,b=0.5,l=0.5))+
  facet_wrap(ncol=1,facets = "regimeLbl",
             scales = "free_y",
             strip.position = "right",
             )
# p1
ggsave("3.Figures/maps/flowRegimes_4panels.svg",width = 1.75,height = 3.225,device = "svg")



# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-25

## This script compares runoff ratios ANUSPLIN vs ANUSPLIN-adjusted for 23 catchments


library(terra)
library(tmap)
library(dplyr)
library(sf)
library(lubridate)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")
stations$begin.year<-substr(stations$Dates,1,4)%>%as.numeric()

stations<-stations%>%
  # filter(begin.year<1950)%>%
  filter(perc_Gl<0.01)

streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")%>%
  filter(ID %in%stations$ID)%>%
  left_join(stations%>%select(ID,Area_km2))

weatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")%>%
  filter(ID %in%stations$ID)
weatherDataAdj<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly_adjpcp.rds")%>%
  filter(ID %in%stations$ID)%>%
  filter(NA_frac<0.2)

streamDataAll$wateryear

streamDataYrly<-streamDataAll%>%group_by(ID,wateryear,Area_km2)%>%
  summarize(DischargeYrly = mean(Discharge,na.rm = TRUE),
            n_noNA = sum(!is.na(Discharge)),
  )%>%
  mutate(daysInYear = days_in_month(ym(paste(wateryear,"/02")))+337)%>%
  filter(n_noNA>=(daysInYear-5))%>%
  mutate(DischargeYrly_mm = DischargeYrly*daysInYear*86400/(Area_km2*10^6)*1000)

weatherData$wateryear<-weatherData$Year+floor(weatherData$Month/10)
weatherDataAdj$wateryear<-weatherDataAdj$Year+floor(weatherDataAdj$Month/10)

weatherDataYrly<-
  weatherData%>%
  group_by(ID,wateryear)%>%
  summarize(Total.Precip..mm.=sum(Total.Precip..mm.))

weatherDataAdjYrly<-
  weatherDataAdj%>%
  group_by(ID,wateryear)%>%
  summarize(Total.Precip..mm.Adj=sum(pcp))%>%
  dplyr::filter(wateryear<=2015)

streamDataYrly2<-left_join(streamDataYrly,weatherDataYrly,by = c("ID","wateryear"))%>%
  left_join(weatherDataAdjYrly,by = c("ID","wateryear"))


streamDataYrly2$RR<-streamDataYrly2$DischargeYrly_mm/streamDataYrly2$Total.Precip..mm.
streamDataYrly2$RR_adj<-streamDataYrly2$DischargeYrly_mm/streamDataYrly2$Total.Precip..mm.Adj
# stns<-streamDataYrly2%>%
#   filter(wateryear<1950&!is.na(RR_adj))%>%
#   group_by(ID)%>%
#   summarise(N_pre1950 = n())
# stations<-filter(stations,ID %in%stns$ID)
library(ggplot2)
for(it in 1:nrow(stations)){
  print(ggplot(streamDataYrly2%>%filter(ID ==stations$ID[it]),
               aes(x= wateryear,y=RR))+
          scale_x_continuous(breaks = seq(1900,2020,10))+
          ggtitle(paste(stations$Station.Name[it],stations$ID[it]))+
          geom_point(aes(col = "Unadjusted"))+
          geom_point(aes(y = RR_adj,col = "Adjusted")))
  readline("press enter to continue")
}

bc<-bcmaps::bc_neighbours()%>%filter(name=="British Columbia")

stations_sf<-stations%>%
  st_as_sf(coords = c("Lon","Lat"),crs = "EPSG:4326")
stn_labels<-stations_sf%>%filter(ID%in%c("08MG001","08MH006","08LB038","08HA001","08HA003","08NH005"))%>%
  st_transform(st_crs(bc))

RR_pre1950<-streamDataYrly2%>%
  filter(wateryear<1950)%>%
  group_by(ID)%>%
  summarise(RR = mean(RR,na.rm = TRUE),
            RR_adj = mean(RR_adj,na.rm = TRUE))%>%
  filter(!is.na(RR_adj))
# stations_sf<-left_join(stations_sf,RR_pre1950)


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  inner_join(stations%>%select(ID,Area_km2))%>%
  arrange(-Area_km2)%>%
  st_transform(st_crs(bc))
watersheds<-watersheds%>%
  filter(Area_km2>=200)%>%
  rbind(watersheds%>%filter(Area_km2<200)%>%
          st_centroid()%>%
          st_buffer(dist = 10000))




watersheds2<-watersheds%>%inner_join(RR_pre1950)

tmap_mode("plot")

library(scico)
# scico_palette_show()
tm<-
  tm_shape(stn_labels)+tm_text(text  = "ID")+
  tm_shape(bc)+tm_borders(col = "grey")+
     tm_shape(watersheds2)+
tm_polygons(col = "RR",
              title = "Runoff Ratio\n(unadjusted P)\n1900-1949",
            lwd = 0.1,
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,Inf)
  )+
  tm_shape(stn_labels)+tm_text(text  = "ID")+
  tm_shape(stn_labels)+tm_dots(size = 1)+
  
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.show = FALSE,
            legend.frame = TRUE)
tm
tmap_save(tm = tm,"3.figures/Runoff_ratio_1900_1950_unadjusted.png",width = 4,dpi = 600)
tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR_adj",
              title = "Runoff Ratio\n(adjusted P)\n1900-1949",
              lwd = 0.1,
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm
tmap_save(tm = tm,"3.figures/Runoff_ratio_1900_1950_adjusted.png",width = 4,dpi = 600)


RR_post1950<-streamDataYrly2%>%
  filter(wateryear>=1950)%>%
  group_by(ID)%>%
  summarise(RR = mean(RR,na.rm = TRUE),
            RR_adj = mean(RR_adj,na.rm = TRUE))%>%
  filter(!is.na(RR_adj))%>%filter(ID%in%RR_pre1950$ID)

watersheds2<-watersheds%>%inner_join(RR_post1950)
tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR",
              title = "Runoff Ratio\n(unadjusted P)\n1950-2015",
              lwd = 0.1,
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm
tmap_save(tm = tm,"3.figures/Runoff_ratio_1950_2015_unadjusted.png",width = 4,dpi = 600)
tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR_adj",
              title = "Runoff Ratio\n(adjusted P)\n1950-2015",
              lwd = 0.1,
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm
tmap_save(tm = tm,"3.figures/Runoff_ratio_1950_2015_adjusted.png",
          width = 4,dpi = 600)


RR_ratio<-inner_join(RR_pre1950,RR_post1950,by = "ID",suffix = c(".pre1950",".post1950"))

RR_ratio$RR_ratio_unadj<-RR_ratio$RR.pre1950/RR_ratio$RR.post1950
RR_ratio$RR_ratio_adj<-RR_ratio$RR_adj.pre1950/RR_ratio$RR_adj.post1950
watersheds2<-watersheds%>%inner_join(RR_ratio)


tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR_ratio_unadj",
              title = "Radio of Runoff Ratio",
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(-Inf,0.5,0.75,0.9,1.1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm

tmap_save(tm = tm,"3.figures/Runoff_ratio_Ratio_pre_post1950_unadjusted.png",
          width = 4,dpi = 600)

tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR_ratio_adj",
              title = "Radio of Runoff Ratio",
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(-Inf,0.5,0.75,0.9,1.1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm

tmap_save(tm = tm,"3.figures/Runoff_ratio_Ratio_pre_post1950_adjusted.png",
          width = 4,dpi = 600)

RR_ratio_long<-
  RR_ratio%>%
  select(!c(RR_ratio_unadj,RR_ratio_adj))%>%
  tidyr::pivot_longer(cols = RR.pre1950:RR_adj.post1950)%>%
  mutate(dataset = stringr::str_split_i(name,pattern = "\\.",1),
         period = stringr::str_split_i(name,pattern = "\\.",2))%>%
  select(!name)%>%
  ungroup()%>%
  tidyr::pivot_wider(id_cols = c(ID,dataset),values_from = "value",names_from = "period")
RR_ratio_long$dataset<-factor(RR_ratio_long$dataset,levels = c("RR","RR_adj"),labels = c("Unadjusted","Adjusted"))
library(ggrepel)
p1<-ggplot(RR_ratio_long,aes(x = pre1950,y = post1950,col = dataset))+
  geom_point()+
  scale_colour_discrete(name = NULL)+
  geom_line(aes(group = ID),arrow = arrow(length = unit(0.1, "cm")),col = "grey")+
  geom_abline()+
  geom_label_repel(data = filter(RR_ratio_long,dataset == "Adjusted"& pre1950>1.5),
                   aes(label= ID),
                   color= "grey20",
                   seed=2,
                   nudge_x = 0.1,
                   min.segment.length = 0,
                   show.legend = FALSE,
                   alpha = 0.5,
                   max.overlaps = 50)+
  scale_x_continuous(limits = c(0,3.75),name = "Runoff ratio 1900-1949")+
  scale_y_continuous(limits = c(0,2),"Runoff ratio 1950-2015")+
  coord_fixed()+
  theme_bw()
p1
ggsave("3.figures/Runoff_ratio_comparison.png",width = 8,height = 3.5)


# all stations

RR_post1950<-streamDataYrly2%>%
  filter(wateryear>=1950)%>%
  group_by(ID)%>%
  summarise( n = sum(!is.na(RR_adj)),
             RR = exp(mean(log(RR),na.rm = TRUE)),
            RR_adj = exp(mean(log(RR_adj),na.rm = TRUE))
           )%>%
  filter(!is.na(RR_adj))%>%
  filter(n>=1)



watersheds2<-watersheds%>%inner_join(RR_post1950)
tm<-
  tm_shape(bc)+tm_borders(col = "grey")+
  tm_shape(watersheds2)+
  tm_polygons(col = "RR_adj",
              title = "Runoff Ratio\n(adjusted P)\n1950-2015",
              lwd = 0.1,
              # style = "cont",
              midpoint = 1,
              palette = scico(palette = "vik",n = 1000,direction = -1),
              breaks = c(0,0.25,0.5,0.75,1,1.25,1.5,Inf)
  )+
  tm_layout(legend.position = c("right","top"),
            legend.bg.color = "white",
            legend.frame = TRUE)
tm

tmap_save(tm = tm,"3.figures/Runoff_ratio_post1950_adjusted_all.png",
          width = 4,dpi = 600)

# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script combines the surface and groundater use estimates, and creates plots for each catchment

licenses<-st_read("2.data/2.working/WaterUse/LicensedUse.gpkg")
wells<-st_read("2.data/2.working/WaterUse/UnlicensedWellsUse.gpkg")

wells$endDate<-ymd(wells$DECOM_ST)

wells$endDate[is.na(wells$endDate)]<-ymd("20250101")
wells<-wells%>%filter(!is.na(wells$date))
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")


for(it_wtrshd in 1:length(watersheds$StationNum)){
  shp_wtrshd<-watersheds[it_wtrshd,]
  
  licenses_wtrshd<-st_intersection(licenses,shp_wtrshd)%>%filter(!is.na(YrlyDiversion))
  licensed_wtrshd_SW<-licenses_wtrshd%>%filter(PODSUBTYPE%in%"POD")
  licensed_wtrshd_GW<-licenses_wtrshd%>%filter(!PODSUBTYPE%in%"POD")
  
  wells_wtrshd<-st_intersection(wells,shp_wtrshd)%>%filter(!is.na(YrlyDiversion))
  # tm_shape(shp_wtrshd)+tm_borders()+
  #   tm_shape(licenses_wtrshd)+tm_dots(col = "black")
  
  
  Div<-data.frame(year = 1900:2023,
                  Annual_SW = NA,August_SW = NA,September_SW = NA,October_SW = NA, # licensed SW
                  Annual_GW_UL = NA,August_GW_UL = NA,September_GW_UL = NA, October_GW_UL = NA,#Unlicensed GW
                  Annual_GW_LC = NA,August_GW_LC = NA,September_GW_LC = NA, October_GW_LC = NA# Licenses GW
  )
  
  for(it in 1:length(Div$year)){
    Div$Annual_SW[it] <- sum(licensed_wtrshd_SW$YrlyDiversion[year(licensed_wtrshd_SW$date)<=Div$year[it]&
                                                               year(licensed_wtrshd_SW$endDate)>Div$year[it]])  
    Div$August_SW[it] <- sum(licensed_wtrshd_SW$AugDiversion[year(licensed_wtrshd_SW$date)<=Div$year[it]&
                                                               year(licensed_wtrshd_SW$endDate)>Div$year[it]])
    Div$September_SW[it] <- sum(licensed_wtrshd_SW$SepDiversion[year(licensed_wtrshd_SW$date)<=Div$year[it]&
                                                                  year(licensed_wtrshd_SW$endDate)>Div$year[it]])
    Div$October_SW[it] <- sum(licensed_wtrshd_SW$OctDiversion[year(licensed_wtrshd_SW$date)<=Div$year[it]&
                                                                year(licensed_wtrshd_SW$endDate)>Div$year[it]])
    
    Div$Annual_GW_LC[it] <- sum(licensed_wtrshd_GW$YrlyDiversion[year(licensed_wtrshd_GW$date)<=Div$year[it]&
                                                                  year(licensed_wtrshd_GW$endDate)>Div$year[it]])  
    Div$August_GW_LC[it] <- sum(licensed_wtrshd_GW$AugDiversion[year(licensed_wtrshd_GW$date)<=Div$year[it]&
                                                                  year(licensed_wtrshd_GW$endDate)>Div$year[it]])
    Div$September_GW_LC[it] <- sum(licensed_wtrshd_GW$SepDiversion[year(licensed_wtrshd_GW$date)<=Div$year[it]&
                                                                     year(licensed_wtrshd_GW$endDate)>Div$year[it]])
    Div$October_GW_LC[it] <- sum(licensed_wtrshd_GW$OctDiversion[year(licensed_wtrshd_GW$date)<=Div$year[it]&
                                                                   year(licensed_wtrshd_GW$endDate)>Div$year[it]])
    
    Div$Annual_GW_UL[it] <- sum(wells_wtrshd$YrlyDiversion[year(wells_wtrshd$date)<=Div$year[it]&
                                                            year(wells_wtrshd$endDate)>Div$year[it]])  
    Div$August_GW_UL[it] <- sum(wells_wtrshd$AugDiversion[year(wells_wtrshd$date)<=Div$year[it]&
                                                            year(wells_wtrshd$endDate)>Div$year[it]])
    Div$September_GW_UL[it] <- sum(wells_wtrshd$SepDiversion[year(wells_wtrshd$date)<=Div$year[it]&
                                                               year(wells_wtrshd$endDate)>Div$year[it]])
    Div$October_GW_UL[it] <- sum(wells_wtrshd$OctDiversion[year(wells_wtrshd$date)<=Div$year[it]&
                                                             year(wells_wtrshd$endDate)>Div$year[it]])
    
  }  
  
  
  datPlot1<-Div
  datPlot1$Annual_SW<-datPlot1$Annual_SW/(365.25*24*3600)
  datPlot1$Annual_GW_UL<-datPlot1$Annual_GW_UL/(365.25*24*3600)
  datPlot1$Annual_GW_LC<-datPlot1$Annual_GW_LC/(365.25*24*3600)
  datPlot1<-melt(datPlot1,id.vars = "year",measure.vars = c("Annual_SW","Annual_GW_UL","Annual_GW_LC") )
  datPlot1$variable<-factor(datPlot1$variable,levels = c("Annual_SW","Annual_GW_LC","Annual_GW_UL"),
                            labels=c("Licensed Surface Water","Licensed Groundwater","Unlicensed Groundwater"))
  p1<-ggplot(datPlot1)+
    geom_area(aes(x = year,y=value,fill = variable))+
    scale_fill_brewer(palette = "Set3",
                      name = NULL)+
    scale_y_continuous(name = "Water Diversion (cms)")+
    scale_x_continuous(limits = c(1900,2030),
                       breaks = seq(1900,2025,25))+
    theme_bw()+
    theme(legend.position = c(0.15,0.5),
          legend.background = element_blank())
  p1
  
  datPlot2<-Div
  datPlot2$Annual_SW<-datPlot2$Annual_SW/(365.25*24*3600)
  datPlot2$August_SW<-datPlot2$August_SW/(31*24*3600)
  datPlot2$September_SW<-datPlot2$September_SW/(30*24*3600)
  datPlot2$October_SW<-datPlot2$October_SW/(31*24*3600)
  
  
  datPlot2$Annual_GW_UL<-datPlot2$Annual_GW_UL/(365.25*24*3600)
  datPlot2$August_GW_UL<-datPlot2$August_GW_UL/(31*24*3600)
  datPlot2$September_GW_UL<-datPlot2$September_GW_UL/(30*24*3600)
  datPlot2$October_GW_UL<-datPlot2$October_GW_UL/(31*24*3600)
  
  
  datPlot2$Annual_GW_LC<-datPlot2$Annual_GW_LC/(365.25*24*3600)
  datPlot2$August_GW_LC<-datPlot2$August_GW_LC/(31*24*3600)
  datPlot2$September_GW_LC<-datPlot2$September_GW_LC/(30*24*3600)
  datPlot2$October_GW_LC<-datPlot2$October_GW_LC/(31*24*3600)
  
  datPlot2<-melt(datPlot2,id.vars = "year")
  datPlot2$source<-str_remove(datPlot2$variable,"August_|September_|October_|Annual_")%>%
    factor(levels =c("SW","GW_LC","GW_UL"),
           labels = c("Licensed\nSurface Water","Licensed\nGroundwater","Unlicensed\nGroundwater"))
  
  datPlot2$month<-str_remove(datPlot2$variable,"_SW|_GW_UL|_GW_LC")%>%
    factor(levels =c( "Annual","August","September","October"))
  
  p2<-ggplot(datPlot2,aes(year,value,color = month))+geom_line()+
    scale_y_continuous(name = "Water Diversion (cms)")+
    scale_color_manual(name = NULL,values = c("black","#1B9E77","#D95F02","#7570B3"))+
    ggtitle(watersheds$Station.Name[it_wtrshd])+
    facet_wrap(facets = "source",ncol = 1,scales = "free_y",
               strip.position = "right")+
    theme_bw()+
    theme(legend.position = c(0.1,0.85),
          legend.background = element_blank())
  p2
  
  p<-cowplot::plot_grid(p1,p2,ncol = 1,
                        rel_heights = c(1,2))
  p
  ggsave(paste0("3.Figures/WaterUse/WaterUse-",watersheds$NameNom[it_wtrshd],"-",watersheds$StationNum[it_wtrshd],".png"),p,
         width = 8,height = 6)
  
  Div$Total.cms<-(Div$Annual_SW+Div$Annual_GW_UL+Div$Annual_GW_LC)/(365.25*24*3600 )# convert to cms)
  Div$August.cms<-(Div$August_SW+Div$August_GW_LC+Div$August_GW_UL)/(31*24*3600)# convert to cms
  Div$September.cms<-(Div$September_SW+Div$September_GW_LC+Div$September_GW_UL)/(30*24*3600)# convert to cms
  Div$October.cms<-(Div$October_SW+Div$October_GW_LC+Div$October_GW_UL)/(31*24*3600)# convert to cms
  
  
  write.csv(Div,paste0("2.data/2.working/WaterUse/estimates/",watersheds$StationNum[it_wtrshd],".csv"),row.names = FALSE)
  
}

# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-01

# This script runs the trend analysis and creates figure 2 in the manuscript


closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(plyr)
library(dplyr)
library(terra)
library(sf)
library(tmap)
library(scico)
library(zyp)
library(stringr)
library(reshape2)
library(grid)
library(scales)
library(ggrepel)
library(bcmaps)
library(ggtext)
library(ggpattern)
library(cowplot)
library(modifiedmk)
library(nortest)
library(tidyr)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory


# streamData<-readRDS("2.data/2.working/Discharge/streamDataFinal_2.rds")
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")

stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)

streamDataMonthly<-left_join(streamDataMonthly,stations[,c("ID","month_bgn","month_end")])

streamDataYrly<-
  streamDataMonthly%>%
  # dplyr::filter(month%in%month_bgn:month_end)%>%
  dplyr::group_by(ID,year)%>%
  dplyr::summarize(minSumFlow7 = min(minMonFlow7[month%in%month_bgn:month_end]),
                   minJulFlow7 = (minMonFlow7[month==7]),
                   minAugFlow7 = (minMonFlow7[month==8]),
                   minSepFlow7 = (minMonFlow7[month==9]),
                   minOctFlow7 = (minMonFlow7[month==10]))



getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

MKFun<-function(minAugFlow,nYr){if(nYr>3){
  MK<-MannKendall(minAugFlow)
}else{MK<-MannKendall(rep(NA,4))}
  return(c(MK$tau[1],MK$sl[1]))
}

senFun<-function(minFlow,year){
  dat<-data.frame(minFlow = log(minFlow),
                  year = year)%>%
    filter(year>=1950)%>%
    filter(!is.na(minFlow))
  return(exp(zyp.sen(minFlow~year,data = dat)$coefficients[["year"]]*10)-1)
  
}


lmFun<-function(minFlow,year){
  dat<-data.frame(minFlow = minFlow,year = year)
  dat<-dat%>%
    filter(year>=1950)
  
  if(sum(!is.na(dat$minFlow))>2){
    x<-summary(lm(log(minFlow)~year,data = dat,na.action = "na.exclude"))
    return(c(exp(x$coefficients[2,1]*10)-1,x$coefficients[2,4]))
  } else {
    return(c(NA,NA))
  }
  
  
  
}
# streamDataYrly$

stns<-streamDataYrly%>%
  # filter(year>=1993)%>%
  group_by(ID)%>%
  dplyr::summarise(strtYr = min(year[!is.na(minSumFlow7)]),
                   endYr = max(year[!is.na(minSumFlow7)]),
                   nYr = sum(!is.na(minSumFlow7)),
                   # nFullYear = sum(numQnotNAN>350), # lowering this to 335 does not change # stations
                   # minFlowSummerN = sum(minFlowMonth%in%c(8,9,10))/n(),
                   # minFlowMonth = getmode(minFlowMonth),
                   # minSumFlowMonth = getmode(minSumFlowMonth),
                   # medianMinFlowDay = median(minSumFlowDay,na.rm = TRUE),
                   # MK = MannKendall(minAugFlow),
                   # MK.t = MKFun(minSumFlow7,nYr)[1],
                   # MK.p = MKFun(minSumFlow7,nYr)[2],
                   MK.p = mmkh3lag(log(minSumFlow7[!is.na(minSumFlow7)]))[2],
                   # MK.t.Aug = MKFun(minAugFlow7,nYr)[1],
                   MK.p.Jul = mmkh3lag(log(minJulFlow7[!is.na(minJulFlow7)]))[2],
                   
                   MK.p.Aug = mmkh3lag(log(minAugFlow7[!is.na(minAugFlow7)]))[2],
                   # # MK.t.Sep = MKFun(minSepFlow7,nYr)[1],
                   MK.p.Sep = mmkh3lag(log(minSepFlow7[!is.na(minSepFlow7)]))[2],
                   # # MK.t.Oct = MKFun(minOctFlow7,nYr)[1],
                   MK.p.Oct = mmkh3lag(log(minOctFlow7[!is.na(minOctFlow7)]))[2],
                   slope_sen = senFun(minSumFlow7,year),
                   slope_sen.Jul = senFun(minJulFlow7,year),
                   slope_sen.Aug = senFun(minAugFlow7,year),
                   slope_sen.Sep = senFun(minSepFlow7,year),
                   slope_sen.Oct = senFun(minOctFlow7,year),
                   
                   nJul = sum(!is.na(minJulFlow7)),
                   nAug = sum(!is.na(minAugFlow7)),
                   nSep = sum(!is.na(minSepFlow7)),
                   nOct = sum(!is.na(minOctFlow7)),
                   
                   lm.t = lmFun(minSumFlow7,year)[1],
                   lm.p = lmFun(minSumFlow7,year)[2],
                   # lm.t.Aug = lmFun(minAugFlow7,year)[1],
                   # lm.p.Aug = lmFun(minAugFlow7,year)[2],
                   # lm.t.Sep = lmFun(minSepFlow7,year)[1],
                   # lm.p.Sep = lmFun(minSepFlow7,year)[2],
                   # lm.t.Oct = lmFun(minOctFlow7,year)[1],
                   # lm.p.Oct = lmFun(minOctFlow7,year)[2],
                   ShapiroWilk.log = shapiro.test(log(minSumFlow7[!is.na(minSumFlow7)]))$p.value,
                   ShapiroWilk = shapiro.test((minSumFlow7[!is.na(minSumFlow7)]))$p.value,
                   ad.test.log = nortest::ad.test(log(minSumFlow7[!is.na(minSumFlow7)]))$p.value,
                   ad.test = nortest::ad.test((minSumFlow7[!is.na(minSumFlow7)]))$p.value
                   
  )


ggplot()

stns$MK.p.Oct[stns$nOct<20]<-NA
stns$slope_sen.Oct[stns$nOct<20]<-NA

sum(stns$ShapiroWilk<0.05)
sum(stns$ShapiroWilk.log<0.05)
sum(stns$ad.test<0.05)
sum(stns$ad.test.log<0.05)

sum(stns$MK.p<0.05)

# sum(stns$MK.p.2<0.05)


stns$MK.p.Oct[stns$ID=="08FB007"]<-MannKendall(streamDataYrly$minOctFlow7[streamDataYrly$ID=="08FB007"&
                                                                            !is.na(streamDataYrly$minOctFlow7)])[2]$sl



sum(stns$lm.t<0)
sum(stns$slope_sen<0)

sum(stns$MK.p<0.05& stns$slope_sen<0)
sum(stns$MK.p<0.05& stns$slope_sen>0)

# sum(stns$MK.p.2<0.05)


# x<-left_join(stns,station_metadata[,c("ID","regime","perc_Gl")])
# x$regime[x$perc_Gl>0.05]<-"Glacial"
# summary(factor(x$regime
#                ))
# x%>%group_by(regime)%>%
#   summarize(decrease = sum(slope_sen<0),
#             increase = sum(slope_sen>0),
#             sigDecrease = sum(slope_sen<0& MK.p<0.05),
#             sigIncrease = sum(slope_sen>0& MK.p<0.05))

# stnsAll$MK.t_select<-NA
# stnsAll$MK.t_select[stnsAll$minSumFlowMonth==8]<-stnsAll$MK.t.Aug[stnsAll$minSumFlowMonth==8]
# stnsAll$MK.t_select[stnsAll$minSumFlowMonth==9]<-stnsAll$MK.t.Sep[stnsAll$minSumFlowMonth==9]
# stnsAll$MK.t_select[stnsAll$minSumFlowMonth==10]<-stnsAll$MK.t.Oct[stnsAll$minSumFlowMonth==10]



stns<-left_join(stns,stations%>%dplyr::select(ID,Lat,Lon,regime,Area_km2))



stns%>%
  # group_by(regime)%>%
  dplyr::summarize(negSlope = sum(slope_sen<0),
                   sigNegSlope = sum(slope_sen<0&MK.p<0.05),
                   posSlope = sum(slope_sen>=0),
                   sigPosSlope = sum(slope_sen>0&MK.p<0.05))

stns%>%
  group_by(regime)%>%
  dplyr::summarize(negSlope = sum(slope_sen<0),
                   sigNegSlope = sum(slope_sen<0&MK.p<0.05),
                   posSlope = sum(slope_sen>=0),
                   sigPosSlope = sum(slope_sen>0&MK.p<0.05,na.rm = TRUE),
                   n=sum(!is.na(slope_sen)))



stns%>%
  select(ID, regime,MK.p:MK.p.Oct,slope_sen:slope_sen.Oct)%>%
  pivot_longer(cols = MK.p:slope_sen.Oct)%>%
  mutate(p.val = str_detect(name,"slope_sen")%>%plyr::mapvalues(from = c(TRUE,FALSE),to = c("slope_sen","MK.p")),
         month = str_remove(name,"MK.p|slope_sen")%>%factor(levels = c("",".Jul",".Aug",".Sep",".Oct"),
                                                              labels = c("Overall","Jul","Aug","Sep","Oct")))%>%
  pivot_wider(id_cols = c(ID,regime,month),
              values_from = value,
              names_from = p.val
              )%>%
  group_by( month)%>%
  dplyr::summarize(negSlope = sum(slope_sen<0,na.rm = TRUE),
                   sigNegSlope = sum(slope_sen<0&MK.p<0.05,na.rm = TRUE),
                   posSlope = sum(slope_sen>=0,na.rm = TRUE),
                   sigPosSlope = sum(slope_sen>0&MK.p<0.05,na.rm = TRUE),
                   n=sum(!is.na(slope_sen)))


stns%>%
  select(ID, regime,MK.p:MK.p.Oct,slope_sen:slope_sen.Oct)%>%
  pivot_longer(cols = MK.p:slope_sen.Oct)%>%
  mutate(p.val = str_detect(name,"slope_sen")%>%plyr::mapvalues(from = c(TRUE,FALSE),to = c("slope_sen","MK.p")),
         month = str_remove(name,"MK.p|slope_sen")%>%factor(levels = c("",".Jul",".Aug",".Sep",".Oct"),
                                                            labels = c("Overall","Jul","Aug","Sep","Oct")))%>%
  pivot_wider(id_cols = c(ID,regime,month),
              values_from = value,
              names_from = p.val
  )%>%
  group_by(regime, month)%>%
  dplyr::summarize(negSlope = sum(slope_sen<0,na.rm = TRUE),
                   sigNegSlope = sum(slope_sen<0&MK.p<0.05,na.rm = TRUE),
                   posSlope = sum(slope_sen>=0,na.rm = TRUE),
                   sigPosSlope = sum(slope_sen>0&MK.p<0.05,na.rm = TRUE),
                   n=sum(!is.na(slope_sen)))




watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
  st_transform("EPSG:3005")



watersheds<-inner_join(watersheds,stns,by = c("ID"))
# watersheds$Area_km2<-as.numeric(st_area(watersheds)/10^6)
watersheds<-watersheds[order(watersheds$Area_km2,decreasing = TRUE),]





## ggplot maps


bc <- (bc_neighbours()) #Get shp of BC bounds
bc <- bc[which(bc$name == "British Columbia" ),] #Extract just the BC province


stations_sf<-st_as_sf(stns,coords = c("Lon","Lat"),crs = "EPSG:4326")%>%
  st_transform(st_crs(bc))

watersheds<-watersheds%>%
  st_transform(st_crs(bc))




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

plot_sf$MK.p.Oct[is.na(plot_sf$MK.p.Oct)]<-1




# Overall:
sum(watersheds$lm.t<0)
sum(watersheds$lm.t<0& watersheds$lm.p<0.05)
sum(watersheds$lm.t>0& watersheds$lm.p<0.05)

sum(watersheds$lm.t< -.1& watersheds$lm.p<0.05)
sum(watersheds$lm.t< -.1)
watersheds$sigLargeDecreas<-watersheds$slope_sen< -.1& watersheds$MK.p<0.05
tmap_mode("view")
tm_shape(watersheds)+tm_polygons(col = "sigLargeDecreas",breaks = c(FALSE,TRUE))
# 
# # August:
# sum(watersheds$lm.t.Aug<0)
# sum(watersheds$lm.t.Aug<0& watersheds$lm.p.Aug<0.05)
# sum(watersheds$lm.t.Aug>0& watersheds$lm.p.Aug<0.05)
# # September:
# sum(watersheds$lm.t.Sep<0)
# sum(watersheds$lm.t.Sep<0& watersheds$lm.p.Sep<0.05)
# sum(watersheds$lm.t.Sep>0& watersheds$lm.p.Sep<0.05)
# 
# # October:
# sum(watersheds$lm.t.Oct<0)
# sum(watersheds$lm.t.Oct<0& watersheds$lm.p.Oct<0.05)
# sum(watersheds$lm.t.Oct>0& watersheds$lm.p.Oct<0.05)

#### Plotting
plot_sf_buffer<-plot_sf%>%
  st_union()%>%
  st_buffer(dist = 3000)





#######


bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")


brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1872409.8,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()

createPanelPlot_trend<-function(bc,plot_sf,varName,p_varName,labName,include.legend,include.inset){
  
  
  
  bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
  bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
  cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
  bc_nghbrs<-bc_neighbours()%>%
    filter(name!="British Columbia")
  
  
  brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1872409.8,ymax = 1750251.6),
                crs= st_crs(bc_nghbrs))%>%
    
    st_as_sfc()
  # plot_sf$prctDelta10<-exp(10*st_drop_geometry(plot_sf)[,varName])-1
  varName <- sym(varName)
  
  p_varName <- sym(p_varName)
  # watersheds_H = hatched.SpatialPolygons(plot_sf[abs(st_drop_geometry(plot_sf[,p_varName]))>0.05,], density=0.0001, angle=45)
  # st_crs(watersheds_H) = st_crs("EPSG:3005")
  
  fctrs<-cut(seq(-0.2,0.2,0.0001),breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE)
  # fctrLvls<-c("drying",levels(fctrs),"wetting")
  fctrLvls<-levels(fctrs)
  # fctsLabs<- c("<span style = 'color:#601200;font-weight:bold'>**drying**</span>",
  #              "Less than -20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20","More than 20",
  #              "*wetting*")
  fctsLabs<- c("Less than -20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20","More than 20")
  
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = bc_nghbrs,fill="grey90")+
    geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
    geom_sf(data = plot_sf,col = "white",lwd  =0.75)+
    # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+

    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+

    
    geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
    
    geom_sf_pattern(data = plot_sf,aes(fill = cut(!!varName,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                                       # col = cut(!!varName,breaks = c(-Inf,-0.02,-0.01,-0.005,-0.002,0,0.002,0.005,0.01,0.02,Inf),include.lowest = TRUE),
                                       pattern = !!p_varName<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey50",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600)+
    geom_sf(data = brdr,
            col = "black",
            fill = NA)+
    scale_pattern_manual(breaks = c(TRUE,FALSE),
                         values = c("none","stripe"),
                         labels = c("p<0.05","p\u22650.05"),
                         name = "statistical\nsignificance")+
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    # geom_sf(data = watersheds_H,aes(linetype = "1"),color = "grey80",size = 0.5)+
    scale_fill_manual(
      limits = fctrLvls%>%rev(),
      labels = fctsLabs%>%rev(),
      values = RColorBrewer::brewer.pal(10,"RdBu")%>%rev(),
      # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
      # values = brewer_pal(palette = "BlRd")
      # values =scico(10,palette = "vik",direction = -1)%>%rev(),
      drop = FALSE)+
    scale_color_manual(
      limits = fctrLvls%>%rev(),
      labels = fctsLabs%>%rev(),
      # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
      values =rep("grey50",10),
      drop = FALSE)+
    # scale_pattern_manual(values = c("solid","blank"),
    #                       breaks = c("1","2"),
    #                       limits = c("1","2"),
    #                       labels = c("p\u22650.05","p<0.05"),
    #                       drop = FALSE,
    #                       name = "statistical\nsignificance")+
    
    
    labs(fill = " % change in low flow  \n(per decade)  \n<span style = 'color:#601200;font-weight:bold'> *drying*</span>\u2190|\u2192 <span style = 'color:#001260;font-weight:bold'>*wetting*</span>",
         col = " % change in low flow  \n(per decade)  \n<span style = 'color:#601200;font-weight:bold'> *drying*</span>\u2190|\u2192 <span style = 'color:#001260;font-weight:bold'>*wetting*</span>")+
    # annotate("text",  x=1066676, y = 1710000, label = labName, vjust=0, hjust=0.5,
    #          fontface = "bold",size =2.2)+
    
    # annotate("text",  x=1650000, y = 1570000, label = "drying", vjust=0, hjust=0,
    #          fontface = "bold.italic",size = 3)+  
    # annotate("text",  x=1650000, y = 1100000, label = "wetting", vjust=0, hjust=0,
    #                                                        fontface = "bold.italic",size = 3)+
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    scale_x_continuous(expand = c(0,0),limits = c(260942.4,1872409.8))+
    scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
    
    theme(legend.position = c(0.85,0.65),
          plot.title = element_text(hjust = 0,vjust=-20),
          # legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
          
          legend.background = element_rect(fill = "white",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "white",colour = "grey50"),
          legend.text = element_markdown(size = 6,hjust = 0),
          legend.key.size = unit(6,"pt"),
          legend.box.just = "right"
          # legend.key = element_rect()
    )+
    guides(fill = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 6,angle = 0)))
  
  if(labName!=""){
    p1<-p1+geom_label(data=data.frame(x = 1066676,y = 1640866,lbl = labName),
                      aes(x = x,y=y,label = lbl),inherit.aes = FALSE,size = 5,
                      label.size = NA)
  }
  if(include.legend == FALSE){
    p1<-p1+theme(legend.position = "none")
  }
  
  # ggsave("Figures/TrendMapTemp.png",dpi =300,p1,width = 7.5/2,height = 9.67/3)
  if(include.inset == TRUE){
    fctsLabs2<- c("<-20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20",">20")
  
  p_inset<-ggplot(plot_sf,aes(x = cut(!!varName,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                              fill = cut(!!varName,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                              pattern = !!p_varName<0.05))+
    geom_bar_pattern(pattern_colour   = NA,pattern_angle = 45,colour = "black",
                     pattern_fill = "grey80",
                     pattern_res = 600,
                     pattern_density = 0.3,
                     size = 0.1,
                     linewidth = 0.1,
                     pattern_spacing = 0.04,)+
    scale_pattern_manual(values = c("none","stripe"),
                         labels = c("p<0.05","p\u22650.05"),
                         breaks = c(TRUE,FALSE))+
    scale_x_discrete(labels = fctsLabs2,name = "% change in low flow (per decade)")+
    scale_y_continuous(name = "# catchments")+
    scale_fill_manual( name = "trend",
                       limits = fctrLvls%>%rev(),
                       labels = fctsLabs2%>%rev(),
                       # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                       # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                       values = RColorBrewer::brewer.pal(10,"RdBu")%>%rev(),
                       drop = FALSE)+
    theme_bw()+
    theme(legend.position = "none",
          plot.background = NULL,
          axis.text = element_text(size = 5),
          axis.text.x = element_text(angle = 45,hjust=1),
          axis.title = element_text(size = 6),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
  # p_inset<-ggplot(plot_sf,aes(x = MK.t,fill =lm.p<0.05, pattern = lm.p<0.05))+geom_histogram_pattern()
  # p_inset
  
  p1 <-
    ggdraw() +
    draw_plot(p1) +
    draw_plot(p_inset, x = 0, y = 0, width = .37, height = .37)
  }
  
  
  # return(p1)
  p1
}

p1<-createPanelPlot_trend(bc,plot_sf,"slope_sen","MK.p","",include.legend = FALSE,include.inset = TRUE)
ggsave("3.Figures/TrendMaps_overall_MK2.png",bg = "white",dpi =600,p1,width = 7.5/2,height = 9.67/3)

p1<-createPanelPlot_trend(bc,plot_sf,"slope_sen","MK.p","",include.legend = FALSE,include.inset = FALSE)
ggsave("3.Figures/TrendMaps_overall_MK.svg",bg = "white",dpi =600,p1,width = 7.5/2,height = 9.67/3,
       device = "svg")


p1<-createPanelPlot_trend(bc,plot_sf,"slope_sen.Jul","MK.p.Jul","July",include.legend = FALSE,include.inset = TRUE)
ggsave("3.Figures/TrendMaps_07July_MK.png",bg = "white",dpi =600,p1,width = 7.5/2,height = 9.67/3)

p2<-createPanelPlot_trend(bc,plot_sf,"slope_sen.Aug","MK.p.Aug","August",include.legend = TRUE,include.inset = TRUE)
ggsave("3.Figures/TrendMaps_08August_MK.png",bg = "white",dpi =600,plot = p2,width = 7.5/2,height = 9.67/3)

p3<-createPanelPlot_trend(bc,plot_sf,"slope_sen.Sep","MK.p.Sep","September",include.legend = FALSE,include.inset = TRUE)
ggsave("3.Figures/TrendMaps_09September_MK.png",bg = "white",dpi =600,p3,width = 7.5/2,height = 9.67/3)
p4<-createPanelPlot_trend(bc,plot_sf,"slope_sen.Oct","MK.p.Oct","October",include.legend = FALSE,include.inset = TRUE)
ggsave("3.Figures/TrendMaps_10October_MK.png",bg = "white",dpi =600,p4,width = 7.5/2,height = 9.67/3)

p_leg<-ggplot()+geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
theme_void()
ggsave("3.Figures/TrendMaps_legendOutline.svg",bg = "white",dpi =600,p_leg,width = 0.5,height = 0.5,
       device = "svg")


## Histograms
fctsLabs2<- c("<-20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20",">20")
fctrs<-cut(seq(-0.2,0.2,0.0001),breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE)
# fctrLvls<-c("drying",levels(fctrs),"wetting")
fctrLvls<-levels(fctrs)
plot_sf$regime<-factor(plot_sf$regime,
                       levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                       labels =  c("Rainfall","Hybrid","Snowmelt","Glacial")
)

p_hist<-ggplot(plot_sf,aes(x = cut(slope_sen,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                           fill = cut(slope_sen,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                           pattern = MK.p<0.05))+
  geom_bar_pattern(pattern_colour   = NA,pattern_angle = 45,colour = "black",
                   pattern_fill = "grey80",
                   pattern_res = 600,
                   pattern_density = 0.3,
                   size = 0.1,
                   linewidth = 0.1,
                   pattern_spacing = 0.04,)+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE))+
  scale_x_discrete(labels = fctsLabs2,name = "% change in low flow (per decade)")+
  scale_y_continuous(name = "# catchments")+
  scale_fill_manual( name = "trend",
                     limits = fctrLvls%>%rev(),
                     labels = fctsLabs2%>%rev(),
                     # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                     values = RColorBrewer::brewer.pal(10,"RdBu")%>%rev(),
                     drop = FALSE)+
  theme_bw()+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             scales = "free_y")+
  theme(legend.position = "none",
        plot.background = NULL,
        # axis.text = element_text(size = 5),
        axis.text.x = element_text(angle = 45,hjust=1),
        # axis.title = element_text(size = 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(size = 7),
        text = element_text(size = 7)) 
p_hist

ggsave(plot = p_hist,filename = "3.Figures/TrendMaps_overall_hists.png", width = 7.5/3,height = 9.67/3,
       dpi = 600)
ggsave(plot = p_hist,filename = "3.Figures/TrendMaps_overall_hists.svg", width = 7.5/4,height = 9.67/3,device = "svg",
       dpi = 600)





# example - Koksilah
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

ggplot(streamDataYrly%>%filter(ID=="08HA003"&year>1950),aes(x = year,y = minSumFlow7))+
  geom_point()+
  geom_smooth(method = sen ,se = FALSE,aes(col = "Trend"))+
  geom_hline(aes(yintercept = 0.18,col = "Interim Critical\nEnvironmental\nFlow Threshold"))+
  scale_color_manual(values = c("red","grey50"))+
  scale_y_continuous(name = expression(Low~Flow~(m^3/s)))+
  scale_x_continuous(limits = c(1956,2022))+
  theme_bw()+
  theme(legend.title = element_blank(),
        legend.position = c(0.8,0.8))
ggsave("3.figures/Koksilah_trend.png",width = 4,height = 4,dpi = 600)

############################zyp.sen()################

p1<-createPanelPlot_trend(bc,plot_sf,"lm.t","lm.p","",include.legend = TRUE)
ggsave("Figures/TrendMaps_overall.svg",bg = "white",dpi =1200,p1,width = 7.5/2,height = 9.67/3,device = "svg")
p2<-createPanelPlot_trend(bc,plot_sf,"lm.t.Aug","lm.p.Aug","August Low Flow Trend\n(linear regression)",include.legend = TRUE)
ggsave("Figures/TrendMaps_August.svg",bg = "white",dpi =600,p2,width = 7.5/2,height = 9.67/3,device = "svg")
p3<-createPanelPlot_trend(bc,plot_sf,"lm.t.Sep","lm.p.Sep","September Low Flow Trend\n(linear regression)",include.legend = TRUE)
ggsave("Figures/TrendMaps_September.svg",bg = "white",dpi =600,p3,width = 7.5/2,height = 9.67/3,device = "svg")
p4<-createPanelPlot_trend(bc,plot_sf,"lm.t.Oct","lm.p.Oct","October Low Flow Trend\n(linear regression)",include.legend = TRUE)
ggsave("Figures/TrendMaps_October.svg",bg = "white",dpi =600,p4,width = 7.5/2,height = 9.67/3,device = "svg")

p1<-createPanelPlot_trend(bc,plot_sf,"lm.t","lm.p","Overall Summer Low Flow Trend\n(linear regression)",include.legend = TRUE)

p_trend<-cowplot::plot_grid(p2,p3,p4,p1,nrow = 2)
ggsave("Figures/TrendMaps_panels.png",bg = "white",dpi =600,p_trend,width = 7.5,height = 6.45,scale = .99)


## Mann-Kendall with Sen Slope
sum(watersheds$slope_sen<0)
sum(watersheds$slope_sen<0& watersheds$MK.p<0.05)
sum(watersheds$slope_sen>0& watersheds$MK.p<0.05)

watersheds%>%st_drop_geometry()%>%
  group_by(regime)%>%
  summarize(decrease = sum(slope_sen<0),
            increase = sum(slope_sen>0),
            sigDecrease = sum(slope_sen<0& MK.p<0.05),
            sigIncrease = sum(slope_sen>0& MK.p<0.05))

# August:
sum(watersheds$slope_sen.Aug<0)
sum(watersheds$slope_sen.Aug<0& watersheds$MK.p.Aug<0.05)
sum(watersheds$slope_sen.Aug>0& watersheds$MK.p.Aug<0.05)


watersheds%>%st_drop_geometry()%>%
  group_by(regime)%>%
  summarize(decrease = sum(slope_sen.Aug<0),
            increase = sum(slope_sen.Aug>0),
            sigDecrease = sum(slope_sen.Aug<0& MK.p.Aug<0.05),
            sigIncrease = sum(slope_sen.Aug>0& MK.p.Aug<0.05))

# September:
sum(watersheds$slope_sen.Sep<0)
sum(watersheds$slope_sen.Sep<0& watersheds$MK.p.Sep<0.05)
sum(watersheds$slope_sen.Sep>0& watersheds$MK.p.Sep<0.05)

watersheds%>%st_drop_geometry()%>%
  group_by(regime)%>%
  summarize(decrease = sum(slope_sen.Sep<0),
            increase = sum(slope_sen.Sep>0),
            sigDecrease = sum(slope_sen.Sep<0& MK.p.Sep<0.05),
            sigIncrease = sum(slope_sen.Sep>0& MK.p.Sep<0.05))

# October:
sum(watersheds$slope_sen.Oct<0)
sum(watersheds$slope_sen.Oct<0& watersheds$MK.p.Oct<0.05)
sum(watersheds$slope_sen.Oct>0& watersheds$MK.p.Oct<0.05,na.rm = TRUE)


watersheds%>%st_drop_geometry()%>%
  group_by(regime)%>%
  summarize(decrease = sum(slope_sen.Oct<0,na.rm = TRUE),
            increase = sum(slope_sen.Oct>0,na.rm = TRUE),
            sigDecrease = sum(slope_sen.Oct<0& MK.p.Oct<0.05,na.rm = TRUE),
            sigIncrease = sum(slope_sen.Oct>0& MK.p.Oct<0.05,na.rm = TRUE))

p1<-createPanelPlot_trend(bc,plot_sf,"slope_sen","MK.p","Overall Summer Low Flow Trend\n(Mann-Kendall & Sen's Slope)",include.legend = TRUE)

p_trend<-cowplot::plot_grid(p2,p3,p4,p1,nrow = 2)
ggsave("Figures/TrendMaps_panels_MK.png",bg = "white",dpi =600,p_trend,width = 7.5,height = 6.45,scale = .99)




#Hmmm ... the two significant increases in low flow for hybrid regimes are right 
# next to the largest and the second largest open-pit copper mines in Canada...




##


##########################

stations<-filter(stations,substr(ID,1,3)=="08H")
watersheds<-filter(watersheds,substr(StationNum,1,3)=="08H")


# coastline<-st_read("../DATA/1.Spatial_data/regional/BC/zobo_zones_boundaries_admin.units_coastline/regional/BC/zobo_zones_boundaries_admin.units_coastline/zobo1_coastline_VancouverIsland/BCGW_7113060B_1689774254330_3364/FWA_COASTLINES_SP/FWCSTLNSSP_line.shp")
bc<-bcmaps::bc_neighbours()
bc<-bc[5,]
bc_parts<-st_cast(bc,"POLYGON")
VanIsle<-bc_parts[77:85,]

fctrs<-cut(seq(-0.2,0.2,0.0001),breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE)
# fctrLvls<-c("drying",levels(fctrs),"wetting")
fctrLvls<-levels(fctrs)

plot_sf<-filter(plot_sf,plot_sf$StationNum%in%stations$ID)

fctsLabs<- c("<-20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20",">20")

labName= "overall"
varName <- sym("sen_slope")

p_varName <- sym("MK.p")
# watersheds_H = hatched.SpatialPolygons(plot_sf[abs(st_drop_geometry(plot_sf[,p_varName]))>0.05,], density=0.0001, angle=45)
# st_crs(watersheds_H) = st_crs("EPSG:3005")

fctrs<-cut(seq(-0.2,0.2,0.0001),breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE)
# fctrLvls<-c("drying",levels(fctrs),"wetting")
fctrLvls<-levels(fctrs)
# fctsLabs<- c("<span style = 'color:#601200;font-weight:bold'>**drying**</span>",
#              "Less than -20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20","More than 20",
#              "*wetting*")
fctsLabs<- c("Less than -20","-20 to -10","-10 to -5","-5 to -2","-2 to 0","0 to 2","2 to 5","5 to 10","10 to 20","More than 20")

stations_sf<-filter(stations_sf,StationNum%in%stations$ID)
stations_sf<-cbind(stations_sf,st_coordinates(stations_sf))
watersheds$Short.Name<-c("NIMPKISH RIVER","SAN JUAN RIVER",
                         "SALMON RIVER","TSITIKA RIVER","CHEMAINUS RIVER",
                         "OYSTER RIVER","SALMON RIVER","KOKSILAH RIVER",
                         "CRUICKSHANK RIVER","UCONA RIVER","SARITA RIVER",
                         "TSABLE RIVER","BROWNS RIVER","SAN JOSEF RIVER","KLASKISH RIVER",
                         "HARRIS CREEK","PUGH CREEK","BINGS CREEK","SIMPSON CREEK",
                         "CREST LAKE","ZEBALLOS RIVER","COTTONWOOD CREEK","RENFREW CREEK",
                         "CARNATION CREEK","GARBAGE CREEK")
stations_sf<-left_join(stations_sf,(watersheds[,c("StationNum","Short.Name")])%>%st_drop_geometry())

fontFam = "Sans"

p1<-ggplot()+
  
  # geom_sf(data = bc_west,col = "grey25",fill = "grey90",inherit.aes = FALSE)+
  geom_sf(data = VanIsle,fill = "white",col = "grey25",
          
          inherit.aes = FALSE)+
  
  # geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
  
  geom_sf_pattern(data = watersheds,aes(fill = cut(watersheds$slope_sen,breaks = c(-Inf,-0.2,-0.1,-0.05,-0.02,0,0.02,0.05,0.1,0.2,Inf),include.lowest = TRUE),
                                     # col = cut(!!varName,breaks = c(-Inf,-0.02,-0.01,-0.005,-0.002,0,0.002,0.005,0.01,0.02,Inf),include.lowest = TRUE),
                                     pattern = watersheds$MK.p<0.05),
                  pattern_colour   = NA,
                  pattern_angle = 45,
                  colour = "grey50",
                  pattern_fill = "grey80",
                  # pattern_frequency = 10,
                  pattern_density = 0.3,
                  size = 0.1,
                  linewidth = 0.1,
                  pattern_spacing = 0.007,
                  pattern_res = 600)+
  scale_pattern_manual(breaks = c(TRUE,FALSE),
                       values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       name = "statistical\nsignificance")+

  # geom_sf(data = watersheds_H,aes(linetype = "1"),color = "grey80",size = 0.5)+
  scale_fill_manual(
    limits = fctrLvls%>%rev(),
    labels = fctsLabs%>%rev(),
    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
    values =scico(10,palette = "vik",direction = -1)%>%rev(),
    drop = FALSE)+
  scale_color_manual(
    limits = fctrLvls%>%rev(),
    labels = fctsLabs%>%rev(),
    # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
    values =rep("grey50",10),
    drop = FALSE)+
  geom_label_repel(seed = 10,
                   data = stations_sf,aes(x =X, y = Y, label = Short.Name),
                   size = 3,
                   alpha = 0.7,
                   col = "black",
                   fill = "white",
                   label.size = NA,
                   max.overlaps = Inf,
                   label.padding = 0.1,
                   min.segment.length = unit(0,"inches"),
                   family = fontFam,
                   fontface = "italic"
                   # arrow = arrow(angle = 30,length = unit(0.02, "inches"),type = "closed",ends = "last")
  )+
  geom_label_repel(seed = 10,
                   data = stations_sf,aes(x =X, y = Y, label = Short.Name),
                   size = 3,
                   alpha = 1,
                   col = "black",
                   fill = NA,
                   label.size = NA,
                   max.overlaps = Inf,
                   label.padding = 0.1,
                   min.segment.length = unit(0,"inches"),
                   family = fontFam,
                   fontface = "italic",
                   # arrow = arrow(angle = 30,length = unit(0.02, "inches"),type = "closed",ends = "last")
  )+
  # scale_pattern_manual(values = c("solid","blank"),
  #                       breaks = c("1","2"),
  #                       limits = c("1","2"),
  #                       labels = c("p\u22650.05","p<0.05"),
  #                       drop = FALSE,
  #                       name = "statistical\nsignificance")+
  
  
  labs(fill = " % change in low flow  \n(per decade)  \n<span style = 'color:#601200;font-weight:bold'>*drying*</span>\u2190|\u2192 <span style = 'color:#001260;font-weight:bold'>*wetting*</span>",
       col = " % change in low flow  \n(per decade)  \n<span style = 'color:#601200;font-weight:bold'>*drying*</span>\u2190|\u2192 <span style = 'color:#001260;font-weight:bold'>*wetting*</span>")+
  # annotate("text",  x=920000, y = 1710000, label = labName, vjust=0, hjust=0.5,
  #          fontface = "bold",size =2.2)+
  # annotate("text",  x=1650000, y = 1570000, label = "drying", vjust=0, hjust=0,
  #          fontface = "bold.italic",size = 3)+  
  # annotate("text",  x=1650000, y = 1100000, label = "wetting", vjust=0, hjust=0,
  #                                                        fontface = "bold.italic",size = 3)+
  theme_void()+
  # guides(fill = guide_legend(title = "r",title.position = "top"))+
  theme(legend.position = c(0.85,0.65),
        # plot.title = element_text(hjust = 0,vjust=-20),
        legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
        
        legend.background = element_rect(fill = "grey90",colour = "grey50"),
        legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
        plot.background = element_rect(fill = "white",colour = "grey50"),
        legend.text = element_markdown(size = 6,hjust = 0),
        legend.key.size = unit(6,"pt"),
        legend.box.just = "right"
        # legend.key = element_rect()
  )+
  guides(fill = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
         col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
         # pattern = guide_legend(title.position = "top",order = 2,
         #                        title.theme = element_text(face = "bold",hjust = 0.5,size = 6,angle = 0))
         )



ggsave(filename = "3.figures/VancouverIslandTrends.png",p1,
       width = 7,height = 7)

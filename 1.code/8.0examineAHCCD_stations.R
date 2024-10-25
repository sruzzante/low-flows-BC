# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-10


# This script extracts monthly ANUSPLIN data at AHCCD stations and calculates bias trends, and creates Figures G1-G8
# 
closeAllConnections()
rm(list=ls())
graphics.off()

library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)
library(sf)
library(reshape2)
library(stringr)
library(terra)
library(readxl)
library(tmap)
library(tidyr)
library(zyp)
library(modifiedmk)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

studyArea<-st_read("../low-flows-BC/2.data/2.working/CatchmentPolygons/bc_west.gpkg")%>%
  st_transform("EPSG:4326")

stations<-read.csv("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Temperature_Stations_Gen3.csv")
stations$StnId<-stations$StnId%>%str_remove_all(" ")
stations$Station.name<-stations$StnId%>%str_remove_all(" ")
# stations$long..deg.[stations$station.s.name=="ESTEVAN POINT"]<- -126.54
# stations$lat..deg.[stations$station.s.name=="ESTEVAN POINT"]<-49.39

stations$Long.deg.[stations$StnId=="1063690"]<- -128.37
stations$Lat.deg.[stations$StnId=="1063690"]<-52.27

stations$Long.deg.[stations$StnId=="1012010"]<- -128.37
stations$Lat.deg.[stations$StnId=="1012010"]<-48.72

stations$Long.deg.[stations$StnId=="1065010"]<- -123.57
stations$Lat.deg.[stations$StnId=="1065010"]<-52.27

stations$Long.deg.[stations$StnId=="1032731"]<- -126.54
stations$Lat.deg.[stations$StnId=="1032731"]<-49.39

stations$Long.deg.[stations$StnId=="1036572"]<- -128.041
stations$Lat.deg.[stations$StnId=="1036572"]<-50.448

stations$Long.deg.[stations$StnId=="1051351"]<- -131.02
stations$Lat.deg.[stations$StnId=="1051351"]<-51.945


stations<-stations%>%
  st_as_sf(coords = c("Long.deg.","Lat.deg."),crs = "EPSG:4326",remove = FALSE)

sum(stations$Prov=="BC")
stations<-st_filter(stations,studyArea%>%st_buffer(50000))#%>%
st_write(stations,"2.data/2.working/AHCCD/stations_temp.gpkg")
# filter(stnid!="1191440")%>%
# filter(Prov != "AB")

# tm_shape(tmax_plot,raster.downsample = FALSE)+tm_raster()+
#   tm_shape(stations)+tm_dots()+
#   tm_shape(salmonHabitat)+tm_borders()+
#   tm_mouse_coordinates()
# stns<-terra::vect(stations)
tmax<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                         1900,"/maxt60_01.tif"))
tmin<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                         1900,"/mint60_01.tif"))

datMax<-terra::extract(tmax,stations)
datMin<-terra::extract(tmin,stns)
stations[is.na(datMax$maxt60_01),]
data_all<-data.frame()
for(it_yr in 1900:2022){
  for(it_mo in 1:12){
    it_mo_00<-str_pad(it_mo,width = 2,side = "left",pad = "0")
    tmax<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                             it_yr,"/maxt60_",it_mo_00,".tif"))
    dat<-terra::extract(tmax,stations)
    dat$month<-it_mo
    dat$year<-it_yr
    dat$ID<-stns$StnId
    names(dat)[2]<-"maxt"
    data_all<-rbind(data_all,dat)
    
  }
}
saveRDS(data_all,"2.data/2.working/AHCCD/tmax.RDS")
data_all<-data.frame()
library(tictoc)
for(it_yr in 1900:2022){
  tic(sprintf("year %d",it_yr))
  for(it_mo in 1:12){
    it_mo_00<-str_pad(it_mo,width = 2,side = "left",pad = "0")
    tmin<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                             it_yr,"/mint60_",it_mo_00,".tif"))
    dat<-terra::extract(tmin,stations)
    dat$month<-it_mo
    dat$year<-it_yr
    dat$ID<-stns$StnId
    names(dat)[2]<-"mint"
    data_all<-rbind(data_all,dat)
    
  }
  toc()
}
saveRDS(data_all,"2.data/2.working/AHCCD/tmin.RDS")

data_all<-left_join(readRDS("2.data/2.working/AHCCD/tmax.RDS"),
                    readRDS("2.data/2.working/AHCCD/tmin.RDS")
)%>%
  dplyr::rename(maxt_ANU = maxt,
                mint_ANU = mint)
data_all_meas<-data.frame()

for(it in 1:nrow(stations)){
  x<-read.delim(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Homog_monthly_max_temp_Gen3/mx",stations$StnId[it],".txt"),
                sep = ",",
                skip = 4,
                header= FALSE,
                col.names = c("Year","Jan","" , "Feb","" ,"Mar","" ,"Apr","" ,"May","" ,"Jun","" ,"Jul","" ,"Aug","" ,"Sep","" ,"Oct","" ,"Nov","" ,"Dec","" ,"Annual","" ,"Winter","" ,"Spring","" ,"Summer","" ,"Autumn",""))%>%
    select(!contains("X"))%>%
    select(!c("Annual","Summer","Autumn","Winter","Spring"))%>%
    pivot_longer(cols = Jan:Dec,names_to = "month")%>%
    mutate(month = plyr::mapvalues(month, 
                                   from = c("Jan", "Feb" ,"Mar" ,"Apr" ,"May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec"),
                                   to = 1:12)%>%as.numeric(),
           ID=stations$StnId[it])%>%
    dplyr::rename(year = Year,
                  maxt_meas = value)
  
  x1<-read.delim(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Homog_monthly_min_temp_Gen3/mn",stations$StnId[it],".txt"),
                 sep = ",",
                 skip = 4,
                 header= FALSE,
                 col.names = c("Year","Jan","" , "Feb","" ,"Mar","" ,"Apr","" ,"May","" ,"Jun","" ,"Jul","" ,"Aug","" ,"Sep","" ,"Oct","" ,"Nov","" ,"Dec","" ,"Annual","" ,"Winter","" ,"Spring","" ,"Summer","" ,"Autumn",""))%>%
    select(!contains("X"))%>%
    select(!c("Annual","Summer","Autumn","Winter","Spring"))%>%
    pivot_longer(cols = Jan:Dec,names_to = "month")%>%
    mutate(month = plyr::mapvalues(month, 
                                   from = c("Jan", "Feb" ,"Mar" ,"Apr" ,"May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec"),
                                   to = 1:12)%>%as.numeric(),
           ID=stations$StnId[it])%>%
    dplyr::rename(year = Year,
                  mint_meas = value)
  
  X<-left_join(x,x1)
  
  data_all_meas<-rbind(data_all_meas,X)
  
}

data_temp<-inner_join(data_all,data_all_meas)

data_temp$maxt_meas[data_temp$maxt_meas==-9999.9]<-NA
data_temp$mint_meas[data_temp$mint_meas==-9999.9]<-NA

data_temp$tavg_ANU<-(data_temp$maxt_ANU+data_temp$mint_ANU)/2
data_temp$tavg_meas<-(data_temp$maxt_meas+data_temp$mint_meas)/2

data_temp$err<-data_temp$tavg_ANU-data_temp$tavg_meas

mean(data_temp$err,na.rm = TRUE)

saveRDS(data_temp,"2.data/2.working/AHCCD/Temp_error_ANUSPLIN.RDS")
senFun<-function(err,WaterYear){
  dat<-data.frame(err = err,
                  WaterYear = WaterYear)
  
  return(zyp.sen(err~WaterYear, dat)$coefficients[["WaterYear"]])
  
}

senFun<-function(err,year){
  dat<-data.frame(err = err,
                  year = year)%>%
    # filter(year>=1950)%>%
    filter(!is.na(err))
  return(zyp.sen(err~year,data = dat)$coefficients[["year"]])
  
}


for(it in 1:135){
  dat<-filter(data_temp,ID %in%stations$StnId[it])%>%group_by(ID,year)%>%
    dplyr::summarise(err = mean(err))
  dat$date<-lubridate::ym(paste(dat$year,dat$month))
  
  print(ggplot(dat,aes(x=year,y=err))+geom_point())
  readline("press enter")
}

fun_mmkh3lag<-function(x){if(length(x)>2){ MannKendall(x)[2]}else{NA}}
fun_lm1<-function(x,y){if(length(x)>2){ lm(y~x)$coefficients[2]}else{NA}}
fun_lm2<-function(x,y){if(length(x)>2){ (lm(y~x)%>%summary())$coefficients[2,4]}else{NA}}
# table by month#######
stn_err_slope<-
  rbind(
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1900]&year>=1900)%>%
      filter(!is.na(err))%>%
      group_by(ID,month)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1900),
    
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1925]&year>=1925)%>%
      filter(!is.na(err))%>%
      group_by(ID,month)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1925),
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1950]&year>=1950)%>%
      filter(!is.na(err))%>%
      group_by(ID,month)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1950),
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1975]&year>=1975)%>%
      filter(!is.na(err))%>%
      group_by(ID,month)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1975)
  )%>%filter(N>=30)
length(unique(stn_err_slope$ID))
summaryTable<-
stn_err_slope%>%
  group_by(startYear,month)%>%
  summarise(
    # percSignif.lm = mean(err.lm.p<0.05),
    #         percPositive.lm = mean(err.lm>0),
    #         percPositive.lm.p = binom.test(sum(err.lm<0),n())$p.val,
    #         med_delta100y.lm = median((exp(err.lm*100))),
    N = n(),
    # percSignif.mk = mean(err.MK.p<0.05),
    numPositive.sen. = sum(err.sen>0),
    sumNegative.sen. = sum(err.sen<0),
    # sum0=sum(err.sen==0),
    
    percPositive.sen.p = binom.test(sum(err.sen<0),n())$p.val
    # med_delta100y.sen = median((exp(err.sen*100)))
  )%>%
  mutate(period = paste(startYear,"2022",sep = "-"))%>%
  ungroup()%>%
  dplyr::select(period,month,N, numPositive.sen., sumNegative.sen., percPositive.sen.p)

roundFunc<-function(x){
  if(x>0.005){
    round(x,2)%>%as.character()
  }else if(x>=0.001){
    round(x,3)%>%as.character()
  }else if(x<0.001){
    "<0.0001"
  }
  
}

summaryTable$percPositive.sen.p_char<-lapply(summaryTable$percPositive.sen.p,FUN = roundFunc)%>%unlist()

prds<-c("1900-2022","1925-2022","1950-2022","1975-2022")
summaryTable2<-data.frame()
  for(it_period in 1:4){
    summaryTable_x<-filter(summaryTable,period==prds[it_period])
    
    
    k<-rank(summaryTable_x$percPositive.sen.p, ties.method = "first")
    
    m_prime<-12+1-k
    
    summaryTable_x$T1.sig<-""
    sigFunc<-function(x1,m_prime){
      if(x1<(0.001/m_prime)){return("***")}
      if(x1<(0.01/m_prime)){return("**")}
      if(x1<(0.05/m_prime)){return("*")}
      return("")
    }
    
    for(it in 1:12){
      it_row<-which(k==it)
      summaryTable_x$T1.sig[it_row]<-sigFunc(summaryTable_x$percPositive.sen.p[it_row],m_prime[it_row])
      if(summaryTable_x$T1.sig[it_row]==""){break}
    }
    summaryTable2<-rbind(summaryTable2,summaryTable_x)
    
  }

 
 write.csv(summaryTable2%>%select(!percPositive.sen.p),"4.output/AHCCD_error_temp.csv",row.names = FALSE)
  

# annual########
data_temp_yrly<-data_temp%>%
  group_by(ID,year)%>%
  dplyr::summarize(err = mean(err))

stn_err_slope2<-
  rbind(
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1900]&year>=1900)%>%
      group_by(ID,year)%>%
      dplyr::summarise(err = mean(err))%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1900),
    
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1925]&year>=1925)%>%
      group_by(ID,year)%>%
      dplyr::summarise(err = mean(err))%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1925),
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1950]&year>=1950)%>%
      group_by(ID,year)%>%
      dplyr::summarise(err = mean(err))%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1950),
    data_temp%>%
      filter(ID %in%stations$StnId[stations$From<=1975]&year>=1975)%>%
      group_by(ID,year)%>%
      dplyr::summarise(err = mean(err))%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1975)
  )
stn_err_slope2<-filter(stn_err_slope2,N>=30)
stn_err_slope2$err.sen.decade<-stn_err_slope2$err.sen*10

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2022")
stn_err_slope2$err.sen.decade_factor<-cut(stn_err_slope2$err.sen.decade,
                                          c(-Inf,seq(-0.3,0.3,0.1),Inf))

fctrLvls<-levels(stn_err_slope2$err.sen.decade_factor)
fctrLabs<-c("Less than -0.3°C",
            "-0.3°C to -0.2°C",
            "-0.2°C to -0.1°C",
            "-0.1°C to 0°C",
            "0°C to 0.1°C",
            "0.1°C to 0.2°C",
            "0.2°C to 0.3°C",
            "More than 0.3°C")

library(ggpattern)
library(ggplot2)
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  # geom_bar(position = "stack",breaks = c(-100,seq(-0.3,0.3,0.1),100))+
  geom_bar_pattern(pattern_colour   = NA,
                   pattern_angle = 45,
                   pattern_fill = "grey80",
                   pattern_res = 600,
                   pattern_density = 0.3,
                   size = 0.1,
                   linewidth = 0.1,
                   pattern_spacing = 0.04,
                   colour = "grey50",
                   show.legend = TRUE)+
  scale_y_continuous(name = "Number of Stations")+
  scale_fill_manual(name = "Error trend\n(per decade)",
                    drop = FALSE,
                    limits = fctrLvls%>%rev(),
                    labels = fctrLabs%>%rev(),
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = RColorBrewer::brewer.pal(8,"RdBu"))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = fctrLabs,name = "Trend in error", drop = FALSE,)+
  facet_wrap(facets = "label",ncol = 1,scales = "free_y",
             strip.position = "right")+
  theme(
    # legend.position = "none",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 12),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "top",order = 2,
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 12,angle = 0)))


ggsave("3.figures/AHCCD_error/AHCCD_error_trend_tavg_bar.png",width = 5,height = 3.099186*2,p_bar)
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_tavg_bar.svg",width = 5,height = 3.099186*2,p_bar)

library(bcmaps)
bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")

stn_err_slope2_sf<-inner_join(stations,stn_err_slope2,by = c("StnId"="ID"))

stn_err_slope2_sf$err.sen.decade<-stn_err_slope2_sf$err.sen*10

stn_err_slope2_sf<-st_buffer(stn_err_slope2_sf,25000)

brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1890070+50000,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()
x<-rnaturalearth::ne_states(country = c("Canada","United States of America"))%>%
  st_transform(crs = st_crs(bc_nghbrs))%>%
  filter(name_en!="British Columbia")

for(it_yr in c(1900,1925,1950,1975)){
  plot_sf<-stn_err_slope2_sf%>%filter(startYear==it_yr)
  for(it in 1:nrow(plot_sf)){
    plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),])%>%st_make_valid())
  }
  
  
  # 
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = x,fill="grey90")+
    # geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
    # geom_sf(data = stn_err_slope2_sf%>%filter(startYear==1900),col = "white",lwd  =0.75)+
    # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
    
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    
    
    # geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
    
    geom_sf_pattern(data = plot_sf,
                    aes(fill = err.sen.decade_factor,
                        # col = err.sen.decade_factor,
                        pattern = err.MK.p<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey30",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600,
                    show.legend = TRUE)+
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
      
      name= "Error trend\n(°C per decade)",
      limits = fctrLvls%>%rev(),
      labels = fctrLabs%>%rev(),
      values = RColorBrewer::brewer.pal(8,"RdBu"),
      # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
      # values = brewer_pal(palette = "BlRd")
      # values =scico(10,palette = "vik",direction = -1)%>%rev(),
      drop = FALSE)+
    # scale_color_manual(
    #   limits = fctrLvls%>%rev(),
    #   labels = fctrLabs%>%rev(),
    #   # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
    #   values =rep("grey50",10),
    #   drop = FALSE)+
    
    
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    scale_x_continuous(expand = c(0,0),limits = c(260942.4,1890070+50000))+
    scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
    
    theme(legend.position = c(0.83,0.65),
          plot.title = element_text(hjust = 0,vjust=-20),
          # legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
          
          legend.background = element_rect(fill = "white",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "#CDD9EF",colour = "grey50"),
          # legend.text = element_markdown(size = 6,hjust = 0),
          legend.key.width = unit(8,"pt"),
          legend.key.height = unit(8,"pt"),
          legend.box.just = "right"
          # legend.key = element_rect()
    )+
    guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  override.aes = list( pattern_density = 0.3,
                                                       size = 0.1,
                                                       linewidth = 0.1,
                                                       pattern_spacing = 0.01,
                                                       pattern_fill = "grey80",
                                                       fill = "white",
                                                       colour = "grey30",
                                                       pattern_res = 600),
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 12,angle = 0)))
  ggsave(path = "3.Figures",
         filename = paste0("AHCCD_error/AHCCD_error_trend_tavg_",it_yr,"-2022.svg"),
         device = "svg",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
  p1<-p1+theme(legend.position = "none")
  ggsave(path = "3.Figures",
         filename = paste0("AHCCD_error/AHCCD_error_trend_tavg_",it_yr,"-2022.png"),
         device = "png",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
  
  
}



tm_shape(stn_err_slope2_sf%>%filter(startYear=="1975"))+
  tm_dots(col = "err.sen",palette = "-RdBu")

## a couple of examples for temperature ##########
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}
x<-stn_err_slope2%>%filter(N>90)
dat_plot<-data_temp%>%filter(ID==1123939)%>%
  
  group_by(year)%>%
  dplyr::summarize(across(c(tavg_ANU,tavg_meas),~mean(.x)))%>%
  mutate(err = (tavg_ANU-tavg_meas))%>%
  filter(year>=1900)


dat_plot_arrow<-dat_plot%>%select(year,tavg_ANU,tavg_meas)%>%
  pivot_longer(cols = tavg_ANU:tavg_meas)

senFun(dat_plot$year,dat_plot$err
)

lblr<-function(x){plyr::mapvalues(x,
                                  from = c("1","2"),
                                  to = c("Temperature (°C)",
                                         "Bias (°C)"))}

ggplot(dat_plot%>%mutate(fct = "1"),aes(x =year))+
  geom_point(aes(y = tavg_meas,col = "AHCCD"))+
  geom_point(aes(y = tavg_ANU,col = "ANUSPLIN"))+
  geom_line(data = dat_plot_arrow%>%mutate(fct = "1"),
            aes(x = year,y = value,group = year,col = "bias"),arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_line(data = dat_plot%>%mutate(fct = "2",y0=0)%>%pivot_longer(cols = c(err,y0)),
            aes(x = year,y = value,group = year,col = "bias"),
            arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_smooth(data = dat_plot%>%mutate(fct = "2")%>%pivot_longer(cols = c(err)),
              aes(x = year,y = value,col = "bias trend"),method = sen,se = FALSE)+
  
  facet_wrap(facets = "fct",ncol=1,
             scales = "free_y",strip.position = "left",
             labeller  = labeller(fct = lblr))+
  theme_bw()+
  scale_y_continuous(name = NULL)+
  scale_color_manual(values = c("grey50","red","black","purple"),
                     name = NULL)+
  ggtitle("KELOWNA (1123939)")+
  theme(strip.placement = "outside",
        strip.background = element_blank())
ggsave("3.figures/AHCCD_error/AHCCD_error_tavg_KELOWNA (1123939).png",
       width = 6,height = 4)

dat_plot<-data_temp%>%filter(ID==1167337)%>%
  
  group_by(year)%>%
  dplyr::summarize(across(c(tavg_ANU,tavg_meas),~mean(.x)))%>%
  mutate(err = (tavg_ANU-tavg_meas))%>%
  filter(year>=1900)


dat_plot_arrow<-dat_plot%>%select(year,tavg_ANU,tavg_meas)%>%
  pivot_longer(cols = tavg_ANU:tavg_meas)

senFun(dat_plot$err,dat_plot$year
)

lblr<-function(x){plyr::mapvalues(x,
                                  from = c("1","2"),
                                  to = c("Temperature (°C)",
                                         "Bias (°C)"))}

ggplot(dat_plot%>%mutate(fct = "1"),aes(x =year))+
  geom_point(aes(y = tavg_meas,col = "AHCCD"))+
  geom_point(aes(y = tavg_ANU,col = "ANUSPLIN"))+
  geom_line(data = dat_plot_arrow%>%mutate(fct = "1"),
            aes(x = year,y = value,group = year,col = "bias"),arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_line(data = dat_plot%>%mutate(fct = "2",y0=0)%>%pivot_longer(cols = c(err,y0)),
            aes(x = year,y = value,group = year,col = "bias"),
            arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_smooth(data = dat_plot%>%mutate(fct = "2")%>%pivot_longer(cols = c(err)),
              aes(x = year,y = value,col = "bias trend"),method = sen,se = FALSE)+
  
  facet_wrap(facets = "fct",ncol=1,
             scales = "free_y",strip.position = "left",
             labeller  = labeller(fct = lblr))+
  theme_bw()+
  scale_y_continuous(name = NULL)+
  scale_color_manual(values = c("grey50","red","black","purple"),
                     name = NULL)+
  ggtitle("Salmon Arm (1167337)")+
  theme(strip.placement = "outside",
        strip.background = element_blank())
ggsave("3.figures/AHCCD_error/AHCCD_error_tavg_Salmon Arm (1167337).png",
       width = 6,height = 4)



# for precipitation ###########
rm(list = ls())

studyArea<-st_read("../low-flows-BC/2.data/2.working/CatchmentPolygons/bc_west.gpkg")%>%
  st_transform("EPSG:4326")

stations<-read.csv("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Precipitation_Stations.csv")
stations$stnid<-stations$stnid%>%str_remove_all(" ")
stations$long..deg.[stations$station.s.name=="ESTEVAN POINT"]<- -126.54
stations$lat..deg.[stations$station.s.name=="ESTEVAN POINT"]<-49.39

stations$long..deg.[stations$station.s.name=="MCINNES ISLAND"]<- -128.676
stations$lat..deg.[stations$station.s.name=="MCINNES ISLAND"]<-52.27


stations<-stations%>%
  st_as_sf(coords = c("long..deg.","lat..deg."),crs = "EPSG:4326",remove = FALSE)

sum(stations$Prov=="BC")
stations<-st_filter(stations,studyArea%>%st_buffer(50000))#%>%
# filter(stnid!="1191440")%>%

pcp<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                        1900,"/pcp60_01.tif"))

dat<-terra::extract(pcp,stations)
data_all<-data.frame()
for(it_yr in 1900:2022){
  for(it_mo in 1:12){
    it_mo_00<-str_pad(it_mo,width = 2,side = "left",pad = "0")
    pcp<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_unadj/",
                            it_yr,"/pcp60_",it_mo_00,".tif"))
    dat<-terra::extract(pcp,stations)
    dat$month<-it_mo
    dat$year<-it_yr
    dat$ID<-stns$stnid
    names(dat)[2]<-"pcp"
    data_all<-rbind(data_all,dat)
    
  }
}
saveRDS(data_all,"2.data/2.working/AHCCD/pcp.RDS")
data_pcp_ANU<-readRDS("2.data/2.working/AHCCD/pcp.RDS")%>%dplyr::rename(pcp_ANU = pcp)


pcp_ANU_mean<-
  data_pcp_ANU%>%
  filter(year%in%(1991:2020))%>%
  group_by(ID)%>%
  dplyr::summarise(P_annual = sum(pcp_ANU)/30,
                   P_August = sum(pcp_ANU[month==8])/30,
                   P_summer = sum(pcp_ANU[month%in%(6:9)])/30,
                   P_winter = sum(pcp_ANU[month%in%c(12,1,2,3)])/30)

data_all_meas<-data.frame()

for(it in 1:nrow(stations)){
  x<-read.delim(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Adj_monthly_total_prec/mt",stations$stnid[it],".txt"),
                sep = ",",
                skip = 4,
                header= FALSE,
                col.names = c("Year","Jan","" , "Feb","" ,"Mar","" ,"Apr","" ,"May","" ,"Jun","" ,"Jul","" ,"Aug","" ,"Sep","" ,"Oct","" ,"Nov","" ,"Dec","" ,"Annual","" ,"Winter","" ,"Spring","" ,"Summer","" ,"Autumn","",""))%>%
    select(!contains("X"))%>%
    select(!c("Annual","Summer","Autumn","Winter","Spring"))%>%
    pivot_longer(cols = Jan:Dec,names_to = "month")%>%
    mutate(month = plyr::mapvalues(month, 
                                   from = c("Jan", "Feb" ,"Mar" ,"Apr" ,"May" ,"Jun" ,"Jul" ,"Aug" ,"Sep" ,"Oct" ,"Nov" ,"Dec"),
                                   to = 1:12)%>%as.numeric(),
           ID=stations$stnid[it])%>%
    dplyr::rename(year = Year,
                  pcp_meas = value)
  
  data_all_meas<-rbind(data_all_meas,x)
  
}


data_pcp<-left_join(data_pcp_ANU,data_all_meas)%>%
  left_join(pcp_ANU_mean)

data_pcp$pcp_meas[data_pcp$pcp_meas==-9999.9]<-NA

## Annual #########

# data_pcp$err<-log(data_pcp$pcp_ANU/data_pcp$pcp_meas)

data_pcp_yrly<-data_pcp%>%
  group_by(ID,year)%>%
  dplyr::summarize(err = log(sum(pcp_ANU)/sum(pcp_meas)))

mean(data_pcp_yrly$err,na.rm = TRUE)

saveRDS(data_pcp_yrly,"2.data/2.working/AHCCD/PcpYrly_error_ANUSPLIN.RDS")

senFun<-function(err,WaterYear){
  if(length(err)>2){
    dat<-data.frame(err = err,
                    WaterYear = WaterYear)
    
    return(zyp.sen(err~WaterYear, dat)$coefficients[["WaterYear"]])
  }else{NA}
  
  
}



fun_mmkh3lag<-function(x){if(length(x)>2){ MannKendall(x)[2]}else{NA}}
fun_lm1<-function(x,y){if(length(x)>2){ lm(y~x)$coefficients[2]}else{NA}}
fun_lm2<-function(x,y){if(length(x)>2){ (lm(y~x)%>%summary())$coefficients[2,4]}else{NA}}
stn_err_slope2<-
  rbind(
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1900]&year>=1900)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1900),
    
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1925]&year>=1925)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1925),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1950]&year>=1950)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1950),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1975]&year>=1975)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1975)
  )
stn_err_slope2<-filter(stn_err_slope2,N>=30)

stn_err_slope2$err.sen.decade<-exp(stn_err_slope2$err.sen*10)*100-100

ggplot(stn_err_slope2,aes(x = err.sen.decade))+geom_histogram()+facet_wrap("startYear")

for(it in 1:106){
  print(
    data_pcp_yrly%>%
      filter(ID==stations$stnid[it])%>%
      ggplot(aes(x = year,y = err))+
      geom_point()+
      geom_smooth(method = "lm")
  )
  readline(prompt="Press [enter] to continue")
}
##########

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2022")
stn_err_slope2$err.sen.decade_factor<-cut(stn_err_slope2$err.sen.decade,
                                          c(-Inf,seq(-5,5,1),Inf))

fctrLvls<-levels(stn_err_slope2$err.sen.decade_factor)
# fctrLabs<-c("Less than -20%",
#             # "-30% to -20%",
#             "-20% to -15%",
#             "-15% to -10%",
#             "-10% to -5%",
#             "-5% to 0%",
#             "0% to 5%",
#             "5% to 10%",
#             "10% to 15%",
#             "15% to 20%",
#             # "20% to 30%",
#             "More than 20%")



fctrLabs<-c("Less than -5%",
            # "-30% to -20%",
            "-5% to -4%",
            "-4% to -3%",
            "-3% to -2%",
            "-2% to 1%",
            "-1% to 0%",
            "0% to 1%",
            "1% to 2%",
            "2% to 3%",
            "3% to 4%",
            "4% to 5%",
            # "20% to 30%",
            "More than 5%")

library(ggpattern)
library(ggplot2)
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  # geom_bar(position = "stack",breaks = c(-100,seq(-0.3,0.3,0.1),100))+
  geom_bar_pattern(pattern_colour   = NA,
                   pattern_angle = 45,
                   pattern_fill = "grey80",
                   pattern_res = 600,
                   pattern_density = 0.3,
                   size = 0.1,
                   linewidth = 0.1,
                   pattern_spacing = 0.04,
                   colour = "grey50",
                   show.legend = TRUE)+
  scale_y_continuous(name = "Number of Stations")+
  scale_fill_manual(name = "Error trend\n(per decade)",
                    drop = FALSE,
                    limits = fctrLvls%>%rev(),
                    labels = fctrLabs%>%rev(),
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = fctrLabs,name = "Error trend per decade)", drop = FALSE,)+
  facet_wrap(facets = "label",ncol = 1,scales = "free_y",
             strip.position = "right")+
  theme(
    # legend.position = "none",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 12),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "top",order = 2,
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 12,angle = 0)))

p_bar
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_annual_bar.png",width = 5,height = 3.099186*2,p_bar)
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_annual_bar.svg",width = 5,height = 3.099186*2,p_bar)


library(bcmaps)
bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")

stn_err_slope2_sf<-inner_join(stations,stn_err_slope2,by = c("stnid"="ID"))


stn_err_slope2_sf<-st_buffer(stn_err_slope2_sf,25000)

brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1890070+50000,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()
x<-rnaturalearth::ne_states(country = c("Canada","United States of America"))%>%
  st_transform(crs = st_crs(bc_nghbrs))%>%
  filter(name_en!="British Columbia")

for(it_yr in c(1900,1925,1950,1975)){
  plot_sf<-stn_err_slope2_sf%>%filter(startYear==it_yr)
  for(it in 1:nrow(plot_sf)){
    plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),])%>%st_make_valid())
  }
  
  
  # 
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = x,fill="grey90")+
    # geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
    # geom_sf(data = stn_err_slope2_sf%>%filter(startYear==1900),col = "white",lwd  =0.75)+
    # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
    
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    
    
    # geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
    
    geom_sf_pattern(data = plot_sf,
                    aes(fill = err.sen.decade_factor,
                        # col = err.sen.decade_factor,
                        pattern = err.MK.p<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey30",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600,
                    show.legend = TRUE)+
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
      
      name= "Error trend\n(% per decade)",
      limits = fctrLvls%>%rev(),
      labels = fctrLabs%>%rev(),
      values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12),
      # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
      # values = brewer_pal(palette = "BlRd")
      # values =scico(10,palette = "vik",direction = -1)%>%rev(),
      drop = FALSE)+
    # scale_color_manual(
    #   limits = fctrLvls%>%rev(),
    #   labels = fctrLabs%>%rev(),
    #   # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
    #   values =rep("grey50",10),
    #   drop = FALSE)+
    
    
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    scale_x_continuous(expand = c(0,0),limits = c(260942.4,1890070+50000))+
    scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
    
    theme(legend.position = c(0.83,0.65),
          plot.title = element_text(hjust = 0,vjust=-20),
          # legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
          
          legend.background = element_rect(fill = "white",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "#CDD9EF",colour = "grey50"),
          # legend.text = element_markdown(size = 6,hjust = 0),
          legend.key.width = unit(8,"pt"),
          legend.key.height = unit(8,"pt"),
          legend.box.just = "right"
          # legend.key = element_rect()
    )+
    guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 10,angle = 0)))
  p1<-p1+theme(legend.position = "none")
  ggsave(path = "3.Figures",
         filename = paste0("AHCCD_error/AHCCD_error_trend_pcp_annual_",it_yr,"-2022.png"),
         device = "png",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
  
}


stn_err_slope2%>%
  group_by(label)%>%
  dplyr::summarise(mean(err.sen<0  ),
                   n(),
                   binom.test(sum(err.sen<0),n(),0.5)$p.value)

x<-stn_err_slope2%>%filter(N>90)
1108447
1090660
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}
lblr<-function(x){plyr::mapvalues(x,
                                  from = c("1","2"),
                                  to = c("Precipitation (mm)",
                                         "Bias (%)"))}

dat_plot<-data_pcp%>%filter(ID==1108447)%>%
  # mutate(waterYear = year+floor(month/10)
  # )%>%
  # filter(month%in%(c(12,1,2,3)))%>%
  group_by(year)%>%
  dplyr::summarize(across(c(pcp_ANU,pcp_meas),~sum(.x)))%>%
  mutate(err = (pcp_ANU/pcp_meas-1)*100)%>%
  # dplyr::rename(year = waterYear)%>%
  filter(year>=1900)


dat_plot_arrow<-dat_plot%>%select(year,pcp_ANU,pcp_meas)%>%
  pivot_longer(cols = pcp_ANU:pcp_meas)

senFun(dat_plot$err,dat_plot$year
)


ggplot(dat_plot%>%mutate(fct = "1"),aes(x =year))+
  geom_point(aes(y = pcp_meas,col = "AHCCD"))+
  geom_point(aes(y = pcp_ANU,col = "ANUSPLIN"))+
  geom_line(data = dat_plot_arrow%>%mutate(fct = "1"),
            aes(x = year,y = value,group = year,col = "bias"),arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_line(data = dat_plot%>%mutate(fct = "2",y0=0)%>%pivot_longer(cols = c(err,y0)),
            aes(x = year,y = value,group = year,col = "bias"),
            arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  scale_x_continuous(limits = c(min(dat_plot$year[!is.na(dat_plot$pcp_meas)]),
                                max(dat_plot$year[!is.na(dat_plot$pcp_meas)])))+
  
  geom_smooth(data = dat_plot%>%mutate(fct = "2")%>%pivot_longer(cols = c(err)),
              aes(x = year,y = value,col = "bias trend"),method = sen,se = FALSE)+
  
  facet_wrap(facets = "fct",ncol=1,
             scales = "free_y",strip.position = "left",
             labeller  = labeller(fct = lblr))+
  theme_bw()+
  scale_y_continuous(name = NULL)+
  scale_color_manual(values = c("grey50","red","black","purple"),
                     name = NULL)+
  ggtitle("Vancouver Intl A (1108447)")+
  theme(strip.placement = "outside",
        strip.background = element_blank())
ggsave("3.figures/AHCCD_error/AHCCD_error_Vancouver Intl A (1108447).png",
       width = 6,height = 4)


dat_plot<-data_pcp%>%filter(ID==1032730)%>%
  # mutate(waterYear = year+floor(month/10)
  # )%>%
  # filter(month%in%(c(12,1,2,3)))%>%
  group_by(year)%>%
  dplyr::summarize(across(c(pcp_ANU,pcp_meas),~sum(.x)))%>%
  mutate(err = (pcp_ANU/pcp_meas-1)*100)%>%
  # dplyr::rename(year = waterYear)%>%
  filter(year>=1900)


dat_plot_arrow<-dat_plot%>%select(year,pcp_ANU,pcp_meas)%>%
  pivot_longer(cols = pcp_ANU:pcp_meas)

senFun(dat_plot$year,dat_plot$err
)


ggplot(dat_plot%>%mutate(fct = "1"),aes(x =year))+
  geom_point(aes(y = pcp_meas,col = "AHCCD"))+
  geom_point(aes(y = pcp_ANU,col = "ANUSPLIN"))+
  geom_line(data = dat_plot_arrow%>%mutate(fct = "1"),
            aes(x = year,y = value,group = year,col = "bias"),arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  geom_line(data = dat_plot%>%mutate(fct = "2",y0=0)%>%pivot_longer(cols = c(err,y0)),
            aes(x = year,y = value,group = year,col = "bias"),
            arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
  scale_x_continuous(limits = c(min(dat_plot$year[!is.na(dat_plot$pcp_meas)]),
                                max(dat_plot$year[!is.na(dat_plot$pcp_meas)])))+
  
  geom_smooth(data = dat_plot%>%mutate(fct = "2")%>%pivot_longer(cols = c(err)),
              aes(x = year,y = value,col = "bias trend"),method = sen,se = FALSE)+
  
  facet_wrap(facets = "fct",ncol=1,
             scales = "free_y",strip.position = "left",
             labeller  = labeller(fct = lblr))+
  theme_bw()+
  scale_y_continuous(name = NULL)+
  scale_color_manual(values = c("grey50","red","black","purple"),
                     name = NULL)+
  ggtitle("Estevan Point (1032730)")+
  theme(strip.placement = "outside",
        strip.background = element_blank())
ggsave("3.figures/AHCCD_error/AHCCD_error_Estevan Point_1032730.png",
       width = 6,height = 4)

senFun(dat_plot$pcp_meas)
## summer pcp############

data_pcp_yrly<-data_pcp%>%
  filter(month%in%(6:9))%>%
  group_by(ID,year)%>%
  dplyr::summarize(err = log(sum(pcp_ANU)/sum(pcp_meas)))

mean(data_pcp_yrly$err,na.rm = TRUE)

senFun<-function(err,WaterYear){
  if(length(err)>2){
    dat<-data.frame(err = err,
                    WaterYear = WaterYear)
    
    return(zyp.sen(err~WaterYear, dat)$coefficients[["WaterYear"]])
  }else{NA}
  
  
}



fun_mmkh3lag<-function(x){if(length(x)>2){ MannKendall(x)[2]}else{NA}}
fun_lm1<-function(x,y){if(length(x)>2){ lm(y~x)$coefficients[2]}else{NA}}
fun_lm2<-function(x,y){if(length(x)>2){ (lm(y~x)%>%summary())$coefficients[2,4]}else{NA}}
stn_err_slope2<-
  rbind(
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1900]&year>=1900)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1900),
    
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1925]&year>=1925)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1925),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1950]&year>=1950)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1950),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1975]&year>=1975)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1975)
  )
stn_err_slope2<-filter(stn_err_slope2,N>=30)

stn_err_slope2$err.sen.decade<-exp(stn_err_slope2$err.sen*10)*100-100

ggplot(stn_err_slope2,aes(x = err.sen.decade))+geom_histogram()+facet_wrap("startYear")

for(it in 1:106){
  print(
    data_pcp_yrly%>%
      filter(ID==stations$stnid[it])%>%
      ggplot(aes(x = year,y = err))+
      geom_point()+
      geom_smooth(method = "lm")
  )
  readline(prompt="Press [enter] to continue")
}

# sumer precip plots ########

##########

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2022")

stn_err_slope2%>%
  group_by(label)%>%
  dplyr::summarise(mean(err.sen<0  ),
                   binom.test(sum(err.sen<0),n(),0.5)$p.value)


stn_err_slope2$err.sen.decade_factor<-cut(stn_err_slope2$err.sen.decade,
                                          c(-Inf,seq(-5,5,1),Inf))

fctrLvls<-levels(stn_err_slope2$err.sen.decade_factor)

fctrLabs<-c("Less than -5%",
            # "-30% to -20%",
            "-5% to -4%",
            "-4% to -3%",
            "-3% to -2%",
            "-2% to 1%",
            "-1% to 0%",
            "0% to 1%",
            "1% to 2%",
            "2% to 3%",
            "3% to 4%",
            "4% to 5%",
            # "20% to 30%",
            "More than 5%")

library(ggpattern)
library(ggplot2)
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  # geom_bar(position = "stack",breaks = c(-100,seq(-0.3,0.3,0.1),100))+
  geom_bar_pattern(pattern_colour   = NA,
                   pattern_angle = 45,
                   pattern_fill = "grey80",
                   pattern_res = 600,
                   pattern_density = 0.3,
                   size = 0.1,
                   linewidth = 0.1,
                   pattern_spacing = 0.04,
                   colour = "grey50",
                   show.legend = TRUE)+
  scale_y_continuous(name = "Number of Stations")+
  scale_fill_manual(name = "Error trend\n(per decade)",
                    drop = FALSE,
                    limits = fctrLvls%>%rev(),
                    labels = fctrLabs%>%rev(),
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = fctrLabs,name = "Error trend (per decade)", drop = FALSE,)+
  facet_wrap(facets = "label",ncol = 1,scales = "free_y",
             strip.position = "right")+
  theme(
    # legend.position = "none",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 12),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "top",order = 2,
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 12,angle = 0)))

p_bar

ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_summer_bar.png",width = 5,height = 3.099186*2,p_bar)
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_summer_bar.svg",width = 5,height = 3.099186*2,p_bar)

# maps

library(bcmaps)
bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")

stn_err_slope2_sf<-inner_join(stations,stn_err_slope2,by = c("stnid"="ID"))


stn_err_slope2_sf<-st_buffer(stn_err_slope2_sf,25000)

brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1890070+50000,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()
x<-rnaturalearth::ne_states(country = c("Canada","United States of America"))%>%
  st_transform(crs = st_crs(bc_nghbrs))%>%
  filter(name_en!="British Columbia")

for(it_yr in c(1900,1925,1950,1975)){
  plot_sf<-stn_err_slope2_sf%>%filter(startYear==it_yr)
  for(it in 1:nrow(plot_sf)){
    plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),])%>%st_make_valid())
  }
  
  
  # 
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = x,fill="grey90")+
    # geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
    # geom_sf(data = stn_err_slope2_sf%>%filter(startYear==1900),col = "white",lwd  =0.75)+
    # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
    
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    
    
    # geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
    
    geom_sf_pattern(data = plot_sf,
                    aes(fill = err.sen.decade_factor,
                        # col = err.sen.decade_factor,
                        pattern = err.MK.p<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey30",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600,
                    show.legend = TRUE)+
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
      
      name= "Error trend\n(% per decade)",
      limits = fctrLvls%>%rev(),
      labels = fctrLabs%>%rev(),
      values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12),
      # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
      # values = brewer_pal(palette = "BlRd")
      # values =scico(10,palette = "vik",direction = -1)%>%rev(),
      drop = FALSE)+
    # scale_color_manual(
    #   limits = fctrLvls%>%rev(),
    #   labels = fctrLabs%>%rev(),
    #   # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
    #   values =rep("grey50",10),
    #   drop = FALSE)+
    
    
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    scale_x_continuous(expand = c(0,0),limits = c(260942.4,1890070+50000))+
    scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
    
    theme(legend.position = c(0.83,0.65),
          plot.title = element_text(hjust = 0,vjust=-20),
          # legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
          
          legend.background = element_rect(fill = "white",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "#CDD9EF",colour = "grey50"),
          # legend.text = element_markdown(size = 6,hjust = 0),
          legend.key.width = unit(8,"pt"),
          legend.key.height = unit(8,"pt"),
          legend.box.just = "right"
          # legend.key = element_rect()
    )+
    guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 10,angle = 0)))
  p1<-p1+theme(legend.position = "none")
  ggsave(path = "3.Figures",
         filename = paste0("AHCCD_error/AHCCD_error_trend_pcp_summer_",it_yr,"-2022.png"),
         device = "png",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
}
x<-stn_err_slope2%>%filter(N>=100)

## winter pcp############

data_pcp_yrly<-data_pcp%>%
  mutate(year = year+floor(month/10)
  )%>%
  filter(month%in%(c(12,1,2,3)))%>%
  group_by(ID,year)%>%
  dplyr::summarize(err = log(sum(pcp_ANU)/sum(pcp_meas)))

mean(data_pcp_yrly$err,na.rm = TRUE)

senFun<-function(err,WaterYear){
  if(length(err)>2){
    dat<-data.frame(err = err,
                    WaterYear = WaterYear)
    
    return(zyp.sen(err~WaterYear, dat)$coefficients[["WaterYear"]])
  }else{NA}
  
  
}



fun_mmkh3lag<-function(x){if(length(x)>2){ MannKendall(x)[2]}else{NA}}
fun_lm1<-function(x,y){if(length(x)>2){ lm(y~x)$coefficients[2]}else{NA}}
fun_lm2<-function(x,y){if(length(x)>2){ (lm(y~x)%>%summary())$coefficients[2,4]}else{NA}}
stn_err_slope2<-
  rbind(
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1900]&year>=1900)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1900),
    
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1925]&year>=1925)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1925),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1950]&year>=1950)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1950),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1975]&year>=1975)%>%
      filter(!is.na(err))%>%
      group_by(ID)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n()
      )%>%mutate(startYear = 1975)
  )
stn_err_slope2<-filter(stn_err_slope2,N>=30)

stn_err_slope2$err.sen.decade<-exp(stn_err_slope2$err.sen*10)*100-100

ggplot(stn_err_slope2,aes(x = err.sen.decade))+geom_histogram()+facet_wrap("startYear")

for(it in 1:106){
  print(
    data_pcp_yrly%>%
      filter(ID==stations$stnid[it])%>%
      ggplot(aes(x = year,y = err))+
      geom_point()+
      geom_smooth(method = "lm")
  )
  readline(prompt="Press [enter] to continue")
}
##########

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2022")

stn_err_slope2%>%
  group_by(label)%>%
  dplyr::summarise(mean(err.sen<0  ),
                   binom.test(sum(err.sen<0),n(),0.5)$p.value,
                   n())


stn_err_slope2$err.sen.decade_factor<-cut(stn_err_slope2$err.sen.decade,
                                          c(-Inf,seq(-5,5,1),Inf))

fctrLvls<-levels(stn_err_slope2$err.sen.decade_factor)

fctrLabs<-c("Less than -5%",
            # "-30% to -20%",
            "-5% to -4%",
            "-4% to -3%",
            "-3% to -2%",
            "-2% to 1%",
            "-1% to 0%",
            "0% to 1%",
            "1% to 2%",
            "2% to 3%",
            "3% to 4%",
            "4% to 5%",
            # "20% to 30%",
            "More than 5%")

library(ggpattern)
library(ggplot2)
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  # geom_bar(position = "stack",breaks = c(-100,seq(-0.3,0.3,0.1),100))+
  geom_bar_pattern(pattern_colour   = NA,
                   pattern_angle = 45,
                   pattern_fill = "grey80",
                   pattern_res = 600,
                   pattern_density = 0.3,
                   size = 0.1,
                   linewidth = 0.1,
                   pattern_spacing = 0.04,
                   colour = "grey50",
                   show.legend = TRUE)+
  scale_y_continuous(name = "Number of Stations")+
  scale_fill_manual(name = "Error trend\n(per decade)",
                    drop = FALSE,
                    limits = fctrLvls%>%rev(),
                    labels = fctrLabs%>%rev(),
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = fctrLabs,name = "Error trend (per decade)", drop = FALSE,)+
  facet_wrap(facets = "label",ncol = 1,scales = "free_y",
             strip.position = "right")+
  theme(
    # legend.position = "none",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 12),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "top",order = 2,
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 12,angle = 0)))

p_bar

ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_winter_bar.png",width = 5,height = 3.099186*2,p_bar)


ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcp_winter_bar.svg",width = 5,height = 3.099186*2,p_bar)

x<-stn_err_slope2%>%filter(N>90)
IDs<-unique(stn_err_slope2$ID)
for(ID_it in 1:length(IDs)){
  dat_plot<-data_pcp%>%filter(ID==IDs[ID_it])%>%
    mutate(year = year+floor(month/10)
    )%>%
    filter(month%in%(c(12,1,2,3)))%>%
    group_by(year)%>%
    dplyr::summarize(across(c(pcp_ANU,pcp_meas),~sum(.x)))%>%
    mutate(err = (pcp_ANU/pcp_meas-1)*100)
  
  
  dat_plot_arrow<-dat_plot%>%select(year,pcp_ANU,pcp_meas)%>%
    pivot_longer(cols = pcp_ANU:pcp_meas)
  
  
  ggplot(dat_plot%>%mutate(fct = "1"),aes(x =year))+
    geom_point(aes(y = pcp_meas,col = "AHCCD"))+
    geom_point(aes(y = pcp_ANU,col = "ANUSPLIN"))+
    geom_line(data = dat_plot_arrow%>%mutate(fct = "1"),
              aes(x = year,y = value,group = year,col = "error"),arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
    geom_line(data = dat_plot%>%mutate(fct = "2",y0=0)%>%pivot_longer(cols = c(err,y0)),
              aes(x = year,y = value,group = year,col = "error"),
              arrow = arrow(length=unit(0.1,"cm"), ends="first", type = "closed"))+
    geom_smooth(data = dat_plot%>%mutate(fct = "2")%>%pivot_longer(cols = c(err)),
                aes(x = year,y = value,col = "error trend"),method = sen,se = FALSE)+
    
    facet_wrap(facets = "fct",ncol=1,
               scales = "free_y",strip.position = "left",
               labeller  = labeller(fct = lblr))+
    theme_bw()+
    scale_y_continuous(name = NULL)+
    scale_x_continuous(limits = c(min(dat_plot$year[!is.na(dat_plot$pcp_meas)]),
                                  max(dat_plot$year[!is.na(dat_plot$pcp_meas)])))+
    scale_color_manual(values = c("grey50","red","black","purple"),
                       name = NULL)+
    ggtitle(paste0(stations$station.s.name[stations$stnid==IDs[ID_it]],
                   " (",IDs[ID_it],")"))+
    theme(strip.placement = "outside",
          strip.background = element_blank())
  ggsave(paste0(
    "3.figures/AHCCD_error/stationErrorPlots/pcp_winter/AHCCD_error",
    stations$station.s.name[stations$stnid==IDs[ID_it]],"_",IDs[ID_it],".png"),
    width = 6,height = 4)
}


# maps
library(bcmaps)
bc_west<-st_read("2.data/2.working/CatchmentPolygons/bc_west.gpkg")
bc_east<-st_read("2.data/2.working/CatchmentPolygons/bc_east.gpkg")
cont_divide<-st_read("2.data/2.working/CatchmentPolygons/continentalDivide.gpkg")
bc_nghbrs<-bc_neighbours()%>%
  filter(name!="British Columbia")

stn_err_slope2_sf<-inner_join(stations,stn_err_slope2,by = c("stnid"="ID"))


stn_err_slope2_sf<-st_buffer(stn_err_slope2_sf,25000)

brdr<-st_bbox(c(xmin = 260942.4,ymin = 362537.4,xmax = 1890070+50000,ymax = 1750251.6),
              crs= st_crs(bc_nghbrs))%>%
  
  st_as_sfc()
x<-rnaturalearth::ne_states(country = c("Canada","United States of America"))%>%
  st_transform(crs = st_crs(bc_nghbrs))%>%
  filter(name_en!="British Columbia")

for(it_yr in c(1900,1925,1950,1975)){
  plot_sf<-stn_err_slope2_sf%>%filter(startYear==it_yr)
  for(it in 1:nrow(plot_sf)){
    plot_sf[it,]<-st_difference(plot_sf[it,], st_union(plot_sf[it+1:nrow(plot_sf),])%>%st_make_valid())
  }
  
  
  # 
  p1<-ggplot()+
    geom_sf(data = bc_west,col = "grey25",fill = "white",inherit.aes = FALSE)+
    geom_sf(data = bc_east,fill = "grey90",col = "grey25",inherit.aes = FALSE)+
    geom_sf(data = x,fill="grey90")+
    # geom_sf(data = bc_nghbrs%>%filter(name == "Pacific Ocean"),fill="#CDD9EF")+
    # geom_sf(data = stn_err_slope2_sf%>%filter(startYear==1900),col = "white",lwd  =0.75)+
    # geom_sf(data = plot_sf,aes(fill = cut(!!varName,breaks = seq(-1,1,0.25),include.lowest = TRUE)),col = "grey50")+
    
    geom_sf(data = cont_divide,col = "grey25",linetype = "dotted",
            linewidth = 0.2,inherit.aes = FALSE)+
    
    
    # geom_sf(data = plot_sf_buffer,fill = "white",col = NA)+
    
    geom_sf_pattern(data = plot_sf,
                    aes(fill = err.sen.decade_factor,
                        # col = err.sen.decade_factor,
                        pattern = err.MK.p<0.05),
                    pattern_colour   = NA,
                    pattern_angle = 45,
                    colour = "grey30",
                    pattern_fill = "grey80",
                    # pattern_frequency = 10,
                    pattern_density = 0.3,
                    size = 0.1,
                    linewidth = 0.1,
                    pattern_spacing = 0.007,
                    pattern_res = 600,
                    show.legend = TRUE)+
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
      
      name= "Error trend\n(% per decade)",
      limits = fctrLvls%>%rev(),
      labels = fctrLabs%>%rev(),
      values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12),
      # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
      # values = brewer_pal(palette = "BlRd")
      # values =scico(10,palette = "vik",direction = -1)%>%rev(),
      drop = FALSE)+
    # scale_color_manual(
    #   limits = fctrLvls%>%rev(),
    #   labels = fctrLabs%>%rev(),
    #   # values = c("grey90", rep("grey50",10),"grey90")%>%rev(),
    #   values =rep("grey50",10),
    #   drop = FALSE)+
    
    
    theme_void()+
    # guides(fill = guide_legend(title = "r",title.position = "top"))+
    scale_x_continuous(expand = c(0,0),limits = c(260942.4,1890070+50000))+
    scale_y_continuous(expand = c(0,0),limits = c(362537.4,1750251.6))+
    
    theme(legend.position = c(0.83,0.65),
          plot.title = element_text(hjust = 0,vjust=-20),
          # legend.title = element_markdown(face = "bold",hjust = 0.5,size = 6,angle = 90),
          
          legend.background = element_rect(fill = "white",colour = "grey50"),
          legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
          plot.background = element_rect(fill = "#CDD9EF",colour = "grey50"),
          # legend.text = element_markdown(size = 6,hjust = 0),
          legend.key.width = unit(8,"pt"),
          legend.key.height = unit(8,"pt"),
          legend.box.just = "right"
          # legend.key = element_rect()
    )+
    guides(fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
           # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
           pattern = guide_legend(title.position = "top",order = 2,
                                  title.theme = element_text(face = "bold",hjust = 0.5,size = 10,angle = 0)))
  p1<-p1+theme(legend.position = "none")
  ggsave(path = "3.Figures",
         filename = paste0("AHCCD_error/AHCCD_error_trend_pcp_winter_",it_yr,"-2022.png"),
         device = "png",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
}
### table by month###########



stn_err_slope<-
  rbind(data_pcp%>%
          mutate(err = pcp_ANU-pcp_meas)%>%
          dplyr::filter(ID %in%stations$stnid[stations$beg.yr<=1900]&year>=1900)%>%
          dplyr::filter(!is.na(err))%>%
          group_by(ID,month)%>%
          dplyr::summarize(
            err.sen = senFun(err,year),
            err.MK.p = fun_mmkh3lag(err),
            N = n()
          )%>%
          mutate(startYear = 1900)%>%
          filter(N>=30),
        data_pcp%>%
          mutate(err = pcp_ANU-pcp_meas)%>%
          dplyr::filter(ID %in%stations$stnid[stations$beg.yr<=1925]&year>=1925)%>%
          dplyr::filter(!is.na(err))%>%
          group_by(ID,month)%>%
          dplyr::summarize(
            err.sen = senFun(err,year),
            err.MK.p = fun_mmkh3lag(err),
            N = n()
          )%>%
          mutate(startYear = 1925)%>%
          filter(N>=30),
        data_pcp%>%
          mutate(err = pcp_ANU-pcp_meas)%>%
          dplyr::filter(ID %in%stations$stnid[stations$beg.yr<=1950]&year>=1950)%>%
          dplyr::filter(!is.na(err))%>%
          group_by(ID,month)%>%
          dplyr::summarize(
            err.sen = senFun(err,year),
            err.MK.p = fun_mmkh3lag(err),
            N = n()
          )%>%
          mutate(startYear = 1950)%>%
          filter(N>=30),
        
        data_pcp%>%
          mutate(err = pcp_ANU-pcp_meas)%>%
          dplyr::filter(ID %in%stations$stnid[stations$beg.yr<=1975]&year>=1975)%>%
          dplyr::filter(!is.na(err))%>%
          group_by(ID,month)%>%
          dplyr::summarize(
            err.sen = senFun(err,year),
            err.MK.p = fun_mmkh3lag(err),
            N = n()
          )%>%
          mutate(startYear = 1975)%>%
          filter(N>=30)
  )






summaryTable<-
  stn_err_slope%>%
  group_by(startYear,month)%>%
  summarise(
    # percSignif.lm = mean(err.lm.p<0.05),
    #         percPositive.lm = mean(err.lm>0),
    #         percPositive.lm.p = binom.test(sum(err.lm<0),n())$p.val,
    #         med_delta100y.lm = median((exp(err.lm*100))),
    N = n(),
    # percSignif.mk = mean(err.MK.p<0.05),
    numPositive.sen. = sum(err.sen>0),
    sumNegative.sen. = sum(err.sen<0),
    # sum0=sum(err.sen==0),
    
    percPositive.sen.p = binom.test(sum(err.sen<0),n())$p.val
    # med_delta100y.sen = median((exp(err.sen*100)))
  )%>%
  mutate(period = paste(startYear,"2022",sep = "-"))%>%
  ungroup()%>%
  dplyr::select(period,month,N, numPositive.sen., sumNegative.sen., percPositive.sen.p)

roundFunc<-function(x){
  if(x>0.005){
    round(x,2)%>%as.character()
  }else if(x>=0.001){
    round(x,3)%>%as.character()
  }else if(x<0.001){
    "<0.0001"
  }
  
}

summaryTable$percPositive.sen.p_char<-lapply(summaryTable$percPositive.sen.p,FUN = roundFunc)%>%unlist()

prds<-c("1900-2022","1925-2022","1950-2022","1975-2022")
summaryTable2<-data.frame()
for(it_period in 1:4){
  summaryTable_x<-filter(summaryTable,period==prds[it_period])
  
  
  k<-rank(summaryTable_x$percPositive.sen.p, ties.method = "first")
  
  m_prime<-12+1-k
  
  summaryTable_x$T1.sig<-""
  sigFunc<-function(x1,m_prime){
    if(x1<(0.001/m_prime)){return("***")}
    if(x1<(0.01/m_prime)){return("**")}
    if(x1<(0.05/m_prime)){return("*")}
    return("")
  }
  
  for(it in 1:12){
    it_row<-which(k==it)
    summaryTable_x$T1.sig[it_row]<-sigFunc(summaryTable_x$percPositive.sen.p[it_row],m_prime[it_row])
    if(summaryTable_x$T1.sig[it_row]==""){break}
  }
  summaryTable2<-rbind(summaryTable2,summaryTable_x)
  
}


write.csv(summaryTable2%>%select(!percPositive.sen.p),"4.output/AHCCD_error_pcp.csv",row.names = FALSE)

# mostly negative slopes in error means that error is getting more negative, 
# in the past, error was positive - ANUSPLIN overestimates the true precipitation
# at the beginning of the 20th century
# The beginning of the 20th century was actually drier than represented in ANUSPLIN

median(abs(stn_err_slope$err.lm))

median(abs(stn_err_slope$err.lm))*50

stations2<-left_join(stations,stn_err_slope,by = c("stnid"="ID"))
tm_shape(stations2%>%filter(month ==8))+tm_dots(col = "err.sen",breaks = c(-Inf,-0.002,-0.001,0,0.001,0.002,Inf))


tm_shape(stations2%>%filter(month ==8))+tm_dots(col = "err.sen",breaks = c(-Inf,-0.002,-0.001,0,0.001,0.002,Inf))
tm_shape(stations2%>%filter(month ==2))+tm_dots(col = "err.sen",breaks = c(-Inf,-0.002,-0.001,0,0.001,0.002,Inf))

ggplot(data_pcp%>%filter(ID==stations$stnid[1]),aes(x = pcp,y = value))+geom_point()

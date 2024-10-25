# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-25

## This script extracts ANUSPLIN-adjusted precipitation data at AHCCD stations and calculates bias trends

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


studyArea<-st_read("../low-flows-BC/2.data/2.working/CatchmentPolygons/bc_west.gpkg")%>%
  st_transform("EPSG:4326")

stations<-read.csv("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_AHCCD/Precipitation_Stations.csv")
stations$stnid<-stations$stnid%>%str_remove_all(" ")
stations$long..deg.[stations$station.s.name=="ESTEVAN POINT"]<- -126.54
stations$lat..deg.[stations$station.s.name=="ESTEVAN POINT"]<-49.39

stations$long..deg.[stations$station.s.name=="MCINNES ISLAND"]<- -128.676
stations$lat..deg.[stations$station.s.name=="MCINNES ISLAND"]<-52.27

stations$lat..deg.[stations$stnid=="1066481"]<-54.38
stations$lat..deg.[stations$stnid=="3031400"]<-49.01


stations<-stations%>%
  st_as_sf(coords = c("long..deg.","lat..deg."),crs = "EPSG:4326",remove = FALSE)

sum(stations$Prov=="BC")
stations<-st_filter(stations,studyArea%>%st_buffer(50000))#%>%

st_write(stations,"2.data/2.working/AHCCD/stations_pcp.gpkg")
# filter(stnid!="1191440")%>%

pcp<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/1900/pcp_01.asc/pcp_01.asc"))
tmap_mode("view")
tm_shape(pcp,raster.downsample = FALSE)+tm_raster()+
    tm_shape(stations)+tm_dots()+
  tm_mouse_coordinates()

dat<-terra::extract(pcp,stations)
stations[101,
         ]
data_all<-data.frame()
for(it_yr in 1979:2022){
  for(it_mo in 1:12){
    it_mo_00<-str_pad(it_mo,width = 2,side = "left",pad = "0")
    pcp<-terra::rast(paste0("../DATA/1.Spatial_data/regional/North America/clim_climate_precip_aridity_permafrost/clim1_ANUSPLIN_adj/",
                            it_yr,"/pcp_",it_mo_00,".asc/","/pcp_",it_mo_00,".asc"))
    dat<-terra::extract(pcp,stations)
    dat$month<-it_mo
    dat$year<-it_yr
    dat$ID<-stations$stnid
    names(dat)[2]<-"pcp"
    data_all<-rbind(data_all,dat)
    
  }
}
saveRDS(data_all,"2.data/2.working/AHCCD/pcp_adj.RDS")

data_pcp_ANU<-readRDS("2.data/2.working/AHCCD/pcp_adj.RDS")%>%dplyr::rename(pcp_ANU = pcp)

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


data_pcp<-left_join(data_pcp_ANU,data_all_meas)

data_pcp$pcp_meas[data_pcp$pcp_meas==-9999.9]<-NA

## Annual #########

# data_pcp$err<-log(data_pcp$pcp_ANU/data_pcp$pcp_meas)

data_pcp_yrly<-data_pcp%>%
  group_by(ID,year)%>%
  dplyr::summarize(err = log(sum(pcp_ANU)/sum(pcp_meas)))

saveRDS(data_pcp_yrly,"2.data/2.working/AHCCD/PcpYrly_error_ANUSPLIN-adjusted.RDS")

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

tmap_mode("view")
tm_shape(stations%>%filter(beg.yr<1925))+tm_dots()

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2022")
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
  scale_fill_manual(name = "Bias trend\n(% per decade)",
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
  scale_x_discrete(labels = fctrLabs,name = "Bias trend (%per decade)", drop = FALSE,)+
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
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcpADJ_annual_bar.png",width = 5,height = 3.099186*2,p_bar)
ggsave("3.figures/AHCCD_error/AHCCD_error_trend_pcpADJ_annual_bar.svg",width = 5,height = 3.099186*2,p_bar)


# maps


library(bcmaps)
library(ggpattern)
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
         filename = paste0("AHCCD_error/AHCCD_error_ANUSPLIN_adjpcp_trend_pcp_annual_",it_yr,"-2022.png"),
         device = "png",
         p1,
         bg = "white",
         dpi =600,width = 7.5/2,height = 3.099186)
  
}


## station error plots
lblr<-function(x){plyr::mapvalues(x,
                                  from = c("1","2"),
                                  to = c("Temperature (°C)",
                                         "Bias (°C)"))}

sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

ID_it<-"1020270"

dat_plot<-data_pcp%>%filter(ID==ID_it)%>%
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
  ggtitle(paste(stations$station.s.name[stations$stnid==ID_it]))+
  theme(strip.placement = "outside",
        strip.background = element_blank())




## ### table by month###########



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


write.csv(summaryTable2%>%select(!percPositive.sen.p),"4.output/AHCCD_error_pcp_adjusted.csv",row.names = FALSE)

stn_err_slope2%>%
  group_by(startYear)%>%
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
  dplyr::select(period,N, numPositive.sen., sumNegative.sen., percPositive.sen.p)

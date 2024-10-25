# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-10-25

## This script compares bias trends in four gridded datasets to AHCCD station data (Figures G9 and G10)



### temp ######
stations<-st_read("2.data/2.working/AHCCD/stations_temp.gpkg")

data_temp<-
  rbind(
    # readRDS("2.data/2.working/AHCCD/PcpYrly_error_ANUSPLIN-adjusted.RDS")%>%mutate(source = "ANUSPLIN-adjusted"),
    readRDS("2.data/2.working/AHCCD/Temp_error_ANUSPLIN.RDS")%>%select(ID,year,month,err)%>%mutate(source = "ANUSPLIN"),
    readRDS("2.data/2.working/AHCCD/Temp_error_ERA5-Land.RDS")%>%select(ID,year,month,err)%>%mutate(source = "ERA5-Land"),
    readRDS("2.data/2.working/AHCCD/Temp_error_PNWNAmet.RDS")%>%select(ID,year,month,err)%>%mutate(source = "PNWNAmet")
  )


data_temp_yrly<-data_temp%>%
  group_by(ID,year,source)%>%
  dplyr::summarize(err = mean(err))

stn_err_slope2<-
  rbind(
    
    data_temp_yrly%>%
      filter(ID %in%stations$StnId[stations$From<=1950]&year%in%(1950:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1950),
    data_temp_yrly%>%
      filter(ID %in%stations$StnId[stations$From<=1975]&year%in%(1975:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1975)
  )  


stn_err_slope2<-filter(stn_err_slope2,N>=30)
stn_err_slope2$err.sen.decade<-stn_err_slope2$err.sen*10

stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2012")
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
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  geom_vline(xintercept = 4.5,alpha=0.25)+
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
  scale_fill_manual(name = "Bias trend\n(°C per decade)",
                    drop = FALSE,
                    limits = fctrLvls%>%rev(),
                    labels = fctrLabs%>%rev(),
                    guide = "none",
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = RColorBrewer::brewer.pal(8,"RdBu"))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 12)+
  scale_x_discrete(labels = fctrLabs,name = "Bias trend (°C per decade)", drop = FALSE,)+
  facet_grid(rows = vars(source),cols = vars(label))+
  theme(
    legend.position = "bottom",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 9),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(#fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "left",order = 2,
                                override.aes = list( pattern_density = 0.3,
                                                     size = 0.1,
                                                     linewidth = 0.1,
                                                     pattern_spacing = 0.01,
                                                     pattern_fill = "grey80",
                                                     fill = "white",
                                                     colour = "grey30",
                                                     pattern_res = 600),
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 10,angle = 0)))

ggsave("3.Figures/AHCCD_error/AHCCD_error_trend_temp_annual_ALL_bar.png",width = 6,height = 3.099186*2,p_bar)


### precip########

stations<-st_read("2.data/2.working/AHCCD/stations_pcp.gpkg")

data_pcp_yrly<-
  rbind(
    readRDS("2.data/2.working/AHCCD/PcpYrly_error_ANUSPLIN-adjusted.RDS")%>%mutate(source = "ANUSPLIN-adjusted"),
    readRDS("2.data/2.working/AHCCD/PcpYrly_error_ANUSPLIN.RDS")%>%mutate(source = "ANUSPLIN"),
    readRDS("2.data/2.working/AHCCD/PcpYrly_error_ERA5-Land.RDS")%>%mutate(source = "ERA5-Land"),
    readRDS("2.data/2.working/AHCCD/PcpYrly_error_PNWNAmet.RDS")%>%mutate(source = "PNWNAmet")
  )


senFun<-function(err,WaterYear){
  if(length(err)>2){
    dat<-data.frame(err = err,
                    WaterYear = WaterYear)
    
    return(zyp.sen(err~WaterYear, dat)$coefficients[["WaterYear"]])
  }else{NA}
  
  
}

library(zyp)

fun_mmkh3lag<-function(x){if(length(x)>2){ MannKendall(x)[2]}else{NA}}
fun_lm1<-function(x,y){if(length(x)>2){ lm(y~x)$coefficients[2]}else{NA}}
fun_lm2<-function(x,y){if(length(x)>2){ (lm(y~x)%>%summary())$coefficients[2,4]}else{NA}}
stn_err_slope2<-
  rbind(
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1900]&year%in%(1900:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1900),
    
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1925]&year%in%(1925:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p =fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p =fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1925),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1950]&year%in%(1950:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1950),
    data_pcp_yrly%>%
      filter(ID %in%stations$stnid[stations$beg.yr<=1975]&year%in%(1975:2012))%>%
      filter(!is.na(err))%>%
      group_by(ID,source)%>%
      dplyr::summarize(
        err.sen = senFun(err,year),
        err.MK.p = fun_mmkh3lag(err),
        err.lm = fun_lm1(year,err),
        err.lm.p = fun_lm2(year,err),
        N = n(),
        N_noNA = sum(!is.na(err))
      )%>%mutate(startYear = 1975)
  )  


stn_err_slope2<-filter(stn_err_slope2,N>=30)
stn_err_slope2<-filter(stn_err_slope2,!(startYear%in%c(1900,1925)&source %in%c("ERA5-Land","PNWNAmet")))

stn_err_slope2%>%
  group_by(startYear,source)%>%
  summarise(sum(abs(err.sen)>0.005))
  

N_stns<-stn_err_slope2%>%group_by(ID,startYear)%>%
  summarise(N=n())%>%
  filter(N==4|(N==2&startYear%in%c(1900,1925)))
stn_err_slope2$err.sen.decade<-exp(stn_err_slope2$err.sen*10)*100-100

ggplot(stn_err_slope2,aes(x = err.sen.decade))+geom_histogram()+facet_wrap("startYear")

stn_err_slope2%>%
  group_by(startYear,source)%>%
  summarise(mean(abs(err.sen.decade)>2))



stn_err_slope2$label<-paste(stn_err_slope2$startYear,"- 2012")
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
stn_err_slope2$source_fct<-factor(stn_err_slope2$source,
                                  levels = c("ANUSPLIN","ANUSPLIN-adjusted","ERA5-Land","PNWNAmet"),
                                  labels = c("ANUSPLIN","ANUSPLIN\nw/ Adjusted\nPrecipitation","ERA5-Land","PNWNAmet"))


library(ggpattern)
library(ggplot2)
p_bar<-
  ggplot(stn_err_slope2,aes(x = err.sen.decade_factor,fill = err.sen.decade_factor,
                            pattern = err.MK.p<0.05))+
  geom_vline(xintercept = 6.5,alpha = 0.25)+
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
                    guide = "none",
                    # values =c("grey90", scico(10,palette = "vik",direction = -1),"grey90")%>%rev(),
                    # values =scico(10,palette = "vik",direction = -1)%>%rev(),
                    values = colorRampPalette(RColorBrewer::brewer.pal(11,"BrBG")%>%rev())(12))+
  scale_pattern_manual(values = c("none","stripe"),
                       labels = c("p<0.05","p\u22650.05"),
                       breaks = c(TRUE,FALSE),
                       name = "statistical\nsignificance")+
  # scale_x_continuous(limits = c(-0.1,0.1))+
  theme_bw(base_size = 9)+
  scale_x_discrete(labels = fctrLabs,name = "Bias trend (% per decade)", drop = FALSE,)+
  facet_grid(cols = vars(label),rows = vars(source_fct))+
  theme(
    legend.position = "bottom",
    plot.background = NULL,
    # axis.text = element_text(size = 5),
    axis.text.x = element_text(angle = 45,hjust=1),
    # axis.title = element_text(size = 6),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12),
    
    
    text = element_text(size = 9),
    legend.background = element_rect(fill = "white",colour = "grey50"),
    legend.margin=ggplot2::margin(t = 2,r = 2,b = 2,l = 2,unit = "pt"),
    legend.key.size = unit(12,"pt"),
    legend.box.just = "right") +
  
  guides(
    #fill = guide_legend(title.position = "top",order = 1,override.aes=list(pattern = "none")),
         # col = guide_legend(title.position = "left",order = 1,override.aes=list(pattern = "none")),
         pattern = guide_legend(title.position = "left",order = 2,
                                override.aes = list( pattern_density = 0.3,
                                                     size = 0.1,
                                                     linewidth = 0.1,
                                                     pattern_spacing = 0.01,
                                                     pattern_fill = "grey80",
                                                     fill = "white",
                                                     colour = "grey30",
                                                     pattern_res = 600),
                                title.theme = element_text(face = "bold",hjust = 0.5,size = 10,angle = 0)))
ggsave("3.Figures/AHCCD_error/AHCCD_error_trend_pcp_annual_ALL_bar.png",width = 8,height = 3.099186*2,p_bar)

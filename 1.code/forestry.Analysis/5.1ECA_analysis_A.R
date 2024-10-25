# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script runs routine 'A' of the ECA analysis (Appendix E)



closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(sf)
library(tmap)
library(lubridate)
library(bcmaps)
library(stringr)
library(terra)
library(cowplot)
library(scico)
library(grwat)
library(reshape2)
library(ggplot2)
library(ggpattern)
library(lm.beta)
library(tidyr)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")

# Load Station metadata
stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

# Load monthly weather data
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")


streamDataMonthly$NovWaterYear<-streamDataMonthly$year
streamDataMonthly$NovWaterYear[streamDataMonthly$month%in%c(11,12)]<-
  streamDataMonthly$year[streamDataMonthly$month%in%c(11,12)]+1

### ECA  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")



sum(stations$PrivateForestry<0.1)
sum(stations$PrivateForestry<0.1&stations$Total.Cut.Area>0.1)
## Loop through stations and extract variables ################


watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")%>%
    st_transform(st_crs("EPSG:3005"))

watersheds$Area_km2<-as.numeric(st_area(watersheds)/10^6)
stations<-left_join(stations%>%select(!Area_km2),
                    watersheds%>%select(ID,Area_km2),
                    by = c("ID"))





# Zhang and Wei 2012 analysis ###########
# 08KE016


library("tseries")
library(forecast)
stnRs<-data.frame()
stns_with_brks<-c()
sum(stations$PrivateForestry>0.1)


stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)


for(it_stn in 1:nrow(stations)){
  # it_stn = which(stations$ID=="08HA003")
  if(stations$PrivateForestry[it_stn]>0.1|stations$Total.Cut.Area[it_stn]<=0.1){next}
  ECA_x<-filter(ECA,StationNum%in%stations$ID[it_stn])
  
 
  
  streamDataYrly<-streamDataMonthly%>%
    dplyr::filter(ID==stations$ID[it_stn]&
                    month%in%(stations$month_bgn:stations$month_end))%>%
    dplyr::filter(NovWaterYear>=1950)%>%
    group_by(NovWaterYear)%>%
    dplyr::summarize(minSumFlow7 = min(minMonFlow7))
  
  
  
  streamDataYrly<-left_join(streamDataYrly,ECA_x,by = c("NovWaterYear" = "year"))
  streamDataYrly<-dplyr::filter(streamDataYrly,!is.na(minSumFlow7)&!is.na(ECA_60))
  if(!all((streamDataYrly$NovWaterYear[2:nrow(streamDataYrly)]-
           streamDataYrly$NovWaterYear[1:(nrow(streamDataYrly)-1)])==1)){
    print(stations$ID[it_stn])
    stns_with_brks = c(stns_with_brks,stations$ID[it_stn])
  }
  
  
  
  # fitECA60<-auto.arima(streamDataYrly$ECA_60,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # 
  # 
  # fitECA20<-auto.arima(streamDataYrly$ECA_20,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA10<-auto.arima(streamDataYrly$ECA_10,trace = TRUE,
  #                      max.p = 1,max.d=1,max.q = 0,
  #                      allowdrift = FALSE,
  #                      stepwise=FALSE , approximation=FALSE)
  # fitECA5<-auto.arima(streamDataYrly$ECA_5,trace = TRUE,
  #                     max.p = 1,max.d=1,max.q = 0,
  #                     allowdrift = FALSE,
  #                     stepwise=FALSE , approximation=FALSE)
  
  fitECA60<-auto.arima(streamDataYrly$ECA_60)
  fitECA20<-auto.arima(streamDataYrly$ECA_20)
  fitECA10<-auto.arima(streamDataYrly$ECA_10)
  fitECA5<-auto.arima(streamDataYrly$ECA_5)
  
  
  
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  logQ7<-auto.arima(log(streamDataYrly$minSumFlow7))
  
  
  # logQ7.60<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA60$coef,fitECA60$coef),sides = 1)
  # logQ7.20<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA20$coef,fitECA20$coef),sides = 1)
  # logQ7.10<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA10$coef,fitECA10$coef),sides = 1)
  # logQ7.5<-stats::filter(log(streamDataYrly$minSumFlow7),filter = c(1,-1-fitECA5$coef,fitECA5$coef),sides = 1)
  
  
  # ccf(fitECA60$residuals,logQ7.60,na.action=na.omit,lag.max=10 )
  # ccf(fitECA20$residuals,logQ7.20,na.action=na.omit)
  # ccf(fitECA10$residuals,logQ7.10,na.action=na.omit)
  # ccf(fitECA5$residuals,logQ7.5,na.action=na.omit)
  
  # 
  # acf(streamDataYrly$ECA_60)
  # pacf(streamDataYrly$ECA_60)
  # diff1x=diff(streamDataYrly$ECA_60,1)
  # acf(diff1x)
  # pacf(diff1x)
  # plot(logQ7.60)
  c0.60<-cor.test(logQ7$residuals,fitECA60$residuals,method = "spearman")
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  c1.60<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA60$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.60<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA60$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  c0.20<-cor.test(logQ7$residuals,fitECA20$residuals,method = "spearman")
  
  c1.20<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA20$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.20<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA20$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  
  
  c0.10<-cor.test(logQ7$residuals,fitECA10$residuals,method = "spearman")
  
  c1.10<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                  fitECA10$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.10<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                  fitECA10$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  
  c0.5<-cor.test(logQ7$residuals,fitECA5$residuals,method = "spearman")
  
  c1.5<-cor.test(logQ7$residuals[2:length(logQ7$residuals)],
                 fitECA5$residuals[1:(length(logQ7$residuals)-1)],method = "spearman")
  c2.5<-cor.test(logQ7$residuals[3:length(logQ7$residuals)],
                 fitECA5$residuals[1:(length(logQ7$residuals)-2)],method = "spearman")
  
  
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA60$residuals)-2)])
  # 
  # 
  # c1.20<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA60$residuals)-1)])
  # 
  # c2.20<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA20$residuals[1:(length(fitECA$residuals)-2)])
  # 
  # c0.60<-cor.test(fitQ$residuals,fitECA60$residuals)
  # 
  # c1.60<-cor.test(fitQ$residuals[2:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-1)])
  # 
  # c2.60<-cor.test(fitQ$residuals[3:length(fitQ$residuals)],
  #                 fitECA60$residuals[1:(length(fitECA$residuals)-2)])
  stnRs<-rbind(stnRs,
               data.frame(
                 ID = stations$ID[it_stn],
                 regime = stations$regime[it_stn],
                 c0.60 =c0.60$estimate,
                 c0.60.p =c0.60$p.value,
                 c0.20 =c0.20$estimate,
                 c0.20.p =c0.20$p.value,
                 c0.10 =c0.10$estimate,
                 c0.10.p =c0.10$p.value,
                 c0.5 =c0.5$estimate,
                 c0.5.p =c0.5$p.value,
                 
                 
                 c1.60 =c1.60$estimate,
                 c1.60.p =c1.60$p.value,
                 c1.20 =c1.20$estimate,
                 c1.20.p =c1.20$p.value,
                 c1.10 =c1.10$estimate,
                 c1.10.p =c1.10$p.value,
                 c1.5 =c1.5$estimate,
                 c1.5.p =c1.5$p.value,
                 
                 
                 c2.60 =c2.60$estimate,
                 c2.60.p =c2.60$p.value,
                 c2.20 =c2.20$estimate,
                 c2.20.p =c2.20$p.value,
                 c2.10 =c2.10$estimate,
                 c2.10.p =c2.10$p.value,
                 c2.5 =c2.5$estimate,
                 c2.5.p =c2.5$p.value
                 
                 # c2.60 =c2$estimate,
                 # c2.60.p =c2$p.value
               ))
  
}

# stnRs$maxR<-
sum(stnRs$c0.60>0)

# stnRs<-left_join(watersheds,stnRs,by = c( "StationNum" = "ID"))

stnRs<-left_join(stnRs,stations)
stnRs<-filter(stnRs,!is.na(c0.60))

saveRDS(stnRs,"4.output/ECA_analysis_A_stnRs.rds")
stnRs<-readRDS("4.output/ECA_analysis_A_stnRs.rds")

# tmap_mode("view")
# tm_shape(stnRs%>%filter(!is.na(c0.60)))+tm_dots(col = "c0.60")
# 
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Area_km2,y = c0.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)+
#   scale_x_log10()
# 
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c1.60))+geom_point(aes(color = c0.60.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.10))+geom_point(aes(color = c0.10.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# ggplot(stnRs,aes(x = Total.Cut.Area,y = c0.5))+geom_point(aes(color = c0.5.p<0.05))+
#   facet_wrap(facets = "regime",ncol = 1)+
#   geom_hline(yintercept = 0)
# 

stnRs_long<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c0.60:c0.5.p)%>%
  pivot_longer(cols = c0.60:c0.5.p)
stnRs_long$meas<-str_detect(stnRs_long$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long$name<-str_remove(stnRs_long$name,"\\.p")
stnRs_long<-pivot_wider(stnRs_long,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


# stnRs_long<-filter(stnRs_long,!regime=="Glacial")
stnRs_long$regime<-factor(stnRs_long$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                          labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))
stnRs_long$name<-factor(stnRs_long$name,levels = c("c0.60","c0.20","c0.10","c0.5"),
                        labels = c("ECA (60 year)",
                                   "ECA (20 year)",
                                   "ECA (10 year)",
                                   "ECA (5 year)"
                        )
)

ggplot(stnRs_long,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black",formula=y~0+x)+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r",
                     limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())

ggsave("3.figures/ECA_vs_TotalCut_A.png",width =7,height = 4)

stnRs_long%>%
  group_by(regime,name)%>%
  dplyr::summarize(H1.p.pos =  binom.test(sum(p.val<0.05&estimate>0),n(),p=0.05,alternative = "greater")$p.value,
            H1.p.neg =  binom.test(sum(p.val<0.05&estimate<0),n(),p=0.05,alternative = "greater")$p.value,
            H2.perc =  round(sum(estimate>0)/n(),2)*100,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,1],
            H3.p = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,4],
            n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       digits = 2,
                       out = "4.output/ECA_analysis_A.doc")



stnRs_long_lag1<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c1.60:c1.5.p)%>%
  pivot_longer(cols = c1.60:c1.5.p)
stnRs_long_lag1$meas<-str_detect(stnRs_long_lag1$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag1$name<-str_remove(stnRs_long_lag1$name,"\\.p")
stnRs_long_lag1<-pivot_wider(stnRs_long_lag1,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


# stnRs_long_lag1<-dplyr::filter(stnRs_long_lag1,!regime=="Glacial")
stnRs_long_lag1$regime<-factor(stnRs_long_lag1$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                               labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))
stnRs_long_lag1$name<-factor(stnRs_long_lag1$name,levels = c("c1.60","c1.20","c1.10","c1.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag1%>%
  group_by(regime,name)%>%
  dplyr::summarize(H1.p.pos =  binom.test(sum(p.val<0.05&estimate>0),n(),p=0.05,alternative = "greater")$p.value,
                   H1.p.neg =  binom.test(sum(p.val<0.05&estimate<0),n(),p=0.05,alternative = "greater")$p.value,
                   H2.perc =  round(sum(estimate>0)/n(),2)*100,
                   H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
                   H3.b = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,1],
                   H3.p = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,4],
            n = n())

ggplot(stnRs_long_lag1,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black",formula=y~0+x)+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r"
                     # limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut_A_lag1.png",width =7,height = 4)



stnRs_long_lag2<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c2.60:c2.5.p)%>%
  pivot_longer(cols = c2.60:c2.5.p)
stnRs_long_lag2$meas<-str_detect(stnRs_long_lag2$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long_lag2$name<-str_remove(stnRs_long_lag2$name,"\\.p")
stnRs_long_lag2<-pivot_wider(stnRs_long_lag2,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


# stnRs_long_lag2<-filter(stnRs_long_lag2,!regime=="Glacial")
stnRs_long_lag2$regime<-factor(stnRs_long_lag2$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                               labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))
stnRs_long_lag2$name<-factor(stnRs_long_lag2$name,levels = c("c2.60","c2.20","c2.10","c2.5"),
                             labels = c("ECA (60 year)",
                                        "ECA (20 year)",
                                        "ECA (10 year)",
                                        "ECA (5 year)"
                             )
)


stnRs_long_lag2%>%
  group_by(regime,name)%>%
  dplyr::summarize(H1.p =  binom.test(sum(p.val<0.05),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,1],
            H3.p = summary(lm(estimate~Total.Cut.Area-1))$coefficients[1,4],
            n = n())


ggplot(stnRs_long_lag2,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black",formula=y~0+x)+
  geom_hline(yintercept = 0,col = "grey25")+
  facet_grid(rows = vars(regime),
             cols = vars(name))+
  scale_x_continuous(name = "Fraction Harvested or Burned since 1900")+
  scale_y_continuous(name = "Spearman r",
                     limits = c(-0.505,0.505)
  )+
  scale_colour_manual( values = c("grey","red"),
                       labels = c("p≥0.05","p<0.05"),
                       name = "")+
  scale_linetype_manual(name = "",values = "solid")+
  theme_bw()+
  theme(legend.background = element_rect(color = "black"),
        legend.title=element_blank())
ggsave("3.figures/ECA_vs_TotalCut_A_lag2.png",width =7,height = 4)


# do warmer catchments see more positive effects?
Clim<-WeatherData%>%
  group_by(ID)%>%
  dplyr::summarize(meanAnnualTemp = mean(Mean.Temp..C.))

stnRs_long<-left_join(stnRs_long,Clim)

stnRs_long%>%
  group_by(regime,name)%>%
  dplyr::summarize(
    H4.b = summary(lm(estimate~meanAnnualTemp))$coefficients[2,1],
    H4.p = summary(lm(estimate~meanAnnualTemp))$coefficients[2,4],
    n = n())

## what if I try using model residuals?
closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(tidyr)

library("tseries")
library(forecast)

setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

# Load streamflow data
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly_2.RDS")

# Load Station metadata
stations<-readRDS("2.data/2.working/StationMetadata/stations_final_2.RDS")

# Load catchment polygons
watersheds <- st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")

# Load monthly weather data
WeatherData<-read.csv("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.csv")%>%
  filter(StationNum%in%unique(streamDataMonthly$ID))

streamDataMonthly$NovWaterYear<-streamDataMonthly$year
streamDataMonthly$NovWaterYear[streamDataMonthly$month%in%c(11,12)]<-
  streamDataMonthly$year[streamDataMonthly$month%in%c(11,12)]+1

### ECA  
ECA<-readRDS("2.data/2.working/ECA/ECA.rds")



x<-rast("../DATA/1.Spatial_data/regional/Canada/lulc_landuse_landcover/lulc1_ForestManagement/Canada_MFv2020.tif")

maskPrivate<-x==50

plot(maskPrivate)
bc<-bcmaps::bc_neighbours()%>%filter(name=="British Columbia")%>%
  vect()%>%
  project(crs(maskPrivate))
maskPrivate_bc<-terra::mask(maskPrivate,bc)
summary(maskPrivate_bc)
global(maskPrivate_bc,mean,na.rm = TRUE)
plot(maskPrivate_bc)

# maskPrivateBC<-crop(maskPrivate)

watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final_2.gpkg")%>%
  st_transform(st_crs(maskPrivate))

watersheds_Private<-terra::extract(maskPrivate,watersheds,fun = mean)
watersheds<-cbind(watersheds,watersheds_Private[,"Canada_MFv2020"])
watersheds$PrivateForestry<-watersheds$watersheds_Private....Canada_MFv2020..
stations<-left_join(stations,st_drop_geometry(watersheds[,c("ID","PrivateForestry")]),by = c("ID"))

TCA<-readRDS("2.data/2.working/ECA/TCA_2022.rds")
names(TCA)[3]<-"Total.Cut.Area"
stations<-left_join(stations,TCA%>%select(StationNum,Total.Cut.Area),by = c("ID" = "StationNum"))

sum(stations$PrivateForestry<0.1)
sum(stations$PrivateForestry<0.1&stations$Total.Cut.Area>0.1)




stn_MC<-stations%>%
  subset(PrivateForestry<=0.1&Total.Cut.Area>.1)%>%
  select(ID,regime,Area_km2,Total.Cut.Area)

stn_MC<-stn_MC[order(stn_MC$ID),]


# stns_with_brks<-c()

stn_MC_ls<-split(stn_MC,stn_MC$ID)

ECA<-ECA%>%
  dplyr::filter(StationNum%in%stn_MC$ID)
ECA<-ECA[order(ECA$StationNum),]
ECA_ls<-split(ECA,ECA$StationNum)




stations$month_bgn<-pmax(stations$minSumFlowMonth-1,stations$SDD)

stations$month_end<-pmin(stations$minSumFlowMonth+1,stations$SAD)



streamDataMonthly<-left_join(streamDataMonthly,stations[,c("ID","month_bgn","month_end")])

streamDataMonthly<-streamDataMonthly%>%
  filter(year>=1950&month%in% month_bgn:month_end)%>%
  group_by(ID,NovWaterYear)%>%
  dplyr::summarize(minSumFlow7 = min(minMonFlow7))


streamDataMonthly<-left_join(streamDataMonthly,ECA,by = c("NovWaterYear" = "year","ID" = "StationNum"))
streamDataMonthly<-dplyr::filter(streamDataMonthly,!is.na(minSumFlow7)&!is.na(ECA_60)&
                                   ID%in%stn_MC$ID)

streamDataMonthly<-streamDataMonthly[order(streamDataMonthly$ID),]
streamDataMonthly_ls<-split(streamDataMonthly,streamDataMonthly$ID)

WeatherData<-WeatherData[(WeatherData$StationNum%in%stn_MC$ID),]
WeatherData<-WeatherData[order(WeatherData$StationNum),]
WeatherData_ls<-split(WeatherData,WeatherData$StationNum )

mdlFiles<-list.files("2.data/2.working/RegressionOptimization/BestModels/",
                     pattern = "step2_lm_.*rds")
bestModels<-lapply(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",
                          stn_MC$ID,".rds"),
                   readRDS)
names(bestModels)<-stn_MC$ID
  
  

library(foreach)
library(doParallel)
# registerDoParallel(cores=6)

# cl <- makeCluster(22)
#Register cluster
# registerDoParallel(cl)
stnRs<-data.frame()
stn_MC_result<-foreach(stn_MC_x = stn_MC_ls,
                       streamDataYrly = streamDataMonthly_ls,
                       data_x = WeatherData_ls,
                       bestModel_x = bestModels,
                       # fctrs = fctrs,
                       .packages=c("dplyr","forecast")
)%do%{
  # stn_MC_x = stn_MC_ls[[1]]
  # streamDataYrly = streamDataMonthly_ls[[1]]
  # data_x = WeatherData_ls[[1]]
  # bestModel_x = bestModels[[1]]
  
 
  month_rng<-lapply(bestModel_x,function(x){!is.null(x)})%>%unlist()%>%which()
  
  
  dat_predict<-data.frame()
  
  for(it_mn in month_rng){
    
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
        Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
        Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
        Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)]),
        Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)]),
        Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)]),
        Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)]),
        Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)]),
        Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)]),
        
        
        
      )
    dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
                             !is.na(dataYearly$Temp_1),]
    
    Div<-read.csv(paste0("2.data/2.working/WaterUse/estimates/",stn_MC_x$ID,".csv"))
    Div<-Div[,c("year","Total.cms","August.cms","September.cms","October.cms")]
    
    dataYearly<-left_join(dataYearly,Div,by = c("WaterYear"= "year"))
    
    dat_predict<-rbind(dat_predict,
      data.frame(year = dataYearly$WaterYear,
                 month = it_mn,
                 predMin = predict(bestModel_x[[it_mn]],newdata = dataYearly)))
    
    
  }
  
  
  predictMins<-dat_predict%>%
    group_by(year)%>%
    dplyr::summarise(predMin = (min(predMin)))
  dataYearly2<-left_join(streamDataYrly,predictMins,by = c("NovWaterYear" = "year"))
  
  resids<-log(dataYearly2$minSumFlow7)-dataYearly2$predMin
  
  fitQ7<- auto.arima(resids)
  
  
  fitECA5<- auto.arima(dataYearly2$ECA_5)
  fitECA10<-auto.arima(dataYearly2$ECA_10)
  fitECA20<-auto.arima(dataYearly2$ECA_20)
  fitECA60<-auto.arima(dataYearly2$ECA_60)
  

  fitECA60<-auto.arima(dataYearly2$ECA_60)
  # fitECA5<-sarima(streamDataYrly$ECA_5,1,1,0)
  
  c0.60<-cor.test(fitQ7$residuals,fitECA60$residuals,method = "spearman")
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  c1.60<-cor.test(fitQ7$residuals[2:length(fitQ7$residuals)],
                  fitECA60$residuals[1:(length(fitQ7$residuals)-1)],method = "spearman")
  c2.60<-cor.test(fitQ7$residuals[3:length(fitQ7$residuals)],
                  fitECA60$residuals[1:(length(fitQ7$residuals)-2)],method = "spearman")
  
  c0.20<-cor.test(fitQ7$residuals,fitECA20$residuals,method = "spearman")
  
  c1.20<-cor.test(fitQ7$residuals[2:length(fitQ7$residuals)],
                  fitECA20$residuals[1:(length(fitQ7$residuals)-1)],method = "spearman")
  c2.20<-cor.test(fitQ7$residuals[3:length(fitQ7$residuals)],
                  fitECA20$residuals[1:(length(fitQ7$residuals)-2)],method = "spearman")
  
  
  # ccf(logQ7.60,fitECA60$residuals,na.action=na.omit)
  
  
  c0.10<-cor.test(fitQ7$residuals,fitECA10$residuals,method = "spearman")
  
  c1.10<-cor.test(fitQ7$residuals[2:length(fitQ7$residuals)],
                  fitECA10$residuals[1:(length(fitQ7$residuals)-1)],method = "spearman")
  c2.10<-cor.test(fitQ7$residuals[3:length(fitQ7$residuals)],
                  fitECA10$residuals[1:(length(fitQ7$residuals)-2)],method = "spearman")
  
  
  
  c0.5<-cor.test(fitQ7$residuals,fitECA5$residuals,method = "spearman")
  
  c1.5<-cor.test(fitQ7$residuals[2:length(fitQ7$residuals)],
                 fitECA5$residuals[1:(length(fitQ7$residuals)-1)],method = "spearman")
  c2.5<-cor.test(fitQ7$residuals[3:length(fitQ7$residuals)],
                 fitECA5$residuals[1:(length(fitQ7$residuals)-2)],method = "spearman")
  
  
  stnRs<-rbind(stnRs,
               data.frame(
                 ID = stn_MC_x$ID,
                 regime = stn_MC_x$regime,
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
  # plot(streamDataYrly$ECA_5)
  # plot(residuals(fitECA5))
  
  # ccf((streamDataYrly$ECA_5),log(streamDataYrly$minSumFlow7))
  
}
  

# stnRs<-left_join(watersheds,stnRs,by = c( "StationNum" = "ID"))

stnRs<-left_join(stnRs,stations)
stnRs<-filter(stnRs,!is.na(c0.60))

saveRDS(stnRs,"4.output/ECA_analysis_B_stnRs.rds")
stnRs<-readRDS("4.output/ECA_analysis_B_stnRs.rds")
#
stnRs_long<-stnRs%>%
  st_drop_geometry()%>%
  select(ID,regime,Total.Cut.Area,Area_km2,c0.60:c0.5.p)%>%
  pivot_longer(cols = c0.60:c0.5.p)
stnRs_long$meas<-str_detect(stnRs_long$name,"\\.p")%>%plyr::mapvalues(from = c(FALSE,TRUE),to = c("estimate","p.val"))
stnRs_long$name<-str_remove(stnRs_long$name,"\\.p")
stnRs_long<-pivot_wider(stnRs_long,id_cols = c(ID,regime,Total.Cut.Area,Area_km2,name),names_from = meas,values_from = value)


stnRs_long<-filter(stnRs_long,!regime=="Glacial")
stnRs_long$regime<-factor(stnRs_long$regime,levels = c("Rainfall","Hybrid","Snowfall"),
                          labels = c("Rainfall","Hybrid","Snowmelt"))
stnRs_long$name<-factor(stnRs_long$name,levels = c("c0.60","c0.20","c0.10","c0.5"),
                        labels = c("ECA (60 year)",
                                   "ECA (20 year)",
                                   "ECA (10 year)",
                                   "ECA (5 year)"
                        )
)

ggplot(stnRs_long,aes(x = Total.Cut.Area,y = estimate))+geom_point(aes(color = p.val<0.05))+
  facet_wrap(facets = "regime",ncol = 1)+
  geom_smooth(method = "lm",se = FALSE,aes(linetype = "best fit"),col = "black")+
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
ggsave("3.figures/ECA_vs_TotalCut_B.png",width =7,height = 4)



stnRs_long%>%
  group_by(regime,name)%>%
  dplyr::summarize(H1.p.pos =  binom.test(sum(p.val<0.05&estimate>0),n(),p=0.05,alternative = "greater")$p.value,
            H1.p.neg =  binom.test(sum(p.val<0.05&estimate<0),n(),p=0.05,alternative = "greater")$p.value,
            H2.p =  binom.test(sum(estimate>0),n(),p=0.5,alternative = "two.sided")$p.value,
            H3.b = summary(lm(estimate~Total.Cut.Area))$coefficients[2,1],
            H3.p = summary(lm(estimate~Total.Cut.Area))$coefficients[2,4],
            n = n())%>%
  data.frame()%>%
  stargazer::stargazer(type = "html",
                       summary = FALSE,
                       digits = 2,
                       out = "4.output/ECA_analysis_B.doc")


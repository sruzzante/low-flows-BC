# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script hindcasts low flows, droughts, e-flow violations, and low flow anomalies from 1901-2022. It generates Figures 5 and 6 in the manuscript

closeAllConnections()
rm(list=ls())
graphics.off()

library(dplyr)
library(sf)
library(tmap)
# library(lmSupport)
library(car)
library(lubridate)
library(ggplot2)
# library(bcdata)
library(bcmaps)
library(stringr)
library(terra)
library(cowplot)
library(scico)
library(scales)
library(tictoc)
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

stations<-readRDS("2.data/2.working/StationMetadata/stations_final.RDS")

streamDataAll<-readRDS("2.data/2.working/Discharge/streamDataFinal.rds")
streamDataMonthly<-readRDS("2.data/2.working/Discharge/streamDataMonthly.RDS")
watersheds<-st_read("2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")
WeatherData<-readRDS("2.data/2.working/WeatherDataANUSPLIN/dataMonthly.RDS")

# load("2.data/2.working/RegressionOptimization/RegressionOptimization_step2.RData")

Div<-lapply(paste0("2.data/2.working/WaterUse/estimates/",stations$ID,".csv"),
            read.csv)
for(it in 1:nrow(stations)){
  Div[[it]]$ID<-stations$ID[it]
}
Div<-Div%>%
  bind_rows()
Div<-Div[,c("ID","year","Total.cms","August.cms","September.cms","October.cms")]


WeatherData<-left_join(WeatherData,Div,by = c("ID","Year"= "year"))




pcic_models<-list.files("2.data/1.original/PCIC_station_hydro_model_out/",pattern = ".ascii")%>%str_remove(".ascii")
pcic_models[!(tolower(pcic_models)%in%tolower(stations$Station.Name))]
pcic_models<-pcic_models[tolower(pcic_models)%in%tolower(stations$Station.Name)]


findMin<-function(x){
  minInd<-which.min(x)
  if(length(minInd)==0){minInd = 1}
  return(minInd)}


DAT<-data.frame()
tic.clear()
stations$senSlope<-NA
stations$MK.p<-NA
it_stn = 79

for(it_stn in 1:length(stations$ID)){
  tic(sprintf("Station %d",it_stn))
  # it_stn = which(stations$ID=="08HA003")
  streamData<-streamDataAll[streamDataAll$ID==stations$ID[it_stn],]
  # streamData<-streamDataAll[streamDataAll$ID=="08LB020",]
  # streamData$Date<-streamData$Date%>%ymd()
  data_x<-WeatherData%>%filter(ID==stations$ID[it_stn])
  
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",stations$ID[it_stn],".rds"))
  
  month_bgn<-max(stations$minSumFlowMonth[it_stn]-1,stations$SDD[it_stn])
  
  month_end<-min(stations$minSumFlowMonth[it_stn]+1,stations$SAD[it_stn])
  
  # month_end<-min(month_end,length(MonBestModel))
  month_rng<-((month_bgn:month_end)-1)%%12+1
  
  
  streamDataYrly<-streamDataMonthly%>%
    filter(ID==stations$ID[it_stn]&
             month%in%month_rng)%>%
    group_by(year)%>%
    dplyr::summarize(minSumFlow = min(minMonFlow7))
  
  
  
  streamData$DayOfYear<-yday(ymd(streamData$Date))
  streamData$Discharge7Summer<-streamData$Discharge7
  streamData$Discharge7Summer[!streamData$month%in%c(8,9,10)]<-NA
  
  Q_filled<-streamData[streamData$year%in%(1950:2022),c("year","month","DayOfYear","Discharge7")]
  
  
  Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]<-
    Q_filled$DayOfYear[leap_year(Q_filled$year)& Q_filled$DayOfYear>59]-1
  set.seed(123)
  for(it in 1:365){
    sum_notNA<-sum(!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
    if(sum_notNA>0){
      sum_NA<-sum(is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it)
      Q_filled$Discharge7[is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it]<-
        sample(Q_filled$Discharge7[!is.na(Q_filled$Discharge7)&Q_filled$DayOfYear==it],size = sum_NA,replace = TRUE)
    }
    
  }
  if(sum(is.na(Q_filled$Discharge7))>0){
    sprintf("%d of 365 days with no filled Q for station %s - %s",sum(is.na(Q_filled$Discharge7[1:365])),stations$ID[it_stn],stations$Station.Name[it_stn])
  }
  
  anon_func<-function(MAD,N){MAD[N<365]<-NA; MAD}
  MAD<-
    Q_filled%>%
    group_by(year)%>%
    dplyr::summarise(N=n(),
                     MAD = mean(Discharge7))%>%
    mutate(MAD=anon_func(MAD,N))%>%
    dplyr::summarise(LTMAD = mean(MAD,na.rm = TRUE))
  
  if(stations$regime[it_stn]=="Rainfall"){
    CEFT = 0.02*MAD$LTMAD
  }else{
    CEFT = 0.05*MAD$LTMAD
  }
  
  
  DroughtIndics<-
    streamDataYrly%>%
    filter(year%in%(1950:2022))%>%
    dplyr::summarise(D5 = quantile(minSumFlow,0.02,type = 5,na.rm = TRUE),
                     D4 = quantile(minSumFlow,0.05,type = 5,na.rm = TRUE),
                     D3 = quantile(minSumFlow,0.10,type = 5,na.rm = TRUE))
  # DroughtIndic5 = xtrm_val_data%>%
  #   filter(month %in%month_rng)%>%
  #   group_by(month)%>%
  #   summarize(D5 =  quantile(minFlow,0.02,type =5))
  # DroughtIndic5<-left_join(data.frame(month = 1:12),
  #                          DroughtIndic5)
  
  # One drought level for whole season
  
  # quantile(Q_filled$Discharge7,0.02,type =5,na.rm =TRUE)
  
  # DroughtIndic5_Aug = quantile(log(xtrm_val_data$minAugFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  # DroughtIndic5_Sep = quantile(log(xtrm_val_data$minSepFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  # DroughtIndic5_Oct = quantile(log(xtrm_val_data$minOctFlow7),0.02,type =5,na.rm =TRUE)%>%exp()
  # # 
  # MAD = mean(Q_filled$Discharge7,na.rm = TRUE)
  # 
  # FlowStandardSalmon = (148*MAD^-0.36)*MAD/100
  # 
  predMin<-list()
  for(it_mn in month_rng){
    
    # streamDataYrly<-streamDataMonthly%>%
    #   filter(month==it_mn&ID==stations$ID[it_stn])
    
    
    
    data_x$WaterYear<-data_x$Year
    if(it_mn<12){
      data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
    }
    
    # print("here-2")
    
    dataYearly<-data_x%>%
      group_by(WaterYear)%>%
      dplyr::summarize(
        # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
        
        Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
        Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
        Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
        
        Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
        Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
        Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
        Total.cms = Total.cms[n()]
        
        
      )
    
    dataYearly<-dataYearly[!is.na(dataYearly$Precip_1)&
                             !is.na(dataYearly$Temp_1),]
    dataYearly<-dataYearly[dataYearly$WaterYear%in%1901:2022,]
    
    #dataYearly$Temp_1<-residuals(lm(Temp_1~Precip_1,data = dataYearly))
    #dataYearly$Temp_2<-residuals(lm(Temp_2~Precip_2,data = dataYearly))
    #dataYearly$Temp_3<-residuals(lm(Temp_3~Precip_3,data = dataYearly))
    #dataYearly$Temp_4<-residuals(lm(Temp_4~Precip_4,data = dataYearly))
    #dataYearly$Temp_6<-residuals(lm(Temp_6~Precip_6,data = dataYearly))
    #dataYearly$Temp_8<-residuals(lm(Temp_8~Precip_8,data = dataYearly))
    #dataYearly$Temp_12<-residuals(lm(Temp_12~Precip_12,data = dataYearly))
    
    
    
    # dataYearly2<-left_join(dataYearly,streamDataYrly,by = c("WaterYear"= "year"))
    # dataYearly2<-dataYearly2[dataYearly2$WaterYear<2023,]
    # 
    # 
    # 
    # dataYearly2<-dataYearly2[dataYearly2$WaterYear>=1901,]
    # 
    # 
    # 
    # dataYearly2<-dataYearly2[!is.na(dataYearly2$minMonFlow7)&
    #                            !is.na(dataYearly2$Precip_1)&
    #                            !is.na(dataYearly2$Temp_1),]
    # dataYearly2<-dataYearly2[!is.na(dataYearly2$minSumFlow),]
    
    # if(nrow(dataYearly2)<20){print(it_mn);next}
    
    
    # Fit the model to all the data
    
    # dataYearly2_trX<-dataYearly2
    # MonBestModel_fitted[[it_mn]]<-eval(MonBestModel[[it_mn]][[1]])
    predMin[[it_mn]]<-predict(MonBestModel[[it_mn]],newdata = dataYearly)
    
    
  }
  predMin2<-as.data.frame(predMin[month_rng])%>%
    apply(1,min)
  
  dat<-data.frame(ID = stations$ID[it_stn],
                  Station.Name = stations$Station.Name[it_stn],
                  WaterYear =1901:2022,
                  predMin = predMin2,
                  D5 = DroughtIndics$D5,
                  D4 = DroughtIndics$D4,
                  D3 = DroughtIndics$D3,
                  CEFT = CEFT
  )
  dat$predMin.m3s<-exp(dat$predMin)
  
  DAT<-rbind(DAT,dat)
  
  
  
  resids<-left_join(streamDataYrly[,c("year","minSumFlow")],dat[,c("WaterYear","predMin.m3s")],
                    by = c("year" = "WaterYear"))
  
  mkTest<-modifiedmk::mmkh3lag((resids$minSumFlow-resids$predMin.m3s))
  stations$senSlope[it_stn] <-mkTest[7]
  stations$MK.p[it_stn] <-mkTest[2]
  
  # naInds<-which(is.na(streamDataYrly$minSumFlow))
  # naInds<-naInds[!naInds%in%c(1,dim(streamDataYrly)[1])]
  # streamDataYrly$minSumFlow_filled<-streamDataYrly$minSumFlow
  # if(length(naInds)>0){
  #   streamDataYrly$minSumFlow_filled[naInds]<-
  #     (streamDataYrly$minSumFlow_filled[naInds+1]+
  #        streamDataYrly$minSumFlow_filled[naInds-1])/2
  #   
  # }
  # streamDataYrly$minSumFlow.10<-zoo::rollmean(streamDataYrly$minSumFlow_filled,
  #                                             k=10,
  #                                             align = "right",
  #                                             fill = NA)
  # 
  # dat$predMin.m3s.10<-zoo::rollmean(dat$predMin.m3s,
  #                                   k=10,
  #                                   align = "right",
  #                                   fill = NA)
  
  streamDataYrly<-full_join(data.frame(year = (streamDataYrly$year[1]-9):(streamDataYrly$year[1]-1)),
                            streamDataYrly)
  dat<-full_join(data.frame(WaterYear = (dat$WaterYear[1]-9):(dat$WaterYear[1]-1)),
                 dat)
  dat$predMin.m3s.10<-RcppRoll::roll_meanr(dat$predMin.m3s,
                                           n=10,
                                           na.rm = TRUE,
                                           fill = NA)
  dat$predMin.m3s.10.n<-RcppRoll::roll_sumr(is.na(dat$predMin.m3s),
                                            n=10,
                                            na.rm = TRUE,
                                            fill = NA)
  dat$predMin.m3s.10[dat$predMin.m3s.10.n>5]<-NA
  streamDataYrly$minSumFlow.10<-RcppRoll::roll_meanr(streamDataYrly$minSumFlow,
                                                     n=10,
                                                     na.rm = TRUE,
                                                     fill = NA)
  streamDataYrly$minSumFlow.10.n<-RcppRoll::roll_sumr(is.na(streamDataYrly$minSumFlow),
                                                      n=10,
                                                      na.rm = TRUE,
                                                      fill = NA)
  streamDataYrly$minSumFlow.10[streamDataYrly$minSumFlow.10.n>5]<-NA
  streamDataYrly$minSumFlow.10[is.na(streamDataYrly$minSumFlow)]<-NA
  # rolled<-RcppRoll::roll_meanr(dat$predMin.m3s,
  #                              n=10,
  #                              na.rm = TRUE,
  #                              fill = NA)
  
  # dat$predMin.m3s.10.n[is.na(dat$predMin.m3s.10)]<-l
  # dat$predMin.m3s.10[is.na(dat$predMin.m3s.10)]<-
  #   rolled[is.na(dat$predMin.m3s.10)]
  # 
  # rolled2<-RcppRoll::roll_meanr(streamDataYrly$minSumFlow,
  #                               n=l,
  #                               # align = "right",
  #                               fill = NA)
  
  # streamDataYrly$minSumFlow.10.n[is.na(streamDataYrly$minSumFlow.10)]<-l
  # streamDataYrly$minSumFlow.10[is.na(streamDataYrly$minSumFlow.10)]<-
  #   rolled2[is.na(streamDataYrly$minSumFlow.10)]
  
  
  
  
  
  # Threshes<-dat%>%
  #   select(WaterYear,CEFT,D3,D4,D5)%>%
  #   melt(id.vars = "WaterYear")
  # 
  # Threshes$Thresh<-factor(Threshes$variable,levels = c("CEFT","D3","D4","D5"),
  #                         labels = c("Critical Environmental\nFlow Threshold","Drought Level 3","Drought Level 4","Drought Level 5"))
  
  if(mean(streamDataYrly$minSumFlow.10[(nrow(streamDataYrly)-20):nrow(streamDataYrly)],na.rm = TRUE)>
     (min(streamDataYrly$minSumFlow.10,na.rm = TRUE)+max(streamDataYrly$minSumFlow.10,na.rm = TRUE))/2){
    leg.y = 0.15
  }else{
    leg.y = 0.8
  }
  
  p1<-ggplot(streamDataYrly)+
    # geom_line(aes(x = year,y = minSumFlow.10,colour = "measured",linetype = "measured"))+
    # geom_line(data = dat,aes(x = WaterYear,y = predMin.m3s.10,colour = "predicted",linetype = "predicted"))+
    
    geom_line(aes(x = year,y = minSumFlow.10,colour = "measured",group = "measured",alpha = 10-minSumFlow.10.n))+
    geom_line(data = dat,aes(x = WaterYear,y = predMin.m3s.10,colour = "predicted",group = "predicted",alpha = 10-predMin.m3s.10.n))+
    # geom_hline(aes(yintercept = CEFT,colour = "CEFT",linetype = "CEFT"))+
    # geom_hline(data = Threshes,aes(yintercept=value,color = Thresh,linetype = Thresh))+
    # geom_hline(aes(yintercept = (DroughtIndics%>%melt())$value,colour = "Drought 5",linetype = "Drought 5"))+
    # scale_y_continuous(name = expression(Q7[min]~(m^3/s)~10~year~mean))+
    scale_y_continuous(name = expression(Q7[min]~(m^3/s)))+
    scale_x_continuous(limits = c(range(streamDataYrly$year[!is.na(streamDataYrly$minSumFlow.10)])))+
    scale_alpha_continuous(limits = c(5,10),breaks = c(5,10),
                           labels = function(x){10-x},range = c(0.1,1),
                           guide = "alpha")+
    scale_colour_manual(breaks = c("measured","predicted","Critical Environmental\nFlow Threshold","Drought Level 3","Drought Level 4","Drought Level 5"),
                        values = c("black","#1B9E77","black","#E0A758","#D34D4C","#7D3737"),
                        name = "")+
    # scale_linetype_manual(breaks = c("measured","predicted","Critical Environmental\nFlow Threshold","Drought Level 3","Drought Level 4","Drought Level 5"),
    #                       values = c(1,1,2,2,2,2),
    #                       name = "")+
    ggtitle(label = paste(stations$Station.Name[it_stn],", ", stations$ID[it_stn],", ", stations$regime[it_stn]," regime"))+
    theme_bw()+
    theme(plot.title = element_text(hjust=0.03, vjust=-8,size = 8),
          legend.position = c(0.8,leg.y),
          legend.background = element_rect(fill = alpha("white",alpha = 0.5),color = "black"),
          # legend.title = element_blank()
          legend.box = 'horizontal'
    )+
    guides(alpha = guide_legend(title = "# missing",order = 2),
           colour = guide_legend(title = "10-year mean",order = 1),
           linetype = guide_legend(title =  "10-year mean",order = 1))
  p1
  ggsave(plot = p1,filename = paste0("3.Figures/SimulatedHistorical/",stations$ID[it_stn], "-",stations$Station.Name[it_stn],".png"),
         width = 6,height = 4)
  
  if(stations$Station.Name[it_stn]%in%pcic_models){
    
    PCIC_model<-read.csv(paste0("2.data/1.original/PCIC_station_hydro_model_out/",stations$Station.Name[it_stn],".ascii"),skip = 1)
    PCIC_model$Date<-ymd(PCIC_model$Date)
    PCIC_model$month = month(PCIC_model$Date)
    
    PCIC_model$year = year(PCIC_model$Date)
    PCIC_model$Discharge<-PCIC_model$PNWNAmet
    PCIC_model$Discharge7<-RcppRoll::roll_meanr (PCIC_model$PNWNAmet,n=7,fill = NA)
    PCIC_model$Discharge7Summer<-PCIC_model$Discharge7
    PCIC_model$Discharge7Summer[!PCIC_model$month%in%month_rng]<-NA
    
    
    
    
    PCIC_modelYrly<-PCIC_model%>%
      filter(year%in%(1946:2012))%>%
      group_by(year)%>%
      dplyr::summarize(
        
        minSumFlow = min(Discharge7Summer,na.rm = TRUE)
      )
    PCIC_modelYrly<-full_join(data.frame(year = (PCIC_modelYrly$year[1]-9):(PCIC_modelYrly$year[1]-1)),
                              PCIC_modelYrly)
    
    PCIC_modelYrly$minSumFlow.10<-RcppRoll::roll_meanr(PCIC_modelYrly$minSumFlow,
                                                       n=10,
                                                       na.rm = TRUE,
                                                       fill = NA)
    PCIC_modelYrly$minSumFlow.10.n<-RcppRoll::roll_sumr(is.na(PCIC_modelYrly$minSumFlow),
                                                        n=10,
                                                        na.rm = TRUE,
                                                        fill = NA)
    PCIC_modelYrly$minSumFlow.10[PCIC_modelYrly$minSumFlow.10.n>5]<-NA
    PCIC_modelYrly$minSumFlow.10[is.na(PCIC_modelYrly$minSumFlow)]<-NA
    
    p2<-p1+geom_line(data = PCIC_modelYrly,aes(x = year,y = minSumFlow.10,colour = "VIC-GL",group = "measured",alpha = 10-minSumFlow.10.n))+
      scale_colour_manual(breaks = c("measured","predicted","VIC-GL"),
                          labels = c("measured","regression","VIC-GL"),
                          values = c("black","#1B9E77","#D95F02"),
                          name = "")+
      theme(
        # legend.position = c(0.8,(5*leg.y+0.5)/6),
        legend.position = "right",
        legend.box = 'vertical',
        legend.title = element_text(hjust=0.5)
      )+
      guides(colour = guide_legend(title = "10-year\nmean\nlow flow",order = 1))
    
    p2
    
    ggsave(plot = p2,filename = paste0("3.Figures/SimulatedHistorical/PCIC_compare/",stations$ID[it_stn], "-",stations$Station.Name[it_stn],".png"),
           width = 6,height = 4)
  }
  
  
  
  toc()
  
}

stations%>%group_by(regime)%>%
  dplyr::summarize(percSigResidTrends = sum(MK.p<0.05)/n(),
                   percPosResidTrends = sum(senSlope>0)/n(),
                   p1 = binom.test(sum(senSlope>0), n(), p=0.5, alternative = "two.sided")$p.value,
                   percPosSigResidTrends = sum(senSlope>0&MK.p<0.05)/n(),
                   p2 = binom.test(sum(senSlope>0& MK.p<0.05), n(), p=0.05, alternative = "greater")$p.value,
                   percNegSigResidTrends = sum(senSlope<0&MK.p<0.05)/n(),
                   p3 = binom.test(sum(senSlope<0& MK.p<0.05), n(), p=0.05, alternative = "greater")$p.value
  )

stations%>%
  dplyr::summarize(percSigResidTrends = sum(MK.p<0.05)/n(),
                   percPosResidTrends = sum(senSlope>0)/n(),
                   percPosSigResidTrends = sum(senSlope>0&MK.p<0.05)/n(),
                   percNegSigResidTrends = sum(senSlope<0&MK.p<0.05)/n(),
  )
# Hybrid and snowfall regimes - 18% negative and significant 
# Residuals are becoming more negative; we are overpredicting low flows
# Projections are likely to be overpredicted; transgressions underpredicted;
# change underpredicted.

# DAT$predMin.m3s<-exp(DAT$predMin)


# DAT$underD5Aug<-DAT$predMinAug.m3s<DAT$D5Aug
# DAT$underD5Sep<-DAT$predMinSep.m3s<DAT$D5Sep
# DAT$underD5Oct<-DAT$predMinOct.m3s<DAT$D5Oct


DAT<-left_join(DAT,stations[,c("ID","regime")])


DAT$regime<-factor(DAT$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                   labels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))

# If we want to use modelled data to produce drought levels
# D_indics<-
#   DAT%>%
#   filter(WaterYear%in%c(1950:2022))%>%
#   group_by(ID)%>%
#   dplyr::summarise(D5=quantile(predMin.m3s,0.02),
#                    D4=quantile(predMin.m3s,0.05),
#                    D3=quantile(predMin.m3s,0.1)
#   )
# DAT<-DAT%>%select(!c(D5,D4,D3))%>%
#   left_join(D_indics)



DAT$underCEFT<-DAT$predMin.m3s<DAT$CEFT
DAT$underD5<-DAT$predMin.m3s<DAT$D5 # this is not really meaningful
DAT$underD4<-DAT$predMin.m3s<DAT$D4 # this is not really meaningful
DAT$underD3<-DAT$predMin.m3s<DAT$D3 # this is not really meaningful

DAT3<-DAT%>%
  # filter(str_detect(ID,"08H"))%>%
  group_by(regime,WaterYear)%>%
  dplyr::summarize(
    numExceeds = sum(underCEFT)/n(),
    numExceedsD5 = sum(underD5)/n(),
    numExceedsD4 = sum(underD4)/n(),
    numExceedsD3 = sum(underD3)/n(),
    # numExceedsD5Aug = sum(underD5Aug)/n(),
    # numExceedsD5Sep = sum(underD5Sep)/n(),
    # numExceedsD5Oct = sum(underD5Oct,na.rm = TRUE)/sum(!is.na(underD5Oct)),
  )


DAT3.long<-DAT3%>%
  select(WaterYear,regime,numExceeds,numExceedsD5,numExceedsD4,numExceedsD3)%>%
  tidyr::pivot_longer(cols=c(numExceeds,numExceedsD5,numExceedsD4,numExceedsD3))

ggplot(DAT3.long,aes(x = WaterYear,y = value,color = name))+
  geom_smooth(aes(linetype = "SimHist"),linewidth = 1,method = "loess",span = 0.05,se = FALSE)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),
                     # limits = c(1900,2100),
                     # breaks = c(1900,1950,2000,2050,2085),
                     # labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     # minor_breaks = c(1925,1975,2025)
  )+
  scale_y_continuous(name= "Level 5 Drought\n(% Catchments)",
                     # labels = function(x){x*100},
                     # labels = c("","25","50","75","100"),
                     # limits= c(-0.01,1),
                     # breaks = c(0,.25,.5,.75,1),
                     expand = c(0,0),
                     oob = squish)+
  
  scale_color_manual(values=c("black","#E0A758","#D34D4C","#7D3737"),
                     labels = c("CEFT","Level 3","Level 4","Level 5"),
                     name ="Transgression\nfrequency" )+
  # scale_color_manual(values)
  
  # scale_linetype(name = "",
  #                labels = "Simulated\nhistorical\n(10-year mean)",
  #                guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="bottom",
    legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})+
  
  guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.1,order = 2),
         linetype = guide_legend(title = "", title.position = "left",order = 1 ))

# 
# DAT3$numExceeds.10 <- zoo::rollmean(DAT3$numExceeds, k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
# DAT3$numExceedsD5.10 <- zoo::rollmean(DAT3$numExceedsD5,k=10, align = "right", fill=NA)
# DAT3$numExceeds.10[DAT$WaterYear<=1910]<-NA
rollFunc<-function(yr,val){
  
  tibble(
    val = zoo::rollmean(val, k=10, align = "right", fill=NA),
    yr = yr
  )
}

DAT4<-DAT3%>%
  group_by(regime)%>%
  reframe(
    numExceeds.10 = rollFunc(WaterYear,numExceeds),
    numExceedsD5.10 = rollFunc(WaterYear,numExceedsD5),
    numExceedsD4.10 = rollFunc(WaterYear,numExceedsD4),
    numExceedsD3.10 = rollFunc(WaterYear,numExceedsD3)
  )%>%
  tidyr::unnest(cols = c(
    # numExceeds.10, 
    numExceeds.10,
    numExceedsD5.10,
    numExceedsD4.10,
    numExceedsD3.10
  ),names_sep = "_")%>%
  dplyr::select(c("regime",
                  "numExceeds.10_yr",
                  "numExceeds.10_val",
                  "numExceedsD5.10_val",
                  "numExceedsD5.10_yr",
                  "numExceedsD4.10_val",
                  "numExceedsD4.10_yr",
                  "numExceedsD3.10_val",
                  "numExceedsD3.10_yr"
  ))%>%
  dplyr::rename(c(WaterYear = numExceedsD5.10_yr,
                  numExceeds.10 = numExceeds.10_val,
                  numExceedsD5.10 = numExceedsD5.10_val,
                  numExceedsD4.10 = numExceedsD4.10_val,
                  numExceedsD3.10 = numExceedsD3.10_val
                  
  ))







DAT4.long<-DAT4%>%
  select(WaterYear,regime,numExceeds.10,numExceedsD5.10,numExceedsD4.10,numExceedsD3.10)%>%
  tidyr::pivot_longer(cols=c(numExceeds.10,numExceedsD5.10,numExceedsD4.10,numExceedsD3.10))


ggplot(DAT4.long,aes(x = WaterYear,y = value*100,color = name))+
  geom_line(linewidth = 1)+
  
  
  scale_x_continuous(name = NULL,expand = c(0,0),
                     limits = c(1909,2023),
                     breaks = seq(1910,2030,20),
                     # breaks = c(1900,1950,2000,2050,2085),
                     # labels = c("1900","1950","2000","mid-21st\ncentury","late-21st\ncentury"),
                     # minor_breaks = c(1925,1975,2025)
  )+
  scale_y_continuous(name= "% Catchments Transgressing Threshold, 10-year mean",
                     # labels = function(x){x*100},
                     # labels = c("","25","50","75","100"),
                     # limits= c(-0.01,1),
                     # breaks = c(0,.25,.5,.75,1),
                     # expand = c(0,0),
                     oob = squish)+
  scale_color_manual(values=c("black","#E0A758","#D34D4C","#7D3737"),
                     labels = c("CEFT","Level 3","Level 4","Level 5"),
                     name ="Threshold" )+
  
  # scale_linetype(name = "",
  #                labels = "Simulated\nhistorical\n(10-year mean)",
  #                guide = guide_legend())+
  
  theme_bw()+
  
  theme(
    # strip.background = element_blank(),
    #     strip.text.x = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank()
    axis.ticks = element_blank(),
    strip.placement = "outside",
    legend.position="right",
    # legend.justification = c(1,0),
    legend.background = element_rect(colour = "black")
    
  )+
  facet_wrap(facets = "regime",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))})

# guides(fill = guide_legend(title = "Scenario (30-year mean)", title.position = "top", title.hjust = 0.5,order = 2),
#        linetype = guide_legend(title = "", title.position = "left",order = 1 ))

ggsave("3.Figures/ExceedPlot_hist.png",
       width = 6.5,height = 6,dpi = 600)


# What is causing the changes decade to decade?

WeatherDataMean<-WeatherData%>%
  filter(Year<=1999)%>%
  group_by(ID,Month)%>%
  dplyr::summarise(across(Mean.Temp..C.:Total.cms,~mean(.x)))

WeatherDataDecadal<-WeatherData%>%
  mutate(decade = floor(Year/1)*1)%>%
  group_by(ID,decade,Month)%>%
  dplyr::summarise(across(Mean.Temp..C.:Total.cms,~mean(.x)))

WeatherDataDecadal<-left_join(WeatherDataDecadal,WeatherDataMean,by = c("ID","Month"),
                              suffix = c("","mn"))

WeatherDataDecadal$Mean.Temp..C.Delta<-WeatherDataDecadal$Mean.Temp..C.- WeatherDataDecadal$Mean.Temp..C.mn
WeatherDataDecadal$Total.Precip..mm.Delta<-WeatherDataDecadal$Total.Precip..mm.- WeatherDataDecadal$Total.Precip..mm.mn
WeatherDataDecadal$Total.cms.Delta<-WeatherDataDecadal$Total.cms#- WeatherDataDecadal$Total.cmsmn


blankData<-expand.grid(
  decade = seq(1900,2022,1),
  Month=1:12,
  
  Total.Precip..mm. = 0,
  Mean.Temp..C. = 0,
  Total.cms=0
)
it_mn=9
dataYearly_blank<-blankData%>%
  group_by(decade)%>%
  dplyr::summarize(
    # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
    #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
    
    Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
    Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
    Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
    Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
    Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
    Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
    Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
    
    Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
    Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
    Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
    Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
    Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
    Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
    Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
    Total.cms = Total.cms[n()]
    
    
  )%>%
  filter(decade%in%1901:2022)

RES<-data.frame()
for(it_stn in 1:230){
  tic()
  MonBestModel<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",stations$ID[it_stn],".rds"))
  
  it_mn<-stations$minSumFlowMonth[it_stn]
  
  
  ## winter temperature
  
  data_x<-left_join(blankData,WeatherDataDecadal%>%
                      filter(ID==stations$ID[it_stn]& Month%in%c(11,12,1,2,3,4))%>%
                      select(decade, Month, Mean.Temp..C.Delta))
  data_x$Mean.Temp..C.Delta[is.na(data_x$Mean.Temp..C.Delta)]<-0
  data_x$Mean.Temp..C.<-data_x$Mean.Temp..C.+data_x$Mean.Temp..C.Delta
  
  data_x$WaterYear<-data_x$decade
  if(it_mn<12){
    data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
  }
  
  dataYearly<-data_x%>%
    group_by(WaterYear)%>%
    dplyr::summarize(
      # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
      Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      
      Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
      Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      Total.cms = Total.cms[n()]
      
      
    )%>%
    filter(WaterYear%in%c(1901:2022))
  
  res<-data.frame(ID = stations$ID[it_stn],
                  regime = stations$regime[it_stn],
                  year = dataYearly$WaterYear,
                  var = "Temp_winter",
                  delta = predict(MonBestModel[[it_mn]],newdata = dataYearly)-predict(MonBestModel[[it_mn]],newdata = dataYearly_blank))
  
  RES<-rbind(RES,res) 
  ## summer temperature
  
  data_x<-left_join(blankData,WeatherDataDecadal%>%
                      filter(ID==stations$ID[it_stn]& Month%in%c(5:10))%>%
                      select(decade, Month, Mean.Temp..C.Delta))
  data_x$Mean.Temp..C.Delta[is.na(data_x$Mean.Temp..C.Delta)]<-0
  data_x$Mean.Temp..C.<-data_x$Mean.Temp..C.+data_x$Mean.Temp..C.Delta
  
  
  data_x$WaterYear<-data_x$decade
  if(it_mn<12){
    data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
  }
  
  dataYearly<-data_x%>%
    group_by(WaterYear)%>%
    dplyr::summarize(
      # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
      Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      
      Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
      Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      Total.cms = Total.cms[n()]
      
      
    )%>%
    filter(WaterYear%in%c(1901:2022))
  res<-data.frame(ID = stations$ID[it_stn],
                  regime = stations$regime[it_stn],
                  year = dataYearly$WaterYear,
                  var = "Temp_summer",
                  delta = predict(MonBestModel[[it_mn]],newdata = dataYearly)-predict(MonBestModel[[it_mn]],newdata = dataYearly_blank))
  
  RES<-rbind(RES,res)
  
  
  
  ## winter precip
  
  data_x<-left_join(blankData,WeatherDataDecadal%>%
                      filter(ID==stations$ID[it_stn]& Month%in%c(11,12,1,2,3,4))%>%
                      select(decade, Month, Total.Precip..mm.Delta))
  data_x$Total.Precip..mm.Delta[is.na(data_x$Total.Precip..mm.Delta)]<-0
  data_x$Total.Precip..mm.<-data_x$Total.Precip..mm.+data_x$Total.Precip..mm.Delta
  
  
  data_x$WaterYear<-data_x$decade
  if(it_mn<12){
    data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
  }
  
  dataYearly<-data_x%>%
    group_by(WaterYear)%>%
    dplyr::summarize(
      # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
      Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      
      Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
      Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      Total.cms = Total.cms[n()]
      
      
    )%>%
    filter(WaterYear%in%c(1901:2022))
  res<-data.frame(ID = stations$ID[it_stn],
                  regime = stations$regime[it_stn],
                  year = dataYearly$WaterYear,
                  var = "Precip_winter",
                  delta = predict(MonBestModel[[it_mn]],newdata = dataYearly)-predict(MonBestModel[[it_mn]],newdata = dataYearly_blank))
  
  RES<-rbind(RES,res)
  ## summer precip
  
  data_x<-left_join(blankData,WeatherDataDecadal%>%
                      filter(ID==stations$ID[it_stn]& Month%in%c(5:10))%>%
                      select(decade, Month, Total.Precip..mm.Delta))
  data_x$Total.Precip..mm.Delta[is.na(data_x$Total.Precip..mm.Delta)]<-0
  data_x$Total.Precip..mm.<-data_x$Total.Precip..mm.+data_x$Total.Precip..mm.Delta
  
  
  data_x$WaterYear<-data_x$decade
  if(it_mn<12){
    data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
  }
  
  dataYearly<-data_x%>%
    group_by(WaterYear)%>%
    dplyr::summarize(
      # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
      Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      
      Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
      Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      Total.cms = Total.cms[n()]
      
      
    )%>%
    filter(WaterYear%in%c(1901:2022))
  res<-data.frame(ID = stations$ID[it_stn],
                  regime = stations$regime[it_stn],
                  year = dataYearly$WaterYear,
                  var = "Precip_summer",
                  delta = predict(MonBestModel[[it_mn]],newdata = dataYearly)-predict(MonBestModel[[it_mn]],newdata = dataYearly_blank))
  
  RES<-rbind(RES,res)
  
  ## Abstraction
  
  data_x<-left_join(blankData,WeatherDataDecadal%>%
                      filter(ID==stations$ID[it_stn])%>%
                      select(decade, Month, Total.cms.Delta))
  # data_x$Total.Precip..mm.Delta[is.na(data_x$Total.Precip..mm.Delta)]<-0
  data_x$Total.cms<-data_x$Total.cms+data_x$Total.cms.Delta
  
  
  data_x$WaterYear<-data_x$decade
  if(it_mn<12){
    data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)] <- data_x$WaterYear[data_x$Month%in%c((it_mn+1):12)]+1
  }
  
  dataYearly<-data_x%>%
    group_by(WaterYear)%>%
    dplyr::summarize(
      # winterPrecip = sum(Total.Precip..mm.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      #                meanWinterTemp =mean(Mean.Temp..C.[Month%in%c(11,12,1,2,3,4)],na.rm = TRUE),
      
      Precip_1 = sum(Total.Precip..mm.[Month%in%c(it_mn)],na.rm = TRUE),
      Precip_2 = sum(Total.Precip..mm.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_3 = sum(Total.Precip..mm.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_4 = sum(Total.Precip..mm.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_6 = sum(Total.Precip..mm.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_8 = sum(Total.Precip..mm.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Precip_12 = sum(Total.Precip..mm.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      
      Temp_1 = mean(Mean.Temp..C.[Month%in%c(it_mn)],na.rm = TRUE),
      Temp_2 = mean(Mean.Temp..C.[Month%in%((((it_mn-1):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_3 = mean(Mean.Temp..C.[Month%in%((((it_mn-2):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_4 = mean(Mean.Temp..C.[Month%in%((((it_mn-3):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_6 = mean(Mean.Temp..C.[Month%in%((((it_mn-5):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_8 = mean(Mean.Temp..C.[Month%in%((((it_mn-7):it_mn)-1)%%12+1)],na.rm = TRUE),
      Temp_12 = mean(Mean.Temp..C.[Month%in%((((it_mn-11):it_mn)-1)%%12+1)],na.rm = TRUE),
      Total.cms = Total.cms[n()]
      
      
    )%>%
    filter(WaterYear%in%c(1901:2022))
  res<-data.frame(ID = stations$ID[it_stn],
                  regime = stations$regime[it_stn],
                  year = dataYearly$WaterYear,
                  var = "Abstraction",
                  delta = predict(MonBestModel[[it_mn]],newdata = dataYearly)-predict(MonBestModel[[it_mn]],newdata = dataYearly_blank))
  
  RES<-rbind(RES,res)
  print(sprintf("done station %d",it_stn))
  toc()
  
}

RES$regime<-factor(RES$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                   labels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))


deltas<-
  RES%>%
  group_by(regime,year,var)%>%
  # dplyr::summarise(delta_mn=(mean(delta)%>%exp())-1)%>%
  dplyr::summarise(delta_mn=mean(delta))%>%
  tidyr::pivot_wider(id_cols=c(year,regime),
                     values_from = delta_mn,
                     names_from =c(var))%>%
  ungroup()%>%
  mutate(overall = Precip_summer+Temp_summer+Precip_winter+Temp_winter+Abstraction)%>%
  # mutate(across(!year,~function(x){(exp(x)-1)}))%>%
  tidyr::pivot_wider(id_cols = year,
                     values_from = Abstraction:overall,
                     names_from = regime,
                     names_sep = "___")%>%
  ungroup()%>%
  mutate(across(!year,~RcppRoll::roll_meanr(.x,n = 10,na.rm = FALSE,fill = NA)))%>%
  # mutate(x = RcppRoll::roll_meanr(Precip_summer.Rainfall,n = 10,na.rm = FALSE,fill = NA))%>%
  tidyr::pivot_longer(cols = !year,
                      names_to = "var.regime",
                      values_to = "delta_mn")%>%
  mutate(var = str_split_i(var.regime,"___",1),
         regime = str_split_i(var.regime,"___",2))

deltas$var_fct<-factor(deltas$var,
                       levels = c("overall", "Temp_summer","Temp_winter","Precip_summer","Precip_winter","Abstraction"),
                       labels = c("overall","T[summer]","T[winter]","P[summer]","P[winter]","Abstraction"))

deltas$regime_fct<-factor(deltas$regime,levels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))


brkFun<-function(x){
  x2<-100*(exp(x)-1)
  # x2.seq<-seq(-max(abs(x2)),max(abs(x2)),length.out = 7)%>%
  #   round(digits=-1)
  
  
  if(max(abs(x2))>30){
    x3.seq<-log(c(-40,-20,0,20)/100+1)
  } else {
    x3.seq<-log(c(-20,-10,0,10,20)/100+1)
  } 
  
  return(x3.seq)
  
}
minorbrkFun<-function(x){
  x2<-100*(exp(x)-1)
  # x2.seq<-seq(-max(abs(x2)),max(abs(x2)),length.out = 7)%>%
  #   round(digits=-1)
  
  
  if(max(abs(x2))>30){
    x3.seq<-log(c(-40,-30,-20,-10,0,10,20)/100+1)
  } else {
    x3.seq<-log(c(-20,-15,-10,-5,0,5,10,15,20)/100+1)
  }
  
  return(x3.seq)
  
}

ggplot(deltas%>%filter(!var_fct=="overall"),aes(year,y = (delta_mn)))+
  geom_hline(yintercept = 0,
             col = "grey",alpha =1)+
  geom_area(aes(fill = var_fct))+
  # geom_area(aes(y = -(delta_mn),fill = var_fct),alpha = 0)+ # ensure centering
  geom_line(data = deltas%>%filter(var_fct=="overall"),aes(col = "Average\nTotal\nAnomaly"),linewidth = 1)+
  
  theme_bw()+
  scale_x_continuous(name = "Year",breaks = seq(1910,2030,20),
                     expand = c(0,0),
                     limits = c(1909,2023))+
  scale_y_continuous(name = expression(Q7[min]~Anomaly~"(%), 10-year mean"),
                     labels = function(x){100*(exp(x)-1)},
                     breaks = ~brkFun(.x),
                     minor_breaks = ~minorbrkFun(.x)
                     # breaks = log(seq(0.5,1.5,0.1))
                     )+
  scale_color_manual(guide = guide_legend(title = NULL,order = 2),
                     values = "grey25")+
  scale_fill_manual(name = "Anomaly\nDriven By:",
                    values = c("#8C510A", "#DFC27D","#35978F","#80CDC1","purple"),
                    guide = guide_legend(order = 1),
                    labels = parse_format()
  )+
  facet_wrap(facets = "regime_fct",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))},
             scales = "free_y")+
  theme(axis.ticks = element_blank(),
        strip.placement = "outside",
        legend.position="right",
        legend.background = element_blank(),
        legend.spacing.y = unit(0,"mm"),
        # legend.margin = margin(0,0,0,0),
        legend.box.background =  element_rect(colour = "black"))

ggsave("3.figures/Delta_historical.png",width = 6.5,height = 6,dpi = 600)



#not rolling mean


deltas<-
  RES%>%
  group_by(regime,year,var)%>%
  dplyr::summarise(delta_mn=(mean(delta)%>%exp())-1)%>%
  tidyr::pivot_wider(id_cols=c(year,regime),
                     values_from = delta_mn,
                     names_from =c(var))%>%
  ungroup()%>%
  mutate(overall = Precip_summer+Temp_summer+Precip_winter+Temp_winter+Abstraction)%>%
  tidyr::pivot_wider(id_cols = year,
                     values_from = Abstraction:overall,
                     names_from = regime,
                     names_sep = "___")%>%
  ungroup()%>%
  # mutate(across(!year,~RcppRoll::roll_meanr(.x,n = 10,na.rm = FALSE,fill = NA)))%>%
  # mutate(x = RcppRoll::roll_meanr(Precip_summer.Rainfall,n = 10,na.rm = FALSE,fill = NA))%>%
  tidyr::pivot_longer(cols = !year,
                      names_to = "var.regime",
                      values_to = "delta_mn")%>%
  mutate(var = str_split_i(var.regime,"___",1),
         regime = str_split_i(var.regime,"___",2))


deltas$var_fct<-factor(deltas$var,
                       levels = c("overall", "Temp_summer","Temp_winter","Precip_summer","Precip_winter","Abstraction"),
                       labels = c("overall","T[summer]","T[winter]","P[summer]","P[winter]","Abstraction"))

deltas$regime_fct<-factor(deltas$regime,levels =  c("Rainfall","Hybrid","Snowmelt","Glacial"))

ggplot(deltas%>%filter(!var_fct=="overall"),aes(year,y = delta_mn*100))+
  geom_hline(yintercept = 0,
             col = "grey",alpha =1)+
  geom_area(aes(fill = var_fct))+
  geom_area(aes(y = -delta_mn*100,fill = var_fct),alpha = 0)+ # ensure centering
  geom_line(data = deltas%>%filter(var_fct=="overall"),aes(col = "Average\nTotal\nAnomaly"),linewidth = 1)+
  
  theme_bw()+
  scale_x_continuous(name = "Year",breaks = seq(1910,2030,20),
                     expand = c(0,0),
                     limits = c(1980,2023))+
  scale_y_continuous(name = expression(Q7[min]~Anomaly~"(%), 10-year mean"))+
  scale_color_manual(guide = guide_legend(title = NULL,order = 2),
                     values = "grey25")+
  scale_fill_manual(name = "Anomaly\nDriven By:",
                    values = c("#8C510A", "#DFC27D","#35978F","#80CDC1","purple"),
                    guide = guide_legend(order = 1),
                    labels = parse_format()
  )+
  facet_wrap(facets = "regime_fct",ncol = 1,strip.position = "right",
             labeller = function(variable, value){return(paste(value,"Regimes"))},
             scales = "free_y")+
  theme(axis.ticks = element_blank(),
        strip.placement = "outside",
        legend.position="right",
        legend.background = element_blank(),
        legend.spacing.y = unit(0,"mm"),
        # legend.margin = margin(0,0,0,0),
        legend.box.background =  element_rect(colour = "black"))


DAT3%>%
  group_by(regime)%>%
  summarize(WaterYear[which.max(numExceedsD5)],
            WaterYear[which.max(numExceedsD4)],
            WaterYear[which.max(numExceedsD3)],
            WaterYear[which.max(numExceeds)]
  )

# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script creates Figure 4 in the manuscript. 


setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
lm_fl_list<-list.files(path = "2.data/2.working/RegressionOptimization/BestModels/",pattern = "step2_lm.*rds")

lapply(list.files(path = ".",pattern = "step2_lm.*rds"),readRDS)

# lm1<-readRDS("../low-flows-WNA/2.data/2.working/RegressionOptimization/BestModels/step2_lm_08HA010.rds")

test_dat<-expand.grid(shiftedMonth = 1:12,
                      Month=1:12,
                      Total.Precip..mm.=0,
                      Mean.Temp..C. = 0)

test_dat_P<-test_dat
test_dat_P[test_dat_P$shiftedMonth==test_dat_P$Month,"Total.Precip..mm."]<-10
test_dat_T<-test_dat
test_dat_T[test_dat_T$shiftedMonth==test_dat_T$Month,"Mean.Temp..C."]<-1


it_mn<-12

dataYearly_testP<-test_dat_P%>%
  group_by(shiftedMonth)%>%
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
    Total.cms = 0
    
    
    
  )


dataYearly_testT<-test_dat_T%>%
  group_by(shiftedMonth)%>%
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
    Total.cms = 0
    
    
  )
dataYearly_testT$lag<-12-dataYearly_testT$shiftedMonth
dataYearly_testP$lag<-12-dataYearly_testP$shiftedMonth
dataYearly_testT<-dataYearly_testT[order(dataYearly_testT$lag),]
dataYearly_testP<-dataYearly_testP[order(dataYearly_testP$lag),]



T_shift<-data.frame()
P_shift<-data.frame()
for(it_stn in 1:230){
  BestModels<-readRDS(paste0("2.data/2.working/RegressionOptimization/BestModels/step2_lm_",
                             stations$ID[it_stn],
                             ".rds"))
  
  month_rng<-((lapply(BestModels,is.null)%>%unlist())==FALSE)%>%which()
  
  for(it in month_rng){
    
    lm_mnth<-BestModels[[it]]
    T_shift<-rbind(T_shift,
                   data.frame(ID = stations$ID[it_stn],
                              Month = it,
                              t(predict(lm_mnth,newdata = dataYearly_testT)-lm_mnth$coefficients[1])
                              # *
                              #   (24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
                   )
    )
    P_shift<-rbind(P_shift,
                   data.frame(ID = stations$ID[it_stn],
                              Month = it,
                              t(predict(lm_mnth,newdata = dataYearly_testP)-lm_mnth$coefficients[1])
                              # (24*3600)/(stations$Area_km2[it_stn]*10^6)*1000 # convert to mm/d
                   )
    )
    
  }
  
  
  
}
P_shift<-left_join(P_shift,stations[,c("ID","regime","minSumFlowMonth")])
T_shift<-left_join(T_shift,stations[,c("ID","regime","minSumFlowMonth")])

P_shift_long<-pivot_longer(P_shift,cols = X1:X12)
P_shift_long$lag<-(P_shift_long$name%>%str_remove("X")%>%as.numeric())-1
T_shift_long<-pivot_longer(T_shift,cols = X1:X12)
T_shift_long$lag<-(T_shift_long$name%>%str_remove("X")%>%as.numeric())-1


P_shift_summary<-
  P_shift_long%>%
  # filter(Month==minSumFlowMonth)%>%
  group_by(lag,regime)%>%
  dplyr::reframe(meanShift = exp(mean(value)-1),
                 medianShift = median(exp(value)-1),
                 qnts10 = quantile(exp(value)-1,.10),
                 qnts25 = quantile(exp(value)-1,.25),
                 qnts50 = quantile(exp(value)-1,.50),
                 qnts75 = quantile(exp(value)-1,.75),
                 qnts90 = quantile(exp(value)-1,.90))


T_shift_summary<-
  T_shift_long%>%
  # filter(Month==minSumFlowMonth)%>%
  group_by(lag,regime)%>%
  dplyr::reframe(meanShift = exp(mean(value))-1,
                 medianShift = median(exp(value)-1),
                 qnts10 = quantile(exp(value)-1,.10),
                 qnts25 = quantile(exp(value)-1,.25),
                 qnts50 = quantile(exp(value)-1,.50),
                 qnts75 = quantile(exp(value)-1,.75),
                 qnts90 = quantile(exp(value)-1,.90)
  )

shift_summary<-rbind(P_shift_summary%>%mutate(shiftedVar = "P"),
                     T_shift_summary%>%mutate(shiftedVar = "T")
)
shift_summary$shiftedVar<-factor(shift_summary$shiftedVar,
                                 levels = c("P","T"),
                                 labels = c("P effect (+10 mm)",
                                            "T effect (+1°C)"))

shift_summary$regime<-factor(shift_summary$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"),
                             labels = c("Rainfall","Hybrid","Snowmelt","Glacial"))
ggplot(shift_summary,aes(x = lag,y = qnts50*100,ymin = qnts10*100,ymax = qnts90*100 ))+
  geom_ribbon(aes(fill = "10th-90th\npercentile"),alpha =0.5)+
  geom_line(aes(linetype = "median"),linewidth = 1)+
  scale_fill_manual(name = "", values = "pink")+
  scale_linetype_manual(name = "", values = 1)+
  scale_y_continuous(name = expression(Delta~Q7[min]~"(%)"))+
  scale_x_continuous(name = "Lag (months)",
                     breaks = seq(0,11,4))+
  facet_grid(cols = vars(regime),rows = vars(shiftedVar),scales = "free_y")+
  theme_bw()+
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank())

ggsave("3.figures/RegressionModelPT.png",width = 6,height = 3)


# Author: Sacha Ruzzante
# sachawruzzante@gmail.com
# Last Update: 2024-05-24


# This script plots the results of the power analysis of routines A, B, and C described in Appendix E



library(dplyr)
library(ggplot2)

x1<-readRDS("4.output/ECA_power_C_ECA_5.rds")
x1$ECA<-"ECA (5 year)"
x1$routine<-"Routine C"

x2<-readRDS("4.output/ECA_power_C_ECA_60.rds")
x2$ECA<-"ECA (60 year)"
x2$routine<-"Routine C"

x3<-readRDS("4.output/ECA_power_B_ECA_5.rds")
x3$ECA<-"ECA (5 year)"
x3$routine<-"Routine B"

x4<-readRDS("4.output/ECA_power_B_ECA_60.rds")
x4$ECA<-"ECA (60 year)"
x4$routine<-"Routine B"

x5<-readRDS("4.output/ECA_power_A_ECA_5.rds")
x5$ECA<-"ECA (5 year)"
x5$routine<-"Routine A"

x6<-readRDS("4.output/ECA_power_A_ECA_60.rds")
x6$ECA<-"ECA (60 year)"
x6$routine<-"Routine A"

fctrs <- c(0,1,2,5,10,20,50,100)

ECA_pwr<-rbind(x1,x2,x3,x4,x5,x6)

ECA_pwr$ECA<-factor(ECA_pwr$ECA,levels= c("ECA (60 year)","ECA (5 year)"))

ggplot(ECA_pwr,aes(x = (fctrs+1),y = value,col = regime,linetype = hypo))+
  geom_line(linewidth =0.7)+
  geom_point(aes(shape = hypo))+
  # geom_line(aes(y = H2_rate,col = "H2"))+
  scale_x_log10(name = "Harvest Effect (# standard deviations)",labels = function(x){x-1},
                # breaks = c(1,2,4,11,51,101)
                breaks = unique(fctrs)+1,
                minor_breaks = NULL
  )+
  scale_y_continuous(name = "Type II Error Rate")+
  scale_color_manual(values = c("#DF9C41","#599D7A","#B2E3F0","#F6DE92"),
                     name = "Regime")+
  scale_linetype(name = "Hypothesis")+
  scale_shape(name = "Hypothesis")+
  theme_bw()+
  theme(legend.key.width = unit(10,'mm'))+
  facet_grid(rows = vars(routine),cols = vars(ECA))
ggsave("3.figures/ECA_power.png",width = 7,height= 6)

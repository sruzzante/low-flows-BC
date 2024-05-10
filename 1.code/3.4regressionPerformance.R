
setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria/low-flows-BC/")) #Set the working directory

stations_pf<-read.csv("2.data/2.working/StationMetadata/stations_performance.csv")

stations_pf$regime<-factor(stations$regime,levels = c("Rainfall","Hybrid","Snowfall","Glacial"))

summarizeFunc<-function(x){
  medx<-median(x,na.rm = TRUE)%>%round(2)
  minx = min(x,na.rm = TRUE)%>%round(2)
  maxx = max(x,na.rm = TRUE)%>%round(2)
  return(paste0(medx," (",minx,", ",maxx,")"))
}

stations_pf%>%
  dplyr::select(Station.Name,ID,regime, Dates,KGE:pbias)%>%
  # group_by(regime)%>%
  summarise(N=n(),
            across(KGE:pbias,~summarizeFunc(.x)))

summaryTable<-stations_pf%>%
  dplyr::select(Station.Name,ID,regime, Dates,KGE:pbias)%>%
  group_by(regime)%>%
  summarise(N=n(),
            across(KGE:pbias,~summarizeFunc(.x)))%>%
  melt(id.vars = "regime")

summaryTable2<-summaryTable%>%
  filter(variable %in% c("N","KGE.sqrt","NSE.log","R2","RMSE","pbias"))%>%
  tidyr::pivot_wider(id_cols = c(variable),
                     names_from = regime,
                     values_from = value)
summaryTable2$variable<-as.character(summaryTable2$variable)
stargazer::stargazer(summaryTable2,
                     summary = FALSE,
                     type = "html",
                     out = "4.output/RegressionGOF.doc")

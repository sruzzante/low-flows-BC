
args = commandArgs(trailingOnly=TRUE)

# test if there is at least two arguments: if not, return an error
if (length(args)<1) {
  stop("At least 1 arguments must be supplied (partition)", call.=FALSE)
}

yr    <- as.integer( args[1] )  # read first argument as integer

print(paste("Processing with year:", yr))

#library(sf)
library(terra)

# setwd(paste0(Sys.getenv("USERPROFILE"), "/OneDrive - University of Victoria./low-flows-BC/"))
# setwd(paste0("projects/def-tgleeson/ruzzante/low-flows-BC/1.code/"))

# function to extract equivalent cut area based on a variety of functions
ECA_fun<-function(xAge){
  
  ECA<-data.frame(
    ECA_5 =mean(pmax((1-xAge/5 ),0),na.rm = TRUE),
    ECA_10=mean(pmax((1-xAge/10),0),na.rm = TRUE),
    ECA_20=mean(pmax((1-xAge/20),0),na.rm = TRUE),
    ECA_60=mean(pmax((1-xAge/60),0),na.rm = TRUE),
    
    ECA_I_1 = mean(xAge<=1,na.rm = TRUE),
    ECA_I_3 = mean(xAge<=3,na.rm = TRUE),
    ECA_I_5 = mean(xAge<=5,na.rm = TRUE),
    ECA_I_7 = mean(xAge<=7,na.rm = TRUE),
    ECA_I_9 = mean(xAge<=9,na.rm = TRUE),
    ECA_I_11 = mean(xAge<=11,na.rm = TRUE),
    ECA_I_13 = mean(xAge<=13,na.rm = TRUE),
    ECA_I_15 = mean(xAge<=15,na.rm = TRUE),
    ECA_I_17 = mean(xAge<=17,na.rm = TRUE),
    
    ECA_III_12=mean(xAge%in%12:100,na.rm = TRUE),
    ECA_III_14=mean(xAge%in%14:100,na.rm = TRUE),
    ECA_III_16=mean(xAge%in%16:100,na.rm = TRUE),
    ECA_III_18=mean(xAge%in%18:100,na.rm = TRUE),
    ECA_III_20=mean(xAge%in%20:100,na.rm = TRUE),
    ECA_III_22=mean(xAge%in%22:100,na.rm = TRUE),
    ECA_III_24=mean(xAge%in%24:100,na.rm = TRUE),
    ECA_III_26=mean(xAge%in%26:100,na.rm = TRUE),
    ECA_III_28=mean(xAge%in%28:100,na.rm = TRUE),
    ECA_III_30=mean(xAge%in%30:100,na.rm = TRUE),
    ECA_III_32=mean(xAge%in%32:100,na.rm = TRUE),
    ECA_III_34=mean(xAge%in%34:100,na.rm = TRUE),
    ECA_III_36=mean(xAge%in%36:100,na.rm = TRUE)
  )
  
  return(ECA)
  # return(list(ECA_5,ECA_10,ECA_20,ECA_60))
  
}


# watersheds_ls<-rep(list(watersheds),123)
# watersheds<-vect(watersheds)
#yrs<-1903:2022

#splitList<-split(yrs, ceiling(seq_along(1:length(yrs))/ceiling(length(yrs)/20)))


#yrs_x<-splitList[[partition]]

watersheds<-vect("../2.data/2.working/CatchmentPolygons/watersheds_final.gpkg")

forestAge_x<-terra::rast(paste0("../../DATA/1.Spatial_data/regional/BC/lulc_landuse_landcover/lulc2_forestAge/forestAge_",yr,".tif"))

x<-terra::extract(forestAge_x,watersheds,fun = ECA_fun)




# x<- unlist(x)
x<-cbind(data.frame(watersheds[,c("StationNum")]),data.frame(x))
names(x)[3:28]<-c("ECA_5","ECA_10","ECA_20","ECA_60",
                  "ECA_I_1","ECA_I_3","ECA_I_5",
                  "ECA_I_7","ECA_I_9","ECA_I_11","ECA_I_13","ECA_I_15","ECA_I_17",
                  "ECA_III_12","ECA_III_14","ECA_III_16","ECA_III_18","ECA_III_20",
                  "ECA_III_22","ECA_III_24","ECA_III_26","ECA_III_28","ECA_III_30",
                  "ECA_III_32","ECA_III_34","ECA_III_36")
#head(x)
saveRDS(x,file = paste0("../2.data/2.working/ECA/ECA_",yr,".rds"))



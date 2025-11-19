library(plotKML)
library(sp)
library(randomForest)
library(nnet)
library(e1071)
library(GSIF)
library(plyr)
library(raster)
library(caret)
library(Cubist)
library(xgboost)
library(viridis)
library(h2o)
localH2O = h2o.init(startH2O=TRUE)



library(sp)
library(sf)

#lka <- st_read("./in/LKA.shp")
covs <- read.csv("./in/grid_covs.csv")
dats <- read.csv("./in/joined.csv")


grid = SpatialPixelsDataFrame(points = covs[c("x", "y")], data = covs)

# grid_st <- grid
# coordinates(grid_st) ~ x+y
# grid_st <- st_as_sf(grid_st)
# library(leaflet)
# leaflet(data = grid_st) %>% addTiles() %>%
#   addMarkers(st_coordinates(grid_st)[,1], st_coordinates(grid_st)[,2])

# generating input data for the ML

#coordinates(covs) <- ~x+y
coordinates(dats) <- ~Longitude + Latitude
dats_st <- st_as_sf(dats)

library(leaflet)
leaflet(data = dats_st) %>% addTiles() %>%
  addMarkers(st_coordinates(dats_st)[,1], st_coordinates(dats_st)[,2])


outpout <- over(dats, grid)

m <- cbind(outpout, dats@data)
dim(m)
n = m[complete.cases(m),c("x", "y", "Depth..m.",
                          "EBK_PH_0_5", "EBK_CEC_0_5", "EBK_OC_0_5",
                          "EBK_BD_0_5", "EBK_VMC33_0_5", "EBK_VCM1500_1", 
                          "RBF_CLY0_5cm", "RBF_SND_0_5", "RBF_SLT_0_5")]
# lvls = sort(unique(n$Depth..m.))
# write.csv(lvls, "./out/RFC_Unique_levles.csv")
# n$fact_depth <- factor(n$Depth..m., levels = lvls)
# 
# write.csv(n, "./in/RF_input.csv", row.names = F)

### 

# read data

n = read.csv("./in/RF_input.csv")
lvls = sort(unique(n$Depth..m.))
n$fact_depth <- factor(n$Depth..m., levels = lvls)

# transformation
library(car)
lmbd = powerTransform(n$Depth..m.)$lambda
n$trasD <- as.numeric(bcPower(n$Depth..m., lmbd))

n$logPH = log(n$EBK_PH_0_5)
n$logCEC = log(n$EBK_CEC_0_5)
n$logOC = log(n$EBK_OC_0_5)
n$logBD = log(n$EBK_BD_0_5)
n$logVMC33 = log(n$EBK_VMC33_0_5)
n$logVMC1500 = log(n$EBK_VCM1500_1)
n$logCLY = log(n$RBF_CLY0_5cm)
n$logSND = log(n$RBF_SND_0_5)
n$logSLT = log(n$RBF_SLT_0_5)


cov_names = c("EBK_PH_0_5", "EBK_CEC_0_5", "EBK_OC_0_5",
  "EBK_BD_0_5", "EBK_VMC33_0_5", "EBK_VCM1500_1", 
  "RBF_CLY0_5cm", "RBF_SND_0_5", "RBF_SLT_0_5")

cov_names_log <- c("LogPH", "logCEC", "logOC", "logBD", 
                   "logVMC33", "VMC1500", "logCLY", "logSND", "logSLT")


library(h2o)
localH2O = h2o.init(startH2O=TRUE)

depth.hex <- as.h2o(n, destination_frame = "depth.hex")
depth.grid <- as.h2o(grid@data, destination_frame = "depth.grid")

# RF.m <- h2o.randomForest(y = which(names(n)=="Depth..m."), 
#                          x = which(names(n) %in% cov_names), 
#                          training_frame = depth.hex, ntrees = 300, mtries = -1, seed = 1234,
#                          max_depth = 0)

# 
# RF.m <- h2o.randomForest(y = which(names(n)=="Depth..m."), 
#                          x = which(names(n) %in% cov_names), 
#                          training_frame = depth.hex, ntrees = 300, mtries = -1, seed = 1234,
#                          max_depth = 0, nfolds = 10)

RFR.m <- h2o.randomForest(y = which(names(n)=="Depth..m."),
                          x = which(names(n) %in% cov_names),
                          training_frame = depth.hex, nfolds = 20, max_depth = 8, 
                         ntrees = 90, seed = 123456, nbins = 200)

# RFR.m <- h2o.randomForest(y = which(names(n)=="logDepth"),
#                           x = which(names(n) %in% cov_names_log),
#                           training_frame = depth.hex, nfolds = 20, max_depth = 2,
#                           ntrees = 300, seed = 123456, nbins = 200)

RFR.m
h2o.r2(RFR.m)

RFC.m <- h2o.randomForest(y = which(names(n)=="fact_depth"),
                         x = which(names(n) %in% cov_names),
                         training_frame = depth.hex, nfolds = 20, seed = 123456)
RFC.m
h2o.r2(RFC.m)
# https://docs.h2o.ai/h2o/latest-stable/h2o-docs/performance-and-prediction.html#r2-r-squared






sink('./out/ML_training.txt')
print("RF regression Results")
print(RFR.m)
print("R2 is ")
print(h2o.r2(RFR.m))
# print("the best R value")
# print(h2o.get_best_r2_values(RFR.m))
print("")
print("")
print("")
print("")
print("")
print("")
print("")
print("")
print("")
print("")
print("--------------------------------------")
print("FR categorical Results")
print(RFC.m)
print("R2 is ")
print(h2o.r2(RFC.m))
# print("the best R value")
# print(h2o.get_best_r2_values(RFC.m))
sink()


library(scales)
#> 
#> Attaching package: 'scales'
#> The following object is masked from 'package:viridis':
#> 
#>     viridis_pal
library(lattice)
RFR.pred <- as.data.frame(h2o.predict(RFR.m, depth.hex, na.action=na.pass))$predict
RFC.pred <- as.data.frame(h2o.predict(RFC.m, depth.hex, na.action=na.pass))$predict

RFC.pred.num = as.numeric(as.character(RFC.pred))

png("./out/RFR_MeasvsPred.png")
xyplot(n$Depth..m.~ RFR.pred, asp=1, 
               par.settings=list(
                 plot.symbol = list(col=scales::alpha("black", 0.6), 
                                    fill=scales::alpha("red", 0.6), pch=21, cex=0.8)),
               ylab="measured", xlab="predicted (machine learning)")
dev.off()


png("./out/RFC_MeasvsPred.png")
xyplot(n$fact_depth~ RFC.pred.num, asp=1, 
       par.settings=list(
         plot.symbol = list(col=scales::alpha("black", 0.6), 
                            fill=scales::alpha("red", 0.6), pch=21, cex=0.8)),
       ylab="measured", xlab="predicted (machine learning)")
dev.off()




grid$RFR <- as.data.frame(h2o.predict(RFR.m, depth.grid, na.action=na.pass))$predict
grid$RFC <- as.data.frame(h2o.predict(RFC.m, depth.grid, na.action=na.pass))$predict

grid$RFCnum <- as.numeric(as.character(grid$RFC))

library(RStoolbox)
library(terra)
library(raster)
library(ggplot2)

v.rfr = vect(grid["RFR"])
r.rfr = rast(v.rfr, ncol = 464.5697, nrow = 464.5697)
RFR.grid<- raster(rasterize(v.rfr, r.rfr, "RFR"))
writeRaster(RFR.grid, "./out/RFR.grid")

v.rfc = vect(grid["RFCnum"])
r.rfc = rast(v.rfc, ncol = 464.5697, nrow = 464.5697)
RFC.grid<- raster(rasterize(v.rfc, r.rfc, "RFCnum"))
writeRaster(RFC.grid, "./out/RFC.grid")

#RFC.grid<- raster(terra::rasterize(v.rfc, r.rfc, "RFC"))

p1<-ggR(RFR.grid, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("gray60", "lightblue", "lightgreen","yellow","red", "darkred"),
                       breaks =  c(19, 50, 100, 150, 200, 215),  na.value = "white")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(paste0("RFR Predicted for Depth"))+
  theme(plot.title = element_text(hjust = 0.5))

#ggsave(p1, filename = paste0("/RFR_Map_Depth.png"), device = "png",path = paste0("./out/"))

p2<-ggR(RFC.grid, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("gray60", "lightblue", "lightgreen","yellow","red", "darkred"),
                       breaks =  c(19, 50, 100, 150, 200, 215),  na.value = "white")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle(paste0("RFC Predicted for Depth"))+
  theme(plot.title = element_text(hjust = 0.5))
#ggsave(p2, filename = paste0("/RFC_Map_Depth.png"), device = "png",path = paste0("./out/"))

library(gridExtra)
p = grid.arrange(p1, p2, ncol = 2)  
ggsave(p, filename = paste0("/RFC_and_RFR_Map_Depth.png"), device = "png",path = paste0("./out/"))


# proj4string(covs) = CRS("+init=epsg:4326")
# proj4string(dats) = CRS("+init=epsg:4326")
# 


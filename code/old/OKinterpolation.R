library(readr)
library(car)
library(gstat)
library(sp)
library(raster)
library(RStoolbox)
library(ggplot2)
library(gridExtra)
library(rgdal)
library(sf)
library(stars)
library(terra)
library(dplyr)
library(automap)
library(forecast)
library(tidyr)
library(purrr)

#######
## Functions
#######
# Variogram
modelling1 = function(target, a, psill, model, range, nugget){
  
  ##########
  # plotting variogram map
  #########
  
  v.map<-variogram(target~ 1, data = vars, map = TRUE, cutoff = a, width = a/15, cressie = T)
  
  png("./out/OKvariogramMap.png")
  print(plot(v.map, col.regions = bpy.colors(64),
             main="Variogram Map",
             xlab="x",
             ylab="y"))
  dev.off()
  
  ##########
  # variogram cloud
  ##########
  v.cloud<-variogram(target~ 1, data = vars, cloud = TRUE, cutoff = a, width = a/15, cressie = T)
  
  png("./out/OKvariogramCloud.png")
  print(plot(v.cloud, col.regions = bpy.colors(64),
             main="Variogram cloud",
             xlab="x",
             ylab="y"))
  dev.off()
  
  ##########
  # directional variogram
  ##########
  v.directional<-variogram(vars$Depth~ 1, data = vars, alpha = c(45, 90, 180, 360), cutoff = a, width = a/15, cressie = T)
  m.directional<-vgm(psill, model, range, nugget)
  # least square fit
  m.f.directional<-fit.variogram(v.directional, m.directional)
  
  
  png("./out/OKDirectionalVariogram.png")
  print(plot(v.directional, m.f.directional,
             main = "Directional Variograms",
             sub = "Azimuth 30N (left), 120N (right)",
             pch = 20, col = "blue"))
  dev.off()
  
  #########
  # final variogram and its model
  #########
  
  # #Intial parameter set by eye esitmation
  v<-variogram(target~ 1, data = vars, cloud=F, cutoff = a, width = a/15, cressie = T)
  # use cressie>>>>>>>>>
  m<-vgm(psill, model, range, nugget)
  # least square fit
  m.f<-fit.variogram(v, m)
  m.f
  

  # proper plotting
  #
  #
  plot(v, m.f, plot.numbers = TRUE)
  
  png("./out/OKOmniDirectionalVariogram.png")
  print(plot(v, pl=F,
             model=m.f,
             col="black",
             cex=0.9,
             lwd=0.5,
             lty=1,
             pch=19,
             main="Variogram and Fitted Model\n Box-Cox Transformed ",
             xlab="Distance (m)",
             ylab="Semivariance"))
  dev.off()

  
  sink("./out/OK_vario.txt")
    print(m.f)
    print("Goodness-of-fit (The lower the better)")
    print(attributes(m.f)$SSErr)
  sink()
  print("Goodness-of-fit (The lower the better)")
  print(attributes(m.f)$SSErr)
  return(m.f)
}

# cross validation
crosvals <- function(m) {
  #cross validation
  
  out<-krige.cv(Depth~1,
                vars,
                model =m,
                nmax = 40,
                nfold=10)
  
  sink("./out/OK_CV.txt")
  print("mean error, ME, ideally 0:")
  print(mean(out$residual))
  ME = mean(out$residual)
  
  print("MSPE, ideally small")
  print(mean(out$residual^2))
  MSPE = mean(out$residual^2)
  
  print("Mean square normalized error, MSNE, ideally close to 1")
  print(mean(out$zscore^2))
  MSNE = mean(out$zscore^2)
  
  print("correlation observed and predicted, COP, ideally 1")
  print(cor(out$observed, out$observed - out$residual))
  COP = cor(out$observed, out$observed - out$residual)
  
  print("correlation predicted and residual, CPR, ideally 0")
  print(cor(out$observed - out$residual, out$residual))
  CPR = cor(out$observed - out$residual, out$residual)
  sink()
  
  print("mean error, ME, ideally 0:")
  print(mean(out$residual))
  ME = mean(out$residual)
  
  print("MSPE, ideally small")
  print(mean(out$residual^2))
  MSPE = mean(out$residual^2)
  
  print("Mean square normalized error, MSNE, ideally close to 1")
  print(mean(out$zscore^2))
  MSNE = mean(out$zscore^2)
  
  print("correlation observed and predicted, COP, ideally 1")
  print(cor(out$observed, out$observed - out$residual))
  COP = cor(out$observed, out$observed - out$residual)
  
  print("correlation predicted and residual, CPR, ideally 0")
  print(cor(out$observed - out$residual, out$residual))
  CPR = cor(out$observed - out$residual, out$residual)
  
}

# mapping
mapping  <- function(m) {
  OK<-krige(Depth~1,
            loc= vars,        # Data frame
            newdata=grid_projected,      # Prediction grid
            model = m)       # fitted varigram model
  summary(OK)
  
  OK$OK.pred <-InvBoxCox(OK$var1.pred, lambda = lmbda$Depth)
  OK$OK.var <-InvBoxCox(OK$var1.var, lambda = lmbda$Depth)
  
  # OK$x = grid_projected$x
  # OK$y = grid_projected$y
  
  sink("./out/OK_prediction_summaries.txt")
  print("Comparison between targeterved and predicted")
  #print(summary(target$target))
  print(summary(OK["OK.pred"]))
  
  print("Variance summary")
  print(summary(OK["OK.var"]))
  sink()
  
  print("Comparison between targeterved and predicted")
  #print(summary(target$target))
  print(summary(OK["OK.pred"]))
  
  print("Variance summary")
  print(summary(OK["OK.var"]))
  
  
  ###########
  # converting the grid to raster
  ############
  #########
  ##plotting
  #########
  v = vect(OK["OK.pred"])
  r = rast(v, ncol = 464.5697, nrow = 464.5697)
  OK.pred <- raster(rasterize(v, r, "OK.pred"))
  writeRaster(OK.pred, "./out/OK.pred")
  
  v = vect(OK["OK.var"])
  r = rast(v, ncol = 464.5697, nrow = 464.5697)
  OK.var <- raster(rasterize(v, r, "OK.var"))
  writeRaster(OK.var, "./out/OK_var")
  
  p3<-ggR(OK.pred, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("gray60", "lightblue", "lightgreen","yellow","red", "darkred"),
                         breaks =  c(19, 50, 100, 150, 200, 215),  na.value = "white")+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ggtitle(paste0("OK Predicted for Depth"))+
    theme(plot.title = element_text(hjust = 0.5))
  
  p4<-ggR(OK.var, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("black","gray50", "white"),
                         breaks = c(500, 1500, 2300),  na.value = "white")+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ggtitle(paste0("OK Predition Variance for Depth"))+
    theme(plot.title = element_text(hjust = 0.5))
  
  p = grid.arrange(p3,p4, ncol = 2)  # Multiplot
  ggsave(p, filename = paste0("/OK_Map_Depth.png"), device = "png",path = paste0("./out/"))
  return(list(OK.pred, OK.var))
  #return(list(ME, MSPE, MSNE, COP, CPR))
}

########
### Pre-processing
########
# read the target variables
data <- read.csv("./in/data_normal.csv")
lmbda <- read.csv("./in/lambdas.csv")
# read the grid data
grid <- st_read("./in/slk_grid_high.shp")

# target variable preprocessing

#preprocessing <- function(var) {
  # separating the target variable
  #assign("target", targets[,var])
  target = as.data.frame(data$Depth)
  
  # remove outlier based on the variable we chose
  # we only remove extreme outliers https://people.richland.edu/james/lecture/m170/ch03-pos.html#:~:text=Extreme%20outliers%20are%20any%20data,or%20above%20the%20third%20quartile.
  is_outlier <- function(x) {
    return(x < quantile(x, 0.25) - 3 * IQR(x) | x > quantile(x, 0.75) + 3 * IQR(x))
  }
  
  outliers = which(is_outlier(unlist(target)) == TRUE)
  
  if (length(outliers) != 0) {
    t = as.data.frame(target)
    target = t[-outliers,]
    covs = covs[-outliers,]
  }
  
  
  # 
  # # which variables are highly correlated with the target variable
  # variables = tibble()
  # variables = covs[1]
  # j = 1
  # for (i in 5:ncol(covs)){
  #   t = cor.test(target, covs[,i]) # added [,1] to target because it is a data frame
  #   if  (t$p.value <= 0.05 & t$estimate >= 0.6){
  #     variables[j] = covs[,i]
  #     names(variables)[j] = names(covs)[i]
  #     j = j + 1
  #   }
  # }
  # variables$lon = covs$longitude
  # variables$lat = covs$latitude
  # variables$soilId = covs$Id
  # variables$target = target # added [,1] to target because it is a data frame
  # print("final list of variables")
  # print(names(variables))
  
  # return(list(variables, attr(target, 'lambda'), j-1))
  # }


# prepare data 

  coordinates(data) = ~Longitude+Latitude
  
  #vars$long = covs$long
  #vars$latit = covs$latit
  vars = st_as_sf(data)
  st_crs(vars) = 4326
  vars = st_transform(vars, 32644 )
  vars$x = st_coordinates(vars)[,1]
  vars$y = st_coordinates(vars)[,2]
  
  # transforming to https://epsg.io/32644
  grid_projected = st_transform(grid, 32644)
  grid_projected$x = st_coordinates(grid_projected)[,1]
  grid_projected$y = st_coordinates(grid_projected)[,2]




# function(target, a, psill, model, range, nugget)

# successful model: 75000, 30000,"Exp", 30000, 1000
#47400
95000
# 780000: 0.003948495
# 920000: 0.008498575
# 912000: 0.007449733 outlier appears
# 880000: 0.002903889
# 850000: 0.001420433
# 845000: 0.001285435
# 848000: 0.001080929

#35000 2.315417
#45000
m = modelling1(vars$Depth,750000, 40000,"Exp", 40000, 10000) # the phenomenon is monidirectional
#autofitVariogram(data$Depth~1, data)
crosvals(m)
mapping(m)

  ########
  # documenting the result of interpolation
  #######
   #  ok1 = try(ok(vmc1500_obs), silent = T)
   #  ok2 = try(ok(auto_Obs), silent = T)
   #  ok3 = try(ok(auto_m3), silent = T)
   #  ok4 = try(ok(auto_Clay30cm), silent = T)
   #  ok5 = try(ok(auto_silt30cm), silent = T)
   #  ok6 = try(ok(auto_cec30cm), silent = T)
   # 
   #  results = list(vmc1500_obs = NULL,
   #                 auto_Obs = NULL,
   #                 auto_m3 = NULL,
   #                 auto_Clay30cm= NULL,
   #                 auto_silt30cm = NULL,
   #                 auto_cec30cm =NULL)
   # 
   # for (i in c(1:6)){
   #   data = paste0("ok",i)
   #   value = get(data)
   #   if (length(value)==5){
   #     results[[i]] = unlist(value)
   #   }
   # }
   # 
   #  ok.results = as.data.frame(do.call(rbind, results))
   # 
   #  write.csv(ok.results, paste0("./out/ok_results_vmc1500_", ebrahim, ".csv"))
    
    #######
    # Making raster files and writing rasters out
    ######
    m = filter(ok_finalmodels_vmc1500, Target == ebrahim)
    m = get(as.character(m)[2])
    output <- ok(m)
    writeRaster(output[[1]], paste0("./out/ok_PredMap_vmc1500_", ebrahim, ".tif"), overwrite=TRUE)
    writeRaster(output[[2]], paste0("./out/ok_VarMap_vmc1500_", ebrahim, ".tif"), overwrite=TRUE)
}

  # aggregating the data
  data_path <- "./out"   # path to the data
  files <- dir(data_path, pattern = "ok_results_vmc1500") # get file names

  data <- data_frame(filename = files) %>% # create a data frame
    # holding the file names
    mutate(file_contents = map(filename,          # read files into
                               ~ read_csv(file.path(data_path, .))) # a new data column
    )


  Outputs = unnest(data, cols = c(file_contents))
  write.csv(Outputs, "./out/ok_results_aggregated_vmc1500.csv")

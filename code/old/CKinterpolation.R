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
  
  v.map<-variogram(target, data = vars, map = TRUE, cutoff = a, width = a/15, cressie = T)
  
  png("./out/CKvariogramMap.png")
  print(plot(v.map, col.regions = bpy.colors(64),
             main="Variogram Map",
             xlab="x",
             ylab="y"))
  dev.off()
  
  ##########
  # variogram cloud
  ##########
  v.cloud<-variogram(target, data = vars, cloud = TRUE, cutoff = a, width = a/15, cressie = T)
  
  png("./out/CKvariogramCloud.png")
  print(plot(v.cloud, col.regions = bpy.colors(64),
             main="Variogram cloud",
             xlab="x",
             ylab="y"))
  dev.off()
  
  # ##########
  # # directional variogram
  # ##########
  v.directional<-variogram(target, data = vars, alpha = c(45, 90, 180, 360), cutoff = a, width = a/15, cressie = T)
  m.directional<-vgm(psill, model, range, nugget)
  g1 <- gstat(target, id = "target", model = m.directional, fill.all=T)

  m.f.directional <- fit.lmc(v.directional, g1,
                   fit.ranges =F)

  
  png("./out/CKDirectionalVariogram.png")
  print(plot(v.directional, m.f.directional,
             main = "Directional Variograms",
             sub = "Azimuth 30N (left), 120N (right)",
             pch = 20, col = "blue"))
  dev.off()
  
  #########
  # final variogram and its model
  #########
  
  # #Intial parameter set by eye esitmation
  v.directional<-variogram(target, data = vars, cloud=F, cutoff = a, width = a/15, cressie = T)
  m.directional<-vgm(psill, model, range, nugget)
  g1 <- gstat(target, id = "target", model = m.directional, fill.all=T)
  
  m.f.directional <- fit.lmc(v.directional, g1,
                             fit.ranges =F)

  # proper plotting
  #
  #
  plot(v, m.f.directional , plot.numbers = TRUE)
  
  png("./out/CKOmniDirectionalVariogram.png")
  print(plot(v, pl=F,
             model=m.f.directional,
             col="black",
             cex=0.9,
             lwd=0.5,
             lty=1,
             pch=19,
             main="Variogram and Fitted Model\n Box-Cox Transformed ",
             xlab="Distance (m)",
             ylab="Semivariance"))
  dev.off()

  
   sink("./out/CK_vario.txt")
     print(m.f.directional)
  #   print("Goodness-of-fit (The lower the better)")
  #   print(attributes(m.f.directional)$SSErr)
   sink()
  # print("Goodness-of-fit (The lower the better)")
  # print(attributes(m.f.directional)$SSErr)
  return(m.f.directional)
}

# cross validation
crosvals <- function(m) {
  #cross validation
  
  out<-krige.cv(as.formula(paste("Depth ~", paste(names(variables), collapse = "+"))),
                vars,
                model =m,
                nmax = 40,
                nfold=10)
  
  sink("./out/CK_CV.txt")
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
  CK<-krige(Depth~as.formula(paste("Depth ~", paste(names(variables), collapse = "+"))),
            loc= vars,        # Data frame
            newdata=grid_projected,      # Prediction grid
            model = m)       # fitted varigram model
  summary(CK)
  
  CK$CK.pred <-InvBoxCox(CK$var1.pred, lambda = lmbda$Depth)
  CK$CK.var <-InvBoxCox(CK$var1.var, lambda = lmbda$Depth)
  
  # CK$x = grid_projected$x
  # CK$y = grid_projected$y
  
  sink("./out/CK_prediction_summaries.txt")
  print("Comparison between targeterved and predicted")
  #print(summary(target$target))
  print(summary(CK["CK.pred"]))
  
  print("Variance summary")
  print(summary(CK["CK.var"]))
  sink()
  
  print("Comparison between targeterved and predicted")
  #print(summary(target$target))
  print(summary(CK["CK.pred"]))
  
  print("Variance summary")
  print(summary(CK["CK.var"]))
  
  
  ###########
  # converting the grid to raster
  ############
  #########
  ##plotting
  #########
  v = vect(CK["CK.pred"])
  r = rast(v, ncol = 464.5697, nrow = 464.5697)
  CK.pred <- raster(rasterize(v, r, "CK.pred"))
  
  v = vect(CK["CK.var"])
  r = rast(v, ncol = 464.5697, nrow = 464.5697)
  CK.var <- raster(rasterize(v, r, "CK.var"))
  
  
  p3<-ggR(CK.pred, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("red", "green","blue", "darkblue"),
                         breaks =  c(100, 120, 140, 170),  na.value = "white")+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ggtitle(paste0("CK Predicted for Depth"))+
    theme(plot.title = element_text(hjust = 0.5))
  
  p4<-ggR(CK.var, geom_raster = TRUE) +
    scale_fill_gradientn("", colours = c("white","gray60", "black"),
                         breaks = c(1400, 1750, 2050),  na.value = "white")+
    theme_bw()+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    ggtitle(paste0("CK Predition Variance for Depth"))+
    theme(plot.title = element_text(hjust = 0.5))
  
  p = grid.arrange(p3,p4, ncol = 2)  # Multiplot
  ggsave(p, filename = paste0("/CK_Map_Depth.png"), device = "png",path = paste0("./out/"))
  return(list(CK.pred, CK.var))
  #return(list(ME, MSPE, MSNE, COP, CPR))
}

########
### Pre-processing
########
# read the target variables
data <- read.csv("./in/data_normal.csv")
non_normal <- read.csv("./in/joined.csv")
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
    data = data[-outliers,]
  }
  
  
  
  # # which variables are highly correlated with the target variable
  variables = tibble()
  variables = data[1]
  j = 1
  for (i in 5:ncol(data)){
    t = cor.test(data[,4], data[,i]) # added [,1] to target because it is a data frame
    if  (t$p.value <= 0.05 & (t$estimate >= 0.4  | t$estimate <= -0.4)& !is.na(t$p.value)){
      variables[j] = data[,i]
      names(variables)[j] = names(data)[i]
      j = j + 1
    }
  }
  names(variables)
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

  library("PerformanceAnalytics")
  chart.Correlation(data[, c("Depth",names(variables))], histogram=F, pch=19)

# function(target, a, psill, model, range, nugget)
  g <- gstat(NULL, id = "depth", formula =  Depth ~ 1, data=vars)
  g <- gstat(g, id = "ph30cm", formula =  ph30cm ~ 1, data=vars)
  # g <- gstat(g, id = "prec3", formula =  prec3 ~ 1, data=vars)
  # g <- gstat(g, id = "prec4", formula =  prec4 ~ 1, data=vars)
  # g <- gstat(g, id = "prec5", formula =  prec5 ~ 1, data=vars)
  # g <- gstat(g, id = "prec6", formula =  prec6 ~ 1, data=vars)
  # g <- gstat(g, id = "prec7", formula =  prec7 ~ 1, data=vars)
  # g <- gstat(g, id = "prec9", formula =  prec9 ~ 1, data=vars)
  # g <- gstat(g, id = "prec10", formula =  prec10 ~ 1, data=vars)  
  # g <- gstat(g, id = "tmax7", formula =  tmax7 ~ 1, data=vars)
  
m = modelling1(g, 1000000, 40000,"Exp", 40000, 5000) # the phenomenon is monidirectional
#autofitVariogram(data$Depth~Longitude+Latitude, data)
crosvals(m)
mapping(m)

  ########
  # documenting the result of interpolation
  #######
   #  CK1 = try(CK(vmc1500_obs), silent = T)
   #  CK2 = try(CK(auto_Obs), silent = T)
   #  CK3 = try(CK(auto_m3), silent = T)
   #  CK4 = try(CK(auto_Clay30cm), silent = T)
   #  CK5 = try(CK(auto_silt30cm), silent = T)
   #  CK6 = try(CK(auto_cec30cm), silent = T)
   # 
   #  results = list(vmc1500_obs = NULL,
   #                 auto_Obs = NULL,
   #                 auto_m3 = NULL,
   #                 auto_Clay30cm= NULL,
   #                 auto_silt30cm = NULL,
   #                 auto_cec30cm =NULL)
   # 
   # for (i in c(1:6)){
   #   data = paste0("CK",i)
   #   value = get(data)
   #   if (length(value)==5){
   #     results[[i]] = unlist(value)
   #   }
   # }
   # 
   #  CK.results = as.data.frame(do.call(rbind, results))
   # 
   #  write.csv(CK.results, paste0("./out/CK_results_vmc1500_", ebrahim, ".csv"))
    
    #######
    # Making raster files and writing rasters out
    ######
    m = filter(CK_finalmodels_vmc1500, Target == ebrahim)
    m = get(as.character(m)[2])
    output <- CK(m)
    writeRaster(output[[1]], paste0("./out/CK_PredMap_vmc1500_", ebrahim, ".tif"), overwrite=TRUE)
    writeRaster(output[[2]], paste0("./out/CK_VarMap_vmc1500_", ebrahim, ".tif"), overwrite=TRUE)
}

  # aggregating the data
  data_path <- "./out"   # path to the data
  files <- dir(data_path, pattern = "CK_results_vmc1500") # get file names

  data <- data_frame(filename = files) %>% # create a data frame
    # holding the file names
    mutate(file_contents = map(filename,          # read files into
                               ~ read_csv(file.path(data_path, .))) # a new data column
    )


  Outputs = unnest(data, cols = c(file_contents))
  write.csv(Outputs, "./out/CK_results_aggregated_vmc1500.csv")

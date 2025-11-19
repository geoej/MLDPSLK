library(tidyr)
library(ggplot2)
library(e1071) 
library(sf)
library(sp)
library(gstat)
library(leaflet)
library(readr)
library(car)
# or `library(tidyverse)`

##############
# read data and data
##############
# reading target varaibles data
data <- read_csv("./in/joined.csv")
data = data[complete.cases(data),]


#############
# plot to see if the coordinates are right 
#############
dats = data


coordinates(dats) <- ~ longitude + latitude

plot(dats)


dats = st_as_sf(dats)

st_crs(dats) = 4326
st_write(dats, paste0("./", "joind.shp"))


# Show on leaflet
leaflet(data = dats) %>% addTiles() %>%
  addMarkers(st_coordinates(dats)[,1], st_coordinates(dats)[,2])


# make a grid file
#  grid  <-  st_as_sf(data, coords = c("Latitude", "Longitude"),
#                     crs = 4326)
# 
# # https://stackoverflow.com/questions/41787313/how-to-create-a-grid-of-spatial-points
#  slk = st_read("./in/", "LKA")
#  grid <- slk %>%
#    st_make_grid(cellsize = 0.005, what = "centers") #%>% # grid of points
# #  st_intersection(DT_sf)
#  grid
#  ggplot() +
#    geom_sf(data=grid)
# 
#  grid1 = st_intersection(grid, slk)
#  grid1
# 
#  ggplot() +
#    geom_sf(data=grid1)
# 
#  st_write(grid1, "./in/slk_grid.shp")
 slk = st_read("./in/", "slk_grid")
   ggplot() +
     geom_sf(data=slk)

##################
# making histograms
##################


ggplot(gather(data), aes(value)) +
  #geom_histogram(bins = 10) +
  geom_density() +
  facet_wrap(~key, scales = 'free')
ggsave('./out/densityplot.png')



# create a qqplot for each variable
#for (i in 1:length(names(data))){
#  assign(paste0("a" ,i), ggplot(data, aes_string(sample  = names(data[i])))+ stat_qq() + stat_qq_line())
#}
# arrange qqplots in one place

# we will need to transform those variables that are not normal for both the data. 
# https://medium.com/analytics-vidhya/a-guide-to-data-transformation-9e5fa9ae1ca3


############
# transfomr non-normal data
############
options(warn=1)
data_normal = tibble()
data_normal = data[1]
data_normal[2] = data[2]
data_normal[3] = data[3]
lmbda = numeric()
# shapiro's test If it is below 0.05, the data significantly deviate from a normal distribution.
transformed_variables = list()
l = length(names(data))
#If skewness is less than -1 or greater than 1, the distribution is highly skewed. If skewness is between -1 and -0.5 or between 0.5 and 1, the distribution is moderately skewed. If skewness is between -0.5 and 0.5, the distribution is approximately symmetric.
for (i in 4:l){
  if ((skewness(data[i][[1]])> 0.5) | (skewness(data[i][[1]])< -0.5) | (shapiro.test(data[i][[1]])$p.value < 0.05))  {
    if (any(data[i][[1]] == 0)) {
      data[i][[1]][which(data[i][[1]] == 0)] = 0.0034
    }
    #data_normal[i] = log(data[i][[1]])
    
    lmbda[i] = powerTransform(data[i][[1]])$lambda
    if (is.na(lmbda[i])){
      data_normal[i] = log(data[i])
    } else {
      data_normal[i] = bcPower(data[i][[1]], lmbda[i] )
    }
    transformed_variables[i] = paste0(names(data)[i], "_T")
  }  else {
    data_normal[i] = data[i][[1]]
    transformed_variables[i] = paste0(names(data)[i], "_NT")
  }
}

unlist(transformed_variables)
names(data_normal) = names(data)
lambdas = data.frame(rbind(lmbda))
names(lambdas) = names(data)

ggplot(gather(data_normal), aes(value)) + 
  #geom_histogram(bins = 10) + 
  geom_density() +
  facet_wrap(~key, scales = 'free')
ggsave('./out/data_normal.png')

write.csv(data_normal, "./in/data_normal.csv", row.names = T)
write.csv(lambdas, "./in/lambdas.csv", row.names = T)

##########
# correlations
##########

# cors = tibble()
# 
# # correlation test:
# l = length(names(data))
# for (i in 1:l){
#   for (j in 1:l){
#     if (cor.test(data_normal[i][[1]], data_normal[j][[1]], method = "pearson")$p.value < 0.05){
#       cors[i,j] ="SC"
#     } else {
#       cors[i,j] = "NC"
#     }
#   }
# }
# names(cors) = names(data_normal)
# write.csv(cors, "covariate_cors.csv")

########
# how normal are the target variables?
#######3
l =length(names(vmc1500))
normality_vmc1500 = list()
for (i in 4:l){
  if ((skewness(vmc1500[i][[1]])> 0.5) | (skewness(vmc1500[i][[1]])< -0.5) | (shapiro.test(vmc1500[i][[1]])$p.value < 0.05))  {
    normality_vmc1500[i] = paste0(names(vmc1500)[i], "_Not_Normal")
  }else {
    normality_vmc1500[i] = paste0(names(vmc1500)[i], "_Normal")
    
    }
}
# only M3 is normal, so we need to transform others!
unlist(normality_vmc1500)

#########
# removing outliers
########3
# is_outlier <- function(x) {
#   return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
# }
# 
# outliers = list()
# for (i in 4:ncol(vmc1500)) {
#   outliers[i] = list(which(is_outlier(unlist(vmc1500[,i])) == TRUE))
# }
# 
# is_outlier(vmc1500$Obs)
# outliers_overall = unique(unlist(outliers))
# 
# #[1] 71 87 34 70 23 27 72 84 86 88
# vmc1500 = vmc1500[-outliers_overall,]
# data_normal=data_normal[-outliers_overall,]
# 
# nrow(vmc1500)
# nrow(data_normal)
# 
# write_csv(vmc1500, "vmc1500_no_outlier.csv")
# write_csv(data_normal, "data_no_outlier.csv")


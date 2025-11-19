ggR(UK.var, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("white","gray60", "black"),
                       breaks = c(1400, 1750, 2050),  na.value = "white")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_point( data=vars, aes(x=x, y=y, size=Depth, color=Depth)) +
  scale_size_continuous(range=c(1,20)) +
  ggtitle(paste0("UK Predition Variance for Depth"))+
  theme(plot.title = element_text(hjust = 0.5))

library(viridis)


ggR(UK.pred, geom_raster = TRUE) +
  scale_fill_gradientn("", colours = c("red", "green","blue", "darkblue"),
                       breaks =  c(100, 120, 140, 170),  na.value = "white")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  geom_point( data=vars, aes(x=x, y=y, size=Depth, color = Depth)) +
  scale_size_continuous(range=c(1,20)) +
  scale_color_viridis(trans="exp") +
  ggtitle(paste0("UK Predicted for Depth"))+
  theme(plot.title = element_text(hjust = 0.5))






#### used

t <- read.csv("./in/joined.csv")
t <- t[,c("Depth..m.", "latitude", "longitude")]
#t$Depth..m. = log(t$Depth..m.)
t <- t[complete.cases(t),]
coordinates(t) = ~ longitude + latitude
bubble(t)
plot(slk)
bubble(t, "Depth..m.", main = "Depth (cm)", col = 'red', fill = T, maxsize = 2, 
       do.sqrt = F)
plot(slk, add = T)

library(tmap)
library(sf)
data(World, metro)
slk = st_read("./in/", "LKA")
library(plyr)
t$precAve = rowMeans(as.data.frame(t[,12:23]))
t$precSum = rowSums(as.data.frame(t[,12:23]))

elev<- tm_shape(slk["ISO"]) +
  tm_fill("grey70") +
  tm_shape(t) +
  tm_bubbles("Depth..m.", col = "elevation", scale = 4/3.5,
             border.col = "black", border.alpha = .5, 
             style="fixed", breaks=  c(5,100,500,2500), #c(-Inf, seq(0, 6, by=2), Inf),
             palette="-RdYlBu", contrast=1, 
             title.size="Soil depth", 
             title.col="Elevation") + 
  tm_format("World")


prec <- tm_shape(slk["ISO"]) +
  tm_fill("grey70") +
  tm_shape(t) +
  tm_bubbles("Depth..m.", col = "precSum",scale = 4/3.5, 
             border.col = "black", border.alpha = .5, 
             style="fixed", breaks=  c(1000,2000,3000,4500), #c(-Inf, seq(0, 6, by=2), Inf),
             palette="-RdYlBu", contrast=1, 
             title.size="Soil depth", 
             title.col="Precipitation") + 
  tm_format("World")


ph<- tm_shape(slk["ISO"]) +
  tm_fill("grey70") +
  tm_shape(t) +
  tm_bubbles("Depth..m.", col = "ph30cm",scale = 4/3.5,
             border.col = "black", border.alpha = .5, 
             style="fixed", breaks=  c(3,5,7,9), #c(-Inf, seq(0, 6, by=2), Inf),
             palette="-RdYlBu", contrast=1, 
             title.size="Soil depth", 
             title.col="pH") + 
  tm_format("World")

p = tmap_arrange(elev, prec, ph)
tmap_save(p, "./out/bubble_3.png")

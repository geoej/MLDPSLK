library(raster)
library(terra)
library(sf)

PHOX1 <- rast("./in/slklayers/extract/PHOX_OB_SLK_SL1.tif")
CEC1 <- rast("./in/slklayers/extract/CEC_OB_SLK_SL1.tif")
ORGCBN1 <- rast("./in/slklayers/extract/ORGCBN_OB_SLK_SL1.tif")
BD1 <- rast("./in/slklayers/extract/BD_OB_SLK_SL1.tif")
VMCDUL1 <- rast("./in/slklayers/extract/VMC(DUL)_OB_SLK_SL1.tif")
VMC1WP1 <- rast("./in/slklayers/extract/VMC(WP)_OB_SLK_SL1.tif")
CLYPPT1 <- rast("./in/slklayers/extract/CLYPPT_OB_SLK_SL1.tif")
SNDPPT1 <- rast("./in/slklayers/extract/SNDPPT_OB_SLK_SL1.tif")
SLTPPT1 <- rast("./in/slklayers/extract/SLTPPT_OB_SLK_SL1.tif")

# for high grid
grid <- st_read("./in/slk_grid_high.shp")

gridvect <- vect(grid)

PHOX1ext <- extract(PHOX1, gridvect, xy = T)
CEC1ext <- extract(CEC1, gridvect, xy = T)
ORGCBN1ext <- extract(ORGCBN1, gridvect, xy = T)
BD1ext <- extract(BD1, gridvect, xy = T)
VMCDUL1ext <- extract(VMCDUL1, gridvect, xy = T)
VMC1WP1ext <- extract(VMC1WP1, gridvect, xy = T)
CLYPPT1ext <- extract(CLYPPT1, gridvect, xy = T)
SNDPPT1ext <- extract(SNDPPT1, gridvect, xy = T)
SLTPPT1ext <- extract(SLTPPT1, gridvect, xy = T)


output<- cbind(PHOX1ext,CEC1ext,ORGCBN1ext,BD1ext,VMCDUL1ext,VMC1WP1ext,CLYPPT1ext,SNDPPT1ext,SLTPPT1ext)
write.csv(output, './in/grid_covs.csv')
st_write(grid, "./in/slk_grid_high_covs.shp")


###### for low grid
gridlo <- st_read("./in/slk_grid.shp")
gridvect <- vect(gridlo)

PHOX1ext <- extract(PHOX1, gridvect, xy = T)
CEC1ext <- extract(CEC1, gridvect, xy = T)
ORGCBN1ext <- extract(ORGCBN1, gridvect, xy = T)
BD1ext <- extract(BD1, gridvect, xy = T)
VMCDUL1ext <- extract(VMCDUL1, gridvect, xy = T)
VMC1WP1ext <- extract(VMC1WP1, gridvect, xy = T)
CLYPPT1ext <- extract(CLYPPT1, gridvect, xy = T)
SNDPPT1ext <- extract(SNDPPT1, gridvect, xy = T)
SLTPPT1ext <- extract(SLTPPT1, gridvect, xy = T)


output<- cbind(PHOX1ext,CEC1ext,ORGCBN1ext,BD1ext,VMCDUL1ext,VMC1WP1ext,CLYPPT1ext,SNDPPT1ext,SLTPPT1ext)
write.csv(output, './in/gridlo_covs.csv')
st_write(gridlo, "./in/slk_gridlo_high_covs.shp")

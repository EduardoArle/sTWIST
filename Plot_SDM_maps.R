##### CLUSTER #####

#load libraries
library(raster);library(rgdal)

#set working directories in the cluster
wd_shp <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/GRIIS_shp"
wd_rasters <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Figure_impact_paper/impactStack"
wd_raster_eck <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Figure_impact_paper/impactStack_eck"
wd_map_stuff <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/World_simple_map"

#load griis shp
shp <-  readOGR("GRIIS_ISO3", dsn = wd_shp)

#load raster
setwd(wd_rasters)
map <- raster("impactStack_sum.tif")
disease <- raster("ptn_disea.tif")
hybrid <- raster("ptn_hybr.tif")

#load map tools to reproject the raster
setwd(wd_map_stuff)
world <- readRDS("Simple_world")
worldmapframe <- readRDS("World_frame")

#crop map to try and solve the problem when reprojecting (part of the map disapears)
map_crop <- crop(map, extent(-179, 179, -89, 89))
disease_crop <- crop(disease, extent(-179, 179, -89, 89))
hybrid_crop <- crop(hybrid, extent(-179, 179, -89, 89))

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe, CRS(proj4string(world)))
shp2 <- spTransform(shp, CRS(proj4string(world)))
map2 <- projectRaster(map_crop, crs = CRS(proj4string(shp2)))
disease2 <- projectRaster(disease_crop, crs = proj4string(shp2))
hybrid2 <- projectRaster(hybrid_crop, crs = proj4string(shp2))

#save reprojected raster
setwd(wd_raster_eck)
writeRaster(map2, filename = "impactStack_sum_eck.tif", format = "GTiff")
writeRaster(disease2, filename = "ptn_disea_eck.tif", format = "GTiff")
writeRaster(hybrid2, filename = "ptn_hybr_eck.tif", format = "GTiff")

##### LOCAL COMPUTER #####

#load libraries
library(raster);library(rgdal);library(rgeos)

#set working directories in the computer
wd_shp_local <- "/Users/carloseduardoaribeiro/Documents/sTWIST/GRIIS_shp/Updated_shp"
wd_rasters_local <- "/Users/carloseduardoaribeiro/Documents/sTWIST/Impact/impactStack"
wd_raster_eck_local <- "/Users/carloseduardoaribeiro/Documents/sTWIST/Impact/impactStack_eck"
wd_map_stuff_local <- "/Users/carloseduardoaribeiro/Documents/Soup/Map stuff"

#load griis shp
shp <-  readOGR("GRIIS_first_recs", dsn = wd_shp_local)

#the griis shapefile is way too detailed, making it impossible
#to plot, so I simplified the borders a bit
shp2 <- gSimplify(shp,0.2,topologyPreserve = T)

#load raster
setwd(wd_rasters_local)
map <- raster("impactStack_sum.tif")

#load map tools
setwd(wd_map_stuff_local)
world <- readRDS("wrld.rds")
worldmapframe <- readRDS("worldmapframe.rds")

# reproject worldframe to Eckert
worldmapframe2 <- spTransform(worldmapframe, CRS(proj4string(world)))

#load reprojected raster
setwd(wd_raster_eck_local)
map_eck <-  raster("impactStack_sum_eck.tif")
disea_eck <-  raster("ptn_disea_eck.tif")
hybr_eck <-  raster("ptn_hybr_eck.tif")

#crop rasters by worldmapframe2
map_eck2 <- crop(map_eck, extent(worldmapframe2))

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe, CRS(proj4string(world)))
shp3 <- spTransform(shp2, CRS(proj4string(world)))

#make a buffer of the frame to solve the weird things out it
frame2 <- gBuffer(worldmapframe, width = 6000000)
frame3 <- gDifference(frame2, worldmapframe)

##### RED MAPS #####

#create colour ramp
col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

# SUM #

#plot
par(mar=c(2,2,2,2))

plot(map_eck, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width

# DISEASE #

#plot
par(mar=c(2,2,2,2))

plot(disea_eck, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width

# HYBRIDISATION #

#plot
par(mar=c(2,2,2,2))

plot(hybr_eck, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width

##### COLOURFUL MAPS #####

# SUM #

#create colour ramp

col_leg <- colorRampPalette(c("white", "#ffffbf", "#fee08b",
                              "#fdae61", "#f46d43", 
                              "#d53e4f", "#9e0142",
                              "#9e0142", "#9e0142"))

#plot
par(mar=c(2,2,2,2))

plot(map_eck2, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width

# DISEASE #

#plot
par(mar=c(2,2,2,2))

plot(disea_eck, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width

# HYBRIDISATION #

#plot
par(mar=c(2,2,2,2))

plot(hybr_eck, legend = F, bty = "n", box = F, axes = F, add = F, col = col_leg(20))
plot(frame3, col = "white", add = T, border = NA)
plot(worldmapframe, add = T)
plot(shp3, add = T)

# adapted function
myGradientLegend(valRange = c(0, max(map_eck2[], na.rm = T)), 
                 pos=c(0.3,0.13,0.7,.145),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("Low","High"),
                 cex = 1.5)

## save 2000 width



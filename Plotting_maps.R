#load libraries
library(raster);library(rgdal)

#set working directories
wd_shp <- "C:/Users/ca13kute/Dropbox/sTWIST/GRIIS_shp" #griis shapefile
wd_map_stuff <- "C:/Users/ca13kute/Dropbox/sTWIST" #files to shape the map

#load griis_shp
shp <- readOGR("GRIIS_ISO3",dsn = wd_shp)

#the griis shapefile is way too detailed, making it impossible
#to plot, so I simplified the borders a bit
shp2 <- gSimplify(shp,0.2,topologyPreserve = T)

#put data back making a spatialPolygonDataFrame
shp3 <- SpatialPolygonsDataFrame(shp2,shp@data)

# Load world map frame and continent outline
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
    #I get warning messages here, but things keep working!
shp2 <- spTransform(shp2,CRS(proj4string(world)))
    #same!

par(mar=c(1,1,1,1))
plot(shp2)
plot(worldmapframe,add=T)

# load old griis table
setwd("C:/Users/ca13kute/Dropbox/sTWIST")
old_table <- read.csv("AlienSpecies_MultipleDBs_Masterfile.csv")
countries <- as.character(unique(old_table$Country))

missing <- as.character(shp2$Region2[-which(shp2$Region2 %in% 
                                              countries)])

grep(missing[29],countries)

countries[207] <- missing[15]
countries[212] <- missing[16]
countries[27] <- missing[19]
countries[49] <- missing[21]
countries[184] <- missing[25]

merge_table <- table[,c(2,4,8)]

shp3 <- shp2 #create a copy of the shp
shp3$n_species <- rep(9999,nrow(shp3))  #include n_species 
shp3$ISI <- rep(9999,nrow(shp3))  #include ISI
for(i in 1:nrow(shp3))
{
  a <- which(as.character(merge_table$Location) == 
               as.character(shp3$Region[i]))
  if(length(a) == 1)
  {
    shp3$n_species[i] <- merge_table$n_species[a]   
    shp3$ISI[i] <- merge_table$ISI[a]
  }else{
    shp3$n_species[i] <- NA  
    shp3$ISI[i] <- NA
  }
}

head(shp3@data)


#check data on specific country

patt <- "Antarctica"
target_country <- shp3[grep(patt,shp3$Region2),]
target_country@data

shp3$n_species[-which(shp3$Region2 %in% countries)] <- "no data"
shp3$n_species[which(is.na(shp3$n_species))] <- 0
shp3$n_species[which(shp3$n_species == "no data")] <- NA
shp3$n_species <- as.numeric(shp3$n_species)

shp3$ISI[-which(shp3$Region2 %in% countries)] <- "no data"
shp3$ISI[which(is.na(shp3$ISI))] <- 0
shp3$ISI[which(shp3$ISI == "no data")] <- NA
shp3$ISI <- as.numeric(shp3$ISI)

#create vector to populate with the colours
col_ISI <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_ISI <- shp3$ISI[which(!is.na(shp3$ISI))] * 2.55

col_ISI[which(!is.na(shp3$ISI))] <- rgb(40,40,148,
                                        alpha=alpha_ISI,
                                        maxColorValue = 255)

col_ISI[which(col_ISI=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(shp3,col=col_ISI)
plot(worldmapframe,add=T)
plot(shp3[which(is.na(shp3$n_species)),],add=T,density=150)

col_leg <- colorRampPalette(c("white", rgb(40,40,148,
                                           alpha=255,
                                           maxColorValue = 255)))

gradientLegend(valRange = c(0, 1), 
               pos=c(0.3,0.23,0.7,.245),
               color = col_leg(20), 
               side = 1,
               n.seg = 1)


### Plot small map

#create vector to populate with the colours
col_n_sps <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_n_sps <- shp3$n_species[which(!is.na(shp3$n_species))]/17 * 255

col_n_sps[which(!is.na(shp3$n_species))] <- rgb(135,0,0,
                                                alpha=alpha_n_sps,
                                                maxColorValue = 255)

col_n_sps[which(col_n_sps=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(shp3,col=col_n_sps)
plot(worldmapframe,add=T)
plot(shp3[which(is.na(shp3$n_species)),],add=T,density=150)

col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

gradientLegend(valRange = c(0, 17), 
               pos=c(0.3,0.23,0.7,.245),
               color = col_leg(20), 
               side = 1,
               n.seg = 1)




#save tables

table_bubble <- table[,-c(1,10)]

setwd("C:/Users/ca13kute/Dropbox/sTWIST/Figure")

write.csv(table_bubble,"Table_bubble_plot.csv")

head(shp3@data)  

table_maps <- shp3@data[,c(1,8,9)]

write.csv(table_maps,"Table_maps.csv")
maxColorValue = 255)))
##################################


brasil <- shp[which(shp$Region == "France"),]
plot(brasil)


?gradientLegend

emptyPlot(1,1, main='Test plot', axes=FALSE)
box()
# number of segments:
gradientLegend(valRange=c(-14,14), n.seg=3, pos=c(.2,.3,.5,.5),
               side=1)




a <- shp3[which(is.na(shp3$n_species)),]
a@data     

b <- rgb(40,40,148,alpha=100,maxColorValue = 255)

target_country <- shp3[grep(patt,shp3$Region2),]
target_country@data

#check data from the GRIIS old table

grep("Greenland",old_table$Country)
a <- old_table[which(old_table$Country == "Alaska"),]

test <- shp3[1,]
plot(test,col=b)

plot(shp[1,],density=10,col="red")

rgb(40,40,148) #blue
rgb(135,0,0) #red
head(shp3@data)

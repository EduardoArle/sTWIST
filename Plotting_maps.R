#load libraries
library(raster);library(rgdal);library(rgeos)

#set working directories
wd_shp <- "/Users/carloseduardoaribeiro/Documents/sTWIST/GRIIS_shp/Updated_shp" #griis shapefile
wd_tables <- "/Users/carloseduardoaribeiro/Documents/sTWIST/Impact" #tables with info to be put in the map
wd_map_stuff <- "/Users/carloseduardoaribeiro/Documents/Soup/Map stuff" #files to shape the map

#load table with indicator info
setwd(wd_tables)
table <- read.csv("Information_Status_Indicator.csv")

#load griis_shp
shp <- readOGR("GRIIS_first_recs",dsn = wd_shp)

#the griis shapefile is way too detailed, making it impossible
#to plot, so I simplified the borders a bit
shp2 <- gSimplify(shp,0.2,topologyPreserve = T)

#put the attribute table back making a spatialPolygonDataFrame
shp3 <- SpatialPolygonsDataFrame(shp2,shp@data)

Load world map frame and continent outline
setwd(wd_map_stuff)

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
    #I get warning messages here, but things keep working!
shp3 <- spTransform(shp3,CRS(proj4string(world)))
    #same!

#plot maps
par(mar=c(1,1,1,1))
plot(shp3)
plot(worldmapframe,add=T)

#### The next parts are a bit messy because I had to do a lot of
#things manually

# load old griis table
setwd(wd_tables)
old_table <- read.csv("AlienSpecies_MultipleDBs_Masterfile.csv")
countries <- as.character(unique(old_table$Country))

#identify GRIIS regions missing in the tables
#as we are only dealing with amphibians, not all regions are represented
missing <- as.character(shp3$Region2[-which(shp3$Region2 %in% 
                                              countries)])

grep(missing[29],countries)

countries[207] <- missing[15]
countries[212] <- missing[16]
countries[27] <- missing[19]
countries[49] <- missing[21]
countries[184] <- missing[25]

#create a table to merge the info into the shp attribute table
merge_table <- table[,c(2,4,5,9)]

shp4 <- shp3 #create a copy of the shp
shp4$n_species <- rep(9999,nrow(shp4))  #include n_species 
shp4$ISI <- rep(9999,nrow(shp4))  #include ISI
for(i in 1:nrow(shp4))
{
  a <- which(as.character(merge_table$Location) == 
               as.character(shp4$Region[i]))
  if(length(a) == 1)
  {
    shp4$n_species[i] <- merge_table$n_species[a]   
    shp4$ISI[i] <- merge_table$ISI[a]
  }else{
    shp4$n_species[i] <- NA  
    shp4$ISI[i] <- NA
  }
}

head(shp4@data)


#check data on specific country

patt <- "Antarctica"
target_country <- shp3[grep(patt,shp3$Region2),]
target_country@data

### mark as "no data" the regions that are not listed in GRIIS at all
### mark as 0 those regions listed in GRIIS, but without amphibians
shp4$n_species[-which(shp4$Region2 %in% countries)] <- "no data"
shp4$n_species[which(is.na(shp4$n_species))] <- 0
shp4$n_species[which(shp4$n_species == "no data")] <- NA
shp4$n_species <- as.numeric(shp4$n_species)

shp4$ISI[-which(shp4$Region2 %in% countries)] <- "no data"
shp4$ISI[which(is.na(shp4$ISI))] <- 0
shp4$ISI[which(shp4$ISI == "no data")] <- NA
shp4$ISI <- as.numeric(shp4$ISI)

#create vector to populate with the colours
col_ISI <- rep("xx",nrow(shp3)) 

#create vector to populate with the transparency
alpha_ISI <- shp4$ISI[which(!is.na(shp4$ISI))] * 2.55

col_ISI[which(!is.na(shp4$ISI))] <- rgb(40,40,148,
                                        alpha=alpha_ISI,
                                        maxColorValue = 255)

col_ISI[which(col_ISI=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(shp4,col=col_ISI) # plot map with varying transparency to show the overall indicator values
plot(worldmapframe,add=T) # plot frame
plot(shp4[which(is.na(shp4$n_species)),],add=T,density=150) #plot the countries with no data with dashed lines

#prepare legend
col_leg <- colorRampPalette(c("white", rgb(40,40,148,
                                           alpha=255,
                                           maxColorValue = 255)))

# adapted function
myGradientLegend(valRange = c(0, 1), 
                 pos=c(0.3,0.23,0.7,.245),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 0,
                 values = c("No evidence","Complete"),
                 cex = 1.5)

## save 2000 width


### Plot small map

#create vector to populate with the colours
col_n_sps <- rep("xx",nrow(shp4)) 

#create vector to populate with the transparency
alpha_n_sps <- shp4$n_species[which(!is.na(shp4$n_species))]/17 * 255

col_n_sps[which(!is.na(shp4$n_species))] <- rgb(135,0,0,
                                                alpha=alpha_n_sps,
                                                maxColorValue = 255)

col_n_sps[which(col_n_sps=="xx")] <- "white"

par(mar=c(2,2,2,2))

plot(shp4,col=col_n_sps)
plot(worldmapframe,add=T)
plot(shp4[which(is.na(shp4$n_species)),],add=T,density=150)

col_leg <- colorRampPalette(c("white", rgb(135,0,0,
                                           alpha=255,
                                           maxColorValue = 255)))

# adapted function
myGradientLegend(valRange = c(0, 17), 
                 pos=c(0.3,0.23,0.7,.245),
                 color = col_leg(20), 
                 side = 1,
                 n.seg = 1,
                 cex = 3)

## save 2000 width


########################### SCRAP #########################

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

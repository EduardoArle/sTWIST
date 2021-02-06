library(raster);library(rgdal);library(rgeos);library(plotfunctions)

setwd("C:/Users/ca13kute/Dropbox/sTWIST")

table <- read.csv("Information_Status_Indicator.csv")

alfa <- 180

col.asia <- rgb(136,179,213,alpha = alfa,maxColorValue = 255)
col.europe <- rgb(191,152,203,alpha = alfa,maxColorValue = 255)
col.america <- rgb(147,210,144,alpha = alfa,maxColorValue = 255)
col.africa <- rgb(245,116,116,alpha = alfa,maxColorValue = 255)

table$col <- "xx"
table$col[which(table$IPBES == "Asia-Pacific")] <- col.asia
table$col[which(table$IPBES == "Europe-Central Asia")] <- col.europe
table$col[which(table$IPBES == "Americas")] <- col.america
table$col[which(table$IPBES == "Africa")] <- col.africa

size <- (table$Rd/45) +.7

noise <- 3
noise_x <- runif(length(table$In),-noise,noise)
noise_y <- runif(length(table$Im),-noise,noise)

par(pty="s")

plot(table$In + noise_x,table$Im + noise_y,
     xlim=c(-5,105),ylim=c(-5,105),
     xlab = "Introduction evidence (In)",
     ylab = "Impact evidence (Im)",
     col = table$col, pch=19, cex = size)

countries <- c("Australia","Brazil","Norway","Italy",
               "Russian Federation","United States of America",
               "United Kingdom of Great Britain and Northern Ireland",
               "Spain","China","Ireland","Indonesia","Greece")

positions <- which(table$Location %in% countries)

table$Location[positions]

c_labels <- c("Australia","Brazil","Italy",
              "Russia","Spain","UK","USA","Indonesia",
              "China","Greece","Ireland","Norway")

text(table$In[positions]+noise_x[positions],
     table$Im[positions]+noise_y[positions]-5,
     c_labels,cex = 0.7)

### plot legend

plot(1, type="n", xlab="", ylab="", xlim=c(-5, 105), ylim=c(-5, 105))

text(x = 30, y = 100, "Range dynamics evidence (Rd)",cex = 0.7)
text(x = c(19,20,20,20,21), y = c(92,84,76,68,60), 
     c("0","25","50","75","100"),cex = 0.7)
points(x = rep(12,5), y = c(92,84,76,68,60),
       pch = 19, cex = (c(0,25,50,75,100)/45) +.7,
       col = "gray70")

text(x = 19, y = 38, "IPBES Regions",cex = 0.7)
text(x = c(20,23,25,32.5), y = c(30,24,18,12), 
     c("Africa","Americas","Asia-Pacific","Europe-Central Asia"),
     cex = 0.7)
points(x = rep(12,4), y = c(30,24,18,12),
       pch = 19, cex = .7,
       col = c(col.africa,col.america,col.asia,col.europe))


### Plot the maps

#load griis_shp
wd_shp <- "C:/Users/ca13kute/Dropbox/sTWIST/GRIIS_shp"
shp <- readOGR("GRIIS_ISO3",dsn = wd_shp)
shp2 <- gSimplify(shp,0.2,topologyPreserve = T)
shp2$a <- rep(0,length(shp2))
shp2@data <- shp@data

# Load world map frame and continent outline
setwd("C:/Users/ca13kute/Dropbox/sTWIST")

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp2 <- spTransform(shp2,CRS(proj4string(world)))

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


                                           
                                          
     
           
shp3@data                                
                                           
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

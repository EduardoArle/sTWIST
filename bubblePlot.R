library(raster);library(rgdal)

setwd("C:/Users/ca13kute/Documents/sTWIST")

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
wd_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp"
shp <- readOGR("GRIIS_ISO3",dsn = wd_shp)

# Load world map frame and continent outline
setwd("C:/Users/ca13kute/Documents/sTWIST")

world <- readRDS("wrld.rds")
worldmapframe <- readRDS("Worldmapframe.rds")

# reproject everythign to Eckert
worldmapframe <- spTransform(worldmapframe,CRS(proj4string(world)))
shp2 <- spTransform(shp,CRS(proj4string(world)))

par(mar=c(4,4,4,4))
plot(shp2)
plot(worldmapframe,add=T)

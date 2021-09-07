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

#create another column with colours to change the labeled countries contour
table$col2 <- table$col

#select countries for the labels

countries <- c("Australia","Brazil","Norway","Italy",
               "Russian Federation","United States of America",
               "United Kingdom of Great Britain and Northern Ireland",
               "Spain","China","Ireland","Indonesia","Greece")

#identify the positions of the countries to be labeled
positions <- which(table$Location %in% countries)

size <- (table$Rd/45) +.7

noise <- 3
noise_x <- runif(length(table$In),-noise,noise)
noise_y <- runif(length(table$Im),-noise,noise)

setwd("C:/Users/ca13kute/Dropbox/sTWIST/Figure/Bubble_plot")

#jpeg(file="Bubble_plot7.jpeg",width = 1000, height = 1000, quality = 100,
     #res = 250)

par(mar=c(5,5,5,5))
par(pty="s")

plot(table$In + noise_x,table$Im + noise_y,
     xlim=c(-5,105),ylim=c(-5,105),
     xlab = substitute(paste("Introduction evidence (",italic("In"),")")),
     ylab = substitute(paste("Impact evidence ( ",italic("Im"),")")),
     cex.lab = 2,
     cex.axis = 2,
     col = table$col, bg = table$col, pch=21, cex = size*2)



c_labels <- c("Australia","Brazil","Italy",
              "Russia","Spain","UK","USA","Indonesia",
              "China","Greece","Ireland","Norway")

text(table$In[positions]+noise_x[positions],
     table$Im[positions]+noise_y[positions]-5,
     c_labels,cex = 1.4)

points(table$In[positions]+noise_x[positions],
       table$Im[positions]+noise_y[positions],
       pch=21, cex = size[positions]*2) # fix pch


#dev.off()


par(pty="s")

plot(table$In + noise_x,table$Im + noise_y,
     xlim=c(-5,105),ylim=c(-5,105),
     xlab = substitute(paste("Introduction evidence (",italic("In"),")")),
     ylab = substitute(paste("Impact evidence ( ",italic("Im"),")")),
     col = table$col, bg = table$col, pch=21, cex = size)



c_labels <- c("Australia","Brazil","Italy",
              "Russia","Spain","UK","USA","Indonesia",
              "China","Greece","Ireland","Norway")

text(table$In[positions]+noise_x[positions],
     table$Im[positions]+noise_y[positions]-5,
     c_labels,cex = 0.7)

points(table$In[positions]+noise_x[positions],
       table$Im[positions]+noise_y[positions],
       pch=21, cex = size[positions]) # fix pch




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



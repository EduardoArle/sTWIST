setwd("C:/Users/ca13kute/Documents/sTWIST")

table <- read.csv("Information_Status_Indicator.csv")

alfa <- 220

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

countries <- c("Australia","Brazil")
text(table$In[which(table)]+noise_x[1],table$Im[1]+noise_y[1],countries)



head(table)

?plot


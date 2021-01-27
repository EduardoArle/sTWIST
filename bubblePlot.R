setwd("I:/MAS/04_personal/Eduardo/sTWIST")

table <- read.csv("Information_Status_Indicator.csv")

col.asia <- rgb(136,179,213,alpha = 50,maxColorValue = 255)
col.europe <- rgb(191,152,203,alpha = 50,maxColorValue = 255)
col.america <- rgb(147,210,144,alpha = 50,maxColorValue = 255)
col.africa <- rgb(245,116,116,alpha = 50,maxColorValue = 255)

table$col <- "xx"
table$col[which(table$IPBES == "Asia-Pacific")] <- col.asia
table$col[which(table$IPBES == "Europe-Central Asia")] <- col.europe
table$col[which(table$IPBES == "Americas")] <- col.america
table$col[which(table$IPBES == "Africa")] <- col.africa

par(pty="s")
plot(table$In,table$Im,xlim=c(-5,105),ylim=c(-5,105),
     col = table$col, pch=19)

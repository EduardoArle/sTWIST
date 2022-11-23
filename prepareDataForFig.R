### This script calculates the three sub-indicators of the knowledge indicator
### run in the cluster

#use script selectOccurrences to select the occurrences for the species list
#and link them to desired the shapefile regions

#then, use the script occRegion to count the number of unique occurrences per 
#species per region

#load libraries
library(plyr);library(rgdal);library(raster)

#paths 
wd.out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Intermediate_steps"

setwd(wd.out)
table <- read.csv("Amphibians_ccurrence_region_count.csv")

#rename column with the count of unique occ per region
names(table)[4] <- "n"

#keep only records between 1970 and 2019
table2 <- table[which(table$year > 1969),]
table3 <- table2[which(table2$year < 2020),]

#load griis shp
wd_shp <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/GRIIS_shp"
shp <- readOGR("GRIIS_ISO3",dsn=wd_shp)
reg_match <- shp@data[,c("Region","Region2")]

#add "Region" to table (comes with Region2) to match names in master file
table4 <- merge(table3,reg_match,by.x = "Region",by.y = "Region2",
                sort = F, all.x = T)

#rename the griis region to griisRegion
names(table4)[5] <- "griisRegion"

#create column with species and region info
table4$sps_reg <- paste0(table4$species,"_",table4$Region)

#load amphibian master file
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST")
master_file <- read.csv("GRIISAmph_FirstRecords_FromHanno4Dec2020_AmphOnly.xlxs.csv")

master_file$sps_reg <- paste0(master_file$Taxon,"_",
                              master_file$Location)

#create column informing to with lustre the occurrences belong
table4$lustre <- floor((table4$year - 1970) / 5) + 1

#count sps_reg occurrence in the 5 year period
table5 <- ddply(table4,.(species,Region,sps_reg,lustre),
                summarise, n_5y = sum(n))

#eliminate rows with combination sps_reg_n_5y < 10
table6 <- table5[-which(table5$n_5y < 10),]

#eliminate rows with combination sps_reg not contained in the master_file
table7 <- table6[which(table6$sps_reg %in% master_file$sps_reg),]

#count how many periods of five years per region have at least 10 rec
table8 <- ddply(table7,.(Region),nrow)

#make a new table for the plot
mf <- ddply(master_file,.(Location,locationID),nrow)
names(mf)[3] <- "n_species"

#include table 8 info in mf
mf2 <- merge(mf,table8,by.x = "Location",by.y = "Region",
             sort = F, all.x = T)
mf2$V1[which(is.na(mf2$V1))] <- 0

#calculate range dynamics evidence
mf2$Rd <- mf2$V1/mf2$n_species*10
mf2 <- mf2[,-4]

#include Im in the table
master_file$Im <- ifelse(master_file$IsInvasive == "Invasive",1,0)
mf_Im <- ddply(master_file,.(Location),summarise,Impac=sum(Im))

mf3 <- merge(mf2,mf_Im,by = "Location", sort = F, all.x = T)
mf3$Im <- mf3$Impac/mf3$n_species*100
mf3 <- mf3[,-5]

#include In 
master_file$In <- ifelse(master_file$FirstRecord_orig == "",0,1)
mf_In <- ddply(master_file,.(Location),summarise,Intro=sum(In))

mf4 <- merge(mf3,mf_In,by = "Location", sort = F, all.x = T)
mf4$In <- mf4$Intro/mf4$n_species*100
mf4 <- mf4[,-6]

#calculate final indicator
mf4$ISI <- (mf4$Rd + mf4$Im +mf4$In)/3

#include IPBES region
#I'll do it manually this time...

setwd(wd.out)
write.csv(mf4,"Information_Status_Indicator.csv")


###############


wd <- "I:/MAS/04_personal/Eduardo/sTWIST/GRIIS_shp"

library(raster);library(rgdal)

shp <- readOGR("GRIIS_ISO3",dsn=wd)

shp@data
head(shp@data)

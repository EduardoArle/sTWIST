#### This script calculates the number of alien species accumulated in a
#### region and the number of records per species available in GBIF

#load packages 
library(rgdal);library(raster);library(rgeos);library(maptools)
library(plyr);library(data.table)

#set paths

wd_first_recs <- "C:/Users/ca13kute/Documents/sTWIST"
wd_table <- "C:/Users/ca13kute/Documents/sTWIST/Figure/Temporal_figure"
wd_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp"
wd_figures <- "C:/Users/ca13kute/Documents/sTWIST/Figure/Temporal_figure/Plots/Regions"

#load first rec table and select the desired group

setwd(wd_first_recs)
first_recs <- read.csv("GlobalAlienSpeciesFirstRecordDatabase_v1.1.csv")

first_recs2 <- first_recs[first_recs$LifeForm == "Vascular plants",]

#load GRIIS shapefile

shp <- readOGR("GRIIS_ISO3",dsn = wd_shp)

###### check regions listed in first recs database and compare to
##### regions in the shapefile

shp_regs <- unique(shp$Region2)
first_rec_regs <- unique(first_recs2$Region)

missing <- first_rec_regs[-which(first_rec_regs %in% shp_regs)]
missing

#create a new col in the 1st recs to include modified region names
#that match the shapefile regions

first_recs2$Region2 <- first_recs2$Region

#fix missing regions manually

i = 1 #changed "Socotra Island" to "Soqotra"

missing[i]

reg_name <- shp_regs[grep("Soqotra",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 2 #changed "United Kingdom" to 
#"United Kingdom of Great Britain and Northern Ireland (the)"

missing[i]

reg_name <- shp_regs[grep("United Kingdom",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 3 #changed "Corse" to "Corsica"'

missing[i]

reg_name <- shp_regs[grep("Cor",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 4 #changed "Netherlands" to "Netherlands (the)"

missing[i]

reg_name <- shp_regs[grep("Netherlands",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 5 #changed "Russia" to "Russian Federation (the)"

missing[i]

reg_name <- shp_regs[grep("Russia",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 6 #changed "United States" to "United States of America (the)"

missing[i]

reg_name <- shp_regs[grep("United States of",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 7 #changed "Amsterdam Island" to "French Southern Territories (the)"

missing[i]

reg_name <- shp_regs[grep("French Southern",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 8 #changed "Kerguelen Islands" to "French Southern Territories (the)"

missing[i]

reg_name <- shp_regs[grep("French Southern",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 9 #changed "Bahamas" to "Bahamas (the)"

missing[i]

reg_name <- shp_regs[grep("Bahamas",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 10 #changed "British Virgin Islands" to "Virgin Islands (British)"

missing[i]

reg_name <- shp_regs[grep("Virgin Islands",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[1]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 11 #changed "Dominican Republic" to "Dominican Republic (the)"

missing[i]

reg_name <- shp_regs[grep("Dominican",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[1]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 12 #included the Channel Islands in the shapefile

missing[i]

jer <- readOGR("JEY_adm0", 
               dsn = "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp/Updated_shp/JEY_adm")

guer <- readOGR("GGY_adm0", 
                dsn = "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp/Updated_shp/GGY_adm")

plot(shp[which(shp$Region == "France"),])
plot(jer,add=T, col="red", border=NA)
plot(guer,add=T, col="red", border=NA)

jer <- spChFIDs(jer,"jer")
guer <- spChFIDs(guer,"guer")
ch_isl <- spRbind(jer,guer)
ch_isl <- gUnaryUnion(ch_isl)

ch_isl$Region <- "Channel Islands"
ch_isl$Lon <- xmin(ch_isl)
ch_isl$Lat <- ymin(ch_isl)
ch_isl$Area <- area(ch_isl)/1000000
ch_isl$Country <- "United Kingdom of Great Britain and Northern Ireland"
ch_isl$Region2 <- "Channel Islands"
ch_isl$ISO3 <- NA

ch_isl <- spChFIDs(ch_isl,paste(nrow(shp)+1))
shp2 <- spRbind(shp,ch_isl)


i = 13 #changed "Philippines" to "Philippines (the)"

missing[i]

reg_name <- shp_regs[grep("Philippines",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 14 #changed "South Korea" to "Korea, Republic of"

missing[i]

reg_name <- shp_regs[grep("orea",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[2]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 15 #changed "Cote D'Ivoire" to "Cote d'Ivoire"

missing[i]

reg_name <- shp_regs[grep("Ivoire",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 16 #changed "Congo, Democratic Republic of the" 
#to "Congo (the Democratic Republic of the)"

missing[i]

reg_name <- shp_regs[grep("Congo",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[1]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 17 #changed "Laos" to "Lao People's Democratic Republic (the)"

missing[i]

reg_name <- shp_regs[grep("Lao",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 18 #changed "Tanzania" to "Tanzania, United Republic of"

missing[i]

reg_name <- shp_regs[grep("Tanzania",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 19 #changed "Venezuela" to "Venezuela (Bolivarian Republic of)"

missing[i]

reg_name <- shp_regs[grep("Venezuela",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 20 #changed "Sudan" to "Sudan (the)"

missing[i]

reg_name <- shp_regs[grep("Sudan",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[2]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 21 #changed "Cape Verde" to "Cabo Verde"

missing[i]

reg_name <- shp_regs[grep("Cabo",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 22 #changed "Iran" to "Iran (Islamic Republic of)"

missing[i]

reg_name <- shp_regs[grep("Iran",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 23 #include "Zanzibar" in "Tanzania, United Republic of"

missing[i]

reg_name <- shp_regs[grep("Tanzania",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 24 #changed "Bolivia" to "Bolivia (Plurinational State of)" 

missing[i]

reg_name <- shp_regs[grep("Bolivia",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 25 #changed "Cayman Islands" to "Cayman Islands (the)"

missing[i]

reg_name <- shp_regs[grep("Cayman",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 26 #changed "SCentral African Republic" to "Central African Republic (the)"

missing[i]

reg_name <- shp_regs[grep("Central African Republic",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


###### check regions listed in first recs database and compare to
##### regions in the shapefile after the changed

shp_regs <- unique(shp2$Region2)
first_rec_regs <- unique(first_recs2$Region2)

missing <- first_rec_regs[-which(first_rec_regs %in% shp_regs)]
missing

#save new version of shapefile

wd <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp/Updated_shp"

writeOGR(shp2,layer = "GRIIS_first_recs",drive = "ESRI Shapefile",
         dsn = wd)

#save modified version of the first records database

setwd("C:/Users/ca13kute/Documents/sTWIST/Figure/Temporal_figure")

write.csv(first_recs2, "First_records_plants.csv", row.names = F)

#make taxonomic harmonisation on the species entries and save results
#in script named taxonomicHarmonisation

##### get data from the cluster including shapefile regions for this 
##### analysis

#load table with occurrence counts (calculated by script occRegionAnts)
setwd(wd_table)
sps_reg_count <- readRDS("Plants_occurrence_region_count")

names(sps_reg_count)[4] <- "n" #rename species counting column

#create column with species and region info in the occurrence count table
sps_reg_count$sps_reg <- paste0(sps_reg_count$species,"_",
                                sps_reg_count$Region)

#include the harmonised names into the first records table
setwd(wd_table)
harmo <- read.csv("Plants_alien_harmonised.csv")
harmo2 <- harmo[,c(1:2)]

first_recs3 <- merge(first_recs2,harmo2,
                       by.x = "Taxon",
                       by.y = "entry")

#create column with species and region info in the plants table
first_recs3$sps_reg <- paste0(first_recs3$gbifDarwinCore,"_",
                                first_recs3$Region2)

#eliminate duplicated rows in the checklists file (probably due to synonyms
#in the original names that have been resolved)

first_recs4 <- unique(as.data.table(first_recs3), #the table has to be in 
                        by = c("sps_reg"))            #data.table

#save final checklist table (harmonised names and no duplicates)

setwd(wd_table)
write.csv(first_recs4,"Final_first_recs.csv")

#eliminate rows combining sps_reg_count that are not listed in the 
#first records table

sps_reg_count2 <- sps_reg_count[which(sps_reg_count$sps_reg %in% 
                                        first_recs4$sps_reg),]



##### TESTS TO UNDERSTAND REGIONS WITH MISSING INFO

alg <- first_recs4[which(first_recs4$Region2 == "Algeria"),]
sps_reg_count[which(sps_reg_count$sps_reg == 
                      "Acacia melanoxylon_Algeria"),]
# no occ for the only species listed in the first records


###### calculate range dynamics evidence

#eliminate rows not containing year information 
sps_reg_count3 <- sps_reg_count2[which(!is.na(sps_reg_count2$year)),]

#eliminate rows representing records in regions previous to the first records
#date

sps_reg_count4 <- sps_reg_count3

for(i in 1:length(first_recs4$sps_reg))
{
  sps_reg <- first_recs4$sps_reg[i]
  a <- sps_reg_count4[which(sps_reg_count4$sps_reg == sps_reg),]
  
  if(nrow(a) > 0){
    
    b <- which(first_recs4$sps_reg == sps_reg)
    c <- first_recs4[b,]
    year <- c$FirstRecord
    
    d <- which(sps_reg_count4$sps_reg == sps_reg & 
            sps_reg_count4$year < year) 
    
    if(length(d) > 0){
      
      sps_reg_count4 <- sps_reg_count4[-d,]
    }
  }
  
  print(i)
}

nrow(sps_reg_count4)

#save 

setwd(wd_table)

write.csv(sps_reg_count4,"Species_regions_clean.csv")

################### STARTS FROM HERE ###############

setwd(wd_table)

sps_reg_count4 <- read.csv("Species_regions_clean.csv")
first_recs4 <- read.csv("Final_first_recs.csv")
          
###### CALCULATE NUMBER OF ACCUMULATED SPECIES PER REGION BY LUSTRE

#create column informing to with lustre the occurrences belong
#final lustre year
first_recs4$lustre <- floor((first_recs4$FirstRecord - 1970) / 5) + 1

#create a table to populate the new species per region over time
sps_reg_acc <- data.frame(Region = sort(unique(first_recs4$Region2)))

#for loop to populate the table
for(i in 1:9)
{
  f_r_lustre <- first_recs4[which(first_recs4$lustre <= i),]
  sps_reg_lus <- ddply(f_r_lustre, .(Region2), .fun=nrow)
  
  sps_reg_acc <- merge(sps_reg_acc, sps_reg_lus, all = T,
                        by.x = "Region", by.y = "Region2")
  
  sps_reg_acc[which(is.na(sps_reg_acc[,i+1])),i+1] <- 0
  names(sps_reg_acc)[i+1] <- i*5 + 1969
  
  print(i)
}

###### CALCULATE NUMBER OF NEW SPECIES PER REGION BY LUSTRE

#create a table to populate the new species per region over time
sps_reg_temp <- data.frame(Region = sort(unique(first_recs4$Region2)))

#for loop to populate the table
for(i in 1:9)
{
  f_r_lustre <- first_recs4[which(first_recs4$lustre == i),]
  sps_reg_lus <- ddply(f_r_lustre, .(Region2), .fun=nrow)
  
  sps_reg_temp <- merge(sps_reg_temp, sps_reg_lus, all = T,
                       by.x = "Region", by.y = "Region2")
  
  sps_reg_temp[which(is.na(sps_reg_temp[,i+1])),i+1] <- 0
  names(sps_reg_temp)[i+1] <- i*5 + 1969

  print(i)
}

###### CALCULATE NUMBER OF  RECORDS PER REGION BY YEAR

#create column informing to with lustre the occurrences belong
#final lustre year
sps_reg_count4$lustre <- floor((sps_reg_count4$year - 1970) / 5) + 1

#create a table to populate the accumulated species per region over time
recs_reg_temp <- data.frame(Region = sort(unique(first_recs4$Region2)))

#for loop to populate the table
for(i in 1:9)
{
  f_r_lustre <- sps_reg_count4[which(sps_reg_count4$lustre == i),]
  recs_reg_lus <- ddply(f_r_lustre, .(Region), summarise, recs = sum(n))
  
  recs_reg_temp = merge(recs_reg_temp, recs_reg_lus, all = T,
                       by.x = "Region", by.y = "Region")
  
  recs_reg_temp[which(is.na(recs_reg_temp[,i+1])),i+1] <- 0
  names(recs_reg_temp)[i+1] <- i*5 + 1969
  
  print(i)
}


##### PLOT PROPORTIONS

#global

rate <- apply(recs_reg_temp[,-1], 2, function(x) sum(x,na.rm=T))/
        apply(sps_reg_acc[-1], 2, function(x) sum(x,na.rm=T))

n_sps <- apply(sps_reg_temp[-1], 2, function(x) sum(x,na.rm=T))
  
years <- names(rate)

setwd(wd_figures)

jpeg(file=paste0("Global.jpeg"))

par(mar=c(4,4,4,6))

#plot rec/sps rate
plot(years, rate, type = "l", main = "Global", 
     ylab = NA, xlab = "Year", col = "blue", lwd = 2)
mtext("Records/Species", side=2, line=3, col = "blue")

#plot number of species (1st records)
par(new = TRUE)

plot(years, n_sps, 
     type = "l", lwd = 2, col = "darkorange2",
     axes = FALSE, bty = "n", xlab = "", ylab = "")

axis(side=4, at = pretty(range(n_sps)))
mtext("Species-Region Entries", side=4, line=3, col = "darkorange2")

dev.off()

#regions

regions = sort(unique(first_recs4$Region2))

for(i in 1:length(regions))
{
  rate <- as.numeric(recs_reg_temp[which(recs_reg_temp$Region == regions[i]),-1])/
         as.numeric(sps_reg_acc[which(sps_reg_acc$Region == regions[i]),-1])
  
  n_sps <- as.numeric(sps_reg_temp[which(sps_reg_temp$Region == regions[i]),-1])
  
  if(!is.na(unique(rate))){
    
    setwd(wd_figures)
    
    jpeg(file=paste0(regions[i],".jpeg"))
    
    par(mar=c(4,4,4,6))
    
    #plot rec/sps rate
    plot(years, rate, type = "l", main = regions[i], 
         ylab = NA, xlab = "Year", col = "blue", lwd = 2)
    mtext("Records/Species", side=2, line=3, col = "blue")

    #plot number of species (1st records)
    par(new = TRUE)
    
    plot(years, n_sps, 
         type = "l", lwd = 2, col = "darkorange2",
         axes = FALSE, bty = "n", xlab = "", ylab = "")
    
    axis(side=4, at = pretty(range(n_sps)))
    mtext("Species-Region Entries", side=4, line=3, col = "darkorange")
    
    
    dev.off()
  }
    
}

#######################################

#global per period of time

rate <- apply(recs_reg_temp[,-c(1,11)], 2, function(x) sum(x,na.rm=T))/
  apply(sps_reg_temp2[-1], 2, function(x) sum(x,na.rm=T))

n_sps <- apply(sps_reg_temp2[-1], 2, function(x) sum(x,na.rm=T))

years <- names(rate)

setwd(wd_figures)

jpeg(file=paste0("Global2.jpeg"))

par(mar=c(4,4,4,6))

#plot rec/sps rate
plot(years, rate, type = "l", main = "Global", 
     ylab = "Records/Species", xlab = "Year", col = "blue", lwd = 2)

#plot number of species (1st records)
par(new = TRUE)

plot(years, n_sps, 
     type = "l", lwd = 2, col = "darkorange2",
     axes = FALSE, bty = "n", xlab = "", ylab = "")

axis(side=4, at = pretty(range(n_sps)))
mtext("Species-Region Entries", side=4, line=3)

dev.off()

#regions

regions = sort(unique(first_recs4$Region2))


for(i in 6:length(regions))
{
  rate <- as.numeric(recs_reg_temp[which(recs_reg_temp$Region == regions[i]),-c(1,11)])/
    as.numeric(sps_reg_temp2[which(sps_reg_temp2$Region == regions[i]),-1])
  
  n_sps <- as.numeric(sps_reg_temp2[which(sps_reg_temp2$Region == regions[i]),-1])
  
  if(!is.na(unique(rate))){
    
    setwd(wd_figures)
    
    jpeg(file=paste0(regions[i],".jpeg"))
    
    par(mar=c(4,4,4,6))
    
    #plot rec/sps rate
    plot(years, rate, type = "l", main = regions[i], 
         ylab = "Records/Species", xlab = "Year", col = "blue", lwd = 2)
    
    #plot number of species (1st records)
    par(new = TRUE)
    
    plot(years, n_sps, 
         type = "l", lwd = 2, col = "darkorange2",
         axes = FALSE, bty = "n", xlab = "", ylab = "")
    
    axis(side=4, at = pretty(range(n_sps)))
    mtext("Species-Region Entries", side=4, line=3)
    
    
    dev.off()
  }
  
}

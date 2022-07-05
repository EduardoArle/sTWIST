#### This script calculates the number of alien species accumulated in a
#### region and the number of records per species available in GBIF

#load packages 
library(rgdal);library(raster);library(rgeos);library(maptools)

#set paths

wd_first_recs <- "C:/Users/ca13kute/Documents/sTWIST"
wd_sps_occ <- "C:/Users/ca13kute/Documents/2nd_Chapter/GloNAF_Data/GLONAF"
wd_shp <- "C:/Users/ca13kute/Documents/sTWIST/GRIIS_shp"

#load first rec table and select the desired group

setwd(wd_first_recs)
first_recs <- read.csv("GlobalAlienSpeciesFirstRecordDatabase_v1.1.csv")

first_recs2 <- first_recs[first_recs$LifeForm == "Vascular plants",]

#load GRIIS shapefile

shp <- readOGR("GRIIS_ISO3",dsn = wd_shp)

###### check regions listed in first recs database and compare to
##### regions in the shapefile

shp_regs <- unique(shp$Region)
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
#"United Kingdom of Great Britain and Northern Ireland"

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


i = 4 #changed "Russia" to "Russian Federation"

missing[i]

reg_name <- shp_regs[grep("Russia",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 5 #changed "United States" to "United States of America"

missing[i]

reg_name <- shp_regs[grep("United States of",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 6 #changed "Reunion" to "RÃ©union"

missing[i]

reg_name <- shp_regs[grep("union",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 7 #changed "Amsterdam Island" to "French Southern Territories"

missing[i]

reg_name <- shp_regs[grep("French Southern",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 8 #changed "Kerguelen Islands" to "French Southern Territories"

missing[i]

reg_name <- shp_regs[grep("French Southern",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 9 #changed "British Virgin Islands" to "Virgin Islands (British)"

missing[i]

reg_name <- shp_regs[grep("Virgin Islands",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[1]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 10 #included the Channel Islands in the shapefile

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


i = 11 #changed "South Korea" to "Korea, Republic of"

missing[i]

reg_name <- shp_regs[grep("orea",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[2]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 12 #changed "Cote D'Ivoire" to "CÃ´te d'Ivoire"

missing[i]

reg_name <- shp_regs[grep("Ivoire",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 13 #changed "Congo, Democratic Republic of the" 
#to "Democratic Republic of the Congo"

missing[i]

reg_name <- shp_regs[grep("Congo",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name[1]

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 14 #changed "Laos" to "Lao People's Democratic Republic"

missing[i]

reg_name <- shp_regs[grep("Lao",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 15 #changed "Tanzania" to "Tanzania, United Republic of"

missing[i]

reg_name <- shp_regs[grep("Tanzania",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 16 #changed "Venezuela" to "Venezuela, Bolivarian Republic of"

missing[i]

reg_name <- shp_regs[grep("Venezuela",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 17 #changed "Iran" to "Iran, Islamic Republic of"

missing[i]

reg_name <- shp_regs[grep("Iran",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 18 #changed "Syria" to "Syrian Arab Republic"

missing[i]

reg_name <- shp_regs[grep("Syria",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 19 #changed "Vietnam" to "Viet Nam"

missing[i]

reg_name <- shp_regs[grep("Viet",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]


i = 20 #include "Zanzibar" in "Tanzania, United Republic of"

missing[i]

reg_name <- shp_regs[grep("Tanzania",shp_regs)]
reg_name

first_recs2$Region2[which(first_recs2$Region == missing[i])] <- reg_name

first_recs2[which(first_recs2$Region == missing[i]),c(2,9)]

###### check regions listed in first recs database and compare to
##### regions in the shapefile after the changed

shp_regs <- unique(shp2$Region)
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



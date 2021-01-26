wd.external <- "/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020/External_data"
wd.out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Intermediate_steps"

setwd(wd.out)

table <- read.csv("Alien_amphibians_GBIF_occurrences.csv")

table <- table[complete.cases(table$decimalLongitude),]
table <- table[complete.cases(table$decimalLatitude),]

table <- unique(table)

setwd(wd.external)

field <- readRDS("locationID_griisRegionID") #read table with all gbifIDs and corresponding IDs of each field
field <- field[which(field$locationID %in% table$locationID),] #select only the rows with gbifIDs corresponding to the query

trans_table <- readRDS("griisRegion_griisRegionID") #read translating table with names of regions and region IDs 

tab <- merge(table,field,by="locationID",sort=F,all.x=T)
tab <- tab[complete.cases(tab$griisRegionID),]

tab2 <- merge(tab,trans_table,by="griisRegion_griisRegionID",sort=F,all.x=T)

setwd(wd.out)

saveRDS(tab2,"GRIIS_amphibians_GBIF_shpID")

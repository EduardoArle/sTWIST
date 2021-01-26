wd_external <- "/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020/External_data"
wd_out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Fresh_water"

setwd(wd_out)
#table <- read.csv("Fresh_water_GBIF_occurrences.csv",nrow = 100000)
table <- read.csv("Fresh_water_GBIF_occurrences.csv")

table <- table[complete.cases(table$decimalLongitude),]
table <- table[complete.cases(table$decimalLatitude),]

table <- unique(table)

setwd(wd_external)

field <- readRDS("locationID_freshWaterRegionID") #read table with all gbifIDs and corresponding IDs of each field
field <- field[which(field$locationID %in% table$locationID),] #select only the rows with gbifIDs corresponding to the query

trans_table <- readRDS("freshWaterRegion_freshWaterRegionID") #read translating table with names of regions and region IDs 

tab <- merge(table,field,by="locationID",sort=F,all.x=T)
tab <- tab[complete.cases(tab$freshWaterRegionID),]

tab2 <- merge(tab,trans_table,by="freshWaterRegionID",sort=F,all.x=T)

setwd(wd_out)

saveRDS(tab2,"FreshWater_GBIF_shpID")

library("plyr");library("data.table")

#set directory with the selected points
setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Temporal_figure")

#read table in
table <- read.csv("Alien_plants_GBIF_occurrences.csv")
#table <- read.csv("Alien_plants_GBIF_occurrences.csv",nrow=100000)

#eliminate fossil specimens
table2 <- table[-which(table$basisOfRecord == "FOSSIL_SPECIMEN"),]

#select unique occurrences by location and time
table3 <- unique(as.data.table(table2),
                 by=c("locationID","temporalID","speciesID"))

#count observations per species per year per region
table4 <- ddply(table3,.(species,year,Region), nrow) 

saveRDS(table4,"Plants_occurrence_region_count")
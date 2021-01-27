library("plyr")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Intermediate_steps")

table <- readRDS("GRIIS_amphibians_GBIF_shpID")

table2 <- ddply(table,.(species,year,griisRegion,griisRegionID), nrow)

saveRDS(table2,"Occurrence_region_count")
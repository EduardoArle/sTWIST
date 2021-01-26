library("plyr")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Fresh_water")

table <- readRDS("FreshWater_GBIF_shpID")

table2 <- ddply(table,.(species,year,freshWaterRegion), nrow)

saveRDS(table2,"Occurrence_region_count")
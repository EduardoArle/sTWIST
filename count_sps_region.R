# save files by taxon

library("data.table");library("plyr")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Select_GBIF_records/sTWIST") #set wd where the species list is


occ_amphibia <- readRDS("Occurrences_amphibia") #read occurrence file

occ_amphibia2 <- ddply(occ_amphibia,.(speciesID,griisRegionID,year),nrow)

names(occ_amphibia2)[4] <- "n"

saveRDS(occ_amphibia2,"Occurrences_amphibia_count")


occ_mammalia <- readRDS("Occurrences_mammalia") #read occurrence file

occ_mammalia2 <- ddply(occ_mammalia,.(speciesID,griisRegionID,year),nrow)

names(occ_mammalia2)[4] <- "n"

saveRDS(occ_mammalia2,"Occurrences_mammalia_count")


occ_aves <- readRDS("Occurrences_aves") #read occurrence file

occ_aves2 <- ddply(occ_aves,.(speciesID,griisRegionID,year),nrow)

names(occ_aves2)[4] <- "n"

saveRDS(occ_aves2,"Occurrences_aves_count")


occ_plantae <- readRDS("Occurrences_plantae") #read occurrence file

occ_plantae2 <- ddply(occ_plantae,.(speciesID,griisRegionID,year),nrow)

names(occ_plantae2)[4] <- "n"

saveRDS(occ_plantae2,"Occurrences_plantae_count")



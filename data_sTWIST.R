
library("data.table")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Cluster_jobs") #set wd where the species list is

occ <- readRDS("Selected_sps_final_year")

occ_mammalia <- occ[which(occ$classID==193),]

occ_amphibia <- occ[which(occ$classID==11),]

occ_aves <- occ[which(occ$classID==31),]

occ_plantae <- occ[which(occ$kingdomID==7),]

rm(occ)

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Select_GBIF_records/sTWIST") #set wd to save files per taxon

saveRDS(occ_mammalia,"Occurrences_mammalia")
saveRDS(occ_amphibia,"Occurrences_amphibia")
saveRDS(occ_aves,"Occurrences_aves")
saveRDS(occ_plantae ,"Occurrences_plantae")



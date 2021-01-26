# save files by taxon


setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Select_GBIF_records/sTWIST") #wd where the tables are

occ_amphibia <- readRDS("Occurrences_amphibia_count")


occ_mammalia <- readRDS("Occurrences_mammalia_count")


occ_aves <- readRDS("Occurrences_aves_count")


occ_plantae <- readRDS("Occurrences_plantae_count")


setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020/gbifByField") #wd where the matching tables are

sps_spsID <- readRDS("species_speciesID")


setwd("/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020/External_data")

reg_regID <- readRDS("griisRegion_griisRegionID")

setwd("/gpfs1/data/idiv_meyer/01_projects/eduardo/2nd_chapter/Select_GBIF_records/sTWIST") #wd where the tables are

occ_amphibia_comp <- merge(occ_amphibia,sps_spsID,by="speciesID",sort=F)
occ_amphibia_comp <- merge(occ_amphibia_comp,reg_regID,by="griisRegionID",sort=F)
occ_amphibia_comp <- occ_amphibia_comp[-which(is.na(occ_amphibia_comp$griisRegion)),]

saveRDS(occ_amphibia_comp,"Amphibia_complete")

occ_mammalia_comp <- merge(occ_mammalia,sps_spsID,by="speciesID",sort=F)
occ_mammalia_comp <- merge(occ_mammalia_comp,reg_regID,by="griisRegionID",sort=F)
occ_mammalia_comp <- occ_mammalia_comp[-which(is.na(occ_mammalia_comp$griisRegion)),]

saveRDS(occ_mammalia_comp,"Mammalia_complete")

occ_aves_comp <- merge(occ_aves,sps_spsID,by="speciesID",sort=F)
occ_aves_comp <- merge(occ_aves_comp,reg_regID,by="griisRegionID",sort=F)
occ_aves_comp <- occ_aves_comp[-which(is.na(occ_aves_comp$griisRegion)),]

saveRDS(occ_aves_comp,"Aves_complete")

occ_plantae_comp <- merge(occ_plantae,sps_spsID,by="speciesID",sort=F)
occ_plantae_comp <- merge(occ_plantae_comp,reg_regID,by="griisRegionID",sort=F)
occ_plantae_comp <- occ_plantae_comp[-which(is.na(occ_plantae_comp$griisRegion)),]

saveRDS(occ_plantae_comp,"Plantae_complete")




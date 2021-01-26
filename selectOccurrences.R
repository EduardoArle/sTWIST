

wd.list <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST"
wd.out <- "/gpfs1/data/idiv_meyer/01_projects/eduardo/sTWIST/Intermediate_steps"

setwd(wd.list)
list <- read.csv("GRIISAmph_FirstRecords_FromHanno4Dec2020_AmphOnly.xlxs.csv")

# create a sps_list

sps_list <- unique(list$Taxon)

selectRecords <- function(list,sel.by="species",fields=NULL,original.data=TRUE,
                          wd.data="/gpfs1/data/idiv_meyer/00_data/original/GBIF/26_april_2020",wd.out,
                          file="Selected_GBIF_occurrences",verbose=TRUE){
  if(verbose){
    if(is.null(fields)){
      warning(paste("\n No fields selected.\n", "The function will only return the input data and the gbifID."))
    }
    if(original.data==FALSE){
      warning(paste("\n The function will only return thematic fields IDs.",
                    "\n Set 'original.data' to TRUE to include orginal data in the output."))
    }
  }

  setwd(paste0(wd.data,"/gbifByField")) #set wd with the thematic field files from GBIF

  list_listID <- readRDS(paste0(sel.by,"_",sel.by,"ID"))
  list_listID <- list_listID[list_listID[,paste(sel.by)] %in% sps_list,]  #select only rows corresponding to species listed in sps_list

  gbifID_listID <- readRDS(paste0("gbifID_",sel.by,"ID")) #read table with all gbifIDs and corresponding IDs of objects in the list
  #gbifID_listID <- gbifID_listID[c(1:1000000),] #TEST DELETE
  gbifID_sel <- gbifID_listID[gbifID_listID[,paste0(sel.by,"ID")] %in% list_listID[,paste0(sel.by,"ID")],]  #select rows corresponding to the IDs of objects in the list
  rm(gbifID_listID)

  if(original.data){
    tab <- merge(gbifID_sel,list_listID,by=paste0(sel.by,"ID"),sort=F)
  }else{
    tab <- gbifID_sel
  }

  if(!is.null(fields)){
    for(i in 1:length(fields))
    {
      field <- readRDS(paste0("gbifID_",fields[i],"ID")) #read table with all gbifIDs and corresponding IDs of each field
      field <- field[which(field$gbifID %in% tab$gbifID),] #select only the rows with gbifIDs corresponding to the query
      tab <- merge(tab,field,by="gbifID",sort=F)

      if(original.data){
        field_fieldID <- readRDS(paste0(fields[i],"_",fields[i],"ID"))
        field_fieldID <- field_fieldID[field_fieldID[,paste0(fields[i],"ID")] %in% tab[,paste0(fields[i],"ID")],]  #select rows corresponding to the IDs of objects in the list
        tab <- merge(tab,field_fieldID,by=paste0(fields[i],"ID"),sort=F)
      }
    }
  }

  setwd(wd.out)
  write.csv(tab,paste0(file,".csv"),row.names=FALSE)
}

selectRecords(list = sps_list, wd.out = wd.out, 
              file="Alien_amphibians_GBIF_occurrences",
              fields = c("location","temporal"))
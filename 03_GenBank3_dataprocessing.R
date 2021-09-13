# NOTE: 
#     This script performs the first set of metadata parsing on the raw overall
#     GenBank dataset (01), which focuses on the metadata info associated with
#     the SOURCE (i.e., sample specimen). Here, two kinds of information pulled 
#     out from the initial processing of GenBank files - category names of the 
#     SOURCE metadata info (MetaHeader) and the actual entries associated with
#     each category (MetaInfo) - are parsed such that the each actual entry can 
#     be sorted into the column of the appropriate metadata category. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load main GenBank dataset - genbank01_working_overall
genbank_working_all <- fread(file = "genbank01_working_overall", 
                             sep = "\t",
                             header = TRUE, )

# split MetaHeader entries based on COLON and transpose into a data table 
#   NOTE: contains categories of metadata associated with SOURCE
metaHeader <- genbank_working_all[ , data.table::tstrsplit(MetaHeader, 
                                                           split = ":", 
                                                           fixed = TRUE)]

# convert to dataframe
metaHeader <- as.data.frame(metaHeader, stringsAsFactors = FALSE)

# split MetaInfo entries based on DOUBLE COLON and transpose into a data table
#   NOTE: contains actual metadata info for each category associated with SOURCE
metaInfo <- genbank_working_all[ , data.table::tstrsplit(MetaInfo, 
                                                         split = "::", 
                                                         fixed = TRUE)]

# convert to dataframe
metaInfo <- as.data.frame(metaInfo, stringsAsFactors = FALSE)

# define list of unique entries per column in metaHeader data table
uniqueSet <- sapply(metaHeader, FUN = function(x) unique(x))

# unlist and define overall list of unique metaHeader entries
uniqueSet <- unique(unlist(uniqueSet))

# remove NA entry from uniqueSet list
columnSet <- uniqueSet[!is.na(uniqueSet)]

# define matrix with only the number of columns as length of columnSet 
sortMeta <- matrix(NA, nrow = 0, ncol = length(columnSet))

# convert to dataframe
sortMeta <- as.data.frame(sortMeta)

# relabel columns in sortMeta to entries of columnSet
names(sortMeta) <- columnSet

# define another dataframe for adding metaInfo
sortInfo <- sortMeta   

# sort values in metaInfo into their respective headers in sortInfo
options(warn = 2)
for(i in 1:(nrow(metaHeader))){
  
  tempHeader <- as.character(metaHeader[i, ])
  
  dupliInfo <- tempHeader[!is.na(tempHeader)]
  
  dupliInfo <- table(dupliInfo)
  
  dupliHeader <- names(dupliInfo)[dupliInfo > 1]
  
  if(length(dupliHeader) > 0){
    
    for(dup in dupliHeader){
      
      match(tempHeader, dup)
      
      tempMerge <- which(tempHeader %in% dup)
      
      metaInfo[i, tempMerge[1]] <- paste(as.character(metaInfo[i, tempMerge]), collapse = "::")
      
      metaInfo[i, tempMerge[-1]] <- NA
      
      metaHeader[i, tempMerge[-1]] <- NA
      
    }
    
  }
  
  tempOrder <- match(metaHeader[i, ], columnSet)
  
  sortMeta[i, tempOrder[!is.na(tempOrder)]] <- metaHeader[i, !is.na(metaHeader[i, ])]
  
  sortInfo[i, tempOrder[!is.na(tempOrder)]] <- metaInfo[i, !is.na(metaInfo[i, ])]
  
}
options(warn = 1)

# define udpated GenBank dataset by combining sorted metedata to main dataset
genbank_updated_all <- cbind(genbank_working_all, sortInfo)

# write output for updated main GenBank dataset
write.table(genbank_updated_all, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank02_metasort_overall",
            sep = "\t", 
            row.names = FALSE)


########## end of script
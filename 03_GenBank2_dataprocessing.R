# NOTE: 
#     This script performs specific functions created specifically to pull out
#     as much metadata information as possible from individual GenBank files. 
#     The kinds of metadata in focus are the information on taxonomy, author, 
#     publishing institution, gene marker, barcode sequence, and other metadata
#     associated with SOURCE (meaning the sample specimen). The information 
#     obtained will then be compiled into one table, generating a workable 
#     dataframe containing the raw data collected from GenBank. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load functions specifically created to parse through GenBank files
source("/Users/miaberba/Desktop/THESIS/Bio 200/Scripts/annotated/GenBank_functions.R")

# define list of all individual GenBank files generated
accessList <- list.files("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/")

# check length equivalent to total number of GenBank files available
length(accessList)

# define total number of GenBank files available
numBarcode <- length(accessList)

# define list vector with a length equivalent to numBarcode
output <- vector("list", length = numBarcode)

# insert individual file names into list vector
names(output) <- accessList

# pull out information from each individual GenBank file
#   NOTE: functions used were specifically made for GenBank files
for(i in 1: numBarcode){
  
  # define path of individual GenBank file
  fileName <- paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/", 
                    accessList[i], 
                    sep = "")
  
  # read text lines of individual GenBank file
  samp <- readLines(fileName)
  
  # pull out taxonomic info from GenBank file
  taxonomicInfo <- gettaxaInfo(samp)
  
  # pull out publishing author info from GenBank file
  authorInfo <- getauthorInfo(samp)
  
  # pull out publishing institution info from GenBank file
  instiInfo <- getinstiInfo(samp)
  
  # pull out other metadata info from GenBank file 
  #   NOTE: metadata associated with "SOURCE"
  metaInfo <- getmetaInfo(samp)
  
  # pull out gene marker info from GenBank file
  geneInfo <- getgeneInfo(samp)
  
  # pull out sequence info from GenBank file
  seqInfo <- getseqInfo(samp)
  
  # compile all parsed info into output list vector
  output[[i]] <- c(taxonomicInfo, authorInfo, instiInfo, metaInfo, geneInfo, seqInfo)
  
}

# function call rbind on output list vector
genbank_all <- do.call(rbind, output)

# add row names to matrix
genbank_all <- cbind(row.names(genbank_all), genbank_all)

# convert to dataframe
genbank_all <- as.data.frame(genbank_all , stringsAsFactors = FALSE)

# relabel columns in genbank_all 
names(genbank_all) <- c("FileName", "TaxaInfo", "AuthorInfo","InstiInfo", 
                        "MetaHeader", "MetaInfo", "GeneMarker", "Sequence") 

# define data table of main GenBank dataset
examinefilename <- data.table::as.data.table(genbank_all)

# split FileName entries based on DASH and transpose into a data table
file.accession <- examinefilename[ , data.table::tstrsplit(FileName, 
                                                           split = "-", 
                                                           fixed = TRUE)]

# relabel columns in file.accession
#   NOTE: "SearchMarker" refers to the gene marker used while searching in 
#   GenBank website and may/may not be the same as gene marker info pulled  
#   out from individual file
names(file.accession) <- c("SearchMarker", "Accession")

# update main GenBank dataset by combining with file.accession
genbank_all <- cbind(file.accession, genbank_all)

# write output for main GenBank dataset 
write.table(genbank_all, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank01_working_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
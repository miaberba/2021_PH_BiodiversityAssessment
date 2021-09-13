# NOTE: 
#     This script performs the second set of metadata parsing on the updated raw 
#     GenBank dataset (02), which focuses on records metadata - specifically 
#     taxon number and Barcode of Life Data System (BOLD) cross-reference info. 
#     Here, the original entries pulled out contain the combined information of  
#     the two metadata categories. Similar to the previous parsing, the process
#     involves examining each entry and sorting them into their respective 
#     metadata columns.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load updated main GenBank dataset - genbank02_metasort_overall
genbank_metaup_all <- fread(file = "genbank02_metasort_overall", 
                            sep = "\t",
                            header = TRUE)

# split db_xref entries based on DOUBLE COLON and transpose into a data table
#   NOTE: entries in db_xref contain "::" if they have both "BOLD" number 
#   (categorized as boldInfo) and "taxon" number (categorized as genbankInfo),
#   otherwise, db_xref entries would only have taxon info
xrefInfo <- genbank_metaup_all[ , data.table::tstrsplit(db_xref,
                                                        split = "::",
                                                        fixed = TRUE)]

# relabel columns in xrefInfo
xrefInfo[ , ':='(genbankInfo = V1, boldInfo = V2)][ , c("V1", "V2") := NULL]

# identify indices where BOLD and taxon info were mixed up
#   NOTE: genbankInfo (V1) column contains mixed values, majority are taxon info 
#   (for entries that do not have BOLD info) but also some are BOLD info (for 
#   entries that have BOLD and taxon info, formatted as BOLD:number::taxon:number)
mixup <- grep("BOLD", xrefInfo$genbankInfo)

# define subset of xrefInfo that contains BOLD info under genbankInfo column
realboldinfo <- xrefInfo[mixup, "genbankInfo"]

# define subset of xrefInfo that contains taxon info under boldInfo column
realgenbankinfo <- xrefInfo[mixup, "boldInfo"]

# transpose subsets into their appropriate columns
xrefInfo[mixup, "boldInfo"] <- realboldinfo
xrefInfo[mixup, "genbankInfo"] <- realgenbankinfo

# check if mix-ups were fixed
#   NOTE: "BOLD" pattern should only be found in boldInfo column, not genbankInfo
xrefInfo[grep("BOLD", xrefInfo$genbankInfo)]
xrefInfo[grep("BOLD", xrefInfo$boldInfo)]

# define updated GenBank dataset by combining cross-reference data into main dataset
genbank_metaup_all <- cbind(genbank_metaup_all, xrefInfo)

# write output for updated main GenBank dataset
write.table(genbank_metaup_all, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank03_metaxref_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
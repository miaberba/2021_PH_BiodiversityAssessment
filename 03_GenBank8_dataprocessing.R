# NOTE: 
#     This script performs the sixth set of metadata parsing on the most updated  
#     raw GenBank dataset (06), which focuses on the gene marker metadata. Similar
#     to the previous process, this mainly involves manual editing via Google
#     Sheets. The goal is to verify and standardize entries pulled out for gene 
#     markers - keeping in mind of the gene marker of interests that have been 
#     searched (i.e., COI, CYTB, ITS2, rbcL, and matK). This script generates
#     the subset of GenBank data needed to parse through for gene marker and 
#     later, the manually edited version is then incorporated back into the 
#     main dataset.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load updated main GenBank dataset - genbank06_instiparsed_overall
genbank_markerfix <- fread(file = "genbank06_instiparsed_overall", 
                           sep = "\t",
                           header = TRUE)

# define subset of GenBank dataset containing only accession and gene marker info
fix_genemarker <- genbank_markerfix[ , c("SearchMarker", "Accession", "GeneMarker")]

# write output for GenBank gene marker dataset for manual editing
write.table(fix_genemarker,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/GnBk_marker_all.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            qmethod = "double")

# load manually edited GenBank gene marker dataset
#   NOTE: has additional columns added
marker_codes <- fread("GnBk_genemarker_parsed.tsv",
                      header = TRUE,
                      na.strings = "")

# recreate FileName column in main GenBank dataset 
#   NOTE: FileName entries were initially split based on DASH and values were  
#   transposed into a data table of two columns, "SearchMarker" and "Accession"
marker_codes$FileName <- paste(marker_codes$SearchMarker, 
                               marker_codes$Accession, 
                               sep = "-")

# merge parsed gene marker info to main GenBank dataset
#   NOTE: based on FileName entries, which were used as unique IDs
merge_genes <- merge(genbank_markerfix, 
                     marker_codes, 
                     by = 'FileName', 
                     all.x = TRUE,
                     sort = FALSE)

# check column headers in merge_genes
names(merge_genes)

# rearrange column order and drop "SearchMarker.y" [60] and "Accession.y" [61]
#   NOTE: "SearchMarker.y" and "Accession.y" have the same name and values as 
#   "SearchMarker.x" and "Accession.x" but x = from genbank_markerfix while 
#   y = marker_codes
sorted_merge_genes <- merge_genes[ , c(3, 1:2, 4:59, 62:64)]

# relabel SearchMarker.x and Accession.x columns in sorted_merge_genes
names(sorted_merge_genes)[c(1, 3)] <- c("Accession", "SearchMarker")

# write output for main GenBank dataset 
write.table(sorted_merge_genes,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank07_markerparsed_overall",
            sep = "\t", 
            row.names = FALSE)


########## end of script
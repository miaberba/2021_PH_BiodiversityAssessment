# NOTE: 
#     This script performs the fifth set of metadata parsing on the most updated  
#     raw GenBank dataset (05), which focuses on the publishing metadata. However,
#     majority of the parsing has been manually done through Google Sheets with
#     the goal of pulling out the specific university or institution as well as
#     country that published the GenBank data. In this script, the list of unique 
#     original entries for publishing institution is prepared and later, the 
#     manually parsed version of the list is then incorporated back into the 
#     main dataset.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load updated main GenBank dataset - genbank05_latlonfixed_overall
genbank_latlon_all <- fread(file = "genbank05_latlonfixed_overall", 
                            sep = "\t",
                            header = TRUE)

# define subset of GenBank dataset based on unique entries in InstiInfo
#   NOTE: entries in InstiInfo column generally contain address of institution
#   author/s is/are affiliated with and format of entries was not standardized 
unique_insti <- genbank_latlon_all[!duplicated(InstiInfo), ]

# subset unique_insti data table to contain only InstiInfo column
unique_insti <- subset(unique_insti, select = InstiInfo)

# write output for list of unique InstiInfo entries for manual editing
write.table(unique_insti,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/GnBk_insti_sorting.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            qmethod = "double")

# load manually edited list of publishing institutions and corresponding countries
insti_database <- fread("GnBk_insti_parsed.csv",
                        header = TRUE,
                        na.strings = "")

# merge publishing institution and country metadata to main GenBank dataset
#   NOTE: based on original InstiInfo entries, which were used as unique IDs
merged_institable <- merge(genbank_latlon_all, 
                           insti_database, 
                           by.x = 'InstiInfo',
                           by.y = 'Original', 
                           all.x = TRUE)

# rearrange order of columns
setcolorder(merged_institable, 
            c("SearchMarker", "Accession", "FileName", 
              "TaxaInfo", "AuthorInfo", "InstiInfo"))

# write output for updated main GenBank dataset
write.table(merged_institable,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank06_instiparsed_overall",
            sep = "\t", 
            row.names = FALSE)


########## end of script
# NOTE: 
#     This script performs the fourth set of metadata parsing on the most updated  
#     raw GenBank dataset (04), which focuses on the metadata for sampling 
#     coordinates. This requires dividing the original coordinate entries into
#     the appropriate metadata columns - latitude and longitude - and converting 
#     them into numerical values. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load updated main GenBank dataset - genbank04_taxofixed_overall
genbank_taxo_all <- fread(file = "genbank04_taxofixed_overall", 
                          sep = "\t",
                          header = TRUE)

# pull out and separate numerical info of latitude and longitude from lat_lon
#   NOTE: lat_lon entries contain both latitude and longitude info but are in
#   character format (e.g., "9.6383_N_123.838_E")
coordinates <- genbank_taxo_all[ , .(lat = as.numeric(gsub("_[A-Z]_.*$", "", lat_lon)), 
                                     lon = as.numeric(gsub("^.*_[A-Z]_|_[A-Z]$", "", lat_lon)))]

# convert numerical values of longitude info to negative if referring to WEST (W)
coordinates$lon[grep("W", genbank_taxo_all$lat_lon)] <- -coordinates$lon[grep("W", genbank_taxo_all$lat_lon)]

# convert numerical values of latitude info to negative if referring to SOUTH (S)
coordinates$lat[grep("S", genbank_taxo_all$lat_lon)] <- -coordinates$lat[grep("S", genbank_taxo_all$lat_lon)]

# define updated GenBank dataset by combining parsed coordinate info into main dataset
genbank_latlon_all <- cbind(genbank_taxo_all, coordinates)

# write output for updated main GenBank dataset
write.table(genbank_latlon_all, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank05_latlonfixed_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
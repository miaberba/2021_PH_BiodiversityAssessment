# NOTE: 
#     This script mainly serves to generate a workable dataframe of the overall
#     species occurrence data that could be used for the examination of metadata
#     gaps, taxonomic biases, and spatial biases. Here, a subset of the final
#     updated version of the main GBIF dataset will be generated so that it 
#     contains only the metadata columns of interest - specifically, information 
#     on records, taxonomy, publication, and geolocation.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load most updated and final main GBIF dataset - "gbif04_finalgeo_overall"
gbif_complete_data <- fread("gbif04_finalgeo_overall")

# subset GBIF data to contain only metadata of interest for analysis
complete_gbif_subset <- gbif_complete_data[ , c("gbifID", "eventDate", 
                                                "phylum", "class", "order", 
                                                "family", "genus", "species", 
                                                "publisher", "publishingCountry",
                                                "decimalLatitude", "decimalLongitude", 
                                                "countryCode", "province_final",  
                                                "municipality_final", "barangay_final")]

# relabel columns in complete_gbif_subset for sampling location
names(complete_gbif_subset)[14:16] <- c("province", "municipality", "barangay")

# write output for final raw species data for analysis
write.table(complete_gbif_subset, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/overall_species_data", 
            sep = "\t",
            row.names = FALSE)


########## end of script
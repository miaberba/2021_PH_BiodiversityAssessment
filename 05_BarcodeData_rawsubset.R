# NOTE: 
#     This script mainly serves to generate a workable dataframe of the overall
#     barcode data that could be used for the examination of metadata gaps, 
#     taxonomic biases, and spatial biases. Here, a subset from each of the final
#     updated overall GenBank and BOLD datasets is obtained, containing only
#     the metadata columns of interest - specifically, information on records,
#     taxonomy, publication, sequence, and geolocation. These GenBank and BOlD 
#     subsets are then combined into one table.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load final raw GenBank dataset
genbank_all <- fread("genbank08_geoparsed_overall_FINAL")

# load final raw BOLD dataset
bold_all <- fread("bold03_instiupdate_overall_FINAL")


#### 1) This section includes additional preparations of GenBank and/or BOLD datasets ----

# substitute UNDERSCORE in GenBank organism entries with SPACES
#   NOTE: similar format to BOLD species_name entries
unique(genbank_all$organism)
genbank_all$organism <- gsub("_", " ", genbank_all$organism)

# combine GenBank entries on publishing institution of each row 
#   NOTE: to retain info on the specific institution associated with 
#   GenBank data
genbank_all$PublishingInstitution <- paste(genbank_all$SpecificInstitute,
                                           genbank_all$MajorUniversity,
                                           sep = ", ")

# remove patterns with "NA" from GenBank entries for PublishingInstitution
genbank_all$PublishingInstitution <- gsub("^NA, |^NA, NA$", "", 
                                          genbank_all$PublishingInstitution)

# replace blank ("") GenBank entries with NA for PublishingInstitution
genbank_all[PublishingInstitution == ""]$PublishingInstitution <- NA

# assess unique BOLD entries in markercode column
unique(bold_all$markercode)

# standardize markercode entries referring to COI gene
#   NOTE: similar format to GenBank StdGeneMarker entries
bold_all$markercode <- gsub("COI-5P|COI-3P", "COI", bold_all$markercode)


#### 2) This section is a listing of analogous metadata between GenBank and BOLD datasets ----
#     NOTE: only for viewing/checking the columns of interest in both GenBank 
#     and BOLD datasets, no data manipulation/editing was involved at this section

# analogs for records information
genbank_all$collection_date
bold_all$collectiontime

genbank_all$Accession
bold_all$genbank_accession

genbank_all$boldInfo
bold_all$processid

# analogs for taxonomic information
genbank_all$phylum
bold_all$phylum_name

genbank_all$class
bold_all$class_name

genbank_all$order
bold_all$order_name

genbank_all$family
bold_all$family_name

genbank_all$genus
bold_all$genus_name

genbank_all$organism
bold_all$species_name

# analogs for publishing information
#   NOTE: PublishingInstitution and PublishingCountry columns in GenBank was
#   treated as analogs to the three columns that may be referring (though not 
#   explicitly indicated) as publishing institution/country in BOLD
genbank_all$PublishingInstitution # triplicate
bold_all$institution_storing
bold_all$sequencing_centers
bold_all$copyright_institutions

genbank_all$PublishingCountry # triplicate
bold_all$storing_country
bold_all$sequencing_country
bold_all$copyright_country


# analogs for sequence information
genbank_all$Sequence
bold_all$nucleotides

genbank_all$StdGeneMarker
bold_all$markercode


# analogs for geolocation information
genbank_all$lat
bold_all$lat

genbank_all$lon
bold_all$lon

genbank_all$country_edited
bold_all$country_edited

genbank_all$province_edited
bold_all$province_edited

genbank_all$municipality_edited
bold_all$municipality_edited

genbank_all$barangay_edited
bold_all$barangay_edited

genbank_all$major_issues
bold_all$major_issues


#### 3) This section combines GenBank and BOLD data into one barcode dataset ----

# subset main GenBank dataset to contain only metadata columns of interest
#   NOTE: order of metadata categories should be the same with BOLD subset
sub_genbank <- genbank_all[ , c("Accession", "boldInfo", "collection_date", "phylum", 
                                "class", "order", "family", "genus", "organism", 
                                "PublishingInstitution", "PublishingCountry", 
                                "PublishingInstitution", "PublishingCountry", 
                                "PublishingInstitution", "PublishingCountry", 
                                "Sequence", "StdGeneMarker", "lat", "lon", 
                                "country_edited", "province_edited", "municipality_edited", 
                                "barangay_edited", "major_issues")]

# subset main BOLD dataset to contain only metadata columns of interest
#   NOTE: order of metadata categories should be the same with GenBank subset
sub_bold <- bold_all[ , c("genbank_accession", "processid", "collectiontime", "phylum_name", 
                          "class_name", "order_name", "family_name", "genus_name", "species_name", 
                          "institution_storing", "storing_country", 
                          "sequencing_centers", "sequencing_country", 
                          "copyright_institutions", "copyright_country", 
                          "nucleotides", "markercode", "lat", "lon", 
                          "country_edited", "province_edited", "municipality_edited", 
                          "barangay_edited", "major_issues")]

# check if number of columns is the same between the two subset data
length(names(sub_genbank)) == length(names(sub_bold))

# relabel column labels in subsets of GenBank dataset
#   NOTE: must be standardized and the same with labels to be used in BOLD subset
names(sub_genbank) <- c("genbank_accession", "bold_processID", "collection_date", 
                        "phylum", "class", "order", "family", "genus", "species", 
                        "uniXstoring", "uniXstoring_country", "uniXsequencing", 
                        "uniXsequencing_country", "uniXcopyright", "uniXcopyright_country", 
                        "sequence", "gene_marker", "lat", "lon", 
                        "country_location", "province", "municipality", 
                        "barangay", "geolocation_issues")

# relabel column labels in subsets of BOLD dataset
#   NOTE: must be standardized and the same with labels to be used in GenBank subset
names(sub_bold) <- c("genbank_accession", "bold_processID", "collection_date", 
                     "phylum", "class", "order", "family", "genus", "species", 
                     "uniXstoring", "uniXstoring_country", "uniXsequencing", 
                     "uniXsequencing_country", "uniXcopyright", "uniXcopyright_country", 
                     "sequence", "gene_marker", "lat", "lon", 
                     "country_location", "province", "municipality", 
                     "barangay", "geolocation_issues")

# check if column names and order are the same between the two subset data
names(sub_genbank) == names(sub_bold)

# combine the two subsets into one barcode dataframe
overall_barcode_data <- rbind(sub_genbank, sub_bold)

# write output for final raw barcode data for analysis
write.table(overall_barcode_data,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/overall_barcode_data",
            sep = "\t",
            row.names = FALSE)


########## end of script
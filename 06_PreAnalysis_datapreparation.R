# NOTE:
#     This script aims to generate the final, working data tables for species 
#     occurrence and barcode data that would be used in the analyses for metadata 
#     gaps, taxonomic biases, and spatial biases. To do this, additional metadata 
#     were integrated into the raw barcode dataset - namely, countryLocation and 
#     YearSubmitted. Also, unwanted records were excluded from working species 
#     occurrence and/or barcode datasets. Lastly, the working barcode dataset was 
#     updated to fix species entries and incomplete taxo information. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load main raw species occurrence data
species_data <- fread("overall_species_data")

# load main raw barcode data
barcode_data <- fread("overall_barcode_data")

# check total number of records in raw barcode dataset
length(barcode_data$genbank_accession) 

# check total number of records in raw species occurrence dataset
length(species_data$gbifID)

# assign row numbers to species and barcode datasets
barcode_data$rowID <- 1:nrow(barcode_data)
species_data$rowID <- 1:nrow(species_data)


#### 1) This section adds countryLocation metadata in barcode data ----
#     NOTE: raw data from GenBank individual files was obtained by using specific
#     key words for gene marker of interest and "Philippines" however, some GenBank
#     entries for country_location (or country sampled) still remain NA and so,
#     additional step is done to verify if these records were really sampled 
#     in the Philippines

# identify indices of rows that are NA for country_location column
#   NOTE: only among GenBank entries (those with Accession numbers)
rowEdit <- barcode_data[is.na(country_location) & !is.na(genbank_accession), ]$rowID

# define list of the directory of all generated GenBank individual files 
listGB <- list.files("./COMBINED GenBank Individual/", full.names = TRUE)

# define numeric vector with the same length as rowEdit
outInfo <- vector("numeric", length = length(rowEdit))

# evaluate each individual file associated with NA for country_location
#   NOTE: verify if record can be linked with the Philippines by reading
#   through the entire individual file and assessing how many times the term
#   "philippines" appeared in each document
for(i in 1:length(rowEdit)){
  
  matchFile <- (grep(barcode_data$genbank_accession[rowEdit[i]], listGB))
  
  #listGB[matchFile]
  gbFile <- readLines(listGB[matchFile[1]])
  
  gbFile <- tolower(gbFile)
  
  outInfo[i] <- length(grep("philippines", gbFile))
  
  message(i)
  
}

# copy entries in country_location into new column countryLocation
barcode_data$countryLocation <- barcode_data$country_location

# update entries of countryLocation that were originally NA
#   NOTE: only update to "Philippines" if the term appeared in the individual 
#   file one or more times, otherwise, maintain NA value
barcode_data$countryLocation[rowEdit] <- ifelse(outInfo > 0, "Philippines", NA)


#### 2) This section adds yearSubmitted metadata in barcode data ----
#     NOTE: metadata for year of submission pulled out only from GenBank entries
#     since it is explicitly stated in most individual files however, no such 
#     metadata was obtained from the raw data downloaded from BOLD

# define list of all values for genbank_accession
accessList <- barcode_data$genbank_accession

# define numeric vector with the same length as accessList
outYear <- vector("numeric", length = length(accessList))

# pull out year of submission from each individual GenBank file
#   NOTE: still use listGB directory and if barcode record has no accession 
#   number or if there is no submission date indicated in the GenBank individual 
#   file, the value assigned for that entry is NA
for(i in 1:length(accessList)){
  
  if(barcode_data$genbank_accession[i] == "" |is.na(barcode_data$genbank_accession[i])){
    
    outYear[i] <- NA
    
  }else{
    
    indUse <- grep(barcode_data$genbank_accession[i], listGB)
    
    if(length(indUse) == 0){
      
      outYear[i] <- NA
      
    }else{
      
      matchFile <- indUse
      
      #listGB[matchFile]
      gbFile <- readLines(listGB[matchFile[1]])
      
      gbFile <- tolower(gbFile)
      
      outTemp <- strsplit(gsub("^.*submitted \\(|\\).*$", "", gbFile[grep("submitted \\(", gbFile)]), "-")
      
      if(length(outTemp) == 0){
        
        outYear[i] <- NA
        
      }else{
        
        outYear[i] <- as.numeric(outTemp[[1]][3])
        
      }
      
      
    }
    
    
    
  }
  
  message(i)
}

# place pulled out submission year info into respective column in barcode dataset
barcode_data$yearSubmitted <- outYear


#### 3) This section excludes unwanted records from species and barcode data ----
#     NOTE: criteria for "unwanted" records are: duplicates, foreign samples,
#     gene markers not of interest, /Homo/ specimens

# remove duplicated records from barcode_data based on genbank_accession
#   NOTE: keep all entries that are NA for genbank_accession, which represent 
#   records that are submitted only in BOLD
barcode_data_nodup <- barcode_data[(!duplicated(genbank_accession) & 
                                      !is.na(genbank_accession))|
                                     is.na(genbank_accession), ]

# subset barcode_data_nodup to contain records derived only from gene markers of interest
barcode_data_genefilter <- barcode_data_nodup[(gene_marker == "CYTB"|
                                                 gene_marker == "COI"|
                                                 gene_marker == "ITS2"|
                                                 gene_marker == "rbcL"|
                                                 gene_marker == "matK"), ]

# check for records sampled from foreign countries
unique(barcode_data_genefilter$country_location)

# remove foreign records from barcode_data_genefilter based on country_location
#   NOTE: keep entries that are NA for country_location 
barcode_data_noforeign <- barcode_data_genefilter[(country_location == "Philippines")|
                                                    is.na(country_location), ]

# remove records from barcode_data_noforeign that are associated with Hominidae family
barcode_data_final <- barcode_data_noforeign[-(grep("Hominidae", 
                                                    barcode_data_noforeign$family)), ]

# remove records from species_data that are associated with Hominidae family
species_data_final <- species_data[-(grep("Hominidae", 
                                          species_data$family)), ]


#### 4) This section edits entries for taxonomic metadata in updated barcode dataset ----

# copy entries in species into new column speciesUse
#   NOTE:  new column is for the updated/edited species entries
barcode_data_final$speciesUse <- barcode_data_final$species

# browse through unique speciesUse entries 
#   NOTE: observe patterns that indicate entries that do not refer to complete
#   species name, possible unidentified to the species level
head(unique(barcode_data_final$speciesUse), 1000)
tail(unique(barcode_data_final$speciesUse), 1000)

# replace speciesUse entries with incomplete species name to NA
barcode_data_final[grep("sp\\.|gen\\.|uncultured", 
                        barcode_data_final$speciesUse)]$speciesUse <- NA

# check barcode_data_final dataset for rows with incomplete taxonomic information
#   NOTE: only check records that have entries for speciesUse
barcode_data_final[(is.na(phylum)|is.na(class)|is.na(order)|is.na(family)|is.na(genus)) 
                   & !is.na(speciesUse), 4:9]
length(barcode_data_final[(is.na(phylum)|is.na(class)|is.na(order)|is.na(family)|is.na(genus)) 
                          & !is.na(speciesUse)]$genbank_accession)

# subset raw species_data to contain only unique species entries and taxo metadata
#   NOTE: serve as taxonomic database to fix incomplete taxo info in barcode data
#   with the assumption that taxonomic info provided in species data is accurate 
ph_taxa_db <- species_data[!duplicated(species), 4:9]

# remove entries from ph_taxa_db that are NA for species
ph_taxa_db <- ph_taxa_db[!is.na(species), ]

# merge barcode_data_final dataframe with ph_taxa_db based on species 
barcode_data_taxa <- merge(barcode_data_final, 
                           ph_taxa_db,
                           by.x = 'speciesUse',
                           by.y = 'species',
                           sort = FALSE,
                           all.x = TRUE)

# relabel columns of taxonomic metadata derived from ph_taxa_db (y)
#   NOTE: serve as the updated taxonomic metadata 
names(barcode_data_taxa)[29:33]
names(barcode_data_taxa)[29:33] <- c("phylumUse", "classUse", "orderUse", 
                                     "familyUse", "genusUse")

# relabel columns of taxonomic metadata derived from barcode_data_final (x)
#   NOTE: serve as the original taxonomic metadata 
names(barcode_data_taxa)[5:9]
names(barcode_data_taxa)[5:9] <- c("phylum_original", "class_original", "order_original", 
                                   "family_original", "genus_original")

# copy original taxo info if updated taxo metadata remains missing (NA)
#   NOTE: done at each taxonomic rank (i.e., phylum, class, order, family, genus) 
#   and there are also some entries with incomplete taxo info in raw species 
#   data, making it possible to still have missing info after updating barcode 
#   data with ph_taxa_db
barcode_data_taxa[is.na(phylumUse)]$phylumUse <- barcode_data_taxa[is.na(phylumUse)]$phylum_original 
barcode_data_taxa[is.na(classUse)]$classUse <- barcode_data_taxa[is.na(classUse)]$class_original 
barcode_data_taxa[is.na(orderUse)]$orderUse <- barcode_data_taxa[is.na(orderUse)]$order_original 
barcode_data_taxa[is.na(familyUse)]$familyUse <- barcode_data_taxa[is.na(familyUse)]$family_original 
barcode_data_taxa[is.na(genusUse)]$genusUse <- barcode_data_taxa[is.na(genusUse)]$genus_original 

# check updated barcode_data_taxa dataset for rows with incomplete taxonomic information
#   NOTE: likely there are still entries with incomplete taxo info but ideally
#   less than number of records observed before fixing with ph_taxa_db
length(barcode_data_taxa[(is.na(phylumUse)|is.na(classUse)|is.na(orderUse)|is.na(familyUse)|is.na(genusUse)) 
                         & !is.na(speciesUse)]$genbank_accession) 

# redefine final barcode dataset with updated taxonomic metadata
#   NOTE: unwanted records already excluded in previous section
barcode_data_final <- barcode_data_taxa[ , c(2:33, 1)]


###### DOUBLE CHECK - adding Kingdom in Barcode dataset

# define list of animal phyla derived from species_data_final
animalPhylum <- species_data_final[!duplicated(phylum) & 
                                     !is.na(phylum) & 
                                     (kingdom == "Animalia"), ]$phylum

# define list of plant division derived from species_data_final
plantDivision <- species_data_final[!duplicated(phylum) & 
                                      !is.na(phylum) & 
                                      (kingdom == "Plantae"), ]$phylum

# add new column in barcode_data_final
barcode_data_final$kingdomUse <- NA

# identify indices of entries representing animal taxa  
animalTaxa <- which(barcode_data_final$phylumUse %in% animalPhylum)

# categorize taxonomic kingdom of identified entried for animal taxa
barcode_data_final$kingdomUse[animalTaxa] <- "Animalia"

# identify indices of entries representing plant taxa 
plantTaxa <- which(barcode_data_final$phylumUse %in% plantDivision)

# categorize taxonomic kingdom of identified entried for plant taxa
barcode_data_final$kingdomUse[plantTaxa] <- "Plantae"

# check for barcode records with NA entries for kingdomUse
barcode_data_final[is.na(kingdomUse), ]

# edit kingdom entries of records associated with "Mesobiotus " species
#   NOTE: "Mesobiotus" refers to a genus in Kingdom Animalia
barcode_data_final[grep("Mesobiotus ", speciesUse), ]$kingdomUse <- "Animalia"

#### 5) This section saves the final working species and barcode data tables ----

# update assignment of row numbers to species and barcode datasets
#   NOTE: given that unwanted records have been excluded
barcode_data_final$rowID <- 1:nrow(barcode_data_final)
species_data_final$rowID <- 1:nrow(species_data_final)

# check total number of records in final working barcode dataset
length(barcode_data_final$rowID)

# check total number of records in final working species occurrence dataset
length(species_data_final$rowID)

# write output for final working barcode data table
fwrite(barcode_data_final,
       "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/working_barcode_data_FINAL.table",
       na = NA)

# write output for final working species occurrence data table
fwrite(species_data_final,
       "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/working_species_data_FINAL.table",
       na = NA)


########## end of script
# NOTE:
#     This script summarizes the barcode and species occurrence data to be 
#     analyzed - overall and in terms of each gene marker of interest. 
#     Additionally, it also performs first set of analyses for the systematic 
#     review of the Philippines' (PH) biodiversity. The objective is to examine 
#     for metadata gaps in terms of the completeness of records and publishing, 
#     geolocation, and taxonomic information. The process involves calculating 
#     the proportion of missing information (NA) for each metadata category. 
#     For the taxonomic metadata, in particular, much focus has been given at 
#     rate of species identification compared to the proportion of species 
#     represented in both species occurrence and barcode data. Here, the 
#     barcode data analyzed INCLUDES records that are NA for the country 
#     sampled, despite searches being filtered geographically. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load the necessary R packages
#   NOTE: data.table is needed throughout but the rest are necessary specifically
#   for the examination of spatial biases
library("data.table")

# load final working species occurrence data table
species_data_final <- fread("working_species_data_FINAL.table", 
                            na.strings = c("", NA))

# load final working barcode data table
barcode_data_final <- fread("working_barcode_data_FINAL.table", 
                            na.strings = c("", NA))


#### 1) This section determines the summary of amount of data analyzed ----

# check total number of records in species_data_final
length(species_data_final$rowID)

# check total number of records in barcode_data_final - NAinclude
length(barcode_data_final$rowID)

# define data table showing number of barcode records per gene_marker
marker_summary <- barcode_data_final[ , .N, by = .(gene_marker)]

# create bar graph of gene marker summary and save as PDF format
pdf("./Figures_unedited/GeneMarker_summary_NAinclude.pdf", paper = "a4r")

barplot(marker_summary$N, 
        names.arg = marker_summary$gene_marker, 
        col = "ivory4", xlab = "Gene marker", 
        ylab = "Number of genetic records",
        border = NA,
        ylim = c(0, 16000))

dev.off()


#### 2) This section examines the missing information in publishing and records metadata ----

# define object for the value of total barcode records
total_barcode <- length(barcode_data_final$rowID)

# determine percentage of barcode records in total lacking info on collection_date
100*length(barcode_data_final[is.na(collection_date)]$rowID)/total_barcode

# determine percentage of barcode records in total lacking info on yearSubmitted
100*length(barcode_data_final[is.na(yearSubmitted)]$rowID)/total_barcode

# determine percentage of barcode records in total lacking info on uniXcopyright
100*length(barcode_data_final[is.na(uniXcopyright)]$rowID)/total_barcode


#### 3) This section examines the missing information in geolocation metadata ----

# determine percentage of barcode records in total lacking info on coordinates
100*length(barcode_data_final[is.na(lat)|is.na(lon)]$rowID)/total_barcode  

# define object for the value of total barcode records with no coordinates
no_coordinates <- length(barcode_data_final[is.na(lat)|is.na(lon)]$rowID)

# determine proportion in subset of barcode records that have no sampling description
#   NOTE: among barcode records that already lack coordinate information
100*length(barcode_data_final[(is.na(province) & is.na(municipality) & is.na(barangay)) & 
                                (is.na(lat)|is.na(lon))]$rowID)/no_coordinates

# determine percentage of barcode records in total lacking info on sampling location
#   NOTE: records lacking coordinates and sampling description in general 
100*length(barcode_data_final[(is.na(province) & is.na(municipality) & is.na(barangay)) & 
                                (is.na(lat)|is.na(lon))]$rowID)/total_barcode

# view summary of barcode records per geolocation_issues
#   NOTE: some entries were assigned more than one category of issues, as 
#   indicated by the symbol "+", while manually evaluated and parsed
barcode_data_final[ , .N, by = .(geolocation_issues)]

# copy entries in geolocation_issues into new column geolocation_issues_summary
barcode_data_final$geolocation_issues_summary <- barcode_data_final$geolocation_issues

# assign the value "mixed" to all entries categorized with more than one issue 
#   NOTE: indicated by the symbol "+"
barcode_data_final[grep(" \\+ ", barcode_data_final$geolocation_issues_summary)
]$geolocation_issues_summary <- "mixed"

# define data table showing number of barcode records per geolocation_issues_summary
geoissues_summary <- barcode_data_final[ , .N, by = .(geolocation_issues_summary)]

# remove records that are NA for geolocation_issues_summary
#   NOTE: summary focuses on entries that have metadata on sampling locality
#   in terms of Philippine administrative units
geoissues_summary <- geoissues_summary[!is.na(geolocation_issues_summary)]

# create bar graph of geolocation issues summary and save as PDF format
pdf("./Figures_unedited/GeoIssues_summary_NAinclude.pdf", paper = "a4r")

barplot(geoissues_summary$N, 
        names.arg = geoissues_summary$geolocation_issues_summary, 
        col = "lightblue4", 
        xlab = "Issue encountered",
        ylab = "Number of genetic records",
        border = NA,
        width = 10,
        ylim = c(0, 6000))

dev.off()


#### 4) This section assesses percent of identified species at the phylum level ----

# define data table of unique species entries per phylum in species data
gbif_physp <- species_data_final[ , unique(species), by = .(phylum)]

# define data table of unique species entries per phylum in barcode data
gene_physp <- barcode_data_final[ , unique(speciesUse), by = .(phylumUse)]

# relabel phylumUse column in gene_physp
names(gene_physp)[1] <- "phylum"

# combine species and barcode data tables of unique species per phylum
#   NOTE: V1 = unique species entry
physp_list <- rbind(gbif_physp, gene_physp)

# define list of unique entries for phylum
uniquePhyl <- unique(physp_list$phylum)

# remove NA entry for uniquePhyl 
uniquePhyl <- uniquePhyl[!is.na(uniquePhyl)]

# define numeric vector with same length as uniquePhyl
useX <- vector("numeric", length = length(uniquePhyl))

# determine proportion of documented species with barcode data available
#   NOTE: evaluates each species per phylum (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniquePhyl)){
  
  useX[i] <- sum(gbif_physp[phylum == uniquePhyl[i] & !is.na(V1), ]$V1 %in% 
                   gene_physp[phylum == uniquePhyl[i] & !is.na(V1), ]$V1)/length(gbif_physp[phylum == uniquePhyl[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX
useX <- unlist(useX)

# define data table showing number of records associated with each phylumUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[ , .(numNA = sum(is.na(speciesUse)), 
                                 numTOTAL = .N), by = .(phylumUse)]

# add column in tempA for the values associated with matched entries in useX
#   NOTE: represents percent of barcoded species
tempA$useX <- useX[match(tempA$phylumUse, uniquePhyl)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for phylumUse 
tempA <- tempA[!is.na(phylumUse), ]

# create scatter plot of percent identified species per phylum and save as PDF format
pdf("./Figures_unedited/SpeciesID_phylum_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, col = "indianred3",
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1))

dev.off()


#### 5) This section assesses percent of identified species at the class level ----

# define data table of unique species entries per class in species data
gbif_clsp <- species_data_final[ , unique(species), by = .(class)]

# define data table of unique species entries per class in barcode data
gene_clsp <- barcode_data_final[ , unique(speciesUse), by = .(classUse)]

# relabel classUse column in gene_clsp
names(gene_clsp)[1] <- "class"

# combine species and barcode data tables of unique species per class
#   NOTE: V1 = unique species entry
clsp_list <- rbind(gbif_clsp, gene_clsp)

# define list of unique entries for class
uniqueClas <- unique(clsp_list$class)

# remove NA entry for uniqueClas 
uniqueClas <- uniqueClas[!is.na(uniqueClas)]

# define numeric vector with same length as uniqueClas
useX <- vector("numeric", length = length(uniqueClas))

# determine proportion of documented species with barcode data available
#   NOTE: evaluates each species per class (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueClas)){
  
  useX[i] <- sum(gbif_clsp[class == uniqueClas[i] & !is.na(V1), ]$V1 %in% 
                   gene_clsp[class == uniqueClas[i] & !is.na(V1), ]$V1)/length(gbif_clsp[class == uniqueClas[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX
useX <- unlist(useX)

# define data table showing number of records associated with each classUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[ , .(numNA = sum(is.na(speciesUse)), 
                                 numTOTAL = .N), by = .(classUse)]

# add column in tempA for the values associated with matched entries in useX
#   NOTE: represents percent of barcoded species
tempA$useX <- useX[match(tempA$classUse, uniqueClas)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for classUse 
tempA <- tempA[!is.na(classUse), ]

# create scatter plot of percent identified species per class and save as PDF format
pdf("./Figures_unedited/SpeciesID_class_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, col = "royalblue3",
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1))

dev.off()


#### 6) This section assesses percent of identified species at the order level ----

# define data table of unique species entries per order in species data
gbif_ordsp <- species_data_final[ , unique(species), by = .(order)]

# define data table of unique species entries per order in barcode data
gene_ordsp <- barcode_data_final[ , unique(speciesUse), by = .(orderUse)]

# relabel orderUse column in gene_ordsp
names(gene_ordsp)[1] <- "order"

# combine species and barcode data tables of unique species per order
#   NOTE: V1 = unique species entry
ordsp_list <- rbind(gbif_ordsp, gene_ordsp)

# define list of unique entries for order
uniqueOrd <- unique(ordsp_list$order)

# remove NA entry for uniqueOrd 
uniqueOrd <- uniqueOrd[!is.na(uniqueOrd)]

# define numeric vector with same length as uniqueOrd
useX <- vector("numeric", length = length(uniqueOrd))

# determine proportion of documented species with barcode data available
#   NOTE: evaluates each species per order (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueOrd)){
  
  useX[i] <- sum(gbif_ordsp[order == uniqueOrd[i] & !is.na(V1), ]$V1 %in% 
                   gene_ordsp[order == uniqueOrd[i] & !is.na(V1), ]$V1)/length(gbif_ordsp[order == uniqueOrd[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX
useX <- unlist(useX)

# define data table showing number of records associated with each orderUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total  
tempA <- barcode_data_final[ , .(numNA = sum(is.na(speciesUse)), 
                                 numTOTAL = .N), by = .(orderUse)]

# add column in tempA for the values associated with matched entries in useX
#   NOTE: represents percent of barcoded species
tempA$useX <- useX[match(tempA$orderUse, uniqueOrd)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for orderUse 
tempA <- tempA[!is.na(orderUse), ]

# create scatter plot of percent identified species per order and save as PDF format
pdf("./Figures_unedited/SpeciesID_order_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, col = "seagreen3",
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1))

dev.off()


#### 7) This section assesses percent of identified species at the family level ----

# define data table of unique species entries per family in species data
gbif_famsp <- species_data_final[ , unique(species), by = .(family)]

# define data table of unique species entries per family in barcode data
gene_famsp <- barcode_data_final[ , unique(speciesUse), by = .(familyUse)]

# relabel familyUse column in gene_famsp
names(gene_famsp)[1] <- "family"

# combine species and barcode data tables of unique species per family
#   NOTE: V1 = unique species entry
famsp_list <- rbind(gbif_famsp, gene_famsp)

# define list of unique entries for family
uniqueFam <- unique(famsp_list$family)

# remove NA entry for uniqueFam 
uniqueFam <- uniqueFam[!is.na(uniqueFam)]

# define numeric vector with same length as uniqueFam
useX <- vector("numeric", length = length(uniqueFam))

# determine proportion of documented species with barcode data available
#   NOTE: evaluates each species per family (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueFam)){
  
  useX[i] <- sum(gbif_famsp[family == uniqueFam[i] & !is.na(V1), ]$V1 %in% 
                   gene_famsp[family == uniqueFam[i] & !is.na(V1), ]$V1)/length(gbif_famsp[family == uniqueFam[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX
useX <- unlist(useX)

# define data table showing number of records associated with each familyUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total  
tempA <- barcode_data_final[ , .(numNA = sum(is.na(speciesUse)), 
                                 numTOTAL = .N), by = .(familyUse)]

# add column in tempA for the values associated with matched entries in useX
#   NOTE: represents percent of barcoded species
tempA$useX <- useX[match(tempA$familyUse, uniqueFam)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for familyUse 
tempA <- tempA[!is.na(familyUse), ]

# create scatter plot of percent identified species per family and save as PDF format
pdf("./Figures_unedited/SpeciesID_family_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, col = "lightgoldenrod3",
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1))

dev.off()


########## end of script
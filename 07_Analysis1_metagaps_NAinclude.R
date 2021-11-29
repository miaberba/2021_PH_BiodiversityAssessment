# NOTE:
#     This script summarizes the barcode and species occurrence data to be 
#     analyzed - overall and in terms of gene marker of interest and taxonomic 
#     classification. Additionally, it also performs first set of analyses for 
#     the systematic review of the Philippines' (PH) biodiversity. The objective 
#     is to examine for metadata gaps in terms of the completeness of records 
#     and publishing, geolocation, and taxonomic information. The process 
#     involves calculating the proportion of missing information (NA) for 
#     each metadata category. For the taxonomic metadata, in particular, much 
#     focus has been given at rate of species identification compared to 
#     the proportion of species represented in both species occurrence and 
#     barcode data. Here, the barcode data analyzed INCLUDES records that are 
#     NA for the country sampled, despite searches being filtered geographically. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table R package
library("data.table")

# define genCol function for assigning colors derived from RGB
genCol <- function(x, trans){
  
  return(rgb(red = col2rgb(x, alpha = FALSE)[1],
             green = col2rgb(x, alpha = FALSE)[2],
             blue = col2rgb(x, alpha = FALSE)[3],
             max = 255, alpha = trans, names = NULL))
  
}

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
marker_summary

# create bar graph of gene marker summary and save as PDF format
pdf("./Figures_unedited/GeneMarker_summary_NAinclude.pdf", paper = "a4r")

barplot(marker_summary$N,
        names.arg = marker_summary$gene_marker,
        col = "ivory4", xlab = "Gene marker",
        ylab = "Number of genetic records",
        border = NA,
        ylim = c(0, 16000))

dev.off()

# define data table showing number of barcode records per taxonomic kingdom
geneKing_summary <- barcode_data_final[ , .N, by = .(kingdomUse)]
geneKing_summary

# define data table showing number of species records per taxonomic kingdom
speciesKing_summary <- species_data_final[ , .N, by = .(kingdom)]
speciesKing_summary

# check number of unique taxa at each taxonomic rank in barcode_data_final
#   NOTE: separate count for animal and plant taxa then taxonomic ranks to 
#   consider are phylum, class, order, and family
length(unique(barcode_data_final[kingdomUse == "Animalia"]$phylumUse))
length(unique(barcode_data_final[kingdomUse == "Animalia"]$classUse))
length(unique(barcode_data_final[kingdomUse == "Animalia"]$orderUse))
length(unique(barcode_data_final[kingdomUse == "Animalia"]$familyUse))
length(unique(barcode_data_final[kingdomUse == "Plantae"]$phylumUse))
length(unique(barcode_data_final[kingdomUse == "Plantae"]$classUse))
length(unique(barcode_data_final[kingdomUse == "Plantae"]$orderUse))
length(unique(barcode_data_final[kingdomUse == "Plantae"]$familyUse))

# check number of unique taxa at each taxonomic rank in species_data_final
#   NOTE: separate count for animal and plant taxa then taxonomic ranks to 
#   consider are phylum, class, order, and family
length(unique(species_data_final[kingdom == "Animalia"]$phylum))
length(unique(species_data_final[kingdom == "Animalia"]$class))
length(unique(species_data_final[kingdom == "Animalia"]$order))
length(unique(species_data_final[kingdom == "Animalia"]$family))
length(unique(species_data_final[kingdom == "Plantae"]$phylum))
length(unique(species_data_final[kingdom == "Plantae"]$class))
length(unique(species_data_final[kingdom == "Plantae"]$order))
length(unique(species_data_final[kingdom == "Plantae"]$family))

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

# define data table of unique animal (A) entries per phylum in species data
gbif_physp_A <- species_data_final[kingdom == "Animalia", 
                                   unique(species), 
                                   by = .(phylum)]

# define data table of unique animal (A) species entries per phylum in barcode data
gene_physp_A <- barcode_data_final[kingdomUse == "Animalia", 
                                   unique(speciesUse), 
                                   by = .(phylumUse)]

# relabel phylumUse column in gene_physp_A
names(gene_physp_A)[1] <- "phylum"

# combine animal species and barcode data tables of unique species per phylum
#   NOTE: V1 = unique species entry
physp_list_A <- rbind(gbif_physp_A, gene_physp_A)

# define list of unique animal entries for phylum
uniquePhyl_A <- unique(physp_list_A$phylum)

# remove NA entry for uniquePhyl_A 
uniquePhyl_A <- uniquePhyl_A[!is.na(uniquePhyl_A)]

# define numeric vector with same length as uniquePhyl_A
useX_A <- vector("numeric", length = length(uniquePhyl_A))

# determine proportion of documented animal species with barcode data available
#   NOTE: evaluates each species per phylum (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniquePhyl_A)){
  
  useX_A[i] <- sum(gbif_physp_A[phylum == uniquePhyl_A[i] & !is.na(V1), ]$V1 %in% 
                     gene_physp_A[phylum == uniquePhyl_A[i] & !is.na(V1), ]$V1)/length(gbif_physp_A[phylum == uniquePhyl_A[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_A
useX_A <- unlist(useX_A)

# define data table showing number of animal records associated with each phylumUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[kingdomUse == "Animalia", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(phylumUse)]

# add column in tempA for the values associated with matched entries in useX_A
#   NOTE: represents percent of barcoded species
tempA$useX <- useX_A[match(tempA$phylumUse, uniquePhyl_A)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for phylumUse 
tempA <- tempA[!is.na(phylumUse), ]

# define data table of unique plant (P) entries per phylum in species data
gbif_physp_P <- species_data_final[kingdom == "Plantae", 
                                   unique(species), 
                                   by = .(phylum)]

# define data table of unique plant (P) species entries per phylum in barcode data
gene_physp_P <- barcode_data_final[kingdomUse == "Plantae", 
                                   unique(speciesUse), 
                                   by = .(phylumUse)]

# relabel phylumUse column in gene_physp_P
names(gene_physp_P)[1] <- "phylum"

# combine plant species and barcode data tables of unique species per phylum
#   NOTE: V1 = unique species entry
physp_list_P <- rbind(gbif_physp_P, gene_physp_P)

# define list of unique plant entries for phylum
uniquePhyl_P <- unique(physp_list_P$phylum)

# remove NA entry for uniquePhyl_P 
uniquePhyl_P <- uniquePhyl_P[!is.na(uniquePhyl_P)]

# define numeric vector with same length as uniquePhyl_P
useX_P <- vector("numeric", length = length(uniquePhyl_P))

# determine proportion of documented plant species with barcode data available
#   NOTE: evaluates each species per phylum (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniquePhyl_P)){
  
  useX_P[i] <- sum(gbif_physp_P[phylum == uniquePhyl_P[i] & !is.na(V1), ]$V1 %in% 
                     gene_physp_P[phylum == uniquePhyl_P[i] & !is.na(V1), ]$V1)/length(gbif_physp_P[phylum == uniquePhyl_P[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_P
useX_P <- unlist(useX_P)

# define data table showing number of plant records associated with each phylumUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempP <- barcode_data_final[kingdomUse == "Plantae", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(phylumUse)]

# add column in tempP for the values associated with matched entries in useX_P
#   NOTE: represents percent of barcoded species
tempP$useX <- useX_P[match(tempP$phylumUse, uniquePhyl_P)]

# add column in tempP for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempP$useY <- tempP[ , (numNA)/numTOTAL]

# remove records that are NA for phylumUse 
tempP <- tempP[!is.na(phylumUse), ]

# create scatter plot of percent identified species per phylum and save as PDF format
pdf("./Figures_unedited/SpeciesID_phylum_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1),
     main = "Phylum/Division")

points(x = tempP$useX[],
       y = 1 - tempP$useY[],
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

dev.off()


#### 5) This section assesses percent of identified species at the class level ----

# define data table of unique animal (A) entries per class in species data
gbif_clsp_A <- species_data_final[kingdom == "Animalia", 
                                  unique(species), 
                                  by = .(class)]

# define data table of unique animal (A) species entries per class in barcode data
gene_clsp_A <- barcode_data_final[kingdomUse == "Animalia", 
                                  unique(speciesUse), 
                                  by = .(classUse)]

# relabel classUse column in gene_clsp_A
names(gene_clsp_A)[1] <- "class"

# combine animal species and barcode data tables of unique species per class
#   NOTE: V1 = unique species entry
clsp_list_A <- rbind(gbif_clsp_A, gene_clsp_A)

# define list of unique animal entries for class
uniqueClas_A <- unique(clsp_list_A$class)

# remove NA entry for uniqueClas_A 
uniqueClas_A <- uniqueClas_A[!is.na(uniqueClas_A)]

# define numeric vector with same length as uniqueClas_A
useX_A <- vector("numeric", length = length(uniqueClas_A))

# determine proportion of documented animal species with barcode data available
#   NOTE: evaluates each species per class (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueClas_A)){
  
  useX_A[i] <- sum(gbif_clsp_A[class == uniqueClas_A[i] & !is.na(V1), ]$V1 %in% 
                     gene_clsp_A[class == uniqueClas_A[i] & !is.na(V1), ]$V1)/length(gbif_clsp_A[class == uniqueClas_A[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_A
useX_A <- unlist(useX_A)

# define data table showing number of animal records associated with each classUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[kingdomUse == "Animalia", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(classUse)]

# add column in tempA for the values associated with matched entries in useX_A
#   NOTE: represents percent of barcoded species
tempA$useX <- useX_A[match(tempA$classUse, uniqueClas_A)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for classUse 
tempA <- tempA[!is.na(classUse), ]

# define data table of unique plant (P) entries per class in species data
gbif_clsp_P <- species_data_final[kingdom == "Plantae", 
                                  unique(species), 
                                  by = .(class)]

# define data table of unique plant (P) species entries per class in barcode data
gene_clsp_P <- barcode_data_final[kingdomUse == "Plantae", 
                                  unique(speciesUse), 
                                  by = .(classUse)]

# relabel classUse column in gene_clsp_P
names(gene_clsp_P)[1] <- "class"

# combine plant species and barcode data tables of unique species per class
#   NOTE: V1 = unique species entry
clsp_list_P <- rbind(gbif_clsp_P, gene_clsp_P)

# define list of unique plant entries for class
uniqueClas_P <- unique(clsp_list_P$class)

# remove NA entry for uniqueClas_P 
uniqueClas_P <- uniqueClas_P[!is.na(uniqueClas_P)]

# define numeric vector with same length as uniqueClas_P
useX_P <- vector("numeric", length = length(uniqueClas_P))

# determine proportion of documented plant species with barcode data available
#   NOTE: evaluates each species per class (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueClas_P)){
  
  useX_P[i] <- sum(gbif_clsp_P[class == uniqueClas_P[i] & !is.na(V1), ]$V1 %in% 
                     gene_clsp_P[class == uniqueClas_P[i] & !is.na(V1), ]$V1)/length(gbif_clsp_P[class == uniqueClas_P[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_P
useX_P <- unlist(useX_P)

# define data table showing number of plant records associated with each classUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempP <- barcode_data_final[kingdomUse == "Plantae", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(classUse)]

# add column in tempP for the values associated with matched entries in useX_P
#   NOTE: represents percent of barcoded species
tempP$useX <- useX_P[match(tempP$classUse, uniqueClas_P)]

# add column in tempP for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempP$useY <- tempP[ , (numNA)/numTOTAL]

# remove records that are NA for classUse 
tempP <- tempP[!is.na(classUse), ]

# create scatter plot of percent identified species per class and save as PDF format
pdf("./Figures_unedited/SpeciesID_class_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1),
     main = "Class")

points(x = tempP$useX[],
       y = 1 - tempP$useY[],
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

dev.off()


#### 6) This section assesses percent of identified species at the order level ----

# define data table of unique animal (A) entries per order in species data
gbif_ordsp_A <- species_data_final[kingdom == "Animalia", 
                                   unique(species), 
                                   by = .(order)]

# define data table of unique animal (A) species entries per order in barcode data
gene_ordsp_A <- barcode_data_final[kingdomUse == "Animalia", 
                                   unique(speciesUse), 
                                   by = .(orderUse)]

# relabel orderUse column in gene_ordsp_A
names(gene_ordsp_A)[1] <- "order"

# combine animal species and barcode data tables of unique species per order
#   NOTE: V1 = unique species entry
ordsp_list_A <- rbind(gbif_ordsp_A, gene_ordsp_A)

# define list of unique animal entries for order
uniqueOrd_A <- unique(ordsp_list_A$order)

# remove NA entry for uniqueOrd_A 
uniqueOrd_A <- uniqueOrd_A[!is.na(uniqueOrd_A)]

# define numeric vector with same length as uniqueOrd_A
useX_A <- vector("numeric", length = length(uniqueOrd_A))

# determine proportion of documented animal species with barcode data available
#   NOTE: evaluates each species per order (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueOrd_A)){
  
  useX_A[i] <- sum(gbif_ordsp_A[order == uniqueOrd_A[i] & !is.na(V1), ]$V1 %in% 
                     gene_ordsp_A[order == uniqueOrd_A[i] & !is.na(V1), ]$V1)/length(gbif_ordsp_A[order == uniqueOrd_A[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_A
useX_A <- unlist(useX_A)

# define data table showing number of animal records associated with each orderUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[kingdomUse == "Animalia", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(orderUse)]

# add column in tempA for the values associated with matched entries in useX_A
#   NOTE: represents percent of barcoded species
tempA$useX <- useX_A[match(tempA$orderUse, uniqueOrd_A)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for orderUse 
tempA <- tempA[!is.na(orderUse), ]

# define data table of unique plant (P) entries per order in species data
gbif_ordsp_P <- species_data_final[kingdom == "Plantae", 
                                   unique(species), 
                                   by = .(order)]

# define data table of unique plant (P) species entries per order in barcode data
gene_ordsp_P <- barcode_data_final[kingdomUse == "Plantae", 
                                   unique(speciesUse), 
                                   by = .(orderUse)]

# relabel orderUse column in gene_ordsp_P
names(gene_ordsp_P)[1] <- "order"

# combine plant species and barcode data tables of unique species per order
#   NOTE: V1 = unique species entry
ordsp_list_P <- rbind(gbif_ordsp_P, gene_ordsp_P)

# define list of unique plant entries for order
uniqueOrd_P <- unique(ordsp_list_P$order)

# remove NA entry for uniqueOrd_P 
uniqueOrd_P <- uniqueOrd_P[!is.na(uniqueOrd_P)]

# define numeric vector with same length as uniqueOrd_P
useX_P <- vector("numeric", length = length(uniqueOrd_P))

# determine proportion of documented plant species with barcode data available
#   NOTE: evaluates each species per order (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueOrd_P)){
  
  useX_P[i] <- sum(gbif_ordsp_P[order == uniqueOrd_P[i] & !is.na(V1), ]$V1 %in% 
                     gene_ordsp_P[order == uniqueOrd_P[i] & !is.na(V1), ]$V1)/length(gbif_ordsp_P[order == uniqueOrd_P[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_P
useX_P <- unlist(useX_P)

# define data table showing number of plant records associated with each orderUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempP <- barcode_data_final[kingdomUse == "Plantae", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(orderUse)]

# add column in tempP for the values associated with matched entries in useX_P
#   NOTE: represents percent of barcoded species
tempP$useX <- useX_P[match(tempP$orderUse, uniqueOrd_P)]

# add column in tempP for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempP$useY <- tempP[ , (numNA)/numTOTAL]

# remove records that are NA for orderUse 
tempP <- tempP[!is.na(orderUse), ]

# create scatter plot of percent identified species per order and save as PDF format
pdf("./Figures_unedited/SpeciesID_order_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1),
     main = "Order")

points(x = tempP$useX[],
       y = 1 - tempP$useY[],
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

dev.off()


#### 7) This section assesses percent of identified species at the family level ----

# define data table of unique animal (A) entries per family in species data
gbif_famsp_A <- species_data_final[kingdom == "Animalia", 
                                   unique(species), 
                                   by = .(family)]

# define data table of unique animal (A) species entries per family in barcode data
gene_famsp_A <- barcode_data_final[kingdomUse == "Animalia", 
                                   unique(species), 
                                   by = .(familyUse)]

# relabel familyUse column in gene_famsp_A
names(gene_famsp_A)[1] <- "family"

# combine animal species and barcode data tables of unique species per family
#   NOTE: V1 = unique species entry
famsp_list_A <- rbind(gbif_famsp_A, gene_famsp_A)

# define list of unique animal entries for family
uniqueFam_A <- unique(famsp_list_A$family)

# remove NA entry for uniqueFam_A 
uniqueFam_A <- uniqueFam_A[!is.na(uniqueFam_A)]

# define numeric vector with same length as uniqueFam_A
useX_A <- vector("numeric", length = length(uniqueFam_A))

# determine proportion of documented animal species with barcode data available
#   NOTE: evaluates each species per family (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueFam_A)){
  
  useX_A[i] <- sum(gbif_famsp_A[family == uniqueFam_A[i] & !is.na(V1), ]$V1 %in% 
                     gene_famsp_A[family == uniqueFam_A[i] & !is.na(V1), ]$V1)/length(gbif_famsp_A[family == uniqueFam_A[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_A
useX_A <- unlist(useX_A)

# define data table showing number of animal records associated with each familyUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempA <- barcode_data_final[kingdomUse == "Animalia", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(familyUse)]

# add column in tempA for the values associated with matched entries in useX_A
#   NOTE: represents percent of barcoded species
tempA$useX <- useX_A[match(tempA$familyUse, uniqueFam_A)]

# add column in tempA for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempA$useY <- tempA[ , (numNA)/numTOTAL]

# remove records that are NA for familyUse 
tempA <- tempA[!is.na(familyUse), ]

# define data table of unique plant (P) entries per family in species data
gbif_famsp_P <- species_data_final[kingdom == "Plantae", 
                                   unique(species), 
                                   by = .(family)]

# define data table of unique plant (P) species entries per family in barcode data
gene_famsp_P <- barcode_data_final[kingdomUse == "Plantae", 
                                   unique(speciesUse), 
                                   by = .(familyUse)]

# relabel familyUse column in gene_famsp_P
names(gene_famsp_P)[1] <- "family"

# combine plant species and barcode data tables of unique species per family
#   NOTE: V1 = unique species entry
famsp_list_P <- rbind(gbif_famsp_P, gene_famsp_P)

# define list of unique plant entries for family
uniqueFam_P <- unique(famsp_list_P$family)

# remove NA entry for uniqueFam_P 
uniqueFam_P <- uniqueFam_P[!is.na(uniqueFam_P)]

# define numeric vector with same length as uniqueFam_P
useX_P <- vector("numeric", length = length(uniqueFam_P))

# determine proportion of documented plant species with barcode data available
#   NOTE: evaluates each species per family (except NA entries for V1) 
#   represented in either or both species and genetic data
for(i in 1:length(uniqueFam_P)){
  
  useX_P[i] <- sum(gbif_famsp_P[family == uniqueFam_P[i] & !is.na(V1), ]$V1 %in% 
                     gene_famsp_P[family == uniqueFam_P[i] & !is.na(V1), ]$V1)/length(gbif_famsp_P[family == uniqueFam_P[i] & !is.na(V1), ]$V1)
  
}

# unlist values obtained in useX_P
useX_P <- unlist(useX_P)

# define data table showing number of plant records associated with each familyUse entry
#   NOTE: in terms of two criteria - records with missing info (NA) for 
#   speciesUse and records in total 
tempP <- barcode_data_final[kingdomUse == "Plantae", 
                            .(numNA = sum(is.na(speciesUse)),
                              numTOTAL = .N), by = .(familyUse)]

# add column in tempP for the values associated with matched entries in useX_P
#   NOTE: represents percent of barcoded species
tempP$useX <- useX_P[match(tempP$familyUse, uniqueFam_P)]

# add column in tempP for the proportion of records with missing species ID
#   NOTE: represents percent of unidentified species
tempP$useY <- tempP[ , (numNA)/numTOTAL]

# remove records that are NA for familyUse 
tempP <- tempP[!is.na(familyUse), ]

# create scatter plot of percent identified species per family and save as PDF format
pdf("./Figures_unedited/SpeciesID_family_NAinclude.pdf", paper = "a4r")

plot(x = tempA$useX[],
     y = 1 - tempA$useY[], 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Percent of barcoded species",
     ylab = "Percent of identified species",
     xlim = c(0,1),
     ylim = c(0,1),
     main = "Family")

points(x = tempP$useX[],
       y = 1 - tempP$useY[],
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

dev.off()


########## end of script
# NOTE:
#     This script performs second set of analyses for the systematic review
#     of the Philippines' (PH) biodiversity. The objective is to examine for 
#     taxonomic biases. The process is exactly the same as the previous script 
#     (07) designated to comparing the state of species and genetic documentation
#     at different taxonomic levels. However, here, the barcode data analyzed 
#     EXCLUDES records that are NA for the country sampled, despite searches 
#     being filtered geographically. 


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

# load updated working barcode data table 
#   NOTE: resulting data table after completing NAinclude analysis
barcode_data_final <- fread("UPDATEworking_barcode_data_NAinclude.table", 
                            na.strings = c("", NA))

# subset barcode_data_final to contain only explicitly Philippine data
#   NOTE: country_location refers to country sampled and records that are 
#   NA for this are excluded
barcode_data_final <- barcode_data_final[country_location == "Philippines"]

# update assignment of row numbers to barcode datasets
#   NOTE: given that records that are NA for country_location have been excluded
barcode_data_final$rowID <- 1:nrow(barcode_data_final)


#### 1) This section compares genetic and species data at the phylum level ----

# define data table showing number of barcode records per phylumUse
genetic_phylum <- barcode_data_final[ , .N, by = .(phylumUse)]

# define data table showing number of species records per phylum
species_phylum <- species_data_final[ , .N, by = .(phylum)]

# merge genetic_phylum and species_phylum based on phylum
genXsp_phylum <- merge(genetic_phylum, 
                       species_phylum, 
                       by.x = 'phylumUse', 
                       by.y = 'phylum',
                       all = TRUE)

# relabel N.x and N.y columns in genXsp_phylum
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_phylum)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_phylum for logarithm of barcode data count
genXsp_phylum$log_genetic <- log(genXsp_phylum$genetic_data)

# assign -1 value to NA entries for log_genetic
#   NOTE: represent taxa with no barcode records 
genXsp_phylum[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_phylum for logarithm of species data count
genXsp_phylum$log_species <- log(genXsp_phylum$species_data)

# assign -1 value to NA entries for log_species
#   NOTE: represent taxa with no species records
genXsp_phylum[is.na(log_species)]$log_species <- -1

# remove records that are NA for phylumUse 
genXsp_phylum <- genXsp_phylum[!is.na(phylumUse), ]

# create scatter plot of barcode against species records per phylum and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_phylum_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_phylum$log_species, 
     y = genXsp_phylum$log_genetic, 
     pch = 19, col = "indianred3",
     xlab = "Log number of species records",
     ylab = "Log number of genetic records")

speciesPer_phy <- quantile(genXsp_phylum$log_species, seq(0, 1, 0.01))

speciesPer_phy[c(6, 96)]

abline(v = speciesPer_phy[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_phy <- quantile(genXsp_phylum$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_phy[c(6, 96)]

abline(h = genePer_phy[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


#### 2) This section compares genetic and species data at the class level ----

# define data table showing number of barcode records per classUse
genetic_class <- barcode_data_final[ , .N, by = .(classUse)]

# define data table showing number of species records per class
species_class <- species_data_final[ , .N, by = .(class)]

# merge genetic_class and species_class based on class
genXsp_class <- merge(genetic_class, 
                      species_class, 
                      by.x = 'classUse', 
                      by.y = 'class', 
                      all = TRUE)

# relabel N.x and N.y columns in genXsp_class
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_class)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_class for logarithm of barcode data count
genXsp_class$log_genetic <- log(genXsp_class$genetic_data)

# assign -1 value to NA entries for log_genetic
#   NOTE: represent taxa with no barcode records 
genXsp_class[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_class for logarithm of species data count
genXsp_class$log_species <- log(genXsp_class$species_data)

# assign -1 value to NA entries for log_species
#   NOTE: represent taxa with no species records
genXsp_class[is.na(log_species)]$log_species <- -1

# remove records that are NA for classUse 
genXsp_class <- genXsp_class[!is.na(classUse), ]

# create scatter plot of barcode against species records per class and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_class_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_class$log_species, 
     y = genXsp_class$log_genetic, 
     pch = 19, col = "royalblue3",
     xlab = "Log number of species records",
     ylab = "Log number of genetic records")

speciesPer_cl <- quantile(genXsp_class$log_species, seq(0, 1, 0.01))

speciesPer_cl[c(6, 96)]

abline(v = speciesPer_cl[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_cl <- quantile(genXsp_class$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_cl[c(6, 96)]

abline(h = genePer_cl[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


#### 3) This section compares genetic and species data at the order level ----

# define data table showing number of barcode records per orderUse
genetic_order <- barcode_data_final[ , .N, by = .(orderUse)]

# define data table showing number of species records per order
species_order <- species_data_final[ , .N, by = .(order)]

# merge genetic_order and species_order based on order
genXsp_order <- merge(genetic_order, 
                      species_order, 
                      by.x = 'orderUse', 
                      by.y = 'order', 
                      all = TRUE)

# relabel N.x and N.y columns in genXsp_order
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_order)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_order for logarithm of barcode data count
genXsp_order$log_genetic <- log(genXsp_order$genetic_data)

# assign -1 value to NA entries for log_genetic
#   NOTE: represent taxa with no barcode records 
genXsp_order[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_class for logarithm of species data count
genXsp_order$log_species <- log(genXsp_order$species_data)

# assign -1 value to NA entries for log_species
#   NOTE: represent taxa with no species records
genXsp_order[is.na(log_species)]$log_species <- -1

# remove records that are NA for orderUse 
genXsp_order <- genXsp_order[!is.na(orderUse), ]

# create scatter plot of barcode against species records per order and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_order_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_order$log_species,
     y = genXsp_order$log_genetic,
     pch = 19, col = "seagreen3",
     xlab = "Log number of species records",
     ylab = "Log number of genetic records")

speciesPer_ord <- quantile(genXsp_order$log_species , seq(0, 1, 0.01), na.rm = TRUE)

speciesPer_ord[c(6, 96)]

abline(v = speciesPer_ord[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_ord <- quantile(genXsp_order$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_ord[c(6, 96)]

abline(h = genePer_ord[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


#### 4) This section compares genetic and species data at the family level ----

# define data table showing number of barcode records per familyUse
genetic_family <- barcode_data_final[ , .N, by = .(familyUse)]

# define data table showing number of species records per family
species_family <- species_data_final[ , .N, by = .(family)]

# merge genetic_family and species_family based on family
genXsp_family <- merge(genetic_family, 
                       species_family, 
                       by.x = 'familyUse', 
                       by.y = 'family',
                       all = TRUE)

# relabel N.x and N.y columns in genXsp_order
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_family)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_family for logarithm of barcode data count
genXsp_family$log_genetic <- log(genXsp_family$genetic_data)

# assign -1 value to NA entries for log_genetic
#   NOTE: represent taxa with no barcode records 
genXsp_family[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_family for logarithm of species data count
genXsp_family$log_species <- log(genXsp_family$species_data)

# assign -1 value to NA entries for log_species
#   NOTE: represent taxa with no species records
genXsp_family[is.na(log_species)]$log_species <- -1

# remove records that are NA for familyUse
genXsp_family <- genXsp_family[!is.na(familyUse), ]

# create scatter plot of barcode against species records per family and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_family_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_family$log_species, 
     y = genXsp_family$log_genetic, 
     pch = 19, col = "lightgoldenrod3",
     xlab = "Log number of species records",
     ylab = "Log number of genetic records")

speciesPer_fam <- quantile(genXsp_family$log_species, seq(0, 1, 0.01), na.rm = TRUE)

speciesPer_fam[c(6, 96)]

abline(v = speciesPer_fam[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_fam <- quantile(genXsp_family$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_fam[c(6, 96)]

abline(h = genePer_fam[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


########## end of script
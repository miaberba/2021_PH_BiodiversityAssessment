# NOTE:
#     This script performs second set of analyses for the systematic assessment 
#     of PH biodiversity data. The objective is to examine for taxonomic biases. 
#     The process is exactly the same as the previous script (07) designated to 
#     comparing the state of species and genetic documentation at different taxonomic
#     levels. However, here, the barcode data analyzed EXCLUDES records that are NA
#     for the country sampled, despite searches being filtered geographically. 


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

# define data table showing number of animal (A) barcode records per phylumUse
genetic_phylum_A <- barcode_data_final[kingdomUse == "Animalia", 
                                       .N, by = .(phylumUse)]

# define data table showing number of plant (P) barcode records per phylumUse
genetic_phylum_P <- barcode_data_final[kingdomUse == "Plantae", 
                                       .N, by = .(phylumUse)]


# define data table showing number of species records per phylum   
#   NOTE: for both animal and plant datasets
species_phylum_A <- species_data_final[kingdom == "Animalia", 
                                       .N, by = .(phylum)]
species_phylum_P <- species_data_final[kingdom == "Plantae", 
                                       .N, by = .(phylum)]

# merge genetic_phylum and species_phylum based on phylum
#   NOTE: for both animal and plant datasets
genXsp_phylum_A <- merge(genetic_phylum_A, 
                         species_phylum_A, 
                         by.x = 'phylumUse', 
                         by.y = 'phylum',
                         all = TRUE)
genXsp_phylum_P <- merge(genetic_phylum_P, 
                         species_phylum_P, 
                         by.x = 'phylumUse', 
                         by.y = 'phylum',
                         all = TRUE)

# relabel N.x and N.y columns in genXsp_phylum (A & P datasets)
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_phylum_A)[2:3] <- c("genetic_data", "species_data")
names(genXsp_phylum_P)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_phylum (A & P datasets) for logarithm of barcode data count
genXsp_phylum_A$log_genetic <- log(genXsp_phylum_A$genetic_data)
genXsp_phylum_P$log_genetic <- log(genXsp_phylum_P$genetic_data)

# assign -1 value to NA entries for log_genetic in A & P datasets
#   NOTE: represent taxa with no barcode records 
genXsp_phylum_A[is.na(log_genetic)]$log_genetic <- -1
genXsp_phylum_P[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_phylum  (A & P datasets) for logarithm of species data count
genXsp_phylum_A$log_species <- log(genXsp_phylum_A$species_data)
genXsp_phylum_P$log_species <- log(genXsp_phylum_P$species_data)

# assign -1 value to NA entries for log_species  in A & P datasets
#   NOTE: represent taxa with no species records
genXsp_phylum_A[is.na(log_species)]$log_species <- -1
genXsp_phylum_P[is.na(log_species)]$log_species <- -1

# remove records that are NA for phylumUse in A & P datasets
genXsp_phylum_A <- genXsp_phylum_A[!is.na(phylumUse), ]
genXsp_phylum_P <- genXsp_phylum_P[!is.na(phylumUse), ]

# combine animal and plant datasets 
#   NOTE: for calculating percentiles of overall barcode and species datasets
genXsp_phylum <- rbind(genXsp_phylum_A, genXsp_phylum_P)

# create scatter plot of barcode against species records per phylum and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_phylum_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_phylum_A$log_species, 
     y = genXsp_phylum_A$log_genetic, 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Log number of species records",
     ylab = "Log number of genetic records",
     main = "Phylum/Division")

points(x = genXsp_phylum_P$log_species, 
       y = genXsp_phylum_P$log_genetic,
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

speciesPer_phy <- quantile(genXsp_phylum$log_species, seq(0, 1, 0.01))

speciesPer_phy[c(6, 96)]

abline(v = speciesPer_phy[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_phy <- quantile(genXsp_phylum$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_phy[c(6, 96)]

abline(h = genePer_phy[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)


dev.off()


#### 2) This section compares genetic and species data at the class level ----

# define data table showing number of animal (A) barcode records per classUse
genetic_class_A <- barcode_data_final[kingdomUse == "Animalia", 
                                      .N, by = .(classUse)]

# define data table showing number of plant (P) barcode records per classUse
genetic_class_P <- barcode_data_final[kingdomUse == "Plantae", 
                                      .N, by = .(classUse)]


# define data table showing number of species records per class   
#   NOTE: for both animal and plant datasets
species_class_A <- species_data_final[kingdom == "Animalia", 
                                      .N, by = .(class)]
species_class_P <- species_data_final[kingdom == "Plantae", 
                                      .N, by = .(class)]

# merge genetic_class and species_class based on class
#   NOTE: for both animal and plant datasets
genXsp_class_A <- merge(genetic_class_A, 
                        species_class_A, 
                        by.x = 'classUse', 
                        by.y = 'class',
                        all = TRUE)
genXsp_class_P <- merge(genetic_class_P, 
                        species_class_P, 
                        by.x = 'classUse', 
                        by.y = 'class',
                        all = TRUE)

# relabel N.x and N.y columns in genXsp_class (A & P datasets)
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_class_A)[2:3] <- c("genetic_data", "species_data")
names(genXsp_class_P)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_class (A & P datasets) for logarithm of barcode data count
genXsp_class_A$log_genetic <- log(genXsp_class_A$genetic_data)
genXsp_class_P$log_genetic <- log(genXsp_class_P$genetic_data)

# assign -1 value to NA entries for log_genetic in A & P datasets
#   NOTE: represent taxa with no barcode records 
genXsp_class_A[is.na(log_genetic)]$log_genetic <- -1
genXsp_class_P[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_class  (A & P datasets) for logarithm of species data count
genXsp_class_A$log_species <- log(genXsp_class_A$species_data)
genXsp_class_P$log_species <- log(genXsp_class_P$species_data)

# assign -1 value to NA entries for log_species  in A & P datasets
#   NOTE: represent taxa with no species records
genXsp_class_A[is.na(log_species)]$log_species <- -1
genXsp_class_P[is.na(log_species)]$log_species <- -1

# remove records that are NA for classUse in A & P datasets
genXsp_class_A <- genXsp_class_A[!is.na(classUse), ]
genXsp_class_P <- genXsp_class_P[!is.na(classUse), ]

# combine animal and plant datasets 
#   NOTE: for calculating percentiles of overall barcode and species datasets
genXsp_class <- rbind(genXsp_class_A, genXsp_class_P)

# create scatter plot of barcode against species records per class and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_class_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_class_A$log_species, 
     y = genXsp_class_A$log_genetic, 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Log number of species records",
     ylab = "Log number of genetic records",
     main = "Class")

points(x = genXsp_class_P$log_species, 
       y = genXsp_class_P$log_genetic,
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

speciesPer_cl <- quantile(genXsp_class$log_species, seq(0, 1, 0.01))

speciesPer_cl[c(6, 96)]

abline(v = speciesPer_cl[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_cl <- quantile(genXsp_class$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_cl[c(6, 96)]

abline(h = genePer_cl[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


#### 3) This section compares genetic and species data at the order level ----

# define data table showing number of animal (A) barcode records per orderUse
genetic_order_A <- barcode_data_final[kingdomUse == "Animalia", 
                                      .N, by = .(orderUse)]

# define data table showing number of plant (P) barcode records per orderUse
genetic_order_P <- barcode_data_final[kingdomUse == "Plantae", 
                                      .N, by = .(orderUse)]


# define data table showing number of species records per order   
#   NOTE: for both animal and plant datasets
species_order_A <- species_data_final[kingdom == "Animalia", 
                                      .N, by = .(order)]
species_order_P <- species_data_final[kingdom == "Plantae", 
                                      .N, by = .(order)]

# merge genetic_order and species_order based on order
#   NOTE: for both animal and plant datasets
genXsp_order_A <- merge(genetic_order_A, 
                        species_order_A, 
                        by.x = 'orderUse', 
                        by.y = 'order',
                        all = TRUE)
genXsp_order_P <- merge(genetic_order_P, 
                        species_order_P, 
                        by.x = 'orderUse', 
                        by.y = 'order',
                        all = TRUE)

# relabel N.x and N.y columns in genXsp_order (A & P datasets)
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_order_A)[2:3] <- c("genetic_data", "species_data")
names(genXsp_order_P)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_order (A & P datasets) for logarithm of barcode data count
genXsp_order_A$log_genetic <- log(genXsp_order_A$genetic_data)
genXsp_order_P$log_genetic <- log(genXsp_order_P$genetic_data)

# assign -1 value to NA entries for log_genetic in A & P datasets
#   NOTE: represent taxa with no barcode records 
genXsp_order_A[is.na(log_genetic)]$log_genetic <- -1
genXsp_order_P[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_order  (A & P datasets) for logarithm of species data count
genXsp_order_A$log_species <- log(genXsp_order_A$species_data)
genXsp_order_P$log_species <- log(genXsp_order_P$species_data)

# assign -1 value to NA entries for log_species  in A & P datasets
#   NOTE: represent taxa with no species records
genXsp_order_A[is.na(log_species)]$log_species <- -1
genXsp_order_P[is.na(log_species)]$log_species <- -1

# remove records that are NA for orderUse in A & P datasets
genXsp_order_A <- genXsp_order_A[!is.na(orderUse), ]
genXsp_order_P <- genXsp_order_P[!is.na(orderUse), ]

# combine animal and plant datasets 
#   NOTE: for calculating percentiles of overall barcode and species datasets
genXsp_order <- rbind(genXsp_order_A, genXsp_order_P)

# create scatter plot of barcode against species records per order and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_order_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_order_A$log_species, 
     y = genXsp_order_A$log_genetic, 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Log number of species records",
     ylab = "Log number of genetic records",
     main = "Order")

points(x = genXsp_order_P$log_species, 
       y = genXsp_order_P$log_genetic,
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

speciesPer_ord <- quantile(genXsp_order$log_species , seq(0, 1, 0.01), na.rm = TRUE)

speciesPer_ord[c(6, 96)]

abline(v = speciesPer_ord[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_ord <- quantile(genXsp_order$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_ord[c(6, 96)]

abline(h = genePer_ord[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


#### 4) This section compares genetic and species data at the family level ----

# define data table showing number of animal (A) barcode records per familyUse
genetic_family_A <- barcode_data_final[kingdomUse == "Animalia", 
                                       .N, by = .(familyUse)]

# define data table showing number of plant (P) barcode records per familyUse
genetic_family_P <- barcode_data_final[kingdomUse == "Plantae", 
                                       .N, by = .(familyUse)]


# define data table showing number of species records per family   
#   NOTE: for both animal and plant datasets
species_family_A <- species_data_final[kingdom == "Animalia", 
                                       .N, by = .(family)]
species_family_P <- species_data_final[kingdom == "Plantae", 
                                       .N, by = .(family)]

# merge genetic_family and species_family based on family
#   NOTE: for both animal and plant datasets
genXsp_family_A <- merge(genetic_family_A, 
                         species_family_A, 
                         by.x = 'familyUse', 
                         by.y = 'family',
                         all = TRUE)
genXsp_family_P <- merge(genetic_family_P, 
                         species_family_P, 
                         by.x = 'familyUse', 
                         by.y = 'family',
                         all = TRUE)

# relabel N.x and N.y columns in genXsp_family (A & P datasets)
#   NOTE: x = count from barcode data, y = count from species data
names(genXsp_family_A)[2:3] <- c("genetic_data", "species_data")
names(genXsp_family_P)[2:3] <- c("genetic_data", "species_data")

# add column in genXsp_family (A & P datasets) for logarithm of barcode data count
genXsp_family_A$log_genetic <- log(genXsp_family_A$genetic_data)
genXsp_family_P$log_genetic <- log(genXsp_family_P$genetic_data)

# assign -1 value to NA entries for log_genetic in A & P datasets
#   NOTE: represent taxa with no barcode records 
genXsp_family_A[is.na(log_genetic)]$log_genetic <- -1
genXsp_family_P[is.na(log_genetic)]$log_genetic <- -1

# add column in genXsp_family  (A & P datasets) for logarithm of species data count
genXsp_family_A$log_species <- log(genXsp_family_A$species_data)
genXsp_family_P$log_species <- log(genXsp_family_P$species_data)

# assign -1 value to NA entries for log_species  in A & P datasets
#   NOTE: represent taxa with no species records
genXsp_family_A[is.na(log_species)]$log_species <- -1
genXsp_family_P[is.na(log_species)]$log_species <- -1

# remove records that are NA for familyUse in A & P datasets
genXsp_family_A <- genXsp_family_A[!is.na(familyUse), ]
genXsp_family_P <- genXsp_family_P[!is.na(familyUse), ]

# combine animal and plant datasets 
#   NOTE: for calculating percentiles of overall barcode and species datasets
genXsp_family <- rbind(genXsp_family_A, genXsp_family_P)

# create scatter plot of barcode against species records per family and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_family_NAexclude.pdf", paper = "a4r")

plot(x = genXsp_family_A$log_species, 
     y = genXsp_family_A$log_genetic, 
     pch = 19, cex = 1.5,
     col = genCol("orangered", 120),
     xlab = "Log number of species records",
     ylab = "Log number of genetic records",
     main = "Family")

points(x = genXsp_family_P$log_species, 
       y = genXsp_family_P$log_genetic,
       pch = 19, cex = 1.5,
       col = genCol("darkgreen", 120))

speciesPer_fam <- quantile(genXsp_family$log_species, seq(0, 1, 0.01), na.rm = TRUE)

speciesPer_fam[c(6, 96)]

abline(v = speciesPer_fam[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

genePer_fam <- quantile(genXsp_family$log_genetic, seq(0, 1, 0.01), na.rm = TRUE)

genePer_fam[c(6, 96)]

abline(h = genePer_fam[c(6, 96)], col = "snow3", lty = 2, lwd = 1.5)

dev.off()


########## end of script
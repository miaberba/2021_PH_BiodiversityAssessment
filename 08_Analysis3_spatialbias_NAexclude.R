# NOTE:
#     This script performs third and last set of analyses for the systematic 
#     review of the Philippines' (PH) biodiversity. The objective is to examine
#     for spatial biases in terms of sampling locations and origin of barcode
#     contributors at a global and national scale. The process is highly similar 
#     as the previous script (07) designated to analyzing for biases in sampling
#     location and contribution, with some steps that may be skipped when using
#     the updated working barcode data table. Additionally, here, the barcode  
#     data analyzed EXCLUDES records that are NA for the country sampled, despite 
#     searches being filtered geographically.


#############################################
####         script starts here          ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load the necessary R packages
#   NOTE: data.table is needed throughout but the rest are necessary specifically
#   for the examination of spatial biases
library("data.table")
library("sp")
library("raster")
library("rgdal")
library("maptools")
library("RColorBrewer")

# load final working species occurrence data table
species_data_final <- fread("working_species_data_FINAL.table", 
                            na.strings = c("", NA))

# load updated working barcode data table 
#   NOTE: resulting data table after doing NAinclude analysis
barcode_data_final <- fread("UPDATEworking_barcode_data_NAinclude.table", 
                            na.strings = c("", NA))

# subset barcode_data_final to contain only explicitly Philippine data
#   NOTE: country_location refers to country sampled and records that are 
#   NA for this are excluded
barcode_data_final <- barcode_data_final[country_location == "Philippines"]

# update assignment of row numbers to barcode datasets
#   NOTE: given that records that are NA for country_location have been excluded
barcode_data_final$rowID <- 1:nrow(barcode_data_final)

# define genCol function for assigning colors derived from RGB
genCol <- function(x, trans){
  
  return(rgb(red = col2rgb(x, alpha = FALSE)[1],
             green = col2rgb(x, alpha = FALSE)[2],
             blue = col2rgb(x, alpha = FALSE)[3],
             max = 255, alpha = trans, names = NULL))
  
}


#### 1) This section prepares the PH shapefiles used for comparison of species and barcode sampling distribution ----
#     NOTE: some steps, commented functions/codes, may not be necessary if 
#     using updated working barcode data table used NAinclude analysis

# load shapefiles of Philippine map at administrative level 2 (i.e., province boundaries)
prov_info <- readOGR(dsn = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/phl_adm_psa_namria_20200529_shp", 
                     layer = "phl_admbnda_adm2_psa_namria_20200529")

# add new province column in prov_info for editing
prov_info@data$provUse <- as.character(prov_info@data$ADM2_EN)

# convert provUse entries to lowercase
prov_info@data$provUse <- tolower(prov_info@data$provUse)

# replace provUse entries referring to "city of isabela" with "basilan"
prov_info@data$provUse[prov_info@data$provUse == "city of isabela"] <- "basilan"

# replace provUse entries referring to "ncr" with "metro manila"
prov_info@data$provUse[grep("ncr", prov_info@data$provUse)] <- "metro manila"

# replace provUse entries referring to "cotabato city" with "cotabato"
prov_info@data$provUse[grep("cotabato city", prov_info@data$provUse)] <- "cotabato"

# # subset barcode data to contain only entries with coordinate info
# #   NOTE: only include metadata columns for coordinates and row ID
# tempPoints_barcode <- barcode_data_final[!is.na(lat) & !is.na(lon), c("lon", "lat", "rowID")]

# # define SpatialPoints object with coordinates obtained from barcode data
# usePoints_barcode <- SpatialPoints(tempPoints_barcode, proj4string = CRS(proj4string(prov_info)))

# subset species data to contain only entries with coordinate info
#   NOTE: only include metadata columns for coordinates and row ID
tempPoints_species <- species_data_final[!is.na(decimalLongitude) & !is.na(decimalLatitude), 
                                         c("decimalLongitude", "decimalLatitude", "rowID")]

# define SpatialPoints object with coordinates obtained from barcode data
usePoints_species <- SpatialPoints(tempPoints_species, proj4string = CRS(proj4string(prov_info)))

# # pull out projection information corresponding to each point in barcode data
# #   NOTE: determine which province each coordinate belongs to and
# #   converting into a shapefile
# resTemp_barcode <- over(usePoints_barcode, prov_info)

# # pull out projection information corresponding to each point in species data
# #   NOTE: determine which province each coordinate belongs to and
# #   converting into a shapefile
# resTemp_species <- over(usePoints_species, prov_info)

# restore resTemp_species R object
#   NOTE: same species dataset as NAinclude analysis
resTemp_species <- readRDS("resTemp_speciesUpdate.rds")

# get centroids of each Philippine province
centroids <- getSpPPolygonsLabptSlots(prov_info)

# # identify barcode entries with coordinates that do not fall within province shapefile
# #   NOTE: most likely represent coordinates of barcode sampling locations done 
# #   in marine environments (e.g., seas)
# noProv <- which(is.na(resTemp_barcode$provUse))

# # assign province to marine coordinates based on shortest distance to centroid
# for( i in 1:length(noProv)){
# 
#   # pull out a point
#   subPoint <- tempPoints_barcode[noProv[i], c(1, 2)]
# 
#   # merge with centroid info
#   tempIn <- rbind(matrix(c(subPoint[ , 1], subPoint[ ,2]), nrow = 1), centroids)
# 
#   # get distance between points
#   tempDist <- as.matrix(dist(tempIn))
# 
#   # identify the nearest point
#   indUse <- which(min(tempDist[-1 , 1]) == tempDist[-1 , 1])
# 
#   # assign the near point to the province
#   resTemp_barcode$provUse[noProv[i]] <- prov_info@data$provUse[indUse]
# 
# }

# # add new province column in barcode_data_final for editing
# barcode_data_final$provinceUse <- barcode_data_final$province

# # update province of entries with coordinates
# #   NOTE: using provinces assigned based on shapefiles
# barcode_data_final[tempPoints_barcode$rowID, ]$provinceUse <- as.character(resTemp_barcode$provUse)

# # convert provinceUse entries to lowercase
# barcode_data_final$provinceUse <- tolower(barcode_data_final$provinceUse)

# # replace provinceUse entries referring to "cotabato (north cotabato)" with "cotabato"
# barcode_data_final[provinceUse == "cotabato (north cotabato)", ]$provinceUse <- "cotabato"

# # replace provinceUse entries referring to "cotabato city" with "cotabato"
# barcode_data_final[provinceUse == "cotabato city", ]$provinceUse <- "cotabato"

# # replace provinceUse entries referring to "davao de oro (compostela valley)" with "compostela valley"
# barcode_data_final[provinceUse == "davao de oro (compostela valley)" , ]$provinceUse <- "compostela valley"

# # replace provinceUse entries referring to "samar (western samar)" with "samar"
# barcode_data_final[provinceUse == "samar (western samar)" , ]$provinceUse <- "samar"

# identify species entries with coordinates that do not fall within province shapefile
#   NOTE: most likely represent coordinates of species sampling locations done 
#   in marine environments (e.g., seas)
noProv <- which(is.na(resTemp_species$provUse))

# assign province to marine coordinates based on shortest distance to centroid
for( i in 1:length(noProv)){
  
  # pull out a point
  subPoint <- tempPoints_species[noProv[i], c(1, 2)]        
  
  # merge with centroid info
  tempIn <- rbind(matrix(c(subPoint[ , 1], subPoint[ ,2]), nrow = 1), centroids)
  
  # get distance between points
  tempDist <- as.matrix(dist(tempIn))
  
  # identify the nearest point
  indUse <- which(min(tempDist[-1 , 1]) == tempDist[-1 , 1])
  
  # assign the near point to the province
  resTemp_species$provUse[noProv[i]] <- prov_info@data$provUse[indUse]
}

# add new province column in species_data_final for editing
species_data_final$provinceUse <- species_data_final$province

# update province of entries with coordinates
#   NOTE: using provinces assigned based on shapefiles
species_data_final[tempPoints_species$rowID, ]$provinceUse <- as.character(resTemp_species$provUse)

# convert provinceUse entries to lowercase
species_data_final$provinceUse <- tolower(species_data_final$provinceUse)


#### 2) This section compares sampling distribution of species and barcode data ----

# define table showing number of barcode records per provinceUse
provTab <- table(barcode_data_final$provinceUse)

# identify indices of province entries matching between prov_info and provTab
matchInd <- match(prov_info@data$provUse, names(provTab))

# add new element in prov_info for summary of barcode records per province
prov_info@data$geneData <- as.numeric(provTab[matchInd])

# add new element in prov_info for classified values of geneData
prov_info@data$colCatGene <- cut((prov_info@data$geneData), 5)

# define red color palette composed of five colors for barcode data
colUseGen <- brewer.pal(5, "Reds")

# replace "#FEE5D9" (red) in colUseGen with "#BEBEBEB4" (grey)
colUseGen[1] <- genCol("grey", 180)

# add new element in prov_info for appropriate colUseGen colors 
#   NOTE: based on entries in colCatGene and to be used for plotting
prov_info@data$colUseGene <- colUseGen[prov_info$colCatGene]

# assign "grey" as default color for entries that are NA for colUseGene
prov_info@data$colUseGene[is.na(prov_info@data$colUseGene)] <- "grey"

# define table showing number of species records per provinceUse
gbifProvTab <- table(species_data_final$provinceUse)

# identify indices of province names in gbifProvTab that are also used in prov_info
#   NOTE: while geolocation in species data was parsed, province entries were not  
#   completely standardized and cleaned due to large amount of data 
includeList <- which(names(gbifProvTab) %in% prov_info@data$provUse)

# redefine gbifProvTab to only include province entries also used in prov_info
gbifProvTab <- gbifProvTab[includeList]

# identify indices of province entries matching between prov_info and gbifProvTab
matchUse <- match(prov_info@data$provUse, names(gbifProvTab))

# add new element in prov_info for summary of species records per province
prov_info@data$gbifData <- as.numeric(gbifProvTab[matchUse])

# add new element in prov_info for classified values of gbifData
prov_info@data$colgbif <- NA
prov_info@data$colCatgbif <- cut((prov_info@data$gbifData), 5)

# define red color palette composed of five colors for species data
colUsegbif <- brewer.pal(5, "Reds")

# replace "#FEE5D9" (red) in colUsegbif with "#BEBEBEB4" (grey)
colUsegbif[1] <- genCol("grey", 180)

# add new element in prov_info for appropriate colUsegbif colors 
#   NOTE: based on entries in colCatgbif and to be used for plotting
prov_info@data$colUsegbif <- colUsegbif[prov_info@data$colCatgbif]

# # assign "grey" as default color for entries that are NA for colUsegbif
# prov_info@data$colUsegbif[is.na(prov_info@data$colUsegbif)] <- "grey"

# define character list of entries in colUsegbif
gbifCol <- as.character(prov_info@data$colUsegbif)

# define data table showing number of barcode and species records per province
plotUse <- data.table(gbif = as.numeric(prov_info@data$gbifData),
                      gene = as.numeric(prov_info@data$geneData),
                      province = as.character(prov_info@data$provUse))

# save prov_info shapefile (province boundaries)
writeOGR(prov_info, 
         "~/Desktop/tempResearch", 
         "Matias_BerbaThesis_PhilippinesProvince_Update4", 
         driver = "ESRI Shapefile")

# create heat map of barcode sampling distribution and save as PDF format
pdf("./Figures_unedited/GeneheatmapRawScaleNoBroder_NAexclude.pdf", paper = "a4")

plot(prov_info, col = prov_info@data$colUseGene, 
     border = NA, lwd = 0.1)

genelevelUse <- unlist(lapply(strsplit(gsub("\\(|\\[|\\)|\\]", "", levels(prov_info$colCatGene)), ","),
                              function(x) median(as.numeric(x))))

legend("topleft", legend = round(genelevelUse, 0),
       pch = 15, col = colUseGen, bty = "n")

dev.off()

# create heat map of species sampling distribution and save as PDF format
pdf("./Figures_unedited/SpeciesheatmapRawScaleNoBorder_NAexclude.pdf", paper = "a4")

plot(prov_info, col = prov_info@data$colUsegbif, 
     border = NA, lwd = 0.1)

gbiflevelUse <- unlist(lapply(strsplit(gsub("\\(|\\[|\\)|\\]", "", levels(prov_info$colCatgbif)), ","), 
                              function(x) median(as.numeric(x))))

legend("topleft", legend = round((gbiflevelUse), 0),
       pch = 15, col = colUsegbif, bty = "n")

dev.off()

# create scatter plot of barcode against species records per province and save as PDF format
#   NOTE: include lines to represent 5th and 95th percentile of both datasets
pdf("./Figures_unedited/geneXspecies_province_NAexclude.pdf", paper = "a4r")

plot(x = log(plotUse$gbif),
     y = log(plotUse$gene),
     col = genCol("red", 120),
     pch = 19, cex = 1.5, 
     xlab = "Log number of species records",
     ylab = "Log number of genetic records")

gbifPer <- quantile(log(plotUse$gbif), seq(0, 1, 0.01))

gbifPer[c(6, 96)]

abline(v = gbifPer[c(6, 96)], col = "grey", lty = 2, lwd = 1.5)

genePer <- quantile(log(plotUse$gene), seq(0, 1, 0.01), na.rm = TRUE)

genePer[c(6, 96)]

abline(h = genePer[c(6, 96)], col = "grey", lty = 2, lwd = 1.5)

dev.off()


#### 3) This section assesses the distribution of global contribution in barcode data ----

# load wrld_simpl shapefile from maptools package
data(wrld_simpl)

# replace blank ("") entries with NA for uniXcopyright
barcode_data_final[uniXcopyright_country == "", ]$uniXcopyright <- NA

# replace blank ("") entries with NA for uniXcopyright_country
barcode_data_final[uniXcopyright_country == "", ]$uniXcopyright_country <- NA

# define table showing number of barcode records per uniXcopyright_country
copyDist <- table(barcode_data_final$uniXcopyright_country)

# define table showing number of barcode records per uniXstoring_country
storeDist <- table(barcode_data_final$uniXstoring_country)

# replace copyDist entry referring to "USA" with "United States"
names(copyDist)[names(copyDist) == "USA"] <- "United States"

# replace copyDist entry referring to "South Korea" with "Korea, Republic of"
names(copyDist)[names(copyDist) == "South Korea"] <- "Korea, Republic of"

# subset copyDist to exclude entries, "USA/Philippines" (43) and "vague" (44)
copyDist <- copyDist[c(-43, -44)]

# add new element in wrld_simpl 
wrld_simpl@data$storingCount <- NA

# identify indices of country entries matching between copyDist and wrld_simpl
matchInfo <- match(names(copyDist), wrld_simpl@data$NAME)

# update entries of storingCount with the appropriate values in copyDist
wrld_simpl@data$storingCount[matchInfo] <- copyDist

# add new element in wrld_simpl for classified values of storingCount
wrld_simpl@data$storingCat <- cut(wrld_simpl@data$storingCount, 5)

# define red color palette composed of five colors
coluse <- brewer.pal(n = 5, "Reds")

# assign "#BEBEBE64" value to colUse element
wrld_simpl@data$colUse <- genCol("grey", 100)

# add new element in wrld_simpl for appropriate coluse colors 
#   NOTE: based on entries in storingCat and to be used for plotting
wrld_simpl@data$colUse <- coluse[as.integer(wrld_simpl$storingCat)]

# assign "#BEBEBEB4" as default color for entries that are NA for colUse
wrld_simpl@data$colUse[is.na(wrld_simpl@data$colUse)] <- genCol("grey", 180)

# create heat map of global contribution of barcode data and save as PDF format
pdf("./Figures_unedited/worldBarcodeDist_NAexclude.pdf", paper = "a4r")

plot(wrld_simpl, col = wrld_simpl@data$colUse,
     border = NA, lwd = 0.1)

worldLevelUse <- unlist(lapply(strsplit(gsub("\\(|\\[|\\)|\\]", "", levels(wrld_simpl@data$storingCat)), ","), 
                               function(x) median(as.numeric(x))))

legend("topleft", legend = round((worldLevelUse), 0),
       pch = 15, col = coluse, bty = "n")

lines(x = c(105, 145, 145, 105, 105),
      y = c(-5, -5, 32, 32, -5), col = genCol("black", 100), lty = 2, lwd = 0.5)

dev.off()  

# create heat map of global contribution of barcode data, zoomed on PH, and save as PDF format
pdf("./Figures_unedited/worldBarcodeDist_philZoom_NAexclude.pdf", paper = "a4r")

temp <- crop(wrld_simpl, c(105, 145, -5, 32))

plot(temp, col = temp@data$colUse,
     border = NA, lwd = 0.1)

dev.off()


#### 4) This section assesses the global contribution in barcode data across collection time ----
#     NOTE: some steps, commented functions/codes, may not be necessary if 
#     using updated working barcode data table used NAinclude analysis

# # pull out year information from collection_date column in barcode_data_final
# #   NOTE: entries in collection_date are a mix of year only, day-month-year, 
# #   and month-year
# tempYear <- lapply(strsplit(barcode_data_final$collection_date, "-"),
#                    function(x) {
# 
#                      if(length(x[nchar(x) >= 4]) == 0){
# 
#                        return(NA)
#                      }else{
# 
#                        return(x[nchar(x) >= 4])
#                      }
# 
#                    })

# # add new column in barcode_data_final for collection year info (tempYear)
# barcode_data_final$yearCollect <- unlist(tempYear)

# # replace yearCollect entry with "month" pattern to NA 
# barcode_data_final$yearCollect[grep("month", barcode_data_final$yearCollect)] <- NA

# # add new column in barcode_data_final
# barcode_data_final$countryStore <- NA

# # convert class of countryStore column to character
# barcode_data_final$countryStore <- as.character(barcode_data_final$countryStore)

# # categorize uniXcopyright_country entries as "Philippines" or "Foreign"
# #   NOTE: place categorized values in countryStore
# barcode_data_final[!is.na(uniXcopyright_country),
#                    countryStore:= ifelse(uniXcopyright_country == "Philippines", "Philippines", "Foreign")]

# define data table showing number of barcode records per countryStore and yearCollect
yearInfo <- barcode_data_final[ , .N, by =.(countryStore, yearCollect)]

# arrange order of entries in yearInfo by yearCollect
yearInfo <- yearInfo[order(yearCollect)]

# relabel columns in yearInfo 
names(yearInfo) <- c("category", "year", "N")

# subset yearInfo to contain only Philippine entries from later than 1990
philRec <- yearInfo[year > 1990 & category == "Philippines", ]

# subset yearInfo to contain only Foreign entries from later than 1990
foreignRec <- yearInfo[year > 1990 & category == "Foreign", ]

# define curve for Philippine barcoding activity using loess regression
philSMOOTH <- loess(N ~ year, data = philRec)

# define curve for Foreign barcoding activity using loess regression
foreignSMOOTH <- loess(N ~ year, data = foreignRec)

# define Raster object representing philSMOOTH curve
philPlot <- predict(philSMOOTH, philRec, se = T)

# define Raster object representing foreignSMOOTH curve
foreginPlot <- predict(foreignSMOOTH, foreignRec, se = T)

# create scatter plot of barcode sample collection across time and save as PDF format
#   NOTE: include smoothened curves obtained via loess regression
pdf("./Figures_unedited/geneXyearCollect_NAexclude.pdf", paper = "a4r")

plot(x = philSMOOTH$x,
     y = philSMOOTH$y, pch = 19, col = genCol("red", 80), type = "n",
     xlab = "Year", ylab = "Number of genetic records")

# curve for foreign category
polygon(x = c(foreignSMOOTH$x, rev(foreignSMOOTH$x)),
        y = c(foreginPlot$fit - foreginPlot$se.fit, rev(foreginPlot$fit + foreginPlot$se.fit)),
        col = genCol("blue", 50), border = NA)

lines(x = foreignSMOOTH$x,
      y = foreginPlot$fit,
      lty = 1, lwd = 3, col = genCol("blue", 100))

points(x = foreignSMOOTH$x,
       y = foreignSMOOTH$y,
       pch = 19, col = genCol("blue", 80),
       cex = 1.5)

# curve for local category
polygon(x = c(philSMOOTH$x, rev(philSMOOTH$x)),
        y = c(philPlot$fit - philPlot$se.fit, rev(philPlot$fit + philPlot$se.fit)), 
        col = genCol("red", 50), border = NA)

lines(x = philSMOOTH$x,
      y = philPlot$fit, lty = 1, lwd = 3, col = genCol("red", 100))

points(x = philSMOOTH$x,
       y = philSMOOTH$y, pch = 19, col = genCol("red", 80),
       cex = 1.5)


legend("topleft", legend = c("Philippines", "Foreign", '"fitted value"'),
       pch = c(19, 19, NA), lty = c(NA, NA, 1), col = c("red", "blue", "black"),
       bty = "n")

dev.off()


#### 5) This section assesses the global contribution in barcode data across submission time ----

# define data table showing number of barcode records per countryStore and yearSubmitted
yearSub <- barcode_data_final[ , .N, by =.(countryStore, yearSubmitted)]

# relabel columns in yearSub
names(yearSub) <- c("category", "year", "N")

# remove entries in yearSub that have missing values  
yearSub <- yearSub[complete.cases(yearSub), ]

# subset yearInfo to contain only Philippine entries
philRec <- yearSub[category == "Philippines", ]

# range order of entries in philRec by year
philRec <- philRec[order(year)]

# subset yearInfo to contain only Foreign entries
foreignRec <- yearSub[category == "Foreign", ]

# range order of entries in foreignRec by year
foreignRec <- foreignRec[order(year)]

# define curve for Philippine barcoding activity using loess regression
philSMOOTH <- loess(N ~ year, data = philRec)

# define curve for Foreign barcoding activity using loess regression
foreignSMOOTH <- loess(N ~ year, data = foreignRec)

# define Raster object representing philSMOOTH curve
philPlot <- predict(philSMOOTH, philRec, se = T)

# define Raster object representing foreignSMOOTH curve
foreginPlot <- predict(foreignSMOOTH, foreignRec, se = T)

# create scatter plot of barcode submission across time and save as PDF format
#   NOTE: include smoothened curves obtained via loess regression
pdf("./Figures_unedited/geneXyearSubmission_NAexclude.pdf", paper = "a4r")

plot(x = philSMOOTH$x,
     y = philSMOOTH$y, pch = 19, col = genCol("red", 80), type = "n",
     xlab = "Year", ylab = "Number of genetic records", ylim = c(-30, 2000))

# curve for foreign category
polygon(x = c(foreignSMOOTH$x, rev(foreignSMOOTH$x)),
        y = c(foreginPlot$fit - foreginPlot$se.fit, rev(foreginPlot$fit + foreginPlot$se.fit)),
        col = genCol("blue", 50), border = NA)

lines(x = foreignSMOOTH$x,
      y = foreginPlot$fit,
      lty = 1, lwd = 3, col = genCol("blue", 100))

points(x = foreignSMOOTH$x,
       y = foreignSMOOTH$y,
       pch = 19, col = genCol("blue", 80),
       cex = 1.5)

# curve for local category
polygon(x = c(philSMOOTH$x, rev(philSMOOTH$x)),
        y = c(philPlot$fit - philPlot$se.fit, rev(philPlot$fit + philPlot$se.fit)), 
        col = genCol("red", 50), border = NA)

lines(x = philSMOOTH$x,
      y = philPlot$fit, lty = 1, lwd = 3, col = genCol("red", 100))

points(x = philSMOOTH$x,
       y = philSMOOTH$y, pch = 19, col = genCol("red", 80),
       cex = 1.5)


legend("topleft", legend = c("Philippines", "Foreign", '"fitted value"'),
       pch = c(19, 19, NA), lty = c(NA, NA, 1), col = c("red", "blue", "black"),
       bty = "n")

dev.off()


#### 6) This section prepares the matrix used for assessment of local contribution in barcode data ----
#     NOTE: some steps, commented functions/codes, may not be necessary if 
#     using updated working barcode data table used NAinclude analysis

# # load PH province and region info table
# #   NOTE: same info table generated in NAinclude analysis
# provinceRegInfo <- fread("provinceRegionInfo.table", header = T)

# # load data table of unique PH institutions and their corresponding province
# #   NOTE: manually edited and derived from unique entries in barcode_data_final
# ph_uni_province <- fread("PHuniversity_province.tsv", header = T)

# # edit uniXcopyright entry in barcode_data_final
# #   NOTE: standardize entries referring to Mindanao State University
# barcode_data_final$uniXcopyright <- gsub("MSU-IIT",
#                                          "Iligan Institute of Technology, Mindanao State University",
#                                          barcode_data_final$uniXcopyright)

# # identify indices of entries matching between barcode_data_final and ph_uni_province
# #   NOTE: should only match among local institutions
# matchUni <- match(barcode_data_final$uniXcopyright, ph_uni_province$University)

# # add new column in barcode_data_final for appropriate provinces of uniXcopyright entries
# barcode_data_final$uniXcopyright_prov <- ph_uni_province$uni_province[matchUni]

# # identify indices of entries matching between barcode_data_final and provinceRegInfo
# #   NOTE: match only records associated with local institutions and focus on
# #   uniXcopyright_prov
# matchRegion <- match(barcode_data_final$uniXcopyright_prov, provinceRegInfo$prov)

# # add new column in barcode_data_final for appropriate regions of uniXcopyright_prov entries
# barcode_data_final$uniXcopyright_reg <- provinceRegInfo$region[matchRegion]

# # identify indices of entries matching between barcode_data_final and provinceRegInfo
# #   NOTE: match among all records and focus on provinceUse
# matchProv <- match(barcode_data_final$provinceUse, provinceRegInfo$prov)

# # add new column in barcode_data_final for appropriate regions of provinceUse entries
# barcode_data_final$regionUse <- provinceRegInfo$region[matchProv]


#### 7) This section assesses the distribution of local contribution in barcode data ----

# subset barcode_data_final to contain only records sampled and generated locally
#   NOTE: sampled = in terms of countryLocation, generated = in terms of uniXcopyright_country
philSub <- barcode_data_final[countryLocation == "Philippines" & 
                                uniXcopyright_country == "Philippines", ]

# replace blank ("") entries with NA for provinceUse
philSub$provinceUse[philSub$provinceUse == ""] <- NA

# define data table showing number of barcode records per regionUse and uniXcopyright_reg
#   NOTE: only include records that have info for provinceUse and uniXcopyright_prov
regionData <- philSub[!is.na(provinceUse) & !is.na(uniXcopyright_prov), 
                      .N, by = .(regionUse, uniXcopyright_reg)]

# add new column in regionData to serve as row ID
regionData[ , id := paste(regionUse, uniXcopyright_reg, sep = "__")]

# define data frame of all combinations between regionUse and uniXcopyright_reg
#   NOTE: Var1 = regionUse, Var2 = uniXcopyright_reg
tempMatch <- expand.grid(unique(c(regionData$regionUse, regionData$uniXcopyright_reg)),
                         unique(c(regionData$regionUse, regionData$uniXcopyright_reg)))

# convert class of tempMatch to data table
tempMatch <- data.table(tempMatch)

# add new column in tempMatch to serve as row ID
tempMatch[ , id := paste(Var1, Var2, sep = "__")]

# add new numeric column in tempMatch
tempMatch$numRec <- 0

# identify indices of id entries matching between regionData and tempMatch 
matchUse <- match(regionData$id, tempMatch$id)

# update numRec entries with barcode data summary in regionData
tempMatch$numRec[matchUse] <- regionData$N

# convert Var1 entries to character
tempMatch$Var1 <- as.character(tempMatch$Var1)

# convert Var2 entries to character
tempMatch$Var2 <- as.character(tempMatch$Var2)

# define matrix of tempMatch in the form of a data table
#   NOTE: Var1 (vertical) = region sampled, Var2 (horizontal) = processing center
tempPlot <- dcast(tempMatch, Var1 ~ Var2, value.var = "numRec")

# load manually edited PH region number and name info table
regName <- fread("region_name.tsv")

# relabel Var1 entries with appropriate region names from regName
tempPlot$Var1 <- regName$reg_name[match(tempPlot$Var1, regName$reg_number)]

# relabel Var2 entries with appropriate region names from regName
#   NOTE: Var2 are treated as column headers and are in same order as Var1 entries 
names(tempPlot)[-1] <- tempPlot$Var1

# rearrange order of Var1 and Var2 entries based on spatial context
#   NOTE: from northwest most to southeast most regions across the Philippines
PHuni_matrix_sort <- tempPlot[c(4, 2, 9, 10, 3, 11, 13, 12, 14:17, 5, 8, 1, 7, 6),
                              c(1, 5, 3, 10, 11, 4, 12, 14, 13, 15:18, 6, 9, 2, 8, 7)]

# identify indices of Var1 entries matching between PHuni_matrix_sort and tempPlot 
orderMat <- match(PHuni_matrix_sort$Var1, tempPlot$Var1)

# define matrix of tempPlot values
#   NOTE: rows and columns sorted similarly to PHuni_matrix_sort and 
#   first column dropped
matUse <- (as.matrix(tempPlot[orderMat, ][ , -1])[ , orderMat])

# define RasterLayer object of matUse
rastTemp <- raster(matUse)

# load shapefiles of Philippine map at administrative level 1 (i.e., region boundaries)
region_info <- readOGR(dsn = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/phl_adm_psa_namria_20200529_shp", 
                       layer = "phl_admbnda_adm1_psa_namria_20200529")

# add new province column in region_info for editing 
region_info@data$regInfo <- tolower(region_info@data$ADM1ALT1EN)

# replace regInfo entry referring to "ncr" (3) with "metro manila"
region_info@data$regInfo[3] <- "metro manila"

# identify indices of region entries matching between region_info and matUse 
tempMatch <- match(region_info@data$regInfo,  tolower(colnames(matUse)))

# determine sum per column in matUse 
#   NOTE: represents total record count associated per processing region 
sumProcess <- apply(matUse[ , -1], 2, function(x) sum(x))

# add new element in region_info for record count per region
region_info@data$count <- sumProcess[tempMatch]

# add new element in region_info for classified log values of count
region_info@data$regionCat <- cut(log(region_info@data$count + 1), 5)

# define red color palette composed of five colors
colReg <- brewer.pal(5, "Reds")

# define list of medians of value category in regionCat
regionCol <- unlist(lapply(strsplit(gsub("\\(|\\[|\\)|\\]", "", levels(region_info@data$regionCat)), ","),
                           function(x) median(as.numeric(x))))

# add new element in region_info for appropriate colReg colors 
#   NOTE: based on entries in regionCat and to be used for plotting
region_info@data$regionCol <- colReg[as.integer(region_info@data$regionCat)]

# assign "#BEBEBE64" as default color for entries that are NA for regionCol
region_info@data$regionCol[is.na(region_info@data$regionCol)] <- genCol("grey", 100)

# create Philippine map with region boundaries highlightedv and save as PDF format
#   NOTE: blank map to be used for spatial reference of regions
pdf("./Figures_unedited/refmapRegion_NAexclude.pdf", paper = "a4")

plot(region_info, col = genCol("grey", 100),
     border = genCol("black", 50), lwd = 0.05)

dev.off()

# create correlation matrix of local barcode contribution and save as PDF format
pdf("./Figures_unedited/sourceXprocess_matrix_NAexclude.pdf", paper = "a4r")

plot(rastTemp,
     xaxt = "n", yaxt = "n", box = FALSE, axes= FALSE)

axis(side = 1, at = seq(1/17/2, 1 - (1/17/2), 1/17), labels = FALSE)

axis(side = 2, at = seq(1/17/2, 1 - (1/17/2), 1/17), labels = FALSE)

y <- 1 - seq(0, 1, 1/17)

x <- seq(0, 1, 1/17)

for(i in 1:(length(x) - 1)){
  
  for(j in 1:(length(y) - 1)){
    
    if(i == j){
      
      lines(x = c(x[j], x[j + 1]),
            y = c(y[i], y[i + 1]), col = genCol("black", 100))
    }
    
    lines(x = c(x[j], x[j], x[j+1], x[j+1], x[j]),
          y = c(y[i], y[i + 1], y[i+1], y[i], y[i]), col = genCol("black", 100))
    
  }
  
}

dev.off()


########## end of script
# NOTE: 
#     This script performs the first set of metadata parsing on the raw overall
#     BOLD dataset (01), which focuses on the metadata for the description of 
#     sampling location. The process is similar to the geolocation parsing done
#     on the GenBank dataset, with the objective also being to pull out the 
#     sampling location info in terms of Philippine administrative units
#     (i.e., province, municipality, barangay). It involves both matching explicit 
#     entries or patterns with known administrative names and manually assessing 
#     and editing the unique entries via Google Sheet.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")


#### 1) This section prepares the PH admin database ----

# load and subset Philippine (PH) admin database derived from PSGC data
geo.table <- fread("locationInfo.table", header = TRUE, na.strings = "")

# subset PH admin database to contain only province, municipality, barangay
geo.database <- geo.table[ ,16:18, drop = FALSE]

# relabel columns in geo.database
names(geo.database) <- c("province", "municipality", "barangay")

# convert barangay entries to uppercase
geo.database[ , 3] <- toupper(geo.database$barangay)

# define list of unique entries for province, municipality, and barangay
name_location <- lapply(geo.database, function(x){temp <- unique(x)})

# define list of the ranks for each entry in PH admin database
rank_location <- rep(names(geo.database), 
                     unlist(lapply(name_location, function(x) length(x))))

# define data table containing list of PH admin names and corresponding rank
location.ranking <- data.table(rank = rank_location, 
                               name = unlist(name_location))

# remove rows that are NA for name column
geo.ranklist <- location.ranking[!is.na(name), ]


#### 2) This section examines and cleans descriptive geolocation metadata ----

# load overall BOLD dataset - bold01_raw_overall
bold_overall <- fread("bold01_raw_overall",
                      sep = "\t",
                      header = TRUE)

# subset BOLD dataset to contain only metadata on sampling location
bold_geolocation <- bold_overall[ ,c(47:48, 55:59)]

# combine descriptive entries on sampling location of each row into one entry 
#   NOTE: can also serve as unique identifier
bold_geolocation$geo_entry <- paste(bold_geolocation$province_state, 
                                    bold_geolocation$region, 
                                    bold_geolocation$sector, 
                                    bold_geolocation$exactsite, 
                                    sep = ",")

# check number of entries based on unique identifier
length(unique(bold_geolocation$geo_entry))

# subset unique geo_entry entries in BOLD geolocation dataset
working_bold_geoparse <- bold_geolocation[!duplicated(geo_entry), 
                                          c("country", "geo_entry")]

# write output of subset of BOLD geolocation data for manual editing
write.table(working_bold_geoparse,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold_unique_geolocation.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            qmethod = "double")

# load manually edited BOLD geolocation data 
edited_location <- fread("bold_geoparsing_edited.tsv", 
                         header = TRUE, 
                         na.strings = "")  

# convert edited_geo_entry entries to uppercase
edited_location[ , 3] <- toupper(edited_location$edited_geo_entry)

# split edited_geo_entry entries based on COMMA and transpose into a data table
geolocation_data <- edited_location[ , data.table::tstrsplit(edited_geo_entry, 
                                                             split = ",", 
                                                             fixed = TRUE)]

# evaluate entries in geolocation_data and match with PH admin database
#   NOTE: use list with names and rankings - geo.ranklist 
geolocationUse <- t(apply(geolocation_data, 1, function(x, a){
  
  tempOut  <- a$rank[match(x, a$name)]
  
  orderUse <- match(c("province", "municipality", "barangay"), tempOut)
  
  x[orderUse]
  
}, a = geo.ranklist))

# convert matching results into data table
geolocationUse <- data.table(geolocationUse)

# relabel columns in geolocationUse 
names(geolocationUse) <- c("province_match", "municipality_match", "barangay_match")

# combine matching results to BOLD geolocation dataset
geolocation_complete <- cbind(edited_location, geolocationUse)


#### 3) This section aims to cross-check the matching results with PH admin database ----
#     NOTE: possible mismatching of province, municipality, and barangay 
#     as well as missing information

# merge BOLD geolocation dataframe with PH admin dataframe based on barangay
#   NOTE: match only among rows with barangay entries in geo.database
merged_geo_brgy <- merge(geolocation_complete, 
                         geo.database[!is.na(geo.database$barangay)], 
                         by.x = 'barangay_match',
                         by.y = 'barangay',
                         all.x = TRUE)

# rearrange order of columns
setcolorder(merged_geo_brgy, 
            c("country", "geo_entry", "edited_geo_entry", "province_match", 
              "municipality_match", "barangay_match", "municipality", "province"))

# merge merged_geo_brgy dataframe with PH admin dataframe based on municipality
#   NOTE: only among entries that have no barangay entries 
merged_geo_munic <- merge(merged_geo_brgy[is.na(merged_geo_brgy$barangay_match)],
                          geo.database[!is.na(geo.database$municipality),-3],
                          by.x = 'municipality_match',
                          by.y = 'municipality',
                          all.x = TRUE)

# rearrange order of columns
setcolorder(merged_geo_munic, 
            c("country", "geo_entry", "edited_geo_entry", "province_match", 
              "municipality_match", "barangay_match"))

# remove duplicates of geo_entry entries from merged_geo_munic dataframe
merged_geo_munic <- merged_geo_munic[!duplicated(geo_entry)]

# drop province.x column from merged_geo_munic dataframe
# NOTE: province.x is original, blank while province.y is the updated column 
merged_geo_munic <- merged_geo_munic[ , -8]

# relabel province.y column in merged_geo_munic into the main "province" column
names(merged_geo_munic)[8] <- "province"

# remove rows with NA barangay_match entries for merged_geo_brgy dataframe
merged_geo_brgy <- merged_geo_brgy[!is.na(merged_geo_brgy$barangay_match)]

# check if column names and order are similar between the two merge outputs
#   NOTE: all must be TRUE
names(merged_geo_brgy) == names(merged_geo_munic)

# combine the two merge outputs - based on barangay and on municipality
merged_geo_brgxmun <- rbind(merged_geo_brgy, merged_geo_munic)

# write output for BOLD geolocation data (unique) for manual editing and checking
write.table(merged_geo_brgxmun,
            file = "bold_mergedgeo_brgxmun.csv",
            sep = ",",
            row.names = FALSE,
            qmethod = "double")

# define another dataframe for BOLD raw data
#   NOTE: to be used for incorporating the manually parsed geolocation data
bold_fixgeo <- bold_overall

# combine descriptive entries on sampling location of each row into one entry 
#   NOTE: serve as the "original" unique identifier
bold_fixgeo$original_geo_entry <- paste(bold_fixgeo$province_state,
                                        bold_fixgeo$region,
                                        bold_fixgeo$sector,
                                        bold_fixgeo$exactsite,
                                        sep = ",")

# load manually checked and edited BOLD geolocation data
bold_geoparsed <- fread("bold_finalgeo_parsed.tsv", header = TRUE, na.strings = "")



# merge bold_geoparsed into main BOLD dataset based on original_geo_entry
merged_geobold <- merge(bold_fixgeo, 
                        bold_geoparsed, 
                        by = 'original_geo_entry', 
                        all.x = TRUE, 
                        sort = FALSE)

# rearrange order of columns
sorted_merged_geobold <- merged_geobold[ , c(2:60, 1, 82:89, 61:81)]

# write output for updated main BOLD dataset
write.table(sorted_merged_geobold, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold02_geoparsed_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
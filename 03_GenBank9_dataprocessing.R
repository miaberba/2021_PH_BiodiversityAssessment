# NOTE: 
#     This script performs the seventh and last set of metadata parsing on the
#     most updated raw GenBank dataset (07), which focuses on the metadata for
#     the description of sampling location. In this case, the objective is to 
#     pull out the sampling location info in terms of Philippine administrative 
#     units (i.e., province, municipality, barangay). The process involves both
#     matching explicit entries or patterns with known administrative names and
#     manually assessing and editing the unique entries via Google Sheets.
#     Additionally, coordinate info included in the description are also parsed 
#     and placed into the appropriate metadata columns.


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

# load updated main GenBank dataset - genbank07_markerparsed_overall
genbank_fixgeo <- fread(file = "genbank07_markerparsed_overall", 
                        sep = "\t",
                        header = TRUE)

# split country entries based on COLON and transpose into a data table
split.location <- genbank_fixgeo[ , data.table::tstrsplit(country,
                                                          split = ":",
                                                          fixed = TRUE)]

# relabel columns in split.location
#   NOTE: V1 = original_country, V2 = original_geo_entry 
names(split.location) <- c("original_country", "original_geo_entry")

# check number of entries based on unique identifier (V2)
length(unique(split.location$original_geo_entry))

# subset unique original_geo_entry entries in GenBank geolocation dataset
unique_geoentries <- split.location[!duplicated(original_geo_entry), ]

# write output of subset of GenBank geolocation data for manual editing
write.table(unique_geoentries,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/GnBk_geolocation_parsing.csv",
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            qmethod = "double")

# load manually edited GenBank geolocation data 
edited_location <- fread("GnBk_edited_location_parsing.csv", 
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

# combine matching results to GenBank geolocation dataset
geolocation_complete <- cbind(edited_location, geolocationUse)


#### 3) This section aims to cross-check the matching results with PH admin database ----
#     NOTE: possible mismatching of province, municipality, and barangay 
#     as well as missing information

# merge GenBank geolocation dataframe with PH admin dataframe based on barangay
#   NOTE: match only among rows with barangay entries in geo.database
merged_geo_brgy <- merge(geolocation_complete, 
                         geo.database[!is.na(geo.database$barangay)], 
                         by.x = 'barangay_match',
                         by.y = 'barangay',
                         all.x = TRUE)

# rearrange order of columns
setcolorder(merged_geo_brgy, 
            c("original_country", "original_geo_entry", "edited_geo_entry", "province_match", 
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
            c("original_country", "original_geo_entry", "edited_geo_entry", 
              "province_match", "municipality_match", "barangay_match"))

# remove duplicates of original_geo_entry entries from merged_geo_munic dataframe
merged_geo_munic <- merged_geo_munic[!duplicated(original_geo_entry)]

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

# write output for GenBank geolocation data (unique) for manual editing and checking
write.table(merged_geo_brgxmun,
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/GnBk_mergedgeo_brgxmun.csv",
            sep = ",",
            row.names = FALSE,
            qmethod = "double")

# combine split.location data table to main GenBank dataset
genbank_geolocation <- cbind(genbank_fixgeo, split.location)

# load manually checked and edited GenBank geolocation data
genbank_geoparsed <- fread("GnBk_finalgeo_parsed.tsv", 
                           header = TRUE, 
                           na.strings = c("", "NA"))

# merge genbank_geoparsed into main GenBank dataset based on original_geo_entry
merged_geogb <- merge(genbank_geolocation, 
                      genbank_geoparsed, 
                      by = 'original_geo_entry', 
                      all.x = TRUE, 
                      sort = FALSE)

# check for NA entries for original_geo_entry column in merged_geogb
#   NOTE: manually edited GenBank geolocation data has only one row that is NA
#   for original_geo_entry (also blank for the rest of the columns)
merged_geogb[is.na(original_geo_entry), 
             c("original_country", "original_geo_entry", "country_edited")]

# identify indices of rows that are NA for original_geo_entry column
#   NOTE: used to fix entries for country_edited
NA_entries <- which(is.na(merged_geogb$original_geo_entry))

# copy original_country entries into country_edited column 
merged_geogb$country_edited[NA_entries] <- merged_geogb$original_country[NA_entries]

# rearrange order of columns
sorted_merged_geogb <- merged_geogb[ , c(2:25, 27:63, 26, 64, 1, 65:74)]


#### 4) This section includes additional coordinate metadata from geolocation entries in GenBank data ----

# identify indices of rows that have values for extra_notes column
#   NOTE: extra_notes column contains coordinate information pulled out from 
#   the geolocation (description) metadata instead of lat_lon 
additional_latlon <- which(!is.na(sorted_merged_geogb$extra_notes))

# check if coordinate info has already been included/pulled out in lat_lon
sorted_merged_geogb$lat_lon[additional_latlon] 

# substitute SPACES in extra_notes entries with UNDERSCORE
#   NOTE: to mimic format of entries in lat_lon (e.g., "9.6383_N_123.838_E")
#   since in extra_notes entries are slightly different (e.g., "11.6 N, 124.4833 E")
sorted_merged_geogb$extra_notes[additional_latlon] <- gsub(" |, |,", "_", sorted_merged_geogb$extra_notes[additional_latlon])

# pull out and separate numerical info of latitude and longitude from extra_notes
coordinates <- sorted_merged_geogb[additional_latlon, .(lat = as.numeric(gsub("_[A-Z]_.*$", "", extra_notes)),
                                                        lon = as.numeric(gsub("^.*_[A-Z]_|_[A-Z]$", "", extra_notes)))]

# check for extra_notes entries with coordinate info referring to WEST and/or SOUTH
#   NOTE: if none, no need to convert numerical value to negative
sorted_merged_geogb[grep("W|S", sorted_merged_geogb$extra_notes)]$extra_notes

# transpose numerical latitude info pulled out from extra_notes into lat column
#   NOTE: only for rows that have values for extra_notes column
sorted_merged_geogb$lat[additional_latlon] <- coordinates$lat

# transpose numerical longitude info pulled out from extra_notes into lon column
#   NOTE: only for rows that have values for extra_notes column
sorted_merged_geogb$lon[additional_latlon] <- coordinates$lon

# check if numerical values placed in lat and lon column match extra_notes entries
sorted_merged_geogb[additional_latlon, c("lat", "lon", "extra_notes")]

# write output for final raw and main GenBank dataset
write.table(sorted_merged_geogb, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank08_geoparsed_overall_FINAL", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
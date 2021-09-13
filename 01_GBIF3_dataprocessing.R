# NOTE: 
#     This script performs the next stage of geolocation parsing on the updated 
#     raw GBIF dataset (02). This time, the process involves identifying sampling
#     entries that are problematic since they have been categorized as more than 
#     one administrate unit - "MULTIPLE" - and have no additional information on
#     the sampling location (i.e., coordinates). These categories of these entries 
#     are then manually edited through Google Sheets and the codes in this script 
#     before being incorporated back into the overall GBIF dataset to update the 
#     main table. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

#### 1) This section identifies problematic sampling entries categorized as MULTIPLE ----

# load updated main GBIF dataset - "gbif02_geoclassified_overall"
merged_geogbif <- fread("gbif02_geoclassified_overall")

# subset GBIF dataset to contain only metadata on sampling location
sub_geolatlon <- merged_geogbif[ ,243:253]
names(sub_geolatlon)[10:11] <- c("lat","lon")

# create unique ID for each row
sub_geolatlon$unique_id <- paste(sub_geolatlon$stateProvince, 
                                 sub_geolatlon$county, 
                                 sub_geolatlon$municipality, 
                                 sub_geolatlon$locality, sep = ",")

# subset data with "MULTIPLE" entries for each column & no lat/lon info
multi_no_latlon <- sub_geolatlon[(final.state == "MULTIPLE"|
                                    final.county == "MULTIPLE"|
                                    final.muni == "MULTIPLE"|
                                    final.local == "MULTIPLE") 
                                 & (is.na(lat)|is.na(lon)), ]

# remove duplicates based on unique ID
multi_no_latlon_fix <- multi_no_latlon[!duplicated(multi_no_latlon$unique_id)]

# write output of MULTIPLE geolocation entries for manual editing
#   NOTES FOR MANUAL EDIT: 
#     Isabela is an existing city in Basilan (Isabela de Basilan) but does not appear in PH admin database
#     High probability of misspelling for "Batan" entries, may refer to either Batanes or Bataan
#     Problems with categories with entries "Luzon", "...provided...",  "Bgy", "...protect...", "compostela valley"
#     "Province of Tayabas" or "Tayabas province" == Quezon province
write.table(multi_no_latlon_fix, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/gbif_multiple_geofix.tsv",
            sep = "\t",
            row.names = FALSE)


#### 2) This section manually edits categories of MULTIPLE entries ----

# edit category of "Luzon" entries to NA in each column
#   NOTE: "Luzon" categorized as "barangay" while parsing but 
#   more appropriately referring to main island of the Philippines
luzon_state <- which(sub_geolatlon$stateProvince == "Luzon")
sub_geolatlon$final.state[luzon_state] <- NA

luzon_county <- which(sub_geolatlon$county == "Luzon")
sub_geolatlon$final.county[luzon_county] <- NA

luzon_muni <- which(sub_geolatlon$municipality == "Luzon")
sub_geolatlon$final.muni[luzon_muni] <- NA

luzon_local <- which(sub_geolatlon$locality == "Luzon")
sub_geolatlon$final.local[luzon_local] <- NA

# edit category of "Caraga" entries to NA in each column
#   NOTE: "Caraga" categorized as "municipality" while parsing but 
#   more appropriately referring to the alternative name of Region 13
caraga_state <- which(sub_geolatlon$stateProvince == "Caraga")
sub_geolatlon$final.state[caraga_state] <- NA

caraga_county <- which(sub_geolatlon$county == "Caraga")
sub_geolatlon$final.county[caraga_county] <- NA

caraga_muni <- which(sub_geolatlon$municipality == "Caraga")
sub_geolatlon$final.muni[caraga_muni] <- NA

caraga_local <- which(sub_geolatlon$locality == "Caraga")
sub_geolatlon$final.local[caraga_local] <- NA

# edit category of Compostela Valley (cv) entries to NA in each column
#   NOTE: Compostela Valley categorized as NA while parsing but 
#   more appropriately referring to province, "Davao de Oro"
cv_state <- grep("compostella valley|Compostela valley", 
                 sub_geolatlon$stateProvince, 
                 ignore.case = T)
sub_geolatlon$final.state[cv_state] <- "province"

cv_county <- grep("compostella valley|Compostela valley", 
                  sub_geolatlon$county, 
                  ignore.case = T)
sub_geolatlon$final.county[cv_county] <- "province"

cv_muni <- grep("compostella valley|Compostela valley", 
                sub_geolatlon$municipality, 
                ignore.case = T)
sub_geolatlon$final.muni[cv_muni] <- "province"

cv_local <- grep("compostella valley|Compostela valley", 
                 sub_geolatlon$locality, 
                 ignore.case = T)
sub_geolatlon$final.local[cv_local] <- "province"


# identify indices of locality entries that have patterns similar to "pro"/"prov"
prov_issue <- grep("provide|protect|prominent", 
                   sub_geolatlon$locality, 
                   ignore.case = T)

# identify indices of locality entries mistakenly categorized as "province" 
#   due to patterns with "pro"/"prov"
not_prov <- prov_issue[which(sub_geolatlon$final.local[prov_issue] == "province")]
View(sub_geolatlon[not_prov])

# fix mistaken province categories into NA for locality column
sub_geolatlon$final.local[not_prov] <- NA

# load manually edited MULTIPLE geolocation entries
gbif_multigeoparsed <- fread("gbif_multiplegeo_done.tsv", 
                             header = TRUE, 
                             na.strings = "NA")

# merge into subset GBIF data with "MULTIPLE" entries & no coordinates
merged_multigeo <- merge(multi_no_latlon, 
                         gbif_multigeoparsed, 
                         by = 'unique_id', 
                         all.x = TRUE, 
                         sort = FALSE)

# keep only columns from manually edited dataset and unique ID
fixed_multiple <- merged_multigeo[ , c(13:23, 1)]

# relabel columns in fixed_multiple
names(fixed_multiple) <- names(gbif_multigeoparsed)

# replace MULTIPLE entry rows (no coordinates) with manually edited data
sub_geolatlon[(final.state == "MULTIPLE"|
                 final.county == "MULTIPLE"|
                 final.muni == "MULTIPLE"|
                 final.local == "MULTIPLE") 
              & (is.na(lat)|is.na(lon)), ] <- fixed_multiple

# update main GBIF dataset with manually edited MULTIPLE entries
merged_geogbif[ ,243:253] <- sub_geolatlon[ , -12]

# add column in main GBIF dataset for unique ID
merged_geogbif$unique_id <- sub_geolatlon$unique_id

#output for updated main GBIF dataset
write.table(merged_geogbif, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/gbif03_fixed_multigeo_overall",
            sep = "\t",
            row.names = FALSE)


########## end of script
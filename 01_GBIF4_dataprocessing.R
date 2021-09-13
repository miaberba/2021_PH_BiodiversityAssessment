# NOTE: 
#     This script performs the final stage of geolocation parsing on the most 
#     updated version of the GBIF dataset (03) and generates the final updated 
#     version of the raw overall GBIF dataset. Here, the process involves creating
#     two matrices (for administrative categories and for administrative names)  
#     that will be used to organize the entries found in the geolocation metadata
#     (i.e., stateProvince, county, municipality, and locality) into new columns
#     based on what administrative unit they have been categoriezed as. Then, 
#     parsed entries categorized as province will undergo two rounds of cleaning
#     mainly by dividing entries based on their separators and pulling out
#     patterns referring to provinces. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

#### 1) This section generates matrices for entries on administrative category and names ---- 
#     NOTE: from here, matrix containing administrative category (i.e., province, municipality, 
#     barangay) will be referred to as the category matrix while the matrix containing  
#     administrative names (info provided on geolocation) will be referred to as the value matrix 

# load updated main GBIF dataset - "gbif03_fixed_multigeo_overall"
gbif_overallgeo <- fread("gbif03_fixed_multigeo_overall")

# subset GBIF dataset to contain only metedata on sampling location
subgeo_gbif <- gbif_overallgeo[ ,243:254]

# create unique row ID
subgeo_gbif$row_id <- 1:nrow(subgeo_gbif)

# subset GBIF geolocation dataset for additional checking of province issue
checking_prov_issue <- subgeo_gbif[(final.local == "province"), ]

# identify indices of locality entries that have "pro" pattern
prov_issue <- grep("pro", 
                   checking_prov_issue$locality, 
                   ignore.case = T)

# subset dataset to contain only indices with "pro" pattern
fixing_prov <- checking_prov_issue[prov_issue, ]

# identify indices of entries that refer to true provinces
true_prov <- grep("pro\\.|prov", 
                  fixing_prov$locality, 
                  ignore.case = T)

# fix mistaken province categories into NA for locality column
fixing_prov$final.local[-true_prov] <- NA

# update entries in subset of GBIF geolocation data for additional checking 
checking_prov_issue[prov_issue, ] <- fixing_prov

# unlist row ID of entries within subset of GBIF dataset 
fixed_prov_indices <- unlist(checking_prov_issue$row_id)

# update entries from GBIF geolocation dataset pulled out for checking
subgeo_gbif[fixed_prov_indices, ] <- checking_prov_issue

# check if categories of misleading locality entries are appropriate
subgeo_gbif[grep("probably", subgeo_gbif$locality, ignore.case = T), 
            c("row_id", "locality", "final.local")]

# pull out categories from GBIF geolocation dataset
catergory_matrix <- subgeo_gbif[ , c("row_id", "final.state", 
                                     "final.county", "final.muni", 
                                     "final.local")]
# pull out corresponding values of categories from GBIF geolocation dataset
value_matrix <- subgeo_gbif[ , c("row_id", "stateProvince", 
                                 "county", "municipality", 
                                 "locality")]

# evaluate each row in category matrix for MULTIPLE entries (with coordinates)
#   NOTE: if == to MULTIPLE (true >= 1, false = 0)
id_multi <- apply(catergory_matrix[ , ], 
                  1, function(x) sum(ifelse(is.na(x), "x", x) == "MULTIPLE"))

# redefine category matrix to remove MULTIPLE entries (id_multi >= 1)
catergory_matrix <- subgeo_gbif[id_multi < 1 , 
                                c("row_id", "final.state", 
                                  "final.county", "final.muni", 
                                  "final.local")]

# check if MULTIPLE entries have been removed
apply(catergory_matrix, 2, function(x) unique(x)) 

# redefine value matrix to remove MULTIPLE entries (id_multi >= 1)
value_matrix <- subgeo_gbif[id_multi < 1 , 
                            c("row_id", "stateProvince", 
                              "county", "municipality", 
                              "locality")]


#### 2) This section organizes sampling entries into province, municipality, and barangay columns ---- 

# define vector for categories of interests
category_type <- c("province", "municipality", "barangay")

# use both matrices to pull out appropriate sampling entries and place into appropriate columns
#   NOTE: matching entries in value matrix to the appropriate entries in category matrix
#   and organize in the row order of "province", "municipality", "barangay"
match_tab <- lapply(1:nrow(value_matrix), function(x, val, cat){
  
  rowCat <- unlist(cat[x, ])
  
  rowVal <- unlist(val[x, ])
  
  return(rowVal[match(category_type, rowCat)])
}, 
val = value_matrix, 
cat = catergory_matrix) 

# function call rbind on results of the matching
match_fin <- do.call(rbind, match_tab)

# convert to data table
#   NOTE: V1 = province, V2 = municipality, V3 = barangay
match_fin <- data.table(match_fin)

# merge matched results back to main GBIF geolocation data
#   NOTE: entries with MULTIPLE category have been removed in matrices
#   NOTE: V1 = province, V2 = municipality, V3 = barangay
subgeo_gbif$province_update <- NA
subgeo_gbif$province_update[which(id_multi < 1)] <- match_fin$V1

subgeo_gbif$munici_update <- NA
subgeo_gbif$munici_update[which(id_multi < 1)] <- match_fin$V2

subgeo_gbif$brgy_update <- NA
subgeo_gbif$brgy_update[which(id_multi < 1)] <- match_fin$V3


#### 3) This section performs the first attempt of cleaning parsed province data ---- 

# adjust particular patterns to be more standardized
subgeo_gbif$province_update <- gsub("pro\\.|prov\\.|prov:|province:|prov :", 
                                    "prov", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("mt\\.|mtn\\.", 
                                    "mt", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("or\\.", 
                                    "or", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("oc\\.|occ\\.", 
                                    "occ", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("n\\.", 
                                    "n", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("s\\.|so\\.", 
                                    "s", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("e\\.", 
                                    "e", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("w\\.", 
                                    "w", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub("sur\\.", 
                                    "sur", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)
subgeo_gbif$province_update <- gsub(", Province \\[interpreted\\]", 
                                    " Province \\[interpreted\\]", 
                                    subgeo_gbif$province_update, 
                                    ignore.case = T)

# define list of unique entries in province_update column 
prov_list <- unique(subgeo_gbif$province_update)

# define data table of unique provine_update entries with designated row ID
master_prov <- data.table(id = 1:length(prov_list), prov_name = prov_list)

# identify indices of unique entries in prov_name column with COMMA - ","
comma_sub <- grep(",", master_prov$prov_name) 

# separate entries based on COMMA patterns and pull out province info
attempt1 <- lapply(strsplit(master_prov$prov_name[comma_sub], ", |,"), 
                   function(x) x[grep("prov", tolower(x))[1]])

# merge attempt1 list into master_prov table under attempt1 column
master_prov$attempt1 <- NA
master_prov$attempt1[comma_sub] <- unlist(attempt1)

# check other prov_name entries that have not yet been cleaned
master_prov[is.na(attempt1), ]$prov_name

# identify indices of unique entries in prov_name with SEMICOLON - ";"
#   NOTE: only among entries that have not yet been cleaned
semi_col <- grep(";", master_prov[is.na(attempt1), ]$prov_name) # NA at attempt 1 = haven't edited

# separate entries based on SEMICOLON patterns and pull out province info
attempt1.2 <- lapply(strsplit(master_prov[is.na(attempt1),]$prov_name[semi_col], ";|; "),
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt1.2 list into master_prov table under attempt1 column
master_prov$attempt1[master_prov[is.na(attempt1), ]$id[semi_col]] <- attempt1.2

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov$attempt1)

# identify indices of unique entries in prov_name with PERIOD - "."
#   NOTE: only among entries that have not yet been cleaned
period_sub <- grep("\\.", master_prov[is.na(attempt1), ]$prov_name)

# separate entries based on PERIOD patterns and pull out province info
attempt1.3 <- lapply(strsplit(master_prov[is.na(attempt1), ]$prov_name[period_sub], "\\.|\\. | \\. "),
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt1.3 list into master_prov table under attempt1 column
master_prov$attempt1[master_prov[is.na(attempt1), ]$id[period_sub]] <- attempt1.3

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov$attempt1)

# identify indices of unique entries in prov_name with COLON - ":"
#   NOTE: only among entries that have not yet been cleaned
col_sub <- grep(":", master_prov[is.na(attempt1), ]$prov_name)

# separate entries based on PERIOD patterns and pull out province info
attempt1.4 <- lapply(strsplit(master_prov[is.na(attempt1), ]$prov_name[col_sub], ":|: "), 
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt1.4 list into master_prov table under attempt1 column
master_prov$attempt1[master_prov[is.na(attempt1), ]$id[col_sub]] <- attempt1.4

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov$attempt1)

# count total number of words separated by SPACES for each entry in prov_name
#   NOTE: only among entries that have not yet been cleaned
num_count <- lapply(strsplit(master_prov[is.na(attempt1), ]$prov_name, " "), 
                    function(x) length(x))

# unlist num_count list
num_count <- unlist(num_count)

# copy entries with 2 or less words from prov_name column into attempt1 column
#   NOTE: if num_count <= 2, safe to assume entry is actually the province
master_prov$attempt1[master_prov[is.na(attempt1), ]$id[num_count <= 2]] <- master_prov[is.na(attempt1), ]$prov_name[num_count <= 2] 

# identify indices of entries in master_prov that have not yet been cleaned
indices <- master_prov[is.na(attempt1), ]$id

# edit out specific patterns from province entries 
#   NOTE: only among entries that have not yet been cleaned
master_prov$attempt1[indices] <- gsub("^.*province of |prov of ", 
                                      "", 
                                      tolower(master_prov[is.na(attempt1), ]$prov_name))

# convert entries in attempt1 column into lowercase 
master_prov$attempt1 <- tolower(master_prov$attempt1)

# merge master_prov into GBIF geolocation data
merged_masterxsubgeo <- merge(subgeo_gbif, 
                              master_prov,
                              by.x = 'province_update',
                              by.y = 'prov_name',
                              sort = FALSE)

# relabel column in merged_masterxsubgeo 
#   NOTE: column was originally attempt1 from master_prov table
names(merged_masterxsubgeo)[18] <- "province_attempt1"

# rearrange order of columns in updated GBIF geolocation data
merged_masterxsubgeo <- merged_masterxsubgeo[ , c(14, 2:10, 13, 11:12, 1, 18, 15:16)]


#### 4) This section performs the second attempt in cleaning parsed province data ---- 

# subset updated GBIF geolocation data to contain entries with no coordinates 
subgeo_nolatlon <- merged_masterxsubgeo[(!is.na(province_attempt1)|
                                           !is.na(munici_update)|
                                           !is.na(brgy_update)) & 
                                          (is.na(decimalLatitude)|
                                             is.na(decimalLongitude)), ]  

# define list of unique entries in province_attempt1 column 
prov_list2 <- unique(subgeo_nolatlon$province_attempt1)

# define data table of unique provine_update entries with designated row ID
master_prov2 <- data.table(id = 1:length(prov_list2), prov_name = prov_list2)

# identify indices of unique entries in prov_name column with COMMA - ","
comma2 <- grep(",", master_prov2$prov_name)

# separate entries based on COMMA patterns and pull out province info
attempt2 <- lapply(strsplit(master_prov2$prov_name[comma2], ", |,"), 
                   function(x) x[grep("prov", tolower(x))[1]])

# merge attempt2 list into master_prov table under attempt2 column
master_prov2$attempt2 <- NA
master_prov2$attempt2[comma2] <- unlist(attempt2)

# check other prov_name entries that have not yet been cleaned
master_prov2[is.na(attempt2), ]$prov_name

# identify indices of unique entries in prov_name with SEMICOLON - ";"
#   NOTE: only among entries that have not yet been cleaned
semi_col2 <- grep(";", master_prov2[is.na(attempt2), ]$prov_name)

# separate entries based on SEMICOLON patterns and pull out province info
attempt2.2 <- lapply(strsplit(master_prov2[is.na(attempt2), ]$prov_name[semi_col2], ";|; | ; "), 
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt2.2 list into master_prov table under attempt2 column
master_prov2$attempt2[master_prov2[is.na(attempt2), ]$id[semi_col2]] <- attempt2.2

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov2$attempt2)

# identify indices of unique entries in prov_name with COLON - ":"
#   NOTE: only among entries that have not yet been cleaned
col_sub2 <- grep(":", master_prov2[is.na(attempt2), ]$prov_name)

# separate entries based on PERIOD patterns and pull out province info
attempt2.3 <- lapply(strsplit(master_prov2[is.na(attempt2), ]$prov_name[col_sub2], ":|: | : "), 
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt2.3 list into master_prov table under attempt2 column
master_prov2$attempt2[master_prov2[is.na(attempt2), ]$id[col_sub2]] <- attempt2.3

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov2$attempt2)

# identify indices of unique entries in prov_name with PERIOD - "."
#   NOTE: only among entries that have not yet been cleaned
period2 <- grep("\\.", master_prov2[is.na(attempt2), ]$prov_name)

# separate entries based on PERIOD patterns and pull out province info
attempt2.4 <- lapply(strsplit(master_prov2[is.na(attempt2), ]$prov_name[period2], "\\.|\\. | \\. "), 
                     function(x) x[grep("prov", tolower(x))[1]])

# merge attempt2.4 list into master_prov table under attempt2 column
master_prov2$attempt2[master_prov2[is.na(attempt2), ]$id[period2]] <- attempt2.4

# check other unique prov_name entries that have not yet been cleaned
unique(master_prov2$attempt2)

# count total number of words separated by SPACES for each entry in prov_name
#   NOTE: only among entries that have not yet been cleaned
num_count2 <- lapply(strsplit(master_prov2[is.na(attempt2), ]$prov_name, " "),
                     function(x) length(x))

# unlist num_count2 list
num_count2 <- unlist(num_count2)

# copy entries with 2 or less words from prov_name column into attempt1 column
#   NOTE: if num_count <= 2, safe to assume entry is actually the province
master_prov2$attempt2[master_prov2[is.na(attempt2), ]$id[num_count2 <= 2]] <- master_prov2[is.na(attempt2), ]$prov_name[num_count2 <= 2]

# identify indices of entries in master_prov that have not yet been cleaned
indices2 <- master_prov2[is.na(attempt2), ]$id

# edit out specific patterns from province entries 
#   NOTE: only among entries that have not yet been cleaned
master_prov2$attempt2[indices2] <- gsub("^.*province of |prov of ", 
                                        "", 
                                        tolower(master_prov2[is.na(attempt2), ]$prov_name))

# convert entries in attempt1 column into lowercase
master_prov2$attempt2 <- tolower(master_prov2$attempt2)

# edit out other unwanted patterns from province entries
master_prov2$attempt2 <- gsub("\\(|\\)|\\?|\\*|\\[|\\]|\\\\\\\\\\\"|\\{|\\}|!|&", 
                              "", master_prov2$attempt2)
master_prov2$attempt2 <- gsub("philippin|philippines|philippine:|luzon|mindanao|visayas| n | isd\\.| n e |off|luzon,|luzon;| is\\.| i\\.|island|island of|islands|islands of|the philippine islands |luzon is | visayas|visayas | isla de |luzón|ile de | is$", 
                              "", master_prov2$attempt2)

# identify indices of entries referring to the "Mountain Province"
#   NOTE: need to keep "province" in the name
mt_prov_entries <- grep("mt prov|mtn prov|mountain prov", master_prov2$attempt2)

# edit out variations of the pattern "province" from province entries
#   NOTE: except for indices belonging to "Mountain Province" entries
master_prov2$attempt2[-mt_prov_entries] <- gsub("province of |prov of | prov| province|prov |province |provincia| or province |province de", 
                                                "", master_prov2$attempt2[-mt_prov_entries])

# edit out SPACE at the beginning and end of entries
master_prov2$attempt2 <- gsub("^ | $", "", master_prov2$attempt2)

# convert blank entries ("") in attempt2 column to NA
master_prov2[attempt2 == ""]$attempt2 <- NA

# define list for checking length of entries in attempt2 column
length_check <- lapply(strsplit(master_prov2$attempt2, " "),
                       function(x) length(x))

# unlist length_check list
length_check <- unlist(length_check)

# view entries that are too long (length_check > 3)
master_prov2$attempt2[length_check > 3]

# check number of unique province entries that are too long 
#   or not part of attempt2 data cleaning
length(master_prov2$attempt2[length_check > 3])
length(is.na(master_prov2$attempt2))

# merge master_prov2 into subset of GBIF geolocation data (no coordinates)
merged_masterxsubgeo2 <- merge(subgeo_nolatlon,
                               master_prov2,
                               by.x = 'province_attempt1',
                               by.y = 'prov_name',
                               sort = FALSE)

# relabel column in merged_masterxsubgeo2
#   NOTE: column was originally attempt2 from master_prov2 table
names(merged_masterxsubgeo2)[19] <- "province_attempt2"

# drop province_attempt1 column [1]
merged_masterxsubgeo2 <- merged_masterxsubgeo2[ , c(2:15, 19, 16:17)]

# relabel province_attempt2 column in merged_masterxsubgeo2 as province_attempt1
#   NOTED: updated version for entries with no coordinates in overall GBIF data
names(merged_masterxsubgeo2)[15] <- "province_attempt1"

# define list for checking length of entries in province_attempt2 column
length_check <- lapply(strsplit(merged_masterxsubgeo2$province_attempt2, " "),
                       function(x) length(x))

# unlist length_check list
length_check <- unlist(length_check)

# view entries that are too long (length_check > 3)
merged_masterxsubgeo2$province_attempt1[length_check > 3]

# check number of unique province entries that are too long 
length(merged_masterxsubgeo2$province_attempt1[length_check > 3])

# check if column names are same between merged_masterxsubgeo (all GBIF entries)
#   and merged_masterxsubgeo2 (only GBIF entries with no coordinates)
#   NOTE: all must be TRUE
names(merged_masterxsubgeo2) == names(merged_masterxsubgeo) 

# check if row IDs of entries with no coordinates in merged_masterxsubgeo are 
#   equivalent with row IDs of all entries in merged_masterxsubgeo2
#   NOTE: all must be TRUE
merged_masterxsubgeo[(!is.na(province_attempt1)|
                        !is.na(munici_update)|
                        !is.na(brgy_update)) & 
                       (is.na(decimalLatitude)|
                          is.na(decimalLongitude)), ]$row_id == merged_masterxsubgeo2$row_id

# update data of entries with no coordinates in merged_masterxsubgeo
merged_masterxsubgeo[(!is.na(province_attempt1)|
                        !is.na(munici_update)|
                        !is.na(brgy_update)) & 
                       (is.na(decimalLatitude)
                        |is.na(decimalLongitude)), ]  <- merged_masterxsubgeo2

# relabel columns in merged_masterxsubgeo referring to PH admin info
names(merged_masterxsubgeo)[15] <- "province_final" 
names(merged_masterxsubgeo)[16] <- "municipality_final"
names(merged_masterxsubgeo)[17] <- "barangay_final"

# combine GBIF geolocation subset data into main GBIF dataset
final_geogbif_all <- cbind(gbif_overallgeo[ ,-c(243:254)], 
                           merged_masterxsubgeo[ , -c("province_update")])

# write output for final updated main GBIF dataset
write.table(final_geogbif_all, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/gbif04_finalgeo_overall", 
            sep = "\t",
            row.names = FALSE)


########## end of script
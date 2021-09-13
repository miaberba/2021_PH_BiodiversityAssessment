# NOTE: 
#     This script performs the first stage of geolocation parsing on the raw
#     overall GBIF dataset (01). The process involves parsing through the geolocation 
#     metadata and match explicit entries or patterns with known administrative 
#     names for province, municipality, and barangay. The GBIF metadata columns  
#     to be examined are the following: stateProvince, county, municipality, and
#     locality. Information in these columns were found to be mixed, meaning 
#     that entries referring to province, municipality, and/or barangay may be 
#     found in either columns.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# load functions created to help in GBIF geolocation parsing
source("~/Desktop/THESIS/Bio 200/Scripts/annotated/misc_functions.r")


#### 1) This section prepares PH admin database and GBIF geolocation dataset ----

# load and subset Philippine (PH) admin database derived from PSGC data 
#   NOTE: this file contains the original Philippine Standard Geographic Code 
#   (PSGC) data obtained from the Philippine Statistics Authority along with
#   additional columns generated while pulling out the province, municipality,
#   and barangay information for each entry
geo.table <- fread("locationInfo.table", header = TRUE, na.strings = "")

# subset to PH admin database to contain only final province, municipality, barangay info
#   NOTE: columns labelled as provFin (16), munFin (17), and brgFin (18)
geo.database <- geo.table[ ,16:18, drop = FALSE]

# relabel columns in geo.database
names(geo.database) <- c("province", "municipality", "barangay")

# convert province entries to lowercase and remove NA
prov_list <- unique(tolower(geo.database$province))
prov_list <- prov_list[!is.na(prov_list)]

# clean and convert municipality entries to lowercase and remove NA
#   NOTE: patterns such as "city of" or those enclosed in parenthesis need 
#   to be removed to increase chances of matching with GBIF geo entries
muni_list <- unique(tolower(geo.database$municipality))
muni_list <- muni_list[!is.na(muni_list)]
muni_list <- gsub("island garden city of |city of | city| \\(capital\\)| \\(espiritu\\)| \\(concepcion\\| \\(angkaki\\)| \\(baugen\\)| \\(lapog\\)| \\(faire\\)| \\(magsaysay\\)| \\(bigaa\\)| \\(papaya\\)| \\(bitulok & sabani\\)| \\(sexmoan\\)| \\(mendez-nunez\\)| \\(aurora\\)| \\(san pedro\\)| \\(montalban\\)| \\(bacuit\\)| \\(marcos\\)| \\(imelda\\)| \\(locsin\\)| \\(libog\\)| \\(parubcan\\)| \\(payo\\)| \\(calolbon\\)| \\(limbuhan\\)| \\(dao\\)| \\(saravia\\)| \\(asia\\)| \\(magallon\\)| \\(pitogo\\)| \\(borja\\)| \\(opon\\)| \\(ayuquitan\\)| \\(tulong\\)| \\(payabon\\)| \\(luzurriaga\\)| \\(bugho\\)| \\(wright\\)| \\(cabalian\\)| \\(new pinan\\)| \\(ponot\\)| \\(leon t. postigo\\)| \\(liargo\\)| \\(don mariano marcos\\)| \\(karomatan\\)| \\(linugos\\)| \\(saug\\)| \\(san vicente\\)| \\(dona alicia\\)| \\(san mariano\\)| \\(trinidad\\)| \\(dadiangas\\)| \\(mariano marcos\\)| \\(licuan\\)| \\(potia\\)| \\(liwan\\)| \\(bayag\\)| \\(bacolod grande\\)| \\(watu\\)| \\(maguing\\)| \\(tatarikan\\)| \\(gata\\)| \\(sultan gumander\\)| \\(bumbaran\\)| \\(dinaig\\)| \\(maganoy\\)| \\(nuling\\)| \\(lambayong\\)| \\(tumbao\\)| \\(marunggas\\)| \\(new panamao\\)| \\(balimbing\\)| \\(cagayan de tawi-tawi\\)| \\(anao-aon\\)| \\(sapao\\)| \\(rizal\\)| \\(albor\\)",
                  "", muni_list)

# clean and convert barangay entries to lowercase and remove NA
#   NOTE: patterns enclosed in parenthesis need to be removed to increase
#   chances of matching with GBIF geo entries
brgy_list <- unique(tolower(geo.database$barangay))
brgy_list <- brgy_list[!is.na(brgy_list)]
brgy_list <- gsub(" \\(pob.\\)| \\(new pob.\\)", "", brgy_list)

# load main GBIF dataset - gbif01_raw_overall
gbif_overall <- fread("gbif01_raw_overall",
                      sep = "\t",
                      header = TRUE)

# subset GBIF dataset to contain only metadata on sampling location
gbif_geolocation <- gbif_overall[ , c(121:125,133:134)]


#### 2) This section parses through geolocation entries in stateProvince column ----

# define unique entries 
state_col <- unique(gbif_geolocation$stateProvince)

# define data table to pull out PH admin info from entries
state.output <- data.table(state_entry = state_col,
                           category_pattern = rep(NA, length(state_col)),
                           match_province = rep(NA, length(state_col)), 
                           match_municipality = rep(NA, length(state_col)),
                           match_barangay = rep(NA, length(state_col)))

# separate and categorize explicit entries as province/municipality/barangay 
state.output$category_pattern <- catPattern(locIn = state_col)

# define list to match entries with PH admin database
query.list.sp <- tolower(state_col)

# identify indices with entries matching to PH admin database
prov.match <- which(query.list.sp %in% prov_list)
muni.match <- which(query.list.sp %in% muni_list)
brgy.match <- which(query.list.sp %in% brgy_list)

# categorize matched entries with appropriate administrative unit
state.output$match_province[prov.match] <- "province"
state.output$match_municipality[muni.match] <- "municipality"
state.output$match_barangay[brgy.match] <- "barangay"

# summarize final admin category of each entry, MULTIPLE if more than one match
state.output$final <- sumLocTable(state.output)


#### 3) This section parses through geolocation entries in county column ----

# define unique entries 
county_col <- unique(gbif_geolocation$county)

# define data table to pull out PH admin info from entries
county.output <- data.table(county_entry = county_col,
                            category_pattern = rep(NA, length(county_col)),
                            match_province = rep(NA, length(county_col)), 
                            match_municipality = rep(NA, length(county_col)),
                            match_barangay = rep(NA, length(county_col)))

# separate and categorize explicit entries as province/municipality/barangay
county.output$category_pattern <- catPattern(locIn = county_col)

# define list to match entries with PH admin database
query.list.c <- tolower(county_col)

# identify indices with entries matching to PH admin database
prov.match <- which(query.list.c %in% prov_list)
muni.match <- which(query.list.c %in% muni_list)
brgy.match <- which(query.list.c %in% brgy_list)

# categorize matched entries with appropriate administrative unit
county.output$match_province[prov.match] <- "province"
county.output$match_municipality[muni.match] <- "municipality"
county.output$match_barangay[brgy.match] <- "barangay"

# summarize final admin category of each entry, MULTIPLE if more than one match
county.output$final <- sumLocTable(county.output)

#### 4) This section parses through geolocation entries in municipality column ##### 

# define unique entries 
muni_col <- unique(gbif_geolocation$municipality)

# define data table to pull out PH admin info from entries
muni.output <- data.table(muni_entry = muni_col,
                          category_pattern = rep(NA, length(muni_col)),
                          match_province = rep(NA, length(muni_col)), 
                          match_municipality = rep(NA, length(muni_col)),
                          match_barangay = rep(NA, length(muni_col)))

# separate and categorize explicit entries as province/municipality/barangay
muni.output$category_pattern <- catPattern(locIn = muni_col)

# define list to match entries with PH admin database
query.list.m <- tolower(muni_col)

# identify indices with entries matching to PH admin database
prov.match <- which(query.list.m %in% prov_list)
muni.match <- which(query.list.m %in% muni_list)
brgy.match <- which(query.list.m %in% brgy_list)

# categorize matched entries with appropriate administrative unit
muni.output$match_province[prov.match] <- "province"
muni.output$match_municipality[muni.match] <- "municipality"
muni.output$match_barangay[brgy.match] <- "barangay"

# summarize final admin category of each entry, MULTIPLE if more than one match
muni.output$final <-  sumLocTable(muni.output)

#### 5) This section parses through geolocation entries in locality column ---- 

# define unique entries 
local_col <- unique(gbif_geolocation$locality)

# define data table to pull out PH admin info from entries
local.output <- data.table(local_entry = local_col,
                           category_pattern = rep(NA, length(local_col)),
                           match_province = rep(NA, length(local_col)), 
                           match_municipality = rep(NA, length(local_col)),
                           match_barangay = rep(NA, length(local_col)))

# separate and categorize explicit entries as province/municipality/barangay
local.output$category_pattern <- catPattern(locIn = local_col)

# define list to match entries with PH admin database
query.list.l <- tolower(local_col)

# identify indices with entries matching to PH admin database
prov.match <- which(query.list.l %in% prov_list)
muni.match <- which(query.list.l %in% muni_list)
brgy.match <- which(query.list.l %in% brgy_list)

# categorize matched entries with appropriate administrative unit
local.output$match_province[prov.match] <- "province"
local.output$match_municipality[muni.match] <- "municipality"
local.output$match_barangay[brgy.match] <- "barangay"

# summarize final admin category of each entry, MULTIPLE if more than one match
local.output$final <- sumLocTable(local.output)

#### 6) This section incorporates parsed data into main GBIF dataset ---- 

# merge parsed stateProvince entries into geolocation GBIF dataframe (subset)
# NOTE: merged_geogbif1 contains GBIF sampling data and
#   added columns derived from state.output
gbif_geolocation$final.state <- state.output$final[match(gbif_geolocation$stateProvince, state.output$state_entry)]

# merge parsed county entries into geolocation GBIF dataframe (subset)
#   NOTE: merged_geogbif2 contains GBIF sampling data and
#   added columns derived from state.output & county.output
gbif_geolocation$final.county <- county.output$final[match(gbif_geolocation$county, county.output$county_entry)]

# merge parsed municipality entries into geolocation GBIF dataframe (subset)
#   NOTE: merged_geogbif2 contains GBIF sampling data  and
#   added columns derived from state.output, county.output, & muni.output
gbif_geolocation$final.muni <- muni.output$final[match(gbif_geolocation$municipality, muni.output$muni_entry)]

# merge parsed locality entries into geolocation GBIF dataframe (subset)
#   NOTE: merged_geogbif2 contains GBIF sampling data and
#   added columns derived from state.output, county.output, muni.output, & local.output
gbif_geolocation$final.local <- local.output$final[match(gbif_geolocation$locality, local.output$local_entry)]

# re-arrange the table to match downstream script
gbif_geolocation <- gbif_geolocation[ ,c(1, 2, 8, 3, 9, 4, 10, 5, 11, 6, 7)]

# define updated GBIF dataset by combining GBIF sampling data into main dataset
final_merged_geogbif <- cbind(gbif_overall[ , -c(121:125,133:134)], 
                              gbif_geolocation)

# write output for updated main GBIF dataset
write.table(final_merged_geogbif, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/gbif02_geoclassified_overall",
            sep = "\t",
            row.names = FALSE)


########## end of script
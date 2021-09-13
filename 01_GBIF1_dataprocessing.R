# NOTE: 
#     This script is the first step in processing the species occurrence data
#     that will be used in the review. In this case, the raw data was only 
#     obtained from the Global Biodiversity Information Facility (GBIF) database.
#     The raw GBIF data for animal and plant taxa found in the Philippines (PH) 
#     were downloaded separately and in this script, they are combined into one 
#     overall raw GBIF dataset that will undergo further processing in later 
#     scripts. Additionally, a subset of the GBIF animal and plant data will be 
#     generated based on unique species and will be used in the processing
#     of GenBank data.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")


#### 1) This section loads and combines raw GBIF animal and plant datasets ----

# separately load raw animal and plant GBIF data (TXT file)
PH_gbif_plants  <- fread("./RawGBIF_101820/GBIF_plant_occurrence.txt",
                         quote = "",
                         na.strings = c("", NA))

PH_gbif_animals <- fread("./RawGBIF_101820/GBIF_animal_occurrence.txt",
                         quote = "",
                         na.strings = c("", NA))

# combine animal and plant GBIF datasets into one dataframe
GBIF_overall <- rbind(PH_gbif_animals, PH_gbif_plants)

# write output for main GBIF dataset - combined animal and plant taxa
write.table(GBIF_overall, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/gbif01_raw_overall", 
            sep = "\t", 
            row.names = FALSE)


#### 2) This section prepares the GBIF taxonomic subset for GenBank processing ---- 

# define subset of GBIF dataset based on unique animal species     
subUnique_animals <- PH_gbif_animals[!duplicated(species), ]

# write output for subset of GBIF dataset with unique species of animal taxa
write.table(subUnique_animals, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/SubUnique_animals", 
            sep = "\t", 
            row.names = FALSE)

# define subset of GBIF dataset based on unique plant species
subUnique_plants <- PH_gbif_plants[!duplicated(species), ]

# write output for subset of GBIF dataset with unique species of plant taxa
write.table(subUnique_plants, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/SubUnique_plants", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
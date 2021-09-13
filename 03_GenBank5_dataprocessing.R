# NOTE: 
#     This script performs the third set of metadata parsing on the most updated  
#     raw GenBank dataset (03), which focuses on the taxonomic metadata. After 
#     the initial processing of GenBank data, the taxonomic entries pulled out
#     consist of a string of taxonomic names of different ranks. To parse and 
#     sort through each entry, a taxonomic database derived from the unique 
#     species represented in GBIF data was used to pull out the taxonomic names
#     that correspond specifically to each of the following taxonomic ranks:
#     phylum, class, order, family, and genus. 


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")


#### 1) This section prepares the taxonomic database derived from GBIF data---- 

# load GBIF dataset with unique animal species
animaltaxo <- fread("SubUnique_animals")

# subset animal GBIF dataset to contain only columns for taxonomic metadata
#   NOTE: metadata on phylum, class, order, family, genus
taxaInfo_animals <- animaltaxo[ , 192:196, drop = FALSE]

# define list of unique taxonomic names for each taxonomic rank
nameInfo_animals <- lapply(taxaInfo_animals, 
                           function(x){temp <- unique(x)})

# define list of taxonomic ranks derived from columns in taxaInfo_animals
#   NOTE: rank values (phylum, class, order, family, genus) are repeated based on
#   length of values (or taxonomic names) in nameInfo_animals they correspond to 
rankInfo_animals <- rep(names(taxaInfo_animals), 
                        unlist(lapply(nameInfo_animals, function(x) length(x))))

# define data table of unique animal taxonomic names and corresponding ranks
#   NOTE: serve as animal taxonomic database derived from GBIF entries
rankDB_animals <- data.table(rank = rankInfo_animals, 
                             name = unlist(nameInfo_animals))

# remove NA entries from animal taxonomic database
rankDB_animals <- rankDB_animals[!is.na(name), ]

# load GBIF dataset with unique plant species
planttaxo <- fread("SubUnique_plants")

# subset plant GBIF dataset to contain only columns for taxonomic metadata
#   NOTE: metadata on phylum, class, order, family, genus
taxaInfo_plants <- planttaxo[ , 192:196, drop = FALSE]

# define list of unique taxonomic names for each taxonomic rank
nameInfo_plants <- lapply(taxaInfo_plants, 
                          function(x){temp <- unique(x)})

# define list of taxonomic ranks derived from columns in taxaInfo_plants
#   NOTE: rank values (phylum, class, order, family, genus) are repeated based on
#   length of values (or taxonomic names) in nameInfo_plants they correspond to 
rankInfo_plants <- rep(names(taxaInfo_plants), 
                       unlist(lapply(nameInfo_plants, function(x) length(x))))

# define data table of unique plant taxonomic names and corresponding ranks
#   NOTE: serve as plant taxonomic database derived from GBIF entries
rankDB_plants <- data.table(rank = rankInfo_plants,
                            name = unlist(nameInfo_plants))

# remove NA entries from animal taxonomic database
rankDB_plants <- rankDB_plants[!is.na(name), ]

# combine animal and plant taxonomic database
rankDB_all <- rbind(rankDB_animals, rankDB_plants)


#### 2) This section examines and sorts taxonomic metadata of GenBank data ----

# load updated main GenBank dataset - genbank03_metaxref_overall
genbank_metaref_all <- fread(file = "genbank03_metaxref_overall", 
                             sep = "\t",
                             header = TRUE)

# split TaxaInfo entries based on SEMICOLON and transpose into a data table
alltaxaGB <- genbank_metaref_all[ , data.table::tstrsplit(TaxaInfo, 
                                                          split = ";", 
                                                          fixed = TRUE)]

# match taxonomic names in alltaxaGB with names in rankDB_all 
#   NOTE: generates a matrix with five columns and rows that are as many as
#   the overall number of GenBank entries obtained
taxaUse <- t(apply(alltaxaGB, 1, function(x, a){
  
  tempOut <- a$rank[match(x, a$name)]
  
  orderUse <- match(c("phylum", "class", "order", "family", "genus"), tempOut)
  
  x[orderUse]
  
}, a = rankDB_all))

# convert to data table
taxaUse <- data.table(taxaUse)

# relabel columns in taxaUse to the appropriate taxonomic ranks
names(taxaUse) <- c("phylum", "class", "order", "family", "genus")

# define updated GenBank dataset by combining parsed GenBank taxo info into main dataset
genbank_all_taxo <- cbind(genbank_metaref_all, taxaUse)

# write output for updated main GenBank dataset
write.table(genbank_all_taxo, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/genbank04_taxofixed_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
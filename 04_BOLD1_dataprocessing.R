# NOTE: 
#     This script is the initial step in processing the raw files obtained from
#     the Barcode of Life Data System (BOLD) - 1 of 2 databases that comprise 
#     the overall barcode data that will be examined in the review. The raw BOLD
#     data for each animal and plant phylum found in the Philippines (PH) were
#     downloaded separately and in this script, they are combined into one 
#     overall raw GBIF dataset that will undergo further processing in later 
#     scripts.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")

# separately load raw BOLD data of each animal phylum (TXT file)
PH_bold_tardigrada <- fread("./RawBOLD_110320/BOLD_Tardigrada.txt", 
                            na.strings = c("", NA))

PH_bold_rotifera <- fread("./RawBOLD_110320/BOLD_Rotifera.txt", 
                          na.strings = c("", NA))

PH_bold_porifera <- fread("./RawBOLD_110320/BOLD_Porifera.txt", 
                          na.strings = c("", NA))

PH_bold_platyhelminthes <- fread("./RawBOLD_110320/BOLD_Platyhelminthes.txt", 
                                 na.strings = c("", NA))

PH_bold_nemertea <- fread("./RawBOLD_110320/BOLD_Nemertea.txt", 
                          na.strings = c("", NA))

PH_bold_nematoda <- fread("./RawBOLD_110320/BOLD_Nematoda.txt", 
                          na.strings = c("", NA))

PH_bold_mollusca <- fread("./RawBOLD_110320/BOLD_Mollusca.txt", 
                          na.strings = c("", NA))

PH_bold_echinodermata <- fread("./RawBOLD_110320/BOLD_Echinodermata.txt", 
                               na.strings = c("", NA))

PH_bold_ctenophora <- fread("./RawBOLD_110320/BOLD_Ctenophora.txt", 
                            na.strings = c("", NA))

PH_bold_chordata <- fread("./RawBOLD_110320/BOLD_Chordata.txt", 
                          na.strings = c("", NA))

PH_bold_arthropoda <- fread("./RawBOLD_110320/BOLD_Arthropoda.txt", 
                            na.strings = c("", NA))

PH_bold_annelida <- fread("./RawBOLD_110320/BOLD_Annelida.txt", 
                          na.strings = c("", NA))

# separately load raw BOLD data of each plant phylum (TXT file)
PH_bold_rhodophyta <- fread("./RawBOLD_110320/BOLD_Rhodophyta.txt", 
                            na.strings = c("", NA))

PH_bold_pteridophyta <- fread("./RawBOLD_110320/BOLD_Pteridophyta.txt", 
                              na.strings = c("", NA))

PH_bold_pinophyta <- fread("./RawBOLD_110320/BOLD_Pinophyta.txt", 
                           na.strings = c("", NA))

PH_bold_magnoliophyta <- fread("./RawBOLD_110320/BOLD_Magnoliophyta.txt",
                               na.strings = c("", NA))

PH_bold_chlorophyta <- fread("./RawBOLD_110320/BOLD_Chlorophyta.txt", 
                             na.strings = c("", NA))

PH_bold_bryophyta <- fread("./RawBOLD_110320/BOLD_Bryophyta.txt", 
                           na.strings = c("", NA))

# combine animal and plant BOLD datasets into one dataframe
BOLD_overall <- rbind(PH_bold_annelida, PH_bold_arthropoda, PH_bold_chordata,
                      PH_bold_ctenophora, PH_bold_echinodermata, PH_bold_mollusca,
                      PH_bold_nematoda, PH_bold_nemertea, PH_bold_platyhelminthes,
                      PH_bold_porifera, PH_bold_rotifera, PH_bold_tardigrada,
                      PH_bold_bryophyta, PH_bold_chlorophyta, PH_bold_magnoliophyta,
                      PH_bold_pinophyta, PH_bold_pteridophyta, PH_bold_rhodophyta)

# write output for main BOLD dataset - combined animal and plant taxa
write.table(BOLD_overall, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/bold01_raw_overall", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
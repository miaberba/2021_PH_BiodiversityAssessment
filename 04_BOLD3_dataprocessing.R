# NOTE: 
#     This script performs the second and last set of metadata parsing on the 
#     most updated raw BOLD dataset (02), which focuses on the publishing 
#     country metadata. The process involves identifying the unique entries 
#     for publishing institution and manually assigning the country from which
#     the institution is based via Google Sheets. Because the raw dataset directly
#     downloaded from the BOLD website had no metadata explicitly for publishing
#     institution, only metadata available that may be appropriate are
#     institution_storing, copyright_institutions, and sequencing_centers. Thus,
#     countries have been assigned to the unique entries in each metadata before
#     updating the BOLD dataset.


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")


#### 1) This section prepares the outputs for publishing country information ----

# load most updated main BOLD dataset - bold02_geoparsed_overall
bold_insti_update <- fread("bold02_geoparsed_overall",
                           sep = "\t",
                           header = TRUE)

# define one-column data table of unique entries for institution_storing
storing_insti <- data.table(unique(bold_insti_update$institution_storing))

# relabel column in storing_insti to original name
names(storing_insti) <- "institution_storing"

# write output for unique entries of BOLD institution_storing for manual editing
write.table(storing_insti, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold_instistoring.csv",
            sep = ",",
            row.names = FALSE)

# define one-column data table of unique entries for copyright_institutions
copyright_insti <- data.table(unique(bold_insti_update$copyright_institutions))

# relabel column in copyright_insti to original name
names(copyright_insti) <- "copyright_institutions"

# write output for unique entries of BOLD copyright_institutions for manual editing
write.table(copyright_insti, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold_copyright.csv",
            sep = ",",
            row.names = FALSE)

# define one-column data table of unique entries for sequencing_centers
sequencing_insti <- data.table(unique(bold_insti_update$sequencing_centers))

# relabel column in sequencing_insti to original name
names(sequencing_insti) <- "sequencing_centers"

# write output for unique entries of BOLD sequencing_centers for manual editing
write.table(sequencing_insti, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold_sequencing.csv",
            sep = ",",
            row.names = FALSE)


#### 2) This section updates the main BOLD dataset with publishing country metadata ----

# load updated list of unique entries of BOLD institutions and corresponding countries
#   NOTE: countries manually assigned for each entry in 
#   institution_storing, copyright_institutions, and sequencing_centers
storing_update <- fread("bold_instistoring_country.tsv")
sequencing_update <- fread("bold_sequencing_country.tsv")
copyright_update <- fread("bold_copyright_country.tsv")

# merge country information of institution_storing metadata to main BOLD dataset
merge_bold_allxstoring <- merge(bold_insti_update, 
                                storing_update, 
                                by = 'institution_storing',
                                sort = FALSE)

# merge country information of sequencing_centers metadata to main BOLD dataset
#   NOTE: merge_bold_allxstoring is main BOLD dataset with 
#   updated info for institution_storing
merge_bold_allxsequence <- merge(merge_bold_allxstoring, 
                                 sequencing_update, 
                                 by = 'sequencing_centers',
                                 sort = FALSE)

# merge country information of copyright_institutions metadata to main BOLD dataset
#   NOTE: merge_bold_allxsequence is main BOLD dataset with 
#   updated info for institution_storing and sequencing_centers
merge_bold_allxcopyright <- merge(merge_bold_allxsequence, 
                                  copyright_update, 
                                  by = 'copyright_institutions',
                                  sort = FALSE)

# rearrange column order
sorted_boldinsti_update <- merge_bold_allxcopyright[ , c(4:89, 3, 90, 2, 91, 1, 92)]

# write output for final raw and main BOLD dataset
write.table(sorted_boldinsti_update, 
            file = "/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/bold03_instiupdate_overall_FINAL", 
            sep = "\t", 
            row.names = FALSE)


########## end of script
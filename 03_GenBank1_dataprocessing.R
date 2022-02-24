# NOTE: 
#     This script is the initial step in processing the raw files obtained from
#     the GenBank database - 1 of 2 databases that comprise the overall barcode
#     data that will be examined in the study. The raw GenBank files (.gb) need
#     to be read through and manually parsed to generate readable individual files  
#     for each barcode record available. This process is repeated for each file 
#     associated with a specific gene marker (i.e., COI, CYTB, ITS2, rbcL, or 
#     matK) and organism category (i.e., animal or plant).


#############################################
####          script starts here         ####
#############################################

# set working directory
setwd("~/Desktop/THESIS/Bio 200/Working Files")

# load data.table package
library("data.table")


#### 1) This section generates individual GenBank files on records derived from animal taxa and COI gene ----

# read text lines of animal COI GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_COIa   <- readLines("./RawGenBank_110120_110320/GenBank_COI_animals.gb")

# define end of text line for each individual COI GenBank file
endLine_COIa   <- grep(x = genbank_COIa, pattern = "^//$")

# define start of text line for each individual COI GenBank file
startLine_COIa <- c(1, (endLine_COIa[-length(endLine_COIa)] + 2))

# identify and write each individual animal COI GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_COIa)){
  
  sub <- genbank_COIa[startLine_COIa[i]:endLine_COIa[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub, 
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/COI-", 
                     fileName, 
                     sep = ""))
}


#### 2) This section generates individual GenBank files on records derived from animal taxa and CYTB gene ----

# read text lines of animal CYTB GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_CYTBa   <- readLines("./RawGenBank_110120_110320/GenBank_CYTB_animals.gb")

# define end of text line for each individual CYTB GenBank file
endLine_CYTBa   <- grep(x = genbank_CYTBa, pattern = "^//$")

# define start of text line for each individual CYTB GenBank file
startLine_CYTBa <- c(1, (endLine_CYTBa[-length(endLine_CYTBa)] + 2))

# identify and write each individual animal CYTB GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_CYTBa)){
  
  sub <- genbank_CYTBa[startLine_CYTBa[i]:endLine_CYTBa[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub, 
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/CYTB-",
                     fileName, 
                     sep = ""))
}


#### 3) This section generates individual GenBank files on records derived from animal taxa and ITS2 gene ----

# read text lines of animal ITS2 GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_ITS2a   <- readLines("./RawGenBank_110120_110320/GenBank_ITS2_animals.gb")

# define end of text line for each individual ITS2 GenBank file
endLine_ITS2a   <- grep(x = genbank_ITS2a, pattern = "^//$")

# define start of text line for each individual ITS2 GenBank file
startLine_ITS2a <- c(1, (endLine_ITS2a[-length(endLine_ITS2a)] + 2))

# identify and write each individual animal ITS2 GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_ITS2a)){
  
  sub <- genbank_ITS2a[startLine_ITS2a[i]:endLine_ITS2a[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub,
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/ITS2-", 
                     fileName,
                     sep = ""))
}


#### 4) This section generates individual GenBank files on records derived from plant taxa and COI gene ----

# read text lines of plant COI GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_COIp   <- readLines("./RawGenBank_110120_110320/GenBank_COI_plants.gb")

# define end of text line for each individual COI GenBank file
endLine_COIp   <- grep(x = genbank_COIp, pattern = "^//$")

# define start of text line for each individual COI GenBank file
startLine_COIp <- c(1, (endLine_COIp[-length(endLine_COIp)] + 2))

# identify and write each individual plant COI GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_COIp)){
  
  sub <- genbank_COIp[startLine_COIp[i]:endLine_COIp[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub,
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/COI-",
                     fileName,
                     sep = ""))
}


#### 5) This section generates individual GenBank files on records derived from plant taxa and CYTB gene ----

# read text lines of plant CYTB GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_CYTBp   <- readLines("./RawGenBank_110120_110320/GenBank_CYTB_plants.gb")

# define end of text line for each individual CYTB GenBank file
endLine_CYTBp   <- grep(x = genbank_CYTBp, pattern = "^//$")

# define start of text line for each individual CYTB GenBank file
startLine_CYTBp <- c(1, (endLine_CYTBp[-length(endLine_CYTBp)] + 2))

# identify and write each individual plant CYTB GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_CYTBp)){
  
  sub <- genbank_CYTBp[startLine_CYTBp[i]:endLine_CYTBp[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub,
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/CYTB-",
                     fileName,
                     sep = ""))
}


#### 6) This section generates individual GenBank files on records derived from plant taxa and ITS2 gene ----

# read text lines of plant ITS2 GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_ITS2p   <- readLines("./RawGenBank_110120_110320/GenBank_ITS2_plants.gb")

# define end of text line for each individual ITS2 GenBank file
endLine_ITS2p   <- grep(x = genbank_ITS2p, pattern = "^//$")

# define start of text line for each individual ITS2 GenBank file
startLine_ITS2p <- c(1, (endLine_ITS2p[-length(endLine_ITS2p)] + 2))

# identify and write each individual plant ITS2 GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_ITS2p)){
  
  sub <- genbank_ITS2p[startLine_ITS2p[i]:endLine_ITS2p[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub, 
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/ITS2-",
                     fileName,
                     sep = ""))
}


#### 7) This section generates individual GenBank files on records derived from plant taxa and rbcL gene ----

# read text lines of plant rbcl GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_rbcl   <- readLines("./RawGenBank_110120_110320/GenBank_rbcl_plants.gb")

# define end of text line for each individual rbcl GenBank file
endLine_rbcl   <- grep(x = genbank_rbcl, pattern = "^//$")

# define start of text line for each individual rbcl GenBank file
startLine_rbcl <- c(1, (endLine_rbcl[-length(endLine_rbcl)] + 2))

# identify and write each individual plant rbcl GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_rbcl)){
  
  sub <- genbank_rbcl[startLine_rbcl[i]:endLine_rbcl[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub,
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/rbcL-",
                     fileName,
                     sep = ""))
}


#### 8) This section generates individual GenBank files on records derived from plant taxa and matK gene ----

# read text lines of plant matK GenBank file
#   NOTE: .GB file contains all individual barcode files obtained from GenBank
genbank_matk   <- readLines("./RawGenBank_110120_110320/GenBank_matK_plants.gb")

# define end of text line for each individual matK GenBank file
endLine_matk   <- grep(x = genbank_matk, pattern = "^//$")

# define start of text line for each individual matK GenBank file
startLine_matk <- c(1, (endLine_matk[-length(endLine_matk)] + 2))

# identify and write each individual plant rbcl GenBank file
#   NOTE: labeled based on ACCESSION number
for(i in 1:length(startLine_matk)){
  
  sub <- genbank_matk[startLine_matk[i]:endLine_matk[i]]
  
  fileName <- gsub(x = sub[grep(x = sub, pattern = "ACCESSION")],
                   pattern = "ACCESSION   ",
                   replacement = "")
  
  write(x = sub,
        file = paste("/Users/miaberba/Desktop/THESIS/Bio 200/Working Files/COMBINED GenBank Individual/matK-",
                     fileName,
                     sep = ""))
}


########## end of script
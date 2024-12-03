## ---------------------------
##
## Script name: 
##
## Purpose of script:
##
## Author: Mikko Vihtakari // Institute of Marine Research, Norway
## Email: mikko.vihtakari@hi.no
##
## Date Created: 2024-12-03
##
## ---------------------------

# Header ####

### Source the run first script ####

source("R/0 run first.R")

### Load packages ####

### Source or list custom functions used within the script ####

### Read data ###

## ---------------------------

cn <- 
  list("demersal" = 
         read_table("data/from scaa model/CatchAtAge.txt") %>% 
         column_to_rownames("Year") %>% 
         as.matrix(),
       "pelagic" = 
         read_table("data/from scaa model/PelagicCatchAtAge.txt") %>% 
         column_to_rownames("Year") %>% 
         as.matrix()
  )




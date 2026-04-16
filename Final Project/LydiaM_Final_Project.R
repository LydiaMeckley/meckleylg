LM_wd <- function (repo, folder=NULL) {
  if(missing(folder))
  {
    setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
      if (Sys.info()[["sysname"]]=="Darwin"){("/Users/lydiameckley/GitHub")} else ("for Linux run: setwd('/home/[LydiaMeckley]/GitHub')")
    },"/",repo, sep = "")))}
  else 
    
  {
    setwd(print(paste(if (Sys.info()[["sysname"]]=="Windows") {("C:/GitHub")} else {
      if (Sys.info()[["sysname"]]=="Darwin"){("/Users/lydiameckley/GitHub")} else ("for Linux run: setwd('/home/[LydiaMeckley]/GitHub')")
    },"/",repo, "/", folder, sep = "")))}
}

LM_wd("meckleylg")


###Final project due: Thursday April 30th at 8:00am###



###PACKAGES###
library(rgbif)
library(dplyr)
library(readxl)
library(vegan)
library(fitdistrplus)
library(logspline)

###DATASETS###
  #COASTAL WATER DATA#
LM_wd(repo="meckleylg", folder="Final Project")
waterdata <- read.csv("UK_Water_Data.csv")

  #SEAL DATA#
# 1. Species Keys
h_key <- name_backbone("Phoca vitulina")$usageKey
g_key <- name_backbone("Halichoerus grypus")$usageKey
inat_dataset <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"

# 2. Define the years we want
years <- 1990:2018

# 3. Create a helper function to fetch one year at a time
get_yearly_data <- function(yr, species_key, label) {
  # We add a small pause (0.5s) to avoid hitting rate limits
  Sys.sleep(0.5)
  
  # Fetch count
  cnt <- occ_count(
    taxonKey = species_key,
    country = "GB",
    datasetKey = inat_dataset,
    year = yr
  )
  
  return(data.frame(year = yr, count = cnt, species = label))
}

# 4. Run the loop (this might take a minute, but it's safer)
harbor_list <- lapply(years, get_yearly_data, species_key = h_key, label = "Harbor Seal")
grey_list   <- lapply(years, get_yearly_data, species_key = g_key, label = "Grey Seal")

# 5. Combine results
sealdata <- bind_rows(harbor_list, grey_list)


                    #####CLEANING UP THE DATA#####


###REMOVE COLUMNS I DO NOT NEED FROM THE COASTAL WATER DATASET###
waterdata <- waterdata[,-1]
waterdata <- waterdata[,-2:-6]
waterdata <- waterdata[,-3:-7]
waterdata <- waterdata[,-6]

###SUBSET THE COASTAL WATER DATA TO ONLY HAVE DATA FROM THE COUNTRY "ENGLAND"###
waterdata <- subset(waterdata, Country == "England")

###MEAN THE COASTAL WATER DATA BY YEAR###
water.means <- aggregate(x = waterdata, by = list(abiotic$names), FUN = "mean")

###MERGE BOTH OF THE DATASETS (COSTAL WATER DATA AND SEAL POPULATION DATA)###
merge(x, y, by.x = "Year", by.y = "year")



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

# 1.
  #What coastal seawater factors of England are capable of predicting both the harbor seal and grey seal populations over the course of 
  #three decades (1990-2020)?

# 2.
  #

###In the seal one, maybe start at 2009 because both sits 'start' at 2009 
  ###But also maybe only use the chich place because it has all the dates
      #all places in South East England
        #is there a way to sort out the rest of the places and only keep South East

#What changing factors in the ocean affect the two seal populations differently.

###DUE THURSDAY###

###add a region column

#water$side_region <- water$side
#water$side_region <- gsub(water, "South East", "SE subregion")
#and then region 2



###DONT COMPARE YEAR, 

###CH - 1999-

#use i naturalist to get datasets into R

#RGBIF

install.packages("rgbif")
library(rgbif)
library(dplyr)

      ################THIS WORKS NOW##################

# 1. Species Keys
h_key <- name_backbone("Phoca vitulina")$usageKey
g_key <- name_backbone("Halichoerus grypus")$usageKey
inat_dataset <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"

# 2. Define the years we want
years <- 1990:2020

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
seal_data <- bind_rows(harbor_list, grey_list)

############## JUST INCASE I NEED THIS AS A BACKUP ##################

install.packages("galah")
library(galah)

# 1. Config
galah_config(atlas = "United Kingdom")

# 2. Get counts for Harbor Seals
harbor_counts <- galah_call() %>%
  galah_filter(
    scientificName == "Phoca vitulina",
    year >= 1990, 
    year <= 2020
  ) %>%
  galah_group_by(year) %>%
  atlas_counts() %>%
  mutate(species = "Harbor Seal")

# 3. Get counts for Grey Seals
grey_counts <- galah_call() %>%
  galah_filter(
    scientificName == "Halichoerus grypus",
    year >= 1990, 
    year <= 2020
  ) %>%
  galah_group_by(year) %>%
  atlas_counts() %>%
  mutate(species = "Grey Seal")

# 4. Combine
all_seals <- bind_rows(harbor_counts, grey_counts)
print(all_seals)


#just england bc we have england and all of its regions and such and other countries...

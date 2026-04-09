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

water$side_region <- water$side
water$side_region <- gsub(water, "South East", "SE subregion")
#and then region 2



###DONT COMPARE YEAR, 

###CH - 1999-

#use i naturalist to get datasets into R

#RGBIF

install.packages("rgbif")
library(rgbif)
library(dplyr)

?rgbif

# Search for the usage keys
harbor_seal_key <- name_backbone(name='Phoca vitulina')$usageKey
grey_seal_key <- name_backbone(name='Halichoerus grypus')$usageKey

# Fetch Harbor Seals from iNaturalist in the UK
harbor_data <- occ_search(
  taxonKey = harbor_seal_key,
  country = "GB",
  datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7",
  limit = 500
)

# Fetch Grey Seals from iNaturalist in the UK
grey_data <- occ_search(
  taxonKey = grey_seal_key,
  country = "GB",
  datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7",
  limit = 500
)

# Extract data frames
df_harbor <- harbor_data$data
df_grey <- grey_data$data

# Combine them
all_seals_gbif <- bind_rows(df_harbor, df_grey)

# Simple count check
table(all_seals_gbif$scientificName)

###OR TRY THIS###

harbor_seal_key <- name_backbone("Phoca vitulina")$usageKey # 2433482
grey_seal_key <- name_backbone("Halichoerus grypus")$usageKey  # 2433451

get_seal_counts <- function(species_key, species_name) {
  occ_count(
    taxonKey = species_key,
    country = "GB",
    datasetKey = "50c9509d-22c7-4a22-a47d-8c48425ef4a7", # iNaturalist Dataset ID
    year = "1990,2020",
    facet = "year"
  ) %>%
    mutate(species = species_name)
}

harbor_counts <- get_seal_counts(harbor_seal_key, "Harbor Seal")
grey_counts <- get_seal_counts(grey_seal_key, "Grey Seal")

annual_data <- bind_rows(harbor_counts, grey_counts) %>%
  rename(year = term, observations = count) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year >= 1990 & year <= 2020)


### OR THIS ###

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


###OR

# 1. Setup keys
h_key <- 2433482
g_key <- 2433451
inat_dataset <- "50c9509d-22c7-4a22-a47d-8c48425ef4a7"
years <- 2010:2020 # Start with a smaller range to test

# 2. Simplified fetching function
fetch_meta_count <- function(yr, key, label) {
  message(paste("Fetching", label, "for year", yr))
  
  # occ_search with limit=0 just returns the 'meta' count
  res <- occ_search(
    taxonKey = key,
    country = "GB",
    datasetKey = inat_dataset,
    year = yr,
    limit = 0
  )
  
  return(data.frame(year = yr, count = res$meta$count, species = label))
}

# 3. Safe execution
# Let's try just one year first to see if it works
test_run <- fetch_meta_count(2019, h_key, "Harbor Seal")
print(test_run)


###OR### - THIS WORKED, SO TRY OTHER DATASETS TOMORROW


##############

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

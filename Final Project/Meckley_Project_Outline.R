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
  #about three decades (1990-2018)?

# 2.
  #My first dataset, which is the coastal seawater factors, comes from Cefas, more specifically, their data portal. The CSV is given below:
LM_wd(repo="meckleylg", folder="Final Project")
data <- read.csv("UK_Water_Data.csv")
  #The title of this dataset is called: "Dissolved oxygen, temperature and salinity measurements in coastal and estuarine waters in the UK from 1990 to 2018".
  
  #My second dataset, which is the count data of both the harbor and grey seals over time in the UK, comes from the iNaturalist database.
  #I got this data with the help of Google Gemini
  #The code to retrieve this data is below:
install.packages("rgbif")
library(rgbif)
library(dplyr)

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
seal_data <- bind_rows(harbor_list, grey_list)

# 3.
  #From the coastal water data, I plan on using data ranging from the dates specified for this project, which is what they are in the dataset.
  #The columns I plan on using are "Country", "Year", "Oxygen", "Temp", and "Salinity". These will be used to predict the seal populations.
  #From the seal dataset, I plan on using the "year", "count", and "species" columns. These are what is being compared with the coastal water data.

# 4.
  #The statistics that I plan on using to analyze this dataset are...

# 5. 
  #Two figures that I think might be most useful based on the datasets I am comparing are...

# 6. 
  #The data processing methods that I will be using to bring the data into a useable format for my statistics and figures is that...





###DUE THURSDAY###

#add a region column
#water$side_region <- water$side
#water$side_region <- gsub(water, "South East", "SE subregion")
#and then region 2

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
    year <= 2018
  ) %>%
  galah_group_by(year) %>%
  atlas_counts() %>%
  mutate(species = "Harbor Seal")

# 3. Get counts for Grey Seals
grey_counts <- galah_call() %>%
  galah_filter(
    scientificName == "Halichoerus grypus",
    year >= 1990, 
    year <= 2018
  ) %>%
  galah_group_by(year) %>%
  atlas_counts() %>%
  mutate(species = "Grey Seal")

# 4. Combine
all_seals <- bind_rows(harbor_counts, grey_counts)
print(all_seals)


#just england bc we have england and all of its regions and such and other countries...

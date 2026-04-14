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
  #What coastal seawater factors of England (salinity, oxygen, and temperature) are capable of predicting both the harbor seal and grey seal populations over the course of 
  #about three decades (1990-2018), and is there an interaction between year and temperature that affects either of the seal populations?

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
  #The statistics that I plan on using to analyze this dataset are a linear model, GLMM, ANOVA, and AIC.
    #The linear models will be used to determine what is predicting each of the seal populations over the timeframe (1990-2018).
    #The ANOVA will be used to determine which of the coastal water factors significantly predicts the seal populations over time.
    #The GLMM will be used to make the interaction between temperature and year on both of the seal populations.
    #Another ANOVA will be used to see the interaction of temperature and year on both of the seal populations.
    #The AIC will then be used to compare across the different models created revealing the predictors of each of the seal populations over time and to see which model is the best fit.

# 5. 
  #Two figures that I think might be most useful based on the datasets I am comparing are:
    #A plot that shows the interaction between year and temperature for both the harbor and grey seal populations. This would be a figure that has two plots in it.
    #A linear regression plot that compares the two seal populations being affected by changing salinity.

# 6. 
  #The data processing methods that I will be using to bring the data into a usable format for my statistics and figures is that:
    #First, I will change the format of the seal data so that the species of seal is shown as a column rather than both species being in one column and the counts being in another.
    #Second, I will remove all of the columns I do not need from the coastal water dataset I have so that the ones I am actually using will be there.
    #Third, I will get the means of the columns I am using from the coastal water dataset since there are multiple years with measurements (I only want one year with the mean/average of those measurements).
    #Fourth, I will merge my two datasets by the "year" column so that I can make my linear models comparing the species to the factors in the water.
    #Fifth, I will make my figure comparing both of the seal populations and the salinity (if it is something that is significant) to see a visual representation of the data. I will also make an ANOVA comparing all of the models I make comparing all of those water factors and compare the AICs of them.
    #Sixth, I will make a GLMM to look at the interactive effect between the temperature and year to see how that affects both of the seal populations. The data would be considered random so I would use a GLMM rather than a GLM.
    #Seventh, I will make the plot (two plots as one figure), with one that is comparing both of the seal populations with the temperature, and then the other with year.
    



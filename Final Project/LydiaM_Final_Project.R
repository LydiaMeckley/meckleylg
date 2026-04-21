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


###Final project due: Thursday April 30th at 10:00am###



###PACKAGES###
library(rgbif)
library(dplyr)
library(readxl)
library(vegan)
library(fitdistrplus)
library(logspline)
library(MASS)
library(MuMIn)
library(mgcv)

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
years <- 2007:2018

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


                    #####PART 1 -LINEAR MODEL AND SCATTER PLOT#####


###REMOVE COLUMNS I DO NOT NEED FROM THE COASTAL WATER DATASET###
waterdata <- waterdata[,-1]
waterdata <- waterdata[,-2:-6]
waterdata <- waterdata[,-3:-7]
waterdata <- waterdata[,-6]

###SUBSET THE COASTAL WATER DATA TO ONLY HAVE DATA FROM THE COUNTRY "ENGLAND"###
waterdata <- subset(waterdata, Country == "England")

###MEAN THE COASTAL WATER DATA BY YEAR###
waterdata <- na.omit(waterdata)
waterdata$Year <- paste(waterdata$Year)
water.means <- aggregate(x = waterdata, by = list(waterdata$Year), FUN = "mean")

###REMOVE NAs FROM THE COASTAL WATER DATA###
water.means <- water.means[,-2:-3]

###CHANGE THE COLUMN NAME OF THE YEAR COLUMN BACK TO "YEAR"###
colnames(water.means)[1] <- "Year"

###MERGE BOTH OF THE DATASETS (COSTAL WATER DATA AND SEAL POPULATION DATA)###
seal.water <- merge(water.means, sealdata, by.x = "Year", by.y = "year")

###SEE WHAT DISTRIBUTION BEST FITS THE SEAL COUNTS###
fit.weibull <- fitdist(seal.water$count, distr = "weibull")
fit.norm <- fitdist(seal.water$count, distr = "norm")
fit.gamma <- fitdist(seal.water$count, distr = "gamma")
fit.lnorm <- fitdist(seal.water$count, distr = "lnorm")
fit.nbinom <- fitdist(seal.water$count, distr = "nbinom")
fit.logis <- fitdist(seal.water$count, distr = "logis")
fit.geom <- fitdist(seal.water$count, distr = "geom")
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.nbinom, fit.logis, fit.geom))
  #lnorm is the best fit, it has the lowest AIC of all the distributions (205.25)

###CREATE A LINEAR MODEL TO SEE WHAT BEST EXPLAINS BOTH OF THE SEAL POPULATIONS###
mod1 <- lm(count ~ Oxygen + species, family = lnorm, data = seal.water)
summary(mod1)
anova(mod1)
AIC(mod1)

mod2 <- lm(count ~ Salinity + species, family = lnorm, data = seal.water)
summary(mod2)
anova(mod2)
AIC(mod2)

mod3 <- lm(count ~ Temp + species, family = lnorm, data = seal.water)
summary(mod3)
anova(mod3)
AIC(mod3)
#intercept is the other factor (grey seals)
#salinity predicts both of the seal counts best, even if it is far from being significant based on the AICs

###FIGURE 1###
plot(seal.water$count ~ seal.water$Salinity, xlab = "Salinity (PSU)", ylab = "Seal Population Counts", pch=16, col = "navyblue")
abline(lm(seal.water$count ~ seal.water$Salinity), col = "black")
#make it look a little prettier (aka the bottom)
  #should I be getting a summary or ANOVA for this? :O


                    #####PART 2 - GLMM AND PLOT OF SORTS#####

###CREATE A GLMM WITH AN INTERACTIVE EFFECT BETWEEN TEMPERATURE AND YEAR TO SEE HOW IT PREDICTS EACH OF THE SEAL SPECIES###
glmm.mod <- glmmPQL(count ~ Temp * Year, family = gaussian, random = ~ 1 | species, data = seal.water)
summary(glmm.mod)

#i want to do one GLMM for each species
#something is going wrong here... :(

###I HAVE TRIED 50 MILLION DISTRIBUTIONS AND NONE WORK

###climate variability - standard deviation with climate (what can I do with that? ask wednesday! maybe that can be my third plot)









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
library(itsadug)

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


                    ####PART 1 - LINEAR MODEL AND SCATTER PLOT####


###REMOVE COLUMNS I DO NOT NEED FROM THE COASTAL WATER DATASET FOR BETTER ORGANIZATION###
waterdata <- waterdata[,-1]
waterdata <- waterdata[,-2:-6]
waterdata <- waterdata[,-3:-7]
waterdata <- waterdata[,-6]

###SUBSET THE COASTAL WATER DATA TO ONLY HAVE DATA FROM THE COUNTRY "ENGLAND"###
waterdata <- subset(waterdata, Country == "England")

###MEAN THE COASTAL WATER DATA BY YEAR BECAUSE THERE ARE NUMEROUS VALUES FOR EACH VARIABLE I WANT TO USE###
waterdata <- na.omit(waterdata)
waterdata$Year <- paste(waterdata$Year)
water.means <- aggregate(x = waterdata, by = list(waterdata$Year), FUN = "mean")

###REMOVE NAs FROM THE COASTAL WATER DATA SO THAT I CAN MERGE WITHOUT ERROR###
water.means <- water.means[,-2:-3]

###CHANGE THE COLUMN NAME OF THE YEAR COLUMN BACK TO "YEAR" FOR BETTER ORGANIZATION###
colnames(water.means)[1] <- "Year"

###MERGE BOTH OF THE DATASETS (COSTAL WATER DATA AND SEAL POPULATION DATA) FOR COMPARISON###
seal.water <- merge(water.means, sealdata, by.x = "Year", by.y = "year")

###SEE WHAT DISTRIBUTION BEST FITS THE SEAL COUNTS FOR A MORE ACCURATE LINEAR MODEL###
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
#salinity predicts both of the seal counts best, even if it is far from being significant based on the AICs
#summary and ANOVa used to extract results, and this holds true for all of the other sections in the script that have ANOVAs and summaries

###FIGURE 1###
plot(seal.water$count ~ seal.water$Salinity, xlab = "Salinity (PSU)", ylab = "Seal Population Counts", pch=16, col = "navyblue")
abline(lm(seal.water$count ~ seal.water$Salinity), col = "black")


                    ####PART 2 - GAM AND INTERACTION PLOT####


###CHANGE THE YEAR TO NUMERIC SO IT IS COMPATIBLE WITH THE GAM###
seal.water$Year <- as.numeric(seal.water$Year)

###CREATE A GAM WITH AN INTERACTIVE EFFECT BETWEEN TEMPERATURE AND YEAR TO SEE HOW IT PREDICTS EACH OF THE SEAL SPECIES###
  ###THIS IS THE GLOBAL MODEL TO SEE AN OVERALL COMPARISON AND INTERACTION###
gam.mod <- gam(count~Temp*Year+species, data = seal.water)
summary(gam.mod)
anova(gam.mod)

###SUBSET EACH SEAL SPECIES TO MAKE AN INDIVIDUAL GAM FOR THEM###
harbor.seal <- subset(seal.water, species == "Harbor Seal")
grey.seal <- subset(seal.water, species == "Grey Seal")

###CREATE INDIVIDUAL GAMS FOR EACH SEAL SPECIES TO COMPARE IN THE PLOTS###
gam.mod2 <- gam(count~Temp*Year, data = harbor.seal)
gam.mod3 <- gam(count~Temp*Year, data = grey.seal)
summary(gam.mod2)
summary(gam.mod3)
anova(gam.mod2)
anova(gam.mod3)

###FIGURE 2###
###PLOT FOR THE GAM TO VISUALIZE THE INTERACTIVE EFFECT OF TEMPERATURE AND YEAR ON BOTH SEAL SPECIES###
par(mfrow = c(2, 2))
plot_smooth(gam.mod2, view="Year", rm.ranef=FALSE, ylab = "Harbor Seal", xlab = "Year", hide.label = TRUE)
plot_smooth(gam.mod2, view="Temp", rm.ranef=FALSE, ylab = "Harbor Seal", xlab = "Temperature (C°)", hide.label = TRUE)

plot_smooth(gam.mod3, view="Year", rm.ranef=FALSE, ylab = "Grey Seal", xlab = "Year", hide.label = TRUE)
plot_smooth(gam.mod3, view="Temp", rm.ranef=FALSE, ylab = "Grey Seal", xlab = "Temperature (C°)", hide.label = TRUE)


                    ####PART 3 - CLIMATE VARIABILITY####


###TAKE THE STANDARD DEVIATION OF BOTH OXYGEN AND YEAR TO MEASURE CLIMATE VARIABILITY###
sdtemp <- aggregate(Temp ~ Year, data = waterdata, FUN = sd)
sdoxygen <- aggregate(Oxygen ~ Year, data = waterdata, FUN = sd)

###MERGE THE TWO STANDARD DEVIATION DATAFRAMES FOR EASIER COMPARISON IN THE LINEAR MODEL###
climate <- merge(sdtemp, sdoxygen, by = "Year")

###CHANGE YEAR TO BE NUMERIC SO IT IS COMPATIBLE WITH THE LINEAR MODEL###
climate$Year <- as.numeric(climate$Year)

###LINEAR MODEL FOR CLIMATE VARIATION BETWEEN TEMPERATURE STANDARD DEVIATION AND YEAR WITH OXYGEN AS A COVARIATE###
mod4 <- lm(Temp ~ Year + Oxygen, data = climate)
summary(mod4)
anova(mod4)

###FIGURE 3###
###PLOT OF THE RELATIONSHIP BETWEEN TEMPERATURE STANDARD DEVIATION AND YEAR FOR CLIMATE VARIABILITY###
plot(climate$Temp ~ climate$Year, xlab = "Year", ylab = "SD Temperature (C°)", pch=16, col = "navyblue")
abline(lm(climate$Temp ~ climate$Year), col = "black")



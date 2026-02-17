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

# Read in the "Toscano_Griffen_Data.csv" data from GitHub and load the three packages we used in the tutorial this week.
# The paper these data came from is uploaded to Canvas as "Toscano&Griffen_2014_JAE..."
setwd("C:/GitHub/R4Eco_2026/Week6")
LM_wd(repo="R4Eco_2026", folder="week6")

data <- read.csv("Toscano_Griffen_Data.csv")

head(data)

library(MASS)
library(MuMIn)
library(mgcv)

?family

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from this week's tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each)
    # In one model only include additive effects.
glmm.mod <- glmmPQL(activity.level~temperature + carapace.width + claw.width, family = binomial, random = ~ 1 | block, data = data)

    # In the other model include one interactive effect.
glmn.mod2 <- glmmPQL(activity.level~temperature * carapace.width + claw.width, family = binomial, random = ~ 1 | block, data = data)

    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming "df" is your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey

# (Q1) - The code I've provided in line 13 above is performing two operations at once. What are they? (2 pts)


# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, SPECIFICALLY, did the results change? (5 pts)


# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)


# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)


# (Q4) - Which model is a better fit? (2 pt)


# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)









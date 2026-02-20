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
LM_wd(repo="R4Eco_2026", folder="week6")

data <- read.csv("Toscano_Griffen_Data.csv")

head(data)

library(MASS)
library(MuMIn)
library(mgcv)

# First create models with the same (y) and method (GLMM) as the published paper, using the GLMM function from this week's tutorial. 
  #Create two different models using the same 3 predictor (x) variables from the dataset. (4 points each)
    # In one model only include additive effects.
glmm.mod <- glmmPQL(prop.cons~temperature + carapace.width + claw.width, family = binomial, random = ~ 1 | block, data = data)
summary(glmm.mod)

    # In the other model include one interactive effect.
glmm.mod2 <- glmmPQL(prop.cons~temperature * carapace.width + claw.width, family = binomial, random = ~ 1 | block, data = data)
summary(glmm.mod2)

    # Use a binomial distribution and block as a random effect in both models to match the paper's analyses. Remember ?family to find distribution names.

# The authors used proportional consumption of prey as the (y) in their model, but did not include this in the dataset.
  # So we are going to create it - run the following line, assuming "df" is your data frame (feel free to change that):
data$prop.cons <- data$eaten/data$prey

# (Q1) - The code I've provided in line 13 above is performing two operations at once. What are they? (2 pts)
  #The first operation that is being performed is dividing the data from the eaten column by the data from the prey column. The second operation that is being performed is assigning the newly generated data into a new column and naming it proportion consumed.

# (Q2) - Did the interactive effect change which variables predict proportional consumption? How, SPECIFICALLY, did the results change? (5 pts)
  #The interactive effect did change which variables predict proportional consumption. The proportional consumption (intercept) changed from being statistically significant to being not significant when the interaction was added. The other variables remained at having no statistical significance, even with the interactive effect.

# (Q3) - Plot the residuals of both models. Do you think either model is a good fit? Why or why not? (3 pts)
  #I do not think either model is a good fit. Both of the plots show a negative slope in the residuals, and they are not evenly distributed around the zero line.

plot(glmm.mod)
plot(glmm.mod2)

# Re-run both models as generalized additive models instead (using gam). Then compare the AIC of both models. (4 points each)
gam.mod <- gam(prop.cons~temperature + carapace.width + claw.width, family = binomial, random = list(block=~ 1), data = data)
gam.mod2 <- gam(prop.cons~temperature * carapace.width + claw.width, family = binomial, random = list(block=~ 1), data = data)

AIC(gam.mod)
AIC(gam.mod2)

# (Q4) - Which model is a better fit? (2 pt)
  #The better fit model is the gam.mod model (my first gam model) with an AIC score of 607.1383.

# (Q5) - Based on the residuals of your generalized additive models, how confident are you in these results? (2 pts)
  #Based on the residuals and the results provided from the AIC scores, I am fairly confident in these results. Both of the plots look very similar to each other, but I do see a slight difference in that the first model has residuals that are a little more evenly distributed than the second one.

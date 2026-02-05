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

# Look at the plot and model results for our Dryad data in the tutorial. 
  # Part 1: Without knowing which points represent which groups,give one explanation for why these data might be difficult
  # to draw spatial inferences about genes.(4 points)
    #These data might be difficult to draw spatial inferences about genes because the points are very clustered on the chart. Some of the points overlap as well which can make it hard to tell exactly how many groups there are.

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (4 points), and EXPLAIN WHY (6 points).
    #explain what patterns you could possibly see based on the data (literally what do you see)


# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)
install.packages("stability")
library(stability)

require(devtools)
install_version("dplyr", version = "1.2.0", repos = "http://cran.us.r-project.org") #this is the one he tried#
library(dplyr)

devtools::install_version("dplyr", version = "1.1.4", repos = "https://cloud.r-project.org")

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.
ge_data

mod.env <- lm(ge_data$Yield ~ ge_data$Env)
mod.gen <- lm(ge_data$Yield ~ ge_data$Gen)

# Test the significance of both models and look at the model summary. (4 points each)
anova(mod.env)
summary(mod.env)

anova(mod.gen)
summary(mod.gen)

  # Which model is a better fit to explain the yield response(2 pts), and WHY? (4 points)
  # Hint: Does one model seem more likely to be over-fitted?
    #The model that is a better fit to explain yield response is for the genotype. That is because the R-squared value for that model is 0.07706, much smaller and father away from 1 than the yield response model that looks at the environment, which has an R-squared value of 0.4315. The one that looks at environment would be considered over-fitted.

#wait so enviornment is a better fit? I have the opposite - yes I indeed do have the opposite.


# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
  #The environment that would be the very worst choice for generating a strong yield response is Sargodha.

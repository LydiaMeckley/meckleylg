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
    #These data might be difficult to draw spatial inferences about genes because the points are very clustered on the plot. Some of the points overlap as well which can make it hard to tell exactly how many groups there are.

  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (4 points), and EXPLAIN WHY (6 points).
    #The result or interpretation that I feel most confident in is that the diversity is not evenly distributed across the landscape. There are a lot of points located to the left side of the plot, while there are a few that are found on the right side of the plot. That likely means that the genotypes of the data points that are farther away have more variation than the ones that are more clustered together. That is because if the data points represent areas in which organisms live, the organisms that live farther away will have more genetic variation and diversity than the ones living closer together since they are interacting with each other more and less.


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
    #The model that is a better fit to explain yield response is for the environment. That is because the R-squared value for that model is 0.4315, much larger and closer to 1 than the yield response model that looks at the genotype, which has an R-squared value of 0.07706. The one that looks at genotype would be considered over-fitted as well because of there being a lot of data points, and the fact that most of the P values for the genotypes are not statistically significant.

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
  #The environment that would be the very worst choice for generating a strong yield response is Sargodha because of it having the highest P value out of all the other environments.

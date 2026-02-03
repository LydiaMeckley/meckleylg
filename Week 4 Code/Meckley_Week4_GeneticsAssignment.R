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
    #
  # Part 2: Despite the drawbacks, give the result or interpretation that you feel most confident in (4 points), and EXPLAIN WHY (6 points).
    #

# For your scripting assignment we will use the "ge_data" data frame found in the "stability" package.
  # Install the "stability" package, load it into your R environment, and use the data() function to load the "ge_data". (2 points)

# Create two linear models for Yield Response: one related to the Environment and one to the Genotype. (2 points each)
  # 'Yield Response' in this dataset is a measure of phenotype expression.
  # Hint: Look at the help file for this dataset.

# Test the significance of both models and look at the model summary. (4 points each)
  # Which model is a better fit to explain the yield response(2 pts), and WHY? (4 points)
  # Hint: Does one model seem more likely to be over-fitted?

# Which environment would be your very WORST choice for generating a strong yield response? (2 points)
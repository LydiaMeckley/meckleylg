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

# (1) Approximately how many hours ahead of Sunbury was the peak flow in Lewisburg during the 2011 flood? (2 pt)
  #flow will be slower because lewi is on the west branch of the Sus river 8-9 hours (chart is on the tutorial)


# (2) Give one reason why information on the time between peak flow events up- and downstream could be valuable information? (4 pts)
# to warn ppl

# Package scavenger hunt! (12 pts each)



## (3) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that contains at least one function specifically designed to measure genetic drift.
install.packages("plot3D")
library(plot3D)

    # Copy-paste into your script - and run - an example from the reference manual for a function within this package related to a measure of genetic drift. 
N <- 32 #population size
n_alleles <- 2*N 
p_gen0 <- 0.25 #Frequency of allele A1 in the first gen
p_gen1 <- rbinom(1, n_alleles, p_gen0) / n_alleles
p_gen1

p_gen2 <- rbinom(1, n_alleles, p_gen1) / n_alleles
p_gen2

p_gen3 <- rbinom(1, n_alleles, p_gen2) / n_alleles
p_gen3

p_gen4 <- rbinom(1, n_alleles, p_gen3) / n_alleles
p_gen4

p_gen5 <- rbinom(1, n_alleles, p_gen4) / n_alleles
p_gen5

generations <- seq(from = 0, to = 5, by = 1)
p_through_time <- c(p_gen0, p_gen1, p_gen2, p_gen3, p_gen4, p_gen5)
plot(generations, p_through_time, type="l", lwd = 2, col = "darkorchid3",
     ylab = "p", xlab = "generations", las = 1)

# DO I NEED TO RUN THEIR OTHER STUFFS? ASK!!! THERE ARE A FEW OTHER THINGS BUT DON'T REALLY APPLY TO THE EXAMPLE?!

        # Depending on the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
    # After running the function example, manipulate a parameter within the function to create a new result.
        # Common options might be allele frequency, population size, fitness level, etc. 
N <- 60 #population size
n_alleles <- 2*N 
p_gen0 <- 0.25 #Frequency of allele A1 in the first gen
p_gen1 <- rbinom(1, n_alleles, p_gen0) / n_alleles
p_gen1

p_gen2 <- rbinom(1, n_alleles, p_gen1) / n_alleles
p_gen2

p_gen3 <- rbinom(1, n_alleles, p_gen2) / n_alleles
p_gen3

p_gen4 <- rbinom(1, n_alleles, p_gen3) / n_alleles
p_gen4

p_gen5 <- rbinom(1, n_alleles, p_gen4) / n_alleles
p_gen5

generations <- seq(from = 0, to = 5, by = 1)
p_through_time <- c(p_gen0, p_gen1, p_gen2, p_gen3, p_gen4, p_gen5)
plot(generations, p_through_time, type="l", lwd = 2, col = "darkorchid3",
     ylab = "p", xlab = "generations", las = 1)

        # Add the results of this manipulation to your script (if in the console) or upload the new plot.

          # By manipulating these parameters you can see how it impacts the results.
          # This type of manipulation is one example of how theoretical ecology and modelling are used to predict patterns in nature.



## (4) Using Google and ONLY packages from GitHub or CRAN:
    # Find a package that will generate standard diversity metrics for community ecology, specifically Simpson's Diversity Index.
install.packages("OnomasticDiversity")
library(OnomasticDiversity)

    # Copy-paste into your script - and run - an example from the reference manual for a function to calculate Simpson's diversity. 
data(namesmengal16)
result = fSimpson (x= namesmengal16, k="number",
                   n="population", location = "muni" )
result

#### IS THIS ONE BETTER THAN THE ONE ABOVE??? ###
## are the numbers in parentheses the species in each population?? ##

install.packages("abdiv")
library(abdiv)

x <- c(15, 6, 4, 0, 3, 0)
dominance(x)

simpson(x)
1 - dominance(x)

        # Depending on the example usage of the function, either upload a plot of the result or use print() and copy/paste the console output into your script.
# [1] 0.6352041

    # After running the function example, modify your script to generate another diversity metric that is NOT part of the example. 
        # If there are multiple diversity metrics in the example script, none of these will count as the modified script.
        # Hint: If the function can "only" calculate Simpson's diversity, the inverse of Simpson's diversity is another common metric.

##SO DO I CHANGE THE NUMBERS IN THE INVERSE? OR DO I KEEP THEIR INVERSE HERE? OR DO I USE THE INVERSE UP THERE AND DOWN HERE?
invsimpson(x)
1 / dominance(x)

simpson_e(x)
1 / (dominance(x) * richness(x))

        # Add the results of this manipulation to your script (if in the console) or upload the new plot.
#[1] 0.6853147
        
          # Diversity metrics are frequently used in community ecology for reasons ranging from a quick comparison between sites to understanding community stability.
          # Their calculation can be very tedious by hand - and very fast with a package designed for the operation.




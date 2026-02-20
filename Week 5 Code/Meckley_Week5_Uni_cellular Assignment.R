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

      ######### DATA SET #########

### Script to  analyze chlamy timelapse data ###
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
install.packages("reshape2")
library(reshape2)

LM_wd(repo="meckleylg", folder="Week 5 Code")
data <- read.csv(file=("Figure4Data.csv"), header=T)

# Originally (in 2017-04-12 script) I used unweighted means for ordering strains
#  but used weighted means for plotting size distributions
# In this current script, I use weighted means for both plotting and calculating mean progeny sizes
# In other words, I use weighted means for everything in this analysis.


# The following weights cell counts of progeny size data by proportion biomass
# This script repeats a given row by a frequency equal to the progeny size 
#   E.g. num.cells.progeny = 8 will have 8 rows, therefore weighting the
#        count towards these larger but fewer progeny

# First I create a new, separate dataframe from the original
#   containing just strain and number of cells in the progeny
data.new <- data.frame(data[1:4])
#   and I create a last column which is the desired frequency (or biomass) which
#   is equal to the number of cells in the progeny
data.new$freq <- data.new$Num.Cells.Progeny
# I omit NAs which cannot be handled by the next line of code
data.new <- na.omit(data.new)
# This line of code repeats rows by times=$freq
#   which has now created extra data points and weighted the data by biomass
#   E.g. a row of progeny = 8 cells is now repeated 8 times
data.weight <- data.new[rep(row.names(data.new), data.new$freq), 1:4]

# Log-base 2 transforms the weighted data
data.weight$log.progeny <- log(data.weight$Num.Cells.Progeny, 2)

# Calculate WEIGHTED mean progeny sizes and standard deviations for each strain at reproduction
#   aggregate will go through a particular vector and perform function
means.progeny <- aggregate(data.weight$Num.Cells.Progeny ~ data.weight$Strain, FUN = mean)
sd.progeny <- aggregate(data.weight$Num.Cells.Progeny ~ data.weight$Strain, FUN = sd)
colnames(means.progeny) <- c("Strain","Mean.Num.Progeny")
colnames(sd.progeny) <- c("Strain","SD.Num.Progeny")

# Assign weighted mean progeny size and SD to weighted counts by strain
data.weight <- inner_join(data.weight,means.progeny, by = "Strain")
data.weight <- inner_join(data.weight,sd.progeny, by = "Strain")

# Going back to working with the data.new dataframe to plot actual data points
# over boxplots

#   Log transform the unweighted data
data.new$log.progeny <- log(data.new$Num.Cells.Progeny, 2)

# PLOT PROGENY SIZE ACROSS STRAINS 
# note that the default geom_boxplot command plots:
#   lower hinge marks 25th quantile
#   middle hash marks median = 50% quantile
#   upper hinge marks 75th quantile
#   whiskers 

# Manually obtain and input sample sizes above boxplots
samplesizes <- summary(data.new$Strain)
samplesizes

# This plot orders the strains along the x-axis by mean progeny size EXCEPT
#  it has been manually ordered for K101, K106, B506, and B210 because all have a mean of 1

data.weight$Strain <- factor(data.weight$Strain, levels = c('K101','K106',
                                                            'B506','B210',
                                                            'B201','B505',
                                                            'B211','B203',
                                                            'B204'))

data.new$Strain <- factor(data.new$Strain, levels = c('K101','K106',
                                                      'B506','B210',
                                                      'B201','B505',
                                                      'B211','B203',
                                                      'B204'))

myplot <- ggplot(data = data.weight,
                 aes(x=Strain,
                     y=log.progeny)) +
  geom_boxplot(size = 1.2) +
  geom_jitter(data = data.new,
              aes(x=factor(Strain),
                  y=log.progeny),
              size = 0.7, alpha = 0.3, color = 'green4', fill = 'green4') +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent"),
        axis.line = element_line(colour = 'black'),
        text = element_text(size=20),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black')) +
  scale_y_continuous(limits=c(-0.5,6)) +
  scale_x_discrete(labels = c('K101','K106',
                              'B506','B210',
                              'B201','B505',
                              'B211','B203',
                              'B204')) +
  labs(x="Strain", y=expression("Cells per Propagule (Log"[2]*")")) +
  annotate('text', x=1, y=-0.5, label="N = 668") +
  annotate('text', x=2, y=-0.5, label="162") +
  annotate('text', x=3, y=-0.5, label="162") +
  annotate('text', x=4, y=-0.5, label="158") +
  annotate('text', x=5, y=-0.5, label="119") +
  annotate('text', x=6, y=-0.5, label="704") +
  annotate('text', x=7, y=-0.5, label="51") +
  annotate('text', x=8, y=-0.5, label="115") +
  annotate('text', x=9, y=-0.5, label="103")

ggsave(filename=paste("meckleylg", folder="Week 5 Code",
                      paste("Figure4", ".png", sep=""), sep=""), plot=myplot,
       width = 10, height = 4, bg = 'transparent')

library(fitdistrplus)
library(logspline)

                      ######### ASSIGNMENT #########


# First, recreate Figure 4 from Herron et al. (2019). De novo origins of multicellularity in response to predation. Scientific reports.
  # Search datadryad.org by the paper title and download the dataset. It will include .csv files and R scripts, organized by figure.
  # Save the script and change the working directory on lines 8 and 115 to match your computer
  # Upload the plot you've created to GitHub. (4 points)
  # Zoom into your plot to look at the distribution for different strains.

# Do all of the strains in the plot have the same distributions (yes/no)? (1 pt)
  #All the strains do not have the same distributions.

# Based on these observations of your strain distributions, why did the authors use a Kruskal-Wallis test rather than ANOVA to compare the strains? (3 pts)
  #The authors use a Kruskal-Wallis test rather than ANOVA to compare the strains because the data in the plot do not have the same distributions, they have a non-normal or non-parametric distribution.

# Use the fitdist() and gofstat() functions to compare the poisson, negative binomial, and logistic distributions for:
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)
data.new <- na.omit(data.new)
number <- data.new$Num.Cells.Progeny
fitp <- fitdist(number, "pois")
gofstat(fitp)

fitnb <- fitdist(number, "nbinom")
gofstat(fitnb)

fit.logis <- fitdist(number, "logis")
gofstat(fit.logis)

gofstat(list(fitp, fitnb, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

  # (2) - The replication time (data$RepTime.sec)
    #HINT- "Num.Cells.Progeny" has defined breaks. To display results, use the formula with the "chisqbreaks" argument as follows:
      #gofstat(list(fit.1, fit.2, fit.3, etc), chisqbreaks=c(1,2,4,8,16,32,64))
data.new2 <- data$RepTime.sec
data.new2 <- na.omit(data.new2)
number2 <- data.new2
fitp <- fitdist(number2, "pois")
gofstat(fitp)

fitnb <- fitdist(number2, "nbinom")
gofstat(fitnb)

fit.logis <- fitdist(number2, "logis")
gofstat(fit.logis)

gofstat(list(fitp, fitnb, fit.logis), chisqbreaks=c(1,2,4,8,16,32,64))

# Based on the AIC scores, which distribution is the best fit for: (5 pts each)
  # (1) - The number of cells of progeny (data$Num.Cells.Progeny)?
    #The negative binomial distribution is the best fit.

  # (2) - The replication time (data$RepTime.sec)?
    #The negative binomial distribution is the best fit.


# Plot a generic histogram for the replication time (data$RepTime.sec) (4 pt)
hist(data$RepTime.sec)


# Based on the patterns of this histograms and Figure 4:
  #Give one hypothesis for an evolutionary process represented by the two tallest bars in your histogram. (8 pts)
  # Don't cheat by looking at the paper! 
    # This hypothesis does not need to be correct - it only needs to be ecologically rational based these two figures.
      #A hypothesis for the two tallest bars in the histogram could possibly represent two different phenotypes, and being expressed more than others over specific time intervals during the life history of the organism, and those two have evolved to be the most prevalent in the population out of all other possible phenotypes.
#Perfect!

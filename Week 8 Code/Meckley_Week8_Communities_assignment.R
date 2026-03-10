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

  #colnames df2 (df1)
# Load the vegan package and the "SR_Inverts" csv from GitHub.
# These data are benthic macroinvertebrate samples from the West Branch Susquehanna River and White Deer Hole Creek collected in 2012.
# Sample codes refer to a riffle number for the West Branch, with smaller numbers upstream (R5, R6, etc.).
# White Deer Hole samples are abbreviated as WDH and grouped together.
library(vegan)

LM_wd(repo="R4Eco_2026", folder="week8")
data <- read.csv("SR_Inverts.csv")


#Data prep (10 points):
# These data are setup in a common structure for stream ecology with additional taxonomic information. 
# In our case, the file also contains the Order and Family names for each genus of aquatic insects. 
# We need to remove these for analysis. Subset the data with brackets and a negative symbol to remove the Order and Family info.
df <- data[,-1:-2]

#Now transpose the data - remember species need to be columns and sites need to be rows for analysis:
df2 <- t(df) #now make the first row into column names

# Use these two lines to turn your first row into column names and then remove the first row.
  # This assumes your data frame is named "df". You are welcome to change that.
names(df) <- lapply(df[1, ], as.character)
df <- df[-1,] 

# You should now have a data frame with samples as row names and species as column names.
    # The first column should be the riffle where these samples were collected.

#Data analysis (10 points):
# It is common in community ecology to ask questions about "clustering" of communities spatially (i.e. are closer samples more similar?).
# Now test if "riffle" is a significant predictor of the macroinvertebrate community.  
  # Report your p-value and constrained variance for the model.
  # Plot Axis 1 and Axis 2 of the results with 95% confidence intervals around the riffles.
    # Hint: it will make things easier if you create two separate data frames. One with the Riffle names and one with the bugs.

# If your code results in an "error: 'x' must be numeric" Then run this line of code to force all bugs to numeric
  # Assuming your data frame of macroinvertebrates is called "bugs".
bugs <- sapply(bugs, as.numeric )

# (Q1) - Which group of samples is clearly different along Axis 1? Does this make sense based on what you know about the data? (3 pts)


# Use the rarefaction function from the tutorial to plot 250-individual subsamples grouped and summed by the riffle where they were collected.
  # Hint: use the subset() function to select only the samples from a specific riffle
  # Hint 2: use two equal signs, not one, in the subset() function.
# (Q2) - Which riffle took the most effort to effectively sample? (2 pts)
    # Hint: if you use rbind() to bring your summed riffles together it will be easier to display in a single rarefaction plot.
# (Q3) - Do you think the differences between riffles are ecologically meaningful? (3 pts)
    # Hint: It might help to look at 800-individual subsamples to answer this question.
# (Q4) - Why do the curves stop at different locations on the x-Axis? (2 pts)


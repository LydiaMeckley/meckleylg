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

# With the data frame you created last week you will:

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values (10 points)
  # Add error bars with mean and standard deviation to the plot
  # Change the x and y labels and add a title
  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.

# Create a scatter plot between two of your numeric columns. (10 points)
  # Change the point shape and color to something NOT used in the example.
  # Change the x and y labels and add a title
  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub. (5 points)
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "Week2" folder. (5 points)

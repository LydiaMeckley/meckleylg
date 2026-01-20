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
a <- c('Cats','Dogs','Fish','Rabbits','Birds','Hamsters','Guinea Pigs','Snakes','Rats','Mice','Horses','Cows','Donkeys','Mules','Pigs')
b <- c('Brown','White','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray')
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
d <- c(1,2,3,3,4,4,5,6,7,8,8,8,8,9,10)
e <- c(1,2.1,3.5,4,5,6,7.8,8,9.28,10,11.65,12.98,13,14,15.66)

data <- cbind(a,b,c,d,e)
data
df <- as.data.frame(data)
df
colnames(df) <- c("Pets", "Color", "Age", "Height", "Weight")
df
row.names(df) <- df$`Pets`
df

new.df <- df[,-1]
df[,-1]

Ferrets <- data.frame('Gray', 16,11,17.2)
Ferrets
colnames(Ferrets) <- colnames(new.df)
df.r <- rbind(new.df, Ferrets)
Ferrets
row.names(df.r) <- c(row.names(df[1:15,]), "Ferrets")
df.r

df.r$Age <- as.numeric(df.r$Age)
df.r$Height <- as.numeric(df.r$Height)
df.r$Weight <- as.numeric(df.r$Weight)

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values (10 points)
df.mean <- aggregate(df1$Height ~df1$Color, FUN = "mean")
df.mean

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

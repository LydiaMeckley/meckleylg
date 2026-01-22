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
Pets <- c('Cats','Dogs','Fish','Rabbits','Birds','Hamsters','Guinea Pigs','Snakes','Rats','Mice','Horses','Cows','Donkeys','Mules','Pigs')
Color <- c('Brown','White','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray')
Age <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
Height <- c(1,2,3,3,4,4,5,6,7,8,8,8,8,9,10)
Weight <- c(1,2.1,3.5,4,5,6,7.8,8,9.28,10,11.65,12.98,13,14,15.66)

df <- as.data.frame(cbind(Pets,Color,Age,Height,Weight))

df$Age <- as.numeric(df$Age)
df$Height <- as.numeric(df$Height)
df$Weight <- as.numeric(df$Weight)

Ferrets <- data.frame('Ferrets', 'Gray', 16,11,17.2)

colnames(Ferrets) <- colnames(df)

df1 <- rbind(df, Ferrets)

row.names(df1) <- df1$Pets
df1 <- df1[,-1]
df1

# Create a barplot for one numeric column, grouped by the character vector with 3 unique values (10 points)
df.mean <- aggregate(df1$Height ~ df1$Color, FUN = "mean")
df.mean

colnames(df.mean) <- c("Color","Mean")
df.mean

barplot(df.mean$Mean)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Color, ylim = c(0,10), xlab = "Color", ylab = "Height", main = "Pets of Different Colors Compared to Height")

  # Add error bars with mean and standard deviation to the plot
df.sd <- aggregate(df1$Height ~ df1$Color, FUN = "sd")
colnames(df.sd) <- c("Color","StanDev")
df.sd

arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)

  # Change the x and y labels and add a title
#in the code above

  # Export the plot as a PDF that is 4 inches wide and 7 inches tall.
pdf( file = "Week 2 Code/Meckley_barplot.pdf", width = 4, height = 7)
b.plot <- barplot(df.mean$Mean, names.arg = df.mean$Color, ylim = c(0,10), xlab = "Color", ylab = "Height", main = "Pets of Different Colors Compared to Height")
arrows(b.plot, df.mean$Mean-df.sd$StanDev,
       b.plot, df.mean$Mean+df.sd$StanDev,angle=90,code=3)
dev.off()

# Create a scatter plot between two of your numeric columns. (10 points)
plot(df1$Weight ~ df1$Height)

  # Change the point shape and color to something NOT used in the example.
plot(df1$Weight ~ df1$Height, xlab = "Height", ylab = "Weight", main = "Heights and Weights in Pets", pch=18, col = "darkviolet")
  # Change the x and y labels and add a title
#in the code above

  # Export the plot as a JPEG by using the "Export" button in the plotting pane.

# Upload both plots with the script used to create them to GitHub. (5 points)
  # Follow the same file naming format as last week for the script.
  # Name plots as Lastname_barplot or Lastname_scatterplot. Save them to your "Week2" folder. (5 points)

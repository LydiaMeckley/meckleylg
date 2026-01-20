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

# Now it is time to create your own data frame using the tools we have learned this week.
# First, resave this script as: yourlastname_Week1_Assignment [6 point]
  # e.g. mine would be Wilson_Week1_Assignment


# Create 3 numeric vectors and 2 character vectors that are each 15 values in length with the following structures: [10 points; 2 each]
  # One character vector with all unique values
a <- c('Cats','Dogs','Fish','Rabbits','Birds','Hamsters','Guinea Pigs','Snakes','Rats','Mice','Horses','Cows','Donkeys','Mules','Pigs')
  # One character vector with exactly 3 unique values
b <- c('Brown','White','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray','Gray')
  # One numeric vector with all unique values
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  # One numeric vector with some repeated values (number of your choosing)
d <- c(1,2,3,3,4,4,5,6,7,8,8,8,8,9,10)
  # One numeric vector with some decimal values (of your choosing)
e <- c(1,2.1,3.5,4,5,6,7.8,8,9.28,10,11.65,12.98,13,14,15.66)

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.[3 points]
data <- cbind(a,b,c,d,e)
data
df <- as.data.frame(data)
df
colnames(df) <- c("Pets", "Color", "Age", "Height", "Weight")
df
row.names(df) <- df$`Pets`
df

# Remove the character vector with unique values from the data frame.[2 points]
new.df <- df[,-1]
df[,-1]

# Add 1 row with unique numeric values to the data frame.[2 points]
Ferrets <- data.frame('Gray', 16,11,17.2)
Ferrets
colnames(Ferrets) <- colnames(new.df)
df.r <- rbind(new.df, Ferrets)
Ferrets
row.names(df.r) <- c(row.names(df[1:15,]), "Ferrets")
df.r

# Export the data frame as a .csv file [2 points]
write.csv(df.r, file = "Meckley_Week1_Assignment.csv")
read.df <- read.csv('Meckley_Week1_Assignment.csv')
read.df

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading. [2 points]
df.r$Age <- as.numeric(df.r$Age)
df.r$Height <- as.numeric(df.r$Height)
df.r$Weight <- as.numeric(df.r$Weight)
summary(df.r) 

# Summary Statistics of Data
#Color                Age            Height           Weight      
#Length:16          Min.   : 1.00   Min.   : 1.000   Min.   : 1.000  
#Class :character   1st Qu.: 4.75   1st Qu.: 3.750   1st Qu.: 4.750  
#Mode  :character   Median : 8.50   Median : 6.500   Median : 8.640  
                   #Mean   : 8.50   Mean   : 6.062   Mean   : 8.823  
                   #3rd Qu.:12.25   3rd Qu.: 8.000   3rd Qu.:12.985  
                   #Max.   :16.00   Max.   :11.000   Max.   :17.200 

# Push your script and your .csv file to GitHub in a new "Week1" folder you have created in your repository. [3 points]



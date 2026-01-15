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
  # One character vector with all unique values (is it okay if they are just letters? or should it be something else)
a <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o')
  # One character vector with exactly 3 unique values
b <- c('a','b','c','c','c','c','c','c','c','c','c','c','c','c','c')
  # One numeric vector with all unique values
c <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
  # One numeric vector with some repeated values (number of your choosing)
d <- c(1,2,3,3,4,4,5,6,7,8,8,8,8,9,10)
  # One numeric vector with some decimal values (of your choosing)
e <- c(1,2.1,3.5,4,5,6,7.8,8,9.28,10,11.65,12.98,13,14,15.66)

# Bind the vectors into a single data frame, rename the columns, and make the character vector with unique values the row names.[3 points]
data <- cbind(a,b,c,d,e)
data
df <-as.data.frame(data)
df
colnames(df) <- c("Fur Color", "Type", "Age", "Height", "Weight")
df
row.names(df) <- df$`Fur Color`
df

# Remove the character vector with unique values from the data frame.[2 points] (did I do this right?)
df[,-1]

# Add 1 row with unique numeric values to the data frame.[2 points] (ask if I can add a letter to make it even)
p <- data.frame("c",16,11,17.2)
p
colnames(p) <- colnames(df.a)
df.r <-rbind(df.a, p)
p

# Export the data frame as a .csv file [2 points]

# Generate summary statistics of your data frame and copy them as text into your script under a new section heading. [2 points]

# Push your script and your .csv file to GitHub in a new "Week1" folder you have created in your repository. [3 points]



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
df2 <- t(df)
colnames(df2) <- df$Sample
df3 <- df2[-1,]

# Use these two lines to turn your first row into column names and then remove the first row.
  # This assumes your data frame is named "df". You are welcome to change that.
#names(df2) <- lapply(df2[1, ], as.character)  #IGNORE THIS#
#df3 <- df2[-1,]  #IGNORE THIS#

# You should now have a data frame with samples as row names and species as column names.
    # The first column should be the riffle where these samples were collected.

#Data analysis (10 points):
# It is common in community ecology to ask questions about "clustering" of communities spatially (i.e. are closer samples more similar?).
# Now test if "riffle" is a significant predictor of the macroinvertebrate community.
head(df3)

df3.dataframe <- as.data.frame(df3)

df3.bugs <- df3.dataframe[,-1]
df3.riffle <- df3.dataframe[,1]

bugs <- sapply(df3.bugs, as.numeric)
riffle <- sapply(df3.riffle, as.character)

bugs.dataframe <- as.data.frame(bugs) #do i need this?

mod1 <- rda(bugs.dataframe ~ riffle)
mod1
anova(mod1)

  # Report your p-value and constrained variance for the model.
    #The p-value is 0.004 and the constrained variance is 1.628e-01.

  # Plot Axis 1 and Axis 2 of the results with 95% confidence intervals around the riffles.
    # Hint: it will make things easier if you create two separate data frames. One with the Riffle names and one with the bugs.
plot(mod1, type="n", display = c("sites", "scores"))
text(mod1, display="sites", labels = as.character(riffle))
pl <- ordiellipse(mod1, riffle, kind="se", conf=0.95, lwd=2, draw = "polygon", 
                  col="skyblue", border = "blue")
summary(pl)

# If your code results in an "error: 'x' must be numeric" Then run this line of code to force all bugs to numeric
  # Assuming your data frame of macroinvertebrates is called "bugs".

# (Q1) - Which group of samples is clearly different along Axis 1? Does this make sense based on what you know about the data? (3 pts)
    #Along Axis 1, R6 is the sample that is clearly different. This does make sense based on what I know about the data. That is because there are 29 different species at this riffle, while all the other riffles have less species.

# Use the rarefaction function from the tutorial to plot 250-individual subsamples grouped and summed by the riffle where they were collected.
  # Hint: use the subset() function to select only the samples from a specific riffle
  # Hint 2: use two equal signs, not one, in the subset() function.
rarefaction<-function(x,subsample=5, plot=TRUE, color=TRUE, error=FALSE, legend=TRUE, symbol=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18)){
  
  library(vegan)
  
  
  
  x <- as.matrix(x)
  y1<-apply(x, 1, sum)
  rare.data<-x                                   
  
  select<-unique(sort(c((apply(x, 1, sum)), (seq(0,(max(y1)), by=subsample)), recursive=TRUE)))
  
  
  storesummary.e<-matrix(data=NA, ncol=length(rare.data[,1]),nrow=length(select))
  rownames(storesummary.e)<-c(select)
  colnames(storesummary.e)<-rownames(x)
  storesummary.se<-matrix(data=NA, ncol=length(rare.data[,1]),nrow=length(select))
  rownames(storesummary.se)<-c(select)
  colnames(storesummary.se)<-rownames(x)
  
  
  
  
  for(i in 1:length(select))                      #the for loop
  {
    select.c<-select[i]                     #assigns the 'i'th element of select to select.c
    foo<-rarefy(x,select.c, se=T)           #use whatever vegan fn you want
    
    
    
    storesummary.e[i,]<-foo[1,]
    storesummary.se[i,]<-foo[2,]            
    
  }
  
  storesummary.e<-as.data.frame(storesummary.e)               
  richness.error<<-storesummary.se
  
  for (i in 1:(length(storesummary.e)))
  {
    storesummary.e[,i]<-ifelse(select>sum(x[i,]), NA, storesummary.e[,i])
  }
  
  
  
  ###############plot result################################
  if (plot==TRUE)
  {
    if(color==TRUE){
      plot(select,storesummary.e[,1], xlab="Individuals in Subsample", 
           xlim=c(0,max(select)), ylim=c(0, 5+(max(storesummary.e[,1:(length(storesummary.e))], na.rm=TRUE))),
           ylab="Mean Species Richness", pch =16, col=2, type="n")
      
      for (j in 1:(length(storesummary.e))){
        points(select, storesummary.e[,j], pch=16, col=j+1, type="b", lty=1)}
      
      if(error==TRUE){
        for (m in 1:(length(storesummary.e))){
          segments(select, storesummary.e[,m]+storesummary.se[,m],select, storesummary.e[,m]-storesummary.se[,m])
        }
      }
      if (legend==TRUE){
        legend("bottomright", colnames(storesummary.e), inset=0.05, lty=1, col=1:length(storesummary.e)+1, lwd=2)
      }
    }
    else
    {
      plot(select,storesummary.e[,1], xlab="Individuals in Subsample", 
           xlim=c(0,max(select)), ylim=c(0, 5+(max(storesummary.e[,1:(length(storesummary.e))], na.rm=TRUE))),
           ylab="Mean Species Richness", pch =16, col=2, type="n")
      
      for (j in 1:(length(storesummary.e))){
        points(select, storesummary.e[,j], type="l", lty=1)}
      
      for (k in 1:(length(storesummary.e))){
        symbol<-ifelse(symbol<length(storesummary.e),rep(symbol,2),symbol)
        points(as.numeric(rownames(subset(storesummary.e, storesummary.e[,k]==max(storesummary.e[,k],na.rm=TRUE)))), max(storesummary.e[,k],na.rm=TRUE), pch=symbol[k], cex=1.5)}
      
      if(error==TRUE){
        for (m in 1:(length(storesummary.e))){
          points(select, storesummary.e[,m]+storesummary.se[,m], type="l", lty=2)
          points(select, storesummary.e[,m]-storesummary.se[,m], type="l", lty=2)}}
      
      k<-1:(length(storesummary.e))
      if (legend==TRUE){
        legend("bottomright", colnames(storesummary.e), pch=symbol[k], inset=0.05, cex=1.3)
      }
    }
  }
  print("rarefaction by J. Jacobs, last update April 17, 2009")
  if(error==TRUE)(print("errors around lines are the se of the iterations, not true se of the means")  )     
  list("richness"= storesummary.e, "SE"=richness.error, "subsample"=select)        
  
}

subset.riffle <- subset(df3.dataframe, df3.dataframe$Riffle == "WDH")
sub.riff <- subset.riffle[,-1]
numeric.1 <- sapply(sub.riff, as.numeric)

subset.riffle2 <- subset(df3.dataframe, df3.dataframe$Riffle == "R5")
sub.riff2 <- subset.riffle2[,-1]
numeric.2 <- sapply(sub.riff2, as.numeric)

subset.riffle3 <- subset(df3.dataframe, df3.dataframe$Riffle == "R6")
sub.riff3 <- subset.riffle3[,-1]
numeric.3 <- sapply(sub.riff3, as.numeric)

subset.riffle4 <- subset(df3.dataframe, df3.dataframe$Riffle == "R7")
sub.riff4 <- subset.riffle4[,-1]
numeric.4 <- sapply(sub.riff4, as.numeric)

subset.riffle5 <- subset(df3.dataframe, df3.dataframe$Riffle == "R9")
sub.riff5 <- subset.riffle5[,-1]
numeric.5 <- sapply(sub.riff5, as.numeric)

sample1 <- as.data.frame(t(rowSums(t(numeric.1))))
sample2 <- as.data.frame(t(rowSums(t(numeric.2))))
sample3 <- as.data.frame(t(rowSums(t(numeric.3))))
sample4 <- as.data.frame(t(rowSums(t(numeric.4))))
sample5 <- as.data.frame(t(rowSums(t(numeric.5))))

summed.riffles <- rbind(sample1, sample2, sample3, sample4, sample5)

rarefaction(samples, subsample=250, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)

#samples <- as.data.frame(t(rowSums(t(bugs))))

# (Q2) - Which riffle took the most effort to effectively sample? (2 pts)
    # Hint: if you use rbind() to bring your summed riffles together it will be easier to display in a single rarefaction plot.
rarefaction(summed.riffles, subsample=250, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)

# (Q3) - Do you think the differences between riffles are ecologically meaningful? (3 pts)
    # Hint: It might help to look at 800-individual subsamples to answer this question.
rarefaction(summed.riffles, subsample=800, plot=TRUE, color=TRUE, error=FALSE,  legend=TRUE, symbol)

# (Q4) - Why do the curves stop at different locations on the x-Axis? (2 pts)
#rda isnt the right thing to use because it is used for linear relationships - it should be a cca 

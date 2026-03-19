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

library(readxl)
library(vegan)
library(fitdistrplus)
library(logspline)
LM_wd(repo="R4Eco_2026", folder="Week9")

# For this week it's time to start exploring your own ideas and questions in R.
  # There are at least five options in the dataset to create the following comparisons.

# (Q1 - 12 pts) Use the dataset from the tutorial to complete one redundancy analysis (RDA) with variance partitioning on a different community (NOT the nematodes).
  # Explain the ecological importance of your significant predictor variables, or the importance if none are significant for your community.
invert.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Invertebrate_community")
invert <- as.data.frame(invert.tibble)
head(invert)
inverts <- invert[-1:-2,]
inverte <-as.data.frame(inverts)
colnames(inverte) <- inverte[1,]
invert.df <- inverte[-1,]
invert.df1 <- as.data.frame(sapply(invert.df[3:71], as.numeric))
abiotic.tibble <- read_excel("Penaetal_2016_data.xlsx", sheet = "Abiotic factors")
abiotic.df <- as.data.frame(abiotic.tibble)

abiotic.df$names <- paste(abiotic.df$Parcel, abiotic.df$Land_Use)
invert.df1$names <- paste(invert.df$Parcel, invert.df$Landuse)
abiotic.means <- aggregate(x = abiotic.df, by = list(abiotic.df$names), FUN = "mean")
invert.means <- aggregate(x = invert.df1, by = list(invert.df1$names), FUN = mean)

invert.df$Phenacolimax_major
abiotic.mean1 <- abiotic.means[,-16]
abiotic.mean2 <- abiotic.mean1[,-1:-6]
abiotic.mean2 <- sapply(abiotic.mean2, as.numeric )
abiotic.mean2 <- as.data.frame(abiotic.mean2)
invert.mean1 <- invert.means[,-1]

invert.mean1 <- invert.mean1[-5,]

col_sums <- colSums(invert.mean1[,-70])
condition <- col_sums > 0
df_subset_base <- invert.mean1[, condition]
print(df_subset_base)
invert.mean2 <- df_subset_base[,-54]
print(invert.mean2)

ord <- rda(invert.mean2 ~ pH + totalN + Perc_ash + Kalium + Magnesium + Ca + Al + TotalP + OlsenP, abiotic.mean2)
ord
anova(ord)

ord <- rda(invert.mean2 ~., abiotic.mean2)
ord.int <- rda(invert.mean2 ~1, abiotic.mean2)
step.mod <- ordistep(ord.int, scope = formula(ord), selection = "both")
anova(step.mod)

      ###HOW DO I CONTINUE THEN???###  I need to explain why they do not determine the inverts - why they may not be significant for the inverts (maybe it wasn't measured)

step.R2mod <- ordiR2step(ord.int, scope = formula(ord), selection = "forward") 

# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.

abiotic.mean2$Parcel <- unique(abiotic.df$Parcel)

decomposers <- invert.mean2[,-1:-36]
decomposers2 <- decomposers[,-8:-17]
decomposers2$decomp <- decomposers2$Collembola + decomposers2$Diplopoda + decomposers2$Dermaptera + decomposers2$Hydrobius_fuscipes + decomposers2$Tetranichus_sp + decomposers2$Philoscia_muscorum + decomposers2$Oniscus_asellus

decomposers3 <- merge(abiotic.mean2, invert.df, by = "Parcel")

fit.weibull <- fitdist(decomposers2$decomp, distr = "weibull")
fit.norm <- fitdist(soil.plants$Leaves, distr = "norm")
fit.gamma <- fitdist(soil.plants$Leaves, distr = "gamma")
fit.lnorm <- fitdist(soil.plants$Leaves, distr = "lnorm")               
fit.nbinom <- fitdist(soil.plants$Leaves, distr = "nbinom")       
fit.logis <- fitdist(soil.plants$Leaves, distr = "logis")
fit.geom <- fitdist(soil.plants$Leaves, distr = "geom")
gofstat(list(fit.weibull, fit.norm, fit.gamma, 
             fit.lnorm, fit.nbinom, fit.logis, fit.geom))

mod1 <- lm(decomposers2 ~ pH + totalN + Kalium + Magnesium + Ca + Al + TotalP + Land_use + Species_code, data = soil.plants)
summary(mod1)   
anova(mod1)
AIC(mod1)
summary(mod1)$adj.r.squared

mod2 <- lm(Leaves ~ pH + totalN + Kalium + Species_code,soil.plants)
summary(mod2)
anova(mod2)
AIC(mod1,mod2)

plot(mod2$residuals)
summary(mod2)$adj.r.squared


#what was the thread between nematodes and nitrogen? PLANTS!!!

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.

#stuff <- as.data.frame(sapply(BIC, as.numeric))

#mod <- lm(thing~thing2 ,data=df)
#plot(same as above)

    #go with something as long as i explain why I though it would have been explained

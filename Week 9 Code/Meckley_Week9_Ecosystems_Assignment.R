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
    #None of the abiotic factors (pH, totalN, Perc_ash, Kalium, Magnesium, Ca, Al, TotalP, and OlsenP) were significant predictor variables for the invertebrate community.
    #There were a lot of invertebrate groups in the data of the community, so that can be a possibility as to why none of those abiotic factors are able to predict anything about the community.
    #For example, one of the abiotic factors could be impacting only one type of invertebrate included in the community, rather than all of the different types of invertebrates that were included in the data.
    #It could also be the case that those abiotic factors were not measured directly for the invertebrate community, so there is no relation between them at all.
    #So, those variables may not predict all of the invertebrates in the community, but may instead predict some of the types of invertebrates.

###Code###
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
#bonus point for ordistep
# (Q2 - 12 pts) Then use the dataset from the tutorial to create a linear model related to your RDA. Try multiple predictors to find the best fit model.
  # Explain the ecological importance of the significant predictors, or lack of significant predictors.
    #There was only one significant predictor for the invertebrate community, specifically the decomposer category of invertebrates.
    #This is important in an ecosystem because decomposer invertebrates play a very important role in the decomposition of dead organisms and plant matter, especially decomposing the nitrogen.
    #When looking at the amount of nitrogen in soil then, it can predict higher or lower amounts of decomposer invertebrates since they are contributing to nitrogen being present in the soil.
    #So, when wanting to determine the importance and prevalence of decomposer organisms, a good thing to look at is nitrogen, which can predict the community of decomposers in the area.

###Code###
decomposers <- invert.mean2[,-1:-36]
decomposers2 <- decomposers[,-8:-17]
invertdf2 <- invert.df[-9:-10,]

abiotic.mean2$Parcel <- unique(abiotic.df$Parcel)
decomposers2$Parcel <- unique(invertdf2$Parcel)
abiotic.mean2$Parcel <- unique(abiotic.df$Parcel)

decomposers2$decomp <- decomposers2$Collembola + decomposers2$Diplopoda + decomposers2$Dermaptera + decomposers2$Hydrobius_fuscipes + decomposers2$Tetranichus_sp + decomposers2$Philoscia_muscorum + decomposers2$Oniscus_asellus
na.omit(decomposers2$decomp)

decomposers3 <- decomposers2[,-1:-7]

decomposers4 <- merge(abiotic.mean2, decomposers3, by = "Parcel")

fit.norm <- fitdist(decomposers4$decomp, distr = "norm")
fit.gamma <- fitdist(decomposers4$decomp, distr = "gamma")
fit.nbinom <- fitdist(decomposers4$decomp, distr = "nbinom")       
fit.logis <- fitdist(decomposers4$decomp, distr = "logis")
fit.geom <- fitdist(decomposers4$decomp, distr = "geom")
gofstat(list(fit.norm, fit.gamma, 
            fit.nbinom, fit.logis, fit.geom))

    ###LOGIS IS BEST FIT (HAS LOWEST AIC)###
#bonus for fitting, but didn't use the fit in the analysis after that.
colnames(decomposers4)

mod1 <- lm(decomp ~ pH + totalN + Kalium + Magnesium + Ca + Al + TotalP, data = decomposers4)
summary(mod1)   
anova(mod1)     #total nitrogen is significant here... just as I predicted for the decomposers...# - it is the only significant one
AIC(mod1)
summary(mod1)$adj.r.squared

mod2 <- lm(decomp ~ totalN + Kalium + Magnesium + Ca + Al + TotalP, data = decomposers4)
summary(mod2)
anova(mod2)   #total nitrogen is significant here too#
AIC(mod1,mod2)
plot(mod2$residuals) #looks okay
summary(mod2)$adj.r.squared

mod3 <- lm(decomp ~ totalN + Ca + TotalP, data = decomposers4)
summary(mod3)
anova(mod3)

mod4 <- lm(decomp ~ pH*totalN*Ca*TotalP, data = decomposers4) #the AIC here is the lowest, but there is no interraction between the predictors. Also, nitrogen remains significant.
summary(mod4)
anova(mod4)

mod5 <- lm(decomp ~ pH*totalN ,data = decomposers4)
summary(mod5)
anova(mod5)

mod6 <- lm(decomp ~ totalN, data = decomposers4) ###
summary(mod6)
anova(mod6)

AIC(mod1, mod2, mod3, mod4, mod5, mod6)

#it seems like nitrogen is always significant and always the only significant one 

# (Q3 - 6 pts) Provide a 3-4 sentence synthesis of how these results relate to one another and the value of considering both together for interpreting biotic-abiotic interactions.
  #The results from the first and the second part of this assignment can be related to each other because they are both used to determine what can predict invertebrate communities based on abiotic factors.
  #The first part revealed that the total community of all invertebrates was not able to be predicted by any one of the abiotic factors, but when only focusing on the decomposer invertebrate community, nitrogen became a significant predictor of that community.
  #The first result can be used to show that not all invertebrate communities can be determined based on the same things. There are so many different abiotic factors that can affect specific communities of the larger group, and that is what the second result showed. 
  #It showed that focusing in on one community of the invertebrates, the decomposers, an abiotic predictor of them can be found.
  #So, both results can be used in combination first to see that there may be no significance across the community at large, and suggest that looking at a smaller community can help to determine what abiotic factor may be used to predict it.


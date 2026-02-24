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

# Population growth over time in isolation ####
library("growthrates")

# This is an example dataset from the growthrates package.
data(bactgrowth)
?bactgrowth
# Most important for us, these data include bacterial strain, density, and antibiotic concentration (Tetracycline) through time.
# The following lines will process these data to structure them to be read by the all_growthmodels() function.
# This pre-processing of the data follows the example from ?all_growthmodels:
splitted.data <- multisplit(value ~ time | strain + conc + replicate,
                            data = bactgrowth)
  # Show which experiments are in splitted.data - the names are the strains
names(splitted.data)
  # Get table from single experiment
dat <- splitted.data[["D:0:1"]]
fit0 <- fit_spline(dat$time, dat$value)
fit1 <- all_splines(value ~ time | strain + conc + replicate,
                    data = bactgrowth, spar = 0.5)
  # These examples require some CPU power and may take a bit longer
  # Initial parameters
p <- c(coef(fit0), K = max(dat$value))
  # avoid negative parameters
lower = c(y0 = 0, mumax = 0, K = 0)
  # Fit all models
  # This allows also to fit to the 'global' data set or any subsets

# Now we will look at the population growth models for all strains.
all_mods <- all_growthmodels(
  value ~ grow_logistic(time, parms) | strain + conc,
  data = bactgrowth, p = p, lower = lower, ncores = 2)
plot(all_mods)
  #Notice that you can scan through the plots and generate them all at once? 
  #This has to do with the data structure we will get to in the next step.

# But what if we only want to look at just a few strains?
# Based on the plotted model results let's look at R:250, T:125, R:125, D:62.5, D:1.95, and T:0.24

# Extract the observations from the model fits - this is an "S4" object. We have only worked with "S3" objects so far.
  # S4 allows for a more complicated data structure that is pre-defined and creates "slots" for objects within the listed larger object.
  # These are called with the "@" symbol instead of a "$". 
  # We can use the slot() function to subset the object. - extract the fit from all the models combined now
fits_slot <- slot(all_mods, "fits")
subset_obs <- slot(fits_slot$`R:250`, "obs")

# Or you can call the vector, data frame, or other info directly through a combo of "@" and "$" depending on the object structure. (a couple diff ways you can do it)
# In our case it's extra complicated and includes a nested S4 object, hence the two @ symbols.
  # This will give you the same result as the slot() function from above.
R250_obs <- all_mods@fits$`R:250`@obs

# You can confirm if they match through a quick logical:
table(subset_obs==R250_obs)
  # Check the output - all 124 values are "TRUE" so these two operations are performing the same function.

# You can also call the slot directly inside of a plot:
plot(all_mods@fits$`R:250`@obs) 


# To look at all 6 plots we are interested in we'll create a multipanel plot. 
# This use of par() will plot 2 rows with 3 plots in each row: (par is paramers function to look at plots and puts the plots in the same thing at once, rows and columns)
par(mfrow = c(2, 3))
plot(all_mods@fits$`R:250`@obs)
plot(all_mods@fits$`T:125`@obs)
plot(all_mods@fits$`R:125`@obs)
plot(all_mods@fits$`D:62.5`@obs)
plot(all_mods@fits$`D:1.95`@obs)
plot(all_mods@fits$`D:0.24`@obs)

# Notice the different patterns in population growth for each of these? (30 generations)(when it flatlines it is getting close to K)(Y axis is diff from each plot so we cant compare them)
# Can you think of anything that might be related to these differences from the dataset?
    # Think about the anibiotic concentrations.

# And notice the values of the y-axis - they differ by plot.
# We will fix the y-axis height to be consistent across all plots now for comparison. (using max value of all 6 for lim)

par(mfrow = c(2, 3))
plot(all_mods@fits$`R:250`@obs, ylim = c(0.01,0.09))
plot(all_mods@fits$`T:125`@obs, ylim = c(0.01,0.09))
plot(all_mods@fits$`R:125`@obs, ylim = c(0.01,0.09))
plot(all_mods@fits$`D:62.5`@obs, ylim = c(0.01,0.09))
plot(all_mods@fits$`D:1.95`@obs, ylim = c(0.01,0.09))
plot(all_mods@fits$`D:0.24`@obs, ylim = c(0.01,0.09))

# Revisit the antibiotic question: now we can quickly see these strains grow at different rates and have differing responses to antibiotics.
  # This response is not unlike a predator-prey relationship. 
  #3 on the bottom, conc is highest in the first one and lowest in the last one
  #takes 30 generations to reach K in the highest conc, in the lowest, 10-15 (reaching K faster in the absence of an environmental stressor)
  #the stressor could potentially be predation or competition

#Predator-prey dynamics ####


#Code from: https://www.r-bloggers.com/lotka-volterra-model%C2%A0%C2%A0intro/
library(deSolve)

# To create these plots and view in the full window, we need to remove the par() settings for multipanel plots.
  # This is done with the dev.off() function - to clear the plot area so we don't mess with anything from above
dev.off()

#create the Lotka-Volterra function we will be using with this operation.
  #model of that equation in the ppt or my notes

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

#Because this is not part of a package, we should spend a little more time exploring the variables to understand what each represents.
  #parameters
Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6) #This is the line we will change
State <- c(x = 10, y = 10)#For now keep this the same.
Time <- seq(0, 100, by = 1)#For now keep this the same.
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time)) #This is the operation that creates the Lotka-Volterra model based on our specified parameters.

#The next two lines plot the model with the predator and prey against each other.
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
  #as bunnies increase, foxes consume them and bunnies decrease, then foxes decrease too because of less food, and then it continues on
    #foxes should be the black one and bunnies should be the red one (looking at the relationship between predator and prey)

# Increase in alpha: (alpha is population growth rate for prey) - fox population should grow too - the population should also crash faster
Pars <- c(alpha = 4, beta = 0.5, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Population fluctuations speed up.
  #lynx and hare, or snowy owls and lemmings (huge fast response rate)

#Decrease in alpha:
Pars <- c(alpha = 1, beta = 0.5, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
#Populations fluctuations slow - population gets closer together in size before it crashes. So they crash slower.
  #white tailed deer and coyotes (predator not completely fixated on this predator, or that the prey are just so high in density that it doesn't really affect anything)

#Increase in beta: (beta is death rate)
Pars <- c(alpha = 2, beta = 0.7, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# This was too much of an increase - killed the cute bunnies off. - the predator ate all the prey - prey decreases

#Decrease in beta:
Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Prey increase without decrease in predator population size - the predator is a not so good predator and can't catch OR the predator eats different prey so it is not focused on one prey.

#Increase in gamma: - predators ability to reproduce
Pars <- c(alpha = 2, beta = 0.5, gamma = .3, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Population fluctuations speed up, and less predictable

#Decrease in gamma: - kill everything off, predators run wild
Pars <- c(alpha = 2, beta = 0.5, gamma = .1, delta = .6)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Population crashed. Why? Because the maximum predation for K has decreased while actual predation has not.

#Increase in delta: - predator dependency of prey item
Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .7)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# Population crash. Why? Think in terms of an opposing effect of gamma

# Decrease in delta:
Pars <- c(alpha = 2, beta = 0.5, gamma = .2, delta = .4)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)
# This creates higher peaks in predator density.

# We will attempt to model the variables in a real predator-prey interaction in the assignment.


# Competitive interactions ####
# Competitive interactions can be modeled with a similar pair of L-V equations.
# The compLV() function from the EcoVirtual package will display model outputs of these equations.
#The package fell apart this fall...installing an historic version for class purposes: 
install.packages("https://cran.r-project.org/src/contrib/Archive/EcoVirtual/EcoVirtual_1.1.tar.gz", repos=NULL, type="source")

library(EcoVirtual)

#First look at ?compLV to get a handle on what is happening here. 
?compLV
#There are 9 parameters that can be changed in the model

  # n01   =  initial population for the superior competitor species.
  # n02	  =  initial population for the inferior competitor species.
  # tmax	=  maximum simulation time.
  # r1    =  intrinsic growth rate for the superior competitor species.
  # r2    =  intrinsic growth rate for the inferior competitor species.
  # k1	  =  carrying capacity for the superior competitor species.
  # k2    =  carrying capacity for the inferior competitor species.
  # alfa	=  alpha coefficient.
  # beta	=  beta coefficient

# Each run of the model will create a viewing window with the population growth and isoclines.
  #we can start to look at population change

# First run with the default values to view the output:
  #10 of each species, first species has a faster growth rate, higher carrying capacity, and higher competition coefficient than the second species.
compLV(n01=10, n02=10,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=200)
  #they have limited resources so they hit K under these conditions and they are competing for resources (or just competition in general)
  #carrying capacity is 80 assuming there are unlimited resources

# What if we make species 2 a better competitor? 
  # I've also increase tmax to give the interaction more time to play out.
compLV(n01=10, n02=10,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.3, tmax=400)
  # Now species 2 'wins' if given enough time
    #species 1 should do better than species 1 now
    #species 1 has a greater growth rate, but the longer the timescale goes on, species two outcompetes and gets the higher population - represents succession like in plants

# What if species two goes back to being a weak competitor but has a much larger initial population? Say, 10x sp1?
  # This takes even longer to reach an equilibrium.
compLV(n01=10, n02=100,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=600)
  # Notice the initial dip in species 1 numbers? This was a large enough population to temporarily out-compete species 1.
    #population 1 is bad - we are also way beyond K so the population is way too large to support iteself

# If 10x the population of sp2 could temporarily win, what about 50x?
compLV(n01=10, n02=500,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=800)
  # Sp1 still recovers eventually, but it's a much longer time scale.

# What about 100x?!? EVEN MORE MUAHAHAHA
compLV(n01=10, n02=1000,r1=0.05, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=800)
  # Finally a number that swamps sp1 and causes their extirpation/extinction.
    #timescale is a LOTTT longer - one sp poor competitor but overtakes another one with population in a short amount of time (like locusts, plankon blooms, etc)
    #the better competitor will go extinct eventually


# What if one is a better competitor, but the other has a faster growth rate?
compLV(n01=10, n02=10,r1=0.01, r2=0.03, k1=80, k2=50, alfa=1.2, beta=0.5, tmax=1000)
  # We can see this time that sp1 still eventually wins, but the process occurs on a much longer time scale.
    #still haven't hit an equilibrium
    #one sp better competitor and one with a higher pop and the higher pop substitutes for competition 


# Adjusting these parameters to model natural patterns is one way to begin generating questions,
  # and think through potential mechanisms, surrounding dynamics of real populations.

# In this week's assignment you use trial and error to identify potential causes of a predator-prey relationship
  # from real plankton population data.








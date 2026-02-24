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

# Load the "anytime" and "ggplot2" packages to complete this week's assignment.
library("anytime")
library("ggplot2")

# Read the "Plankton_move_average" CSV in from GitHub. 
# These are data from the Great Lakes Environmental Research Laboratory plankton sampling.
LM_wd(repo="R4Eco_2026", folder="week7")
data <- read.csv("Plankton_move_average.csv")
head(data)

#Used the following lines to format the date and remove NAs from the dataset:
data$Date <- as.Date(data$Date, origin = "0001-01-01") # Setting values to "day zero".
data <- na.omit(data)

#Plot these population data over time with the following code:
ggplot(data)  +
  xlab("Numeric Date") + ylab("Density Individuals")+
  geom_line(data=data, aes(Date, D.mendotae), color="black", alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, LimncalanusF+LimncalanusM), color="orange",  alpha = 0.7, size=1)+ # adding males and females together, hint: this is actually spelled Limnocalanus
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  geom_line(data=data, aes(Date, Bythotrephes), color="sky blue",  alpha = 0.7, size=1)+
  theme_bw() 

# Export this plot to have on hand for reference in the next section of the assignment (and upload with your script). (8 pts)

# (1) - Which species is most likely to be r-selected prey and which its primary predator? (2 pts)
# What is one relationship the third species MIGHT have to the first two? (2 pts)
  #The species that is most likely to be r-selected prey is D.mendotae, and its primary predator is LimncalanusF LimncalanusM. One relationship that Bythotrephes has with LimncalanusF LimncalanusM, is that they are both predators, so they have competition over prey with one another. The relationship that Bythotrephes has with D.mendotae is that D.mendotae is the prey of Bythotrephes.

      ###WAIT WHAT IS THE THE OTHER LIMNCALANUS?????? THERE ARE TWO###

#Now copy/paste in the Lotka-Volterra function, plotting script, and load the "deSolve" package from the tutorial:
library(deSolve)

LotVmod <- function (Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

      ###WAIT THIS EQUATION???###

# (2) - What do alpha, beta, gamma, and delta represent in this function? (4 pts)
  #Alpha represents the population growth rate of prey.
  #Beta represents predation rate.
  #Gamma represents prey consumption rate for population stability.
  #Delta represents prey consumption rate when predators die off.

    ###DOUBLE CHECK ALL OF THESE###

# (3) - By only changing values for alpha, beta, gamma, and/or delta
# change the default parameters of the L-V model to best approximate the relationship between Limncalanus and D.mendotae, assuming both plots are on the same time scale.
Pars <- c(alpha = 4, beta = 0.5, gamma = 0.3, delta = 0.7)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Limncalanus", "D.mendotae"), lty = c(1,2), col = c(1,2), box.lwd = 0)

    ###DOES THIS LOOK GOOD ENOUGH??? IDK###

# What are the changes you've made to alpha, beta, gamma, and delta from the default values; and what do they say in a relative sense about the plankton data? (4 pts)
  #I kept the alpha and beta values the same, changed gamma from 0.2 to 0.3, and changed delta from 0.6 to 0.7.

    #ASK IF IT WAS OKAY TO ONLY HAVE CHANGED TWO THINGS?#
      ###ALSO ANSWER THE OTHER PART TO THIS QUESTION###

# Are there other paramenter changes that could have created the same end result? (2 pts)
    ###WHAT DOES THIS MEAN???###

# Export your final L-V plot with a legend that includes the appropriate genus and/or species name as if the model results were the real plankton data, 
# and upload with your script. (hint - remember which one is the predator and which is the prey) (8 pts)

Pars <- c(alpha = 4, beta = 0.5, gamma = 0.2, delta = 0.6)
State <- c(x = 10, y = 10)
Time <- seq(0, 100, by = 1)
out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, times = Time))
matplot(out[,-1], type = "l", xlab = "time", ylab = "population")


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

# Load the packages from this week's tutorial, aka vignette
  # https://github.com/Team-FRI/dbfishR
#We looked at brook trout population demographics in relationship to water quality and stream flashiness.
  #change one species name at the start of the vignette

#1: Give two specific conclusions you can make from these patterns. (4 pts)
  #One specific conclusion that I can make from these patterns is that both increased stream flashiness and increased specific conductivity of the water cause more brook trout offspring to be produced. The specific conductivity one is less accurate though because of the visibly wide confidence interval compared to the stream flashiness plot.
    #This means that brook trout prefer to have offspring in flashy streams with a lot of conductivity.
  #A second specific conclusion I can make from these patterns is that there are less brook trout offspring produced with higher alkalinity in the water. The confidence interval is pretty wide for this one, meaning that it is not very accruate.
    #This means that brook trout do not like to have offspring in streams that are really satly.

      ###DID I INTERPRET THESE RIGHT???###

#2: Rerun this analysis with either (a) a different metric of brook trout populations or a different species from the database. (6 pts)
#brown trout
pkgs <- installed.packages()
if (!('devtools' %in% pkgs)) { install.packages('devtools') }
if ('dbfishR' %in% pkgs) { remove.packages('dbfishR') }

devtools::install_github(repo = 'Team-FRI/dbfishR', upgrade = 'never')

library(dbfishR)

sites <- get_sites()
events <- get_events()
events_meta <- merge(sites, events[,c("SiteCode","EventCode","WaterTemp","pH","SpecCond","Alk","DO")])
events_meta$year <-substring(as.character(events_meta$EventCode),1,4)

fish_rec <- get_fish_records()

#yum brownies#
brownie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(brownie_count)[2] <- "TotalCount"
small_brownie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm < 100 & Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(small_brownie_count)[2] <- "SmallCount"
big_brownie_count <- aggregate(ID~EventCode, data = subset(fish_rec, Length_mm > 99 & Species == "Brown Trout" & Pass == "Pass 1"), FUN = length)
colnames(big_brownie_count)[2] <- "BigCount"

df_list <- list(brownie_count,small_brownie_count, big_brownie_count)
all_brownies <- Reduce(function(x, y) merge(x,y, all= TRUE), df_list)

all_brownies$SmallCount[is.na(all_brownies$SmallCount)] <- 0 #this allows the replace NA below to only take care of 100% YOY NAs
all_brownies$YOYRatio <- all_brownies$SmallCount/(all_brownies$BigCount+all_brownies$SmallCount)
all_brownies$YOYRatio[is.na(all_brownies$YOYRatio)] <- 1 #NAs are 100% YOY.

brownie_events <- merge(all_brownies, events_meta)

install.packages("dataRetrieval")
library(dataRetrieval)

HUC6 <- "020501"#North Branch Susquehanna
HUC_list <-paste(rep(HUC6,10), seq(0, 9, length.out = 10), sep="0")#To do a full HUC6 at once, just pick your HUC6 and auto-populate the subwatersheds (only works up to 9 HUC8 in a HUC6)

gage_df <- readNWISdata(huc = HUC_list, parameterCd = "00060", startDate = "2010-01-01", endDate = "2020-12-31")

devtools::install_github(repo = 'leppott/ContDataQC', force = TRUE)
library(ContDataQC)

gage_df$year <- sapply(strsplit(as.character(gage_df$dateTime), "-"),"[[",1)#Create year to get annual R-B index

R_B_HUC <- aggregate(X_00060_00003~year+site_no, data = gage_df, FUN = RBIcalc)#Aggregate by year and site w/in the HUC - designed to work on a single vector at a time - just like max, min, mean sdtv, etc
colnames(R_B_HUC)[3] <- "RBI" #rename column

stations_meta <- readNWISsite(unique(R_B_HUC$site_no))

medium_stations <- subset(stations_meta, drain_area_va > 10 & drain_area_va < 100)#now we brought it to 10 samples, a new, much smaller dataset

library(sf)
medium_stations_so <- st_as_sf(medium_stations,coords = c("dec_lat_va", "dec_long_va"))

events_so <- st_as_sf(brownie_events[!is.na(brownie_events$SiteLon),], coords = c("SiteLat","SiteLon"))#remove NAs to create spatial object

fish_flow_tmp <- st_join(events_so, medium_stations_so, join = st_nearest_feature) #the spatial joint

#Spatial join
library(nngeo)
#distances are in degrees
fish_flow_tmp$dist <- unlist(st_nn(events_so, medium_stations_so, returnDist = T)$dist)
fish_flow_tmp <- subset(fish_flow_tmp, dist < 0.5)

fish_flow <- merge(fish_flow_tmp, R_B_HUC, by = c("year", "site_no"))

mod <- lm(TotalCount~RBI, data = fish_flow)
summary(mod)

mod2 <- lm(BigCount~RBI, data = fish_flow)
summary(mod2)

mod3 <- lm(SmallCount~RBI, data = fish_flow)
summary(mod3)

mod4 <- lm(YOYRatio~RBI, data = fish_flow)
summary(mod4)

library(itsadug)
plot(mod4$residuals)#residuals from the YOYRatio lm() above

library(mgcv)
gam.mod <- gam(YOYRatio~RBI, data = fish_flow, na.action = na.omit, method = "REML")#RBI only
summary(gam.mod)
AIC(gam.mod)

plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)

gam.mod <- gam(YOYRatio~RBI+Alk, data = fish_flow, na.action = na.omit, method = "REML")#RBI + alkalinity
summary(gam.mod)
AIC(gam.mod)

par(mfrow=c(1,2)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")

gam.mod <- gam(YOYRatio~RBI+Alk+SpecCond, data = fish_flow, na.action = na.omit, method = "REML")#RBI, alk, and specific conductivity
summary(gam.mod)
AIC(gam.mod)

par(mfrow=c(1,3)) 
plot_smooth(gam.mod, view="RBI", rm.ranef=FALSE)
plot_smooth(gam.mod, view="Alk", rm.ranef=FALSE, ylab = "", xlab = "Alkalinity")
plot_smooth(gam.mod, view="SpecCond", rm.ranef=FALSE, ylab = "", xlab = "Specific Conductivity")

#3: How do the results of your analysis compare to the vignette? (5 pts)
  #These results only slightly differ compared to the vignette analysis. 
  #With a higher stream flashiness, there are more brown trout offspring produced, but the confidence interval is slightly larger than that of the brook trout's for stream flashiness, indicating that it is less accurate for the brown trout.
  #With a higher alkalinity, there are less brown trout offspring produced, with a much larger confidence interval than that of the brook trout, indicating that, again, there is less accuracy here.
  #With a higher specific conductivity, there are more brown trout offspring produced, with, again, a much larger confidence interval, and the slope is less steep than that of the brook trout plot.
    #So, more brown trout offspring are produced with a higher flashiness and higher specific conductivity, and less offspring are produced with higher alkalinity, or saltiness in the water. The confidence intervals for all of these are wider than all of the brook trout ones, indicating less accuracy to the data.

    ###AM I SUPPOSED TO LOOK AT JUST THE PLOTS FOR THIS ONE?

#4: For your final project you'll need to find two separate data sources to combine similar to the process here.
  #In prep for that, find one data source to compare with either the data in dbfishR OR DataRetrieval. (5 pts)
  #Read data from that source into your script. (5 pts)
  #Create any analysis of your choice that combines the two data sources, this can be as simple as a linear model. (5 pts)
###COMPARE THIS ONE WITH DB FISH###
    #OTHER DATA SET#



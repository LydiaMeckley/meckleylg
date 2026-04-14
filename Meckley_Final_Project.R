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


                  ###Final project due: Thursday April 30th at 8:00am###



###PACKAGES###
library(rgbif)
library(dplyr)
library(readxl)
library(vegan)
library(fitdistrplus)
library(logspline)

###DATASETS###
LM_wd(repo="meckleylg", folder="Final Project")
data <- read.csv("UK_Water_Data.csv")


###HOW TO FIX THE SEAL DATA ACCORDING TO AI BUT ASK HOW TO ACTUALLY DO IT###
library(tidyr)

df_wide <- df %>%
  pivot_wider(
    names_from = species, 
    values_from = count
  )
df_wide <- df %>%
  pivot_wider(
    names_from = species,
    values_from = count,
    values_fill = 0
  )



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

# Install package
remotes::install_github("bnosac/image", subdir = "image.darknet")

# Load package
library(image.darknet)

# Set working directory 
LM_wd(repo="R4Eco_2026", folder="Week11")

# Detect - where are the objects?
#First select the model for identifying objects:
yolo_tiny_voc <- image_darknet_model(type = 'detect', 
                                     model = "tiny-yolo-voc.cfg", 
                                     weights = system.file(package="image.darknet", "models", "tiny-yolo-voc.weights"), 
                                     labels = system.file(package="image.darknet", "include", "darknet", "data", "voc.names"))

#Then test against specific images:
image_darknet_detect(file = "C:/GitHub/R4Eco_2026/Week11/BearSalmon2.jpg",
                     object = yolo_tiny_voc)

image_darknet_detect(file = "C:/GitHub/R4Eco_2026/Week11/BearSalmon.jpg",
                     object = yolo_tiny_voc,
                     threshold = .05)

image_darknet_detect(file = "C:/GitHub/R4Eco_2026/Week11/Bird.jpg",
                     object = yolo_tiny_voc)

image_darknet_detect(file = "C:/GitHub/R4Eco_2026/Week11/Urban.jpg",
                     object = yolo_tiny_voc)


#Lowering the threshold for confidence in detection:
image_darknet_detect(file = "C:/GitHub/R4Eco_2026/Week11/Urban.jpg",
                     object = yolo_tiny_voc,
                     threshold = .05)


# Classify - what is the most likely critter?
#Notice a better answer, but there is no confidence from the model.
tiny_model <- image_darknet_model(type = "classify",
                                  model = "tiny.cfg",
                                  weights = system.file(package = "image.darknet", "models", "tiny.weights"),
                                  labels = system.file(package = "image.darknet", "include", "darknet", "data", "imagenet.shortnames.list"))
image_darknet_classify(file = "C:/GitHub/R4Eco_2026/Week11/BearSalmon.jpg",
                       object = tiny_model)

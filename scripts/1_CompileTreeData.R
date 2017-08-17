# --------------------------------------
# Compiling all tree and tree ring data into a single data frame
# Sierra Lopezalles, slopezal@caltech.edu
# July 30, 2017
# --------------------------------------
# --------------------------------------

# --------------------------------------
# CHECK BEFORE RUNNING!
# --------------------------------------

# Setting which part of the ring data to use
ringwidth.part <- "latewood width"
#ringwidth.part <- "ring width"

# Setting name of the file to be exported
#file.name <- "RingData"
file.name <- "RingData_Latewood"

# Working directory path
path.wd <- "~/Github/URF2017/" # Sierra
# path.wd <- "~/Desktop/Research/URF2017_Lopazalles/" # Christy

# Path to Tree Cencus 2017 folder
path.ew <- "~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/" # Sierra
# path.ew <- "~/Desktop/Research/EastWoods-MonitoringPlots/TreeCensus_2017/data/" # Christy

# Path to East Woods Google Drive folder
path.google <- "C:/Users/macmo/Google Drive/Morton Summer 2017/East Woods/" # Sierra
# path.google <- "~/Google Drive/East Woods" # Christy

# --------------------------------------

library(dplR)
library(car)

# Setting a working directory
setwd(path.wd)

# --------------------------------------
# Reading in data
# --------------------------------------

# Tree survey data to get species information

survey1 <-read.csv(file.path(path.ew, "TreeData-raw_data.csv"), na.strings="")
survey2 <-read.csv(file.path(path.ew, "URF_2017_AdditionalOakData-raw_data.csv"), na.strings="", colClasses=c("Tag"="character"))

# Saving the raw ring files 
rawringfiles <- Sys.glob(file.path(path.google, 'Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/*.rwl'))

# Setting a path to the raw ring width folder
tr.path <- file.path(path.google, "URF_2017_Rollinson/RawRingWidths")

# -------------------------------------
# Data wrangling
# -------------------------------------

# Data wrangling to allow for the rbind

names(survey1)[6] <- "Species"
survey1$Plot <- recode(survey1$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")
survey1[,"Tag"] <- as.character(survey1[,"Tag"])
survey2[,"Tag"] <- as.character(survey2[,"Tag"])

# Fixing the weird Tilia
survey1$Species <- as.factor(substr(survey1$Species, 1, 4))

# Combining survey information from both the permanent and temporary plots
survey <- rbind(survey1[,c("Plot","Tag","Species", "DBH")], survey2[,c("Plot","Tag","Species", "DBH")])                     

# Splitting the raw ring file name into lists

rw.names <- dir(tr.path)
trw.names <- rw.names[grep(ringwidth.part, rw.names)]
trw.names.split <- strsplit(trw.names, "-")

# -------------------------------------
# Combining data
# -------------------------------------

# Initializing master data file with the first ring width file
rw.long <- read.rwl(file.path(tr.path, trw.names[1]))

# Naming columns

aa <- trw.names.split[[1]]

# Creating a the DBH data frame for bai.out function
data.tmp <- data.frame(aa[3])
data.tmp$DBH <- survey[survey$Tag==aa[3],"DBH"] * 10 # Converting DBH to mm
names(rw.long) <- aa[3]

# Converting to basal area increment
bai.tmp <- bai.out(rw.long, data.tmp) 

# Extracting ring width data
rw.long <- data.frame(rw.long) # Extract just the actual data
names(rw.long) <- "RingWidth"
rw.long$Tag <- aa[3]
rw.long$Core <- paste(aa[c(3, 4, 6)], collapse="-")
rw.long$Year <- as.numeric(rownames(rw.long))
names(bai.tmp) <- "BAI"
rw.long$BAI <- bai.tmp$BAI

# Repeating for all other files 

for(i in 2:length(trw.names)){
  
  # Extracting ring width data
  file.tmp <- read.rwl(file.path(tr.path, trw.names[i]))
  
  # Storing tag number
  aa <- trw.names.split[[i]]
  
  # Creating a data frame to allow for bai.out
  data.tmp <- data.frame(aa[3])
  data.tmp$DBH <- survey[survey$Tag==aa[3],"DBH"] * 10 # Converting DBH to mm
  names(file.tmp) <- aa[3]
  names(data.tmp)[1] <- aa[3]
  
  # Converting to basal area increment
  bai.tmp <- bai.out(file.tmp, data.tmp) 
  
  # Renaming columns and adding BAI
  names(bai.tmp) <- "BAI"
  file.tmp <- data.frame(file.tmp)
  names(file.tmp) <- "RingWidth"
  file.tmp$Tag <- aa[3]
  file.tmp$Core <- paste(aa[c(3, 4, 6)], collapse="-")
  file.tmp$Year <- as.numeric(rownames(file.tmp))
  file.tmp$BAI <- bai.tmp$BAI
  
  # Appending to full file
  rw.long <- rbind(rw.long, file.tmp) 
}

# Merging the ring width data to the survey data
data.all <- merge(survey, rw.long, "Tag")

# -------------------------------------
# Exporting data for analysis
# -------------------------------------

write.csv(data.all, file.path("data",file.name))

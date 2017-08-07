# --------------------------------------
# Compiling all tree and tree ring data into a single data frame
# Sierra Lopezalles, slopezal@caltech.edu
# July 30, 2017
# --------------------------------------
# --------------------------------------

# --------------------------------------
# CHECK BEFORE RUNNING!
# --------------------------------------

# Working directory path
path.wd <- "~/Github/URF2017/" # Sierra
# path.wd <- "~/Desktop/Research/URF2017_Lopazelles/" # Christy

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
tr.path <- file.path(path.google, "Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/")

# Climate data
climate.month <- read.csv("data/PRISM_provisional_4km_189501_201706_41.8156_-88.0437.csv")

# Fire data
fire.data <- read.csv(file.path(path.google,"URF_2017_Rollinson/URF2017_BurnInfo.csv"))

# -------------------------------------
# Data wrangling
# -------------------------------------

# Data wrangling to allow for the rbind

names(survey1)[6] <- "Species"
survey1$Plot <- recode(survey1$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")
survey1[,"Tag"] <- as.character(survey1[,"Tag"])
survey2[,"Tag"] <- as.character(survey2[,"Tag"])

# Combining survey information from both the permanent and temporary plots
survey <- rbind(survey1[,c("Plot","Tag","Species", "DBH")], survey2[,c("Plot","Tag","Species", "DBH")])                     

# Splitting the raw ring file name into lists

rw.names <- dir(tr.path)
trw.names <- rw.names[grep('ring width', rw.names)]
trw.names.split <- strsplit(trw.names, "-")

# Fixing column names
names(climate.month) <- c("Date", "Precip", "Temp")

# Averaging climate data by year

climate.year <- aggregate(climate.month[,c("Precip","Temp")],
                         list(substr(climate.month$Date,1,4)),
                         mean)

# Averaging climate data by summer

month.summer <- c("06", "07", "08")

climate.summer <- aggregate(climate.month[substr(climate.month$Date,6,8) %in% month.summer, c("Precip","Temp")],
                          list(substr(climate.month[substr(climate.month$Date,6,8) %in% month.summer,1],1,4)),
                          mean)

# Fixing column names

names(climate.year)[1] <- "Year"
names(climate.summer)[1] <- "Year"

# In the meantime while waiting on more info
# Need to fix the 2013's

fire.data <- fire.data[!is.na(fire.data[,"Burn_Date"]),]

fire.data$Burn_Date <- substr(fire.data$Burn_Date, 1, 4)

# Counting burns per plot
# Will make burns per decade when they actually span more than one decade

fire.count <- aggregate(fire.data$Burn_Date,
                             list(fire.data$Corner),
                             length)

names(fire.count) <- c("Plot", "FireCount")

# Removing the hyphen to allow a merge

fire.count$Plot <- paste0(substr(fire.count$Plot, 1, nchar(paste(fire.count$Plot))-4), substr(fire.count$Plot, nchar(paste(fire.count$Plot))-2,nchar(paste(fire.count$Plot))))

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

rownames(bai.tmp) <- NULL
names(bai.tmp) <- "BAI"

rw.long$BAI <- bai.tmp


# Repeating for all other files 

for(i in 2:length(trw.names)){
  file.tmp <- read.rwl(file.path(tr.path, trw.names[i]))
  aa <- trw.names.split[[i]]
  data.tmp <- data.frame(aa[3])
  data.tmp$DBH <- survey[survey$Tag==aa[3],"DBH"] * 10 # Converting DBH to mm
  names(file.tmp) <- aa[3]
  bai.tmp <- bai.out(file.tmp, data.tmp) # Converting to basal area increment
  file.tmp <- data.frame(file.tmp)
  names(file.tmp) <- "RingWidth"
  file.tmp$Tag <- aa[3]
  file.tmp$Core <- paste(aa[c(3, 4, 6)], collapse="-")
  file.tmp$Year <- as.numeric(rownames(file.tmp))
  
  rownames(bai.tmp) <- NULL
  
  file.tmp$BAI <- bai.tmp
  
  rownames(file.tmp) <- NULL
  rownames(rw.long) <- NULL
  
  rw.long <- rbind(rw.long, file.tmp) 
}

# Merging the ring width data to the survey data
data.some <- merge(survey, rw.long, "Tag")

# Merging climate data to master data list
data.most <- merge(data.some, climate.summer, "Year", all=T) # Lets not get rid of years without climate quite yet

# Saving Tag and Core as factors

data.most$Tag <- as.factor(data.most$Tag)
data.most$Core <- as.factor(data.most$Core)

# Fire Stuff

data.most <- merge(data.most, fire.count, "Plot", all=TRUE)

data.all <- data.most

# data.all <- data.most + pith years
# data.all$Age <- data.all$Year - data.all$pith.year

# -------------------------------------

# Still need pith year, age, treatment (need fire data)


# write.csv(data.all, file.path("data","CombinedData"))

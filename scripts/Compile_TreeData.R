# --------------------------------------
# Compiling all tree and tree ring data into a single data frame
# Sierra Lopezalles, slopezal@caltech.edu
# July 30, 2017
# --------------------------------------
# --------------------------------------

library(dplR)
library(car)

# Setting a working directory

setwd("~/Github/URF2017/")

# --------------------------------------
# Reading in data
# --------------------------------------

# Pulling in tree survey data to get species information

survey1 <-read.csv("~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/TreeData-raw_data.csv", na.strings="")
survey2 <-read.csv("~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/URF_2017_AdditionalOakData-raw_data.csv", na.strings="", colClasses=c("Tag"="character"))

# Saving the raw ring files 

rawringfiles <- Sys.glob('~/Google Drive/Morton Summer 2017/East Woods/Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/*.rwl')

# Setting a path to the raw ring width folder

tr.path <- "~/Google Drive/Morton Summer 2017/East Woods/Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/"

# Grabbing climate data

climate.month <- read.csv("~/GitHub/URF2017/data/PRISM_provisional_4km_189501_201706_41.8156_-88.0437.csv")

# -------------------------------------
# Data wrangling
# -------------------------------------

# Data wrangling to allow for the rbind

names(survey1)[6] <- "Species"
survey1$Plot <- recode(survey1$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")
survey1[,3] <- as.character(survey1[,3])
survey2[,3] <- as.character(survey2[,3])

# Combining survey information from both the permanent and temporary plots

survey <- rbind(survey1[,c(2,3,6)], survey2[,c(2,3,4)])                     

# Splitting the raw ring file name into lists

rw.names <- dir(tr.path)
trw.names <- rw.names[grep('ring width', rw.names)]
trw.names.split <- strsplit(trw.names, "-")

# Fixing column names

names(climate.month) <- c("Date", "Precip", "Temp")

# Averaging climate data by year

climate.year <- aggregate(climate.month[,c(2,3)],
                         list(substr(climate.month$Date,1,4)),
                         mean)



# Averaging climate data by summer

month.summer <- c("06", "07", "08")

climate.summer <- aggregate(climate.month[substr(climate.month$Date,6,8) %in% month.summer, c(2,3)],
                          list(substr(climate.month[substr(climate.month$Date,6,8) %in% month.summer,1],1,4)),
                          mean)

# Fixing column names

names(climate.year)[1] <- "Year"
names(climate.summer)[1] <- "Year"

# -------------------------------------
# Combining data
# -------------------------------------

# Initializing master data file with the first ring width file

rw.long <- read.rwl(file.path(tr.path, trw.names[1]))

# Naming columns

names(rw.long) <- "RingWidth"
aa <- trw.names.split[[1]]
rw.long$Tag <- aa[3]
rw.long$Core <- paste(aa[c(3, 4, 6)], collapse="-")
rw.long$Year <- rownames(rw.long)
rownames(rw.long) <- c()

# Repeating for all other files 

for(i in 2:length(trw.names)){
  file.tmp <- read.rwl(file.path(tr.path, trw.names[i]))
  data.frame(file.tmp)
  names(file.tmp) <- "RingWidth"
  aa <- trw.names.split[[i]]
  file.tmp$Tag <- aa[3]
  file.tmp$Core <- paste(aa[c(3, 4, 6)], collapse="-")
  file.tmp$Year <- rownames(file.tmp)
  rw.long <- rbind(rw.long, file.tmp)
  
}

# Merging the ring width data to the survey data

data.some <- merge(survey, rw.long, "Tag")

# Merging climate data to master data list

data.most <- merge(data.some, climate.summer, "Year")

# -------------------------------------

# Still need pith year, age, treatment (need fire data)

# Merge pith year

#data.all$Age <- data.all$Year - data.all$pith.year


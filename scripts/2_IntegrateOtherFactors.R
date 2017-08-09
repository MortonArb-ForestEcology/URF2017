# --------------------------------------
# Integrating other factors into the tree ring data
# Sierra Lopezalles, slopezal@caltech.edu
# August 9, 2017
# --------------------------------------
# --------------------------------------

# --------------------------------------
# CHECK BEFORE RUNNING!
# --------------------------------------

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

# Tree ring data
trw.data <- read.csv(file.path(path.wd,"data/RingData"))

# Climate data
climate.month <- read.csv("data/PRISM_provisional_4km_189501_201706_41.8156_-88.0437.csv")

# Fire data
fire.data <- read.csv(file.path(path.google,"URF_2017_Rollinson/URF2017_BurnInfo.csv"))

# -------------------------------------
# Data wrangling
# -------------------------------------

# Removing weird first column
trw.data <- trw.data[,2:length(trw.data)]

trw.data$Genus <- substr(trw.data$Species, 1, 2)

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

# Fixing fire data

# Removing month and day from date
fire.data$Burn_Date <- substr(fire.data$Burn_Date, 1, 4)

# Resaving as numeric
fire.data$Burn_Date <- as.numeric(as.character(fire.data$Burn_Date))

# Fixing the 2013's
fire.data[fire.data$NOTES == "2013 unsure on date" & !is.na(fire.data$NOTES),"Burn_Date"] <- 2013

# Removing all rows without a burn date
fire.data <- fire.data[!is.na(fire.data[,"Burn_Date"]),]

# Fixing column name

names(fire.data)[9] <- "Plot"

# Removing the hyphen to allow a merge

fire.data$Plot <- paste0(substr(fire.data$Plot, 1, nchar(paste(fire.data$Plot))-4), substr(fire.data$Plot, nchar(paste(fire.data$Plot))-2,nchar(paste(fire.data$Plot))))

# Merging climate data
trw.data <- merge(trw.data, climate.summer, "Year", all=T) 

# Only looking at the tree ring data from 2000 to present 
trw.data <- trw.data[trw.data$Year >= 2000,]


# -------------------------------------
# Fire Analysis
# -------------------------------------

# -------------------------------------
# Counting burns per plot

fire.count <- aggregate(fire.data$Burn_Date,
                        list(fire.data$Plot),
                        length)

names(fire.count) <- c("Plot", "FireCount")

# Merging fire counts
trw.data <- merge(trw.data, fire.count, "Plot", all=TRUE)

# Setting all NA fire counts to zero
trw.data[is.na(trw.data$FireCount),"FireCount"] <- 0

# -------------------------------------
# Time since last burn, with 10 years as max

for (i in 1:nrow(trw.data)){
  fire.date.tmp <- fire.data[fire.data$Plot == trw.data[i,"Plot"],"Burn_Date"]
  fire.date.tmp <- sort(fire.date.tmp, decreasing = TRUE)
  
  if(length(fire.date.tmp) == 0){
    last.burn <- trw.data[i,"Year"] - 10
  }
  
  for (a in fire.date.tmp){
    if (a <= trw.data[i,"Year"]){
      last.burn <- a
      break
    } else {
      last.burn <- trw.data[i,"Year"] - 10
    }
  }
  
  trw.data[i, "LastBurn"] <- last.burn
}

# Measuring time since last burn
trw.data$SinceBurn <- as.numeric(trw.data$Year) - trw.data$LastBurn

# -------------------------------------
# Count burns in the past 3 years

for (i in 1:nrow(trw.data)){
  if (trw.data[i, "SinceBurn"] <= 3){
    fire.date.tmp <- fire.data[fire.data$Plot == trw.data[i,"Plot"],"Burn_Date"]
    fire.date.tmp <- sort(fire.date.tmp, decreasing = TRUE)
    burn.count <- 0
    
    for (a in fire.date.tmp){
      if (trw.data[i,"Year"] - a <= 3 & trw.data[i,"Year"] - a >= 0 ){
        burn.count <- burn.count + 1
      }
    }
  } else {burn.count <- 0}
  trw.data[i, "BurnIn3"] <- burn.count
}

# ------------------------------------

# Saving Tag and Core as factors
trw.data$Tag <- as.factor(trw.data$Tag)
trw.data$Core <- as.factor(trw.data$Core)

# -------------------------------------
# Data export
# -------------------------------------

write.csv(trw.data, file.path("data","CombinedData"))


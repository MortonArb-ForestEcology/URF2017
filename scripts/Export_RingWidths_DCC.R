# --------------------------------------
# Organizing and reformatting tree ring data
# Sierra Lopezalles, slopezal@caltech.edu
# July 27, 2017
# --------------------------------------
# --------------------------------------

library(dplR)

# Setting a working directory

setwd("~/Github/URF2017/")

# Pulling all raw tree ring files into one object
# CHANGE HERE IF USING ANOTHER FOLDER

rawringfiles <- Sys.glob('~/Google Drive/Morton Summer 2017/East Woods/Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/*.rwl')

# Grabbing all the species data

permtree.data <-read.csv(file = "~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/TreeData-raw_data.csv", na.strings = "", colClasses=c("Tag"="character"))
temptree.data <-read.csv("~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/URF_2017_AdditionalOakData-raw_data.csv", na.strings="", colClasses=c("Tag"="character"))

names(permtree.data)[6] <- "Species"
permtree.data[,3] <- as.character(permtree.data[,3])
temptree.data[,3] <- as.character(temptree.data[,3])

tree.species <- rbind(permtree.data[,c(3,6)], temptree.data[,c(3,4)])                     
tree.species$Species <- as.factor(substr(tree.species$Species, 1, 2))

# Setting a path to the raw ring width folder

tr.path <- "~/Google Drive/Morton Summer 2017/East Woods/Rollinson_Monitoring/Data/Tree Cores/RawRingWidths/"

# Splitting the file name into lists

rw.names <- dir(tr.path)
trw.names <- rw.names[grep('ring width', rw.names)]
trw.names.split <- strsplit(trw.names, "-")

# --------------------------------------
# Combining and renaming ring width files
# --------------------------------------

# Initializing the master list with the first ring width set

# Reading in the first file
files.all <- read.rwl(file.path(tr.path, rw.names[1])) 

# Storing the parts of the name
aa <- trw.names.split[[1]]  

# Combining the parts of the name with species in the proper order
nam <- paste0(as.character(tree.species[tree.species$Tag==aa[3],2]), paste(aa[c(3, 4, 6)], collapse=""))

# Assigning the name to the file
names(files.all) <- nam

# Repeating for all other files 

for(i in 2:length(trw.names)){
  file.tmp <- read.rwl(file.path(tr.path, trw.names[i]))
  aa <- trw.names.split[[i]]
  nam <- paste0(as.character(tree.species[tree.species$Tag==aa[3],2]), paste(aa[c(3, 4, 6)], collapse=""))
  names(file.tmp) <- nam
  
  # Appending each file to the master list
  files.all <- combine.rwl(files.all, file.tmp)
}

# CHECK NAME

write.rwl(files.all, "oaks_master_trw.rwl", long.names=TRUE)


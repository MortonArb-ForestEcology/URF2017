# --------------------------------------
# Organizing and reformatting tree ring data
# Sierra Lopezalles, slopezal@caltech.edu
# July 27, 2017
# --------------------------------------
# --------------------------------------

# --------------------------------------
# CHECK BEFORE RUNNING!
# --------------------------------------

# Export info
file.name <- "master2_trw.rwl"

# Path to tree ring files
tr.path <- "C:/Users/macmo/Google Drive/Morton Summer 2017/East Woods/URF_2017_Rollinson/RawRingWidths" #Sierra
# tr.path <- "~/Google Drive/East Woods/Rollinson_Monitoring/Data/Tree Cores/RawRingWidths" #Christy

# Working directory path
path.wd <- "~/Github/URF2017/" #Sierra
#path.wd <- "~/Desktop/Research/URF2017_Lopazelles/" #Christy

# Path to Tree Census 2017 data
path.ew <- "~/Github/EastWoods-MonitoringPlots/TreeCensus_2017/data/" # Sierra
# path.ew <- "~/Desktop/Research/EastWoods-MonitoringPlots/TreeCensus_2017/data/" # Christy


# --------------------------------------

library(dplR)

# Setting a working directory
setwd(path.wd)

# Pulling all raw tree ring files into one object
rawringfiles <- Sys.glob(file.path(tr.path, "*.rwl"))

# Grabbing all the species data

permtree.data <-read.csv(file = file.path(path.ew, "TreeData-raw_data.csv"), na.strings = "", colClasses=c("Tag"="character"))
temptree.data <-read.csv(file.path(path.ew, "URF_2017_AdditionalOakData-raw_data.csv"), na.strings="", colClasses=c("Tag"="character"))

names(permtree.data)[6] <- "Species"
permtree.data[,"Species"] <- as.character(permtree.data[,"Species"])
temptree.data[,"Species"] <- as.character(temptree.data[,"Species"])

tree.species <- rbind(permtree.data[,c("Tag","Species")], temptree.data[,c("Tag","Species")])                     
tree.species$Genus <- as.factor(substr(tree.species$Species, 1, 2)) 

# Splitting the file names into lists

rw.names <- dir(tr.path)
trw.names <- rw.names[grep('ring width', rw.names)]
trw.names.split <- strsplit(trw.names, "-")

# --------------------------------------
# Combining and renaming ring width files
# --------------------------------------

# Initializing the master list with the first ring width set

# Reading in the first file
files.all <- read.rwl(file.path(tr.path, trw.names[1])) 

# Storing the parts of the name
aa <- trw.names.split[[1]]  

# Combining the parts of the name with species in the proper order
nam <- paste0(as.character(tree.species[tree.species$Tag==aa[3],"Genus"]), paste(aa[c(3, 4, 6)], collapse=""))

# Assigning the name to the file
names(files.all) <- nam

# Repeating for all other files 

for(i in 2:length(trw.names)){
  file.tmp <- read.rwl(file.path(tr.path, trw.names[i]))
  aa <- trw.names.split[[i]]
  nam <- paste0(as.character(tree.species[tree.species$Tag==aa[3],"Genus"]), paste(aa[c(3, 4, 6)], collapse=""))
  names(file.tmp) <- nam
  
  # Appending each file to the master list
  files.all <- combine.rwl(files.all, file.tmp)
}

# Exporting tree ring widths as one combined file
write.rwl(files.all, file.path("crossdating",file.name), long.names=TRUE)


# --------------------------------------
# Extract fire data for research plots
# Author: Christy Rollinson, crollinson@mortonarb.org
# 2 August 2017
#
# Notes: 
#  - Being done by Christy because Sierra doesn't have access to 
#    the Morton GIS drive
#  - WILL NEED TO BE UPDATED because Kurt is still doing QA/QC on fire files
# --------------------------------------

# --------------------------------------
# Load libraries, define file paths
# --------------------------------------
# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.local <- "~/Desktop/Research/URF2017_Lopazelles/"
path.google <- "~/Google Drive/East Woods/"
path.gis <- "/Volumes/GIS/"
# --------------------------------------


# --------------------------------------
# Loading layers & info
# --------------------------------------
# Load GIS layers
burn <- readOGR(file.path(path.gis, "Collections/Natural Resources Management/Burn", "Burned_Area.shp"))
summary(burn)

# Get table of plot locations
plot.info <- read.csv(file.path(path.google, "URF_2017_Rollinson/URF 2017 Plot Info.csv"))
summary(plot.info)

# Making our plots a shape file so we can make the projection match the Morton data layers
plots.sp <- SpatialPointsDataFrame(coords=plot.info[,c("Longitude", "Latitude")], plot.info,proj4string = CRS("+proj=longlat"))
plots.sp <- spTransform(plots.sp, projection(burn))
summary(plots.sp)
# --------------------------------------


# --------------------------------------
# Extracting the burn info for each point
# --------------------------------------
# Extracting the burn info
plot.burn <- extract(burn, plots.sp)
summary(plot.burn)

# Adding an ID and adding some of our plot info to the burn report
plots.sp$point.ID <- 1:nrow(plots.sp)
summary(plots.sp)

plot.burn <- merge(plot.burn, plots.sp[,c("point.ID", "Corner", "Description", "Type")], all=T)
summary(plot.burn)

write.csv(plot.burn, file.path(path.google, "URF_2017_Rollinson/URF2017_BurnInfo.csv"), row.names=F)
# --------------------------------------

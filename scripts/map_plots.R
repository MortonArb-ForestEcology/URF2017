# --------------------------------------
# Making a map of plot locations for Sierra
# Author: Christy Rollinson, crollinson@mortonarb.org
# 2 August 2017
#
# Note: Being done by Christy because Sierra doesn't have 
#       access to the GIS drive
#
# ------------------
# Layers/Details Requested by Sierra
# ------------------
# 1. Plot Location
#    - Color coded by stand
#    - SHape by type: permanent = square; URF2017 = circle
# 2. Roads
# 3. Trails
# 4. Morton Arb Grid
# ------------------
# --------------------------------------

# --------------------------------------
# Load libraries, define file paths
# --------------------------------------
# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.local <- "~/Desktop/Research/URF2017_Lopazalles/"
path.google <- "~/Google Drive/East Woods/"
path.gis <- "/Volumes/GIS/"
# --------------------------------------


# --------------------------------------
# Loading layers & info
# --------------------------------------
# Load GIS layers
roads <- readOGR(file.path(path.gis, "Collections/Transportation/roads_parking", "circ_veh_rd_2011-2020_ctrln.shp"))
paths <- readOGR(file.path(path.gis, "Collections/Transportation/trails_paths", "paths.shp"))
# morton.grid <- readOGR(file.path(path.gis, "Collections/grid_system", "adjustedgrid.shp"))

# Get table of plot locations
plot.info <- read.csv(file.path(path.google, "URF_2017_Rollinson/URF 2017 Plot Info.csv"))
summary(plot.info)

# Load Sierra's "Combined Info" to get the data she's workign with
burn.dat <- read.csv("../data/CombinedData.csv")
burn.dat <- aggregate(burn.dat$FireCount, by=list(burn.dat$Plot), FUN=max)
names(burn.dat) <- c("Plot2", "n.burn")
summary(burn.dat)

# Merging plot info and burn dat
for(i in 1:nrow(plot.info)){
  # pnow <- paste(strsplit(paste(plot.info$Corner[i]), "-")[[1]], collapse="")
  plot.info[i, "Plot2"] <- paste(strsplit(paste(plot.info$Corner[i]), "-")[[1]], collapse="")
}
plot.info$Plot2 <- as.factor(plot.info$Plot2)
summary(plot.info)

plot.info <- merge(plot.info, burn.dat, all=T)
summary(plot.info)

# Making our plots a shape file so we can make the projection match the Morton data layers
plots.sp <- SpatialPointsDataFrame(coords=plot.info[,c("Longitude", "Latitude")], plot.info,proj4string = CRS("+proj=longlat"))
plots.sp <- spTransform(plots.sp, projection(roads))

# Extracting the coordinates for the coordinate reference system of the rest of the Arb data
plot.info[,c("x", "y")] <- coordinates(plots.sp)
# --------------------------------------


# --------------------------------------
# Make & Save the map!
# --------------------------------------
library(ggplot2)

png(file.path(path.google, "URF_2017_Rollinson/URF2017_Lopazalles_Plots.png"), height=6, width=5, units="in", res=320)
ggplot() +
  geom_path(data=roads, aes(x=long, y=lat, group=group), color="ivory3", size=1) +
  geom_path(data=paths, aes(x=long, y=lat, group=group), color="tan4", size=0.7, linetype="dashed") +
  # geom_path(data=morton.grid, aes(x=long, y=lat, group=group), color="gray50", size=0.5, linetype="solid") +
  geom_point(data=plot.info, aes(x=x, y=y, color=as.factor(n.burn), shape=Type, size=Type)) +
  scale_shape_manual(values=c(15, 19)) + # see ?pch for shapes
  scale_color_manual(values=c("blue2", "green4", "orange3", "red3"), name="# Burns\nsince 2000") +
  scale_size_manual(values=c(5,3.5)) +
  coord_equal(xlim=c(min(plot.info$x)-500, max(plot.info$x)+250), ylim=c(min(plot.info$y)-250, max(plot.info$y)+500), expand=F) +
  theme_bw() +
  guides(color=guide_legend(override.aes = list(size=5))) +
  theme(legend.position="top",
        axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()
        )
dev.off()
# --------------------------------------

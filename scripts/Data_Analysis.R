# --------------------------------------
# Analyzing tree ring width data
# Sierra Lopezalles, slopezal@caltech.edu
# August 5, 2017
# --------------------------------------
# --------------------------------------

# --------------------------------------
# CHECK BEFORE RUNNING!
# --------------------------------------

# Working directory path
path.wd <- "~/Github/URF2017/" # Sierra
# path.wd <- "~/Desktop/Research/URF2017_Lopazelles/" # Christy

# --------------------------------------
# Load libraries, define working directory
# --------------------------------------

# Libraries
library(lubridate); library(ggplot2)
library(mgcv)

# Setting a working directory
setwd(path.wd)

# --------------------------------------
# Reading in data
# --------------------------------------

trw.data <- read.csv(file.path(path.wd,"data/CombinedData.csv"))

# --------------------------------------
# Data Wrangling
# --------------------------------------

# Removing weird first column
trw.data <- trw.data[,2:12]

# Setting all NA fire counts to zero
trw.data[is.na(trw.data$FireCount),"FireCount"] <- 0

# Only looking at the tree ring data from 2000 to present 
# Since missing other burn data
trw.data <- trw.data[trw.data$Year >= 2000,]

# --------------------------------------
# Analysis and graphs
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=RingWidth)) +
  geom_smooth(aes(x=FireCount, y=RingWidth), method="lm")

# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=BAI, color = Species)) +
  geom_smooth(aes(x=FireCount, y=BAI, color = Species), method="lm")

# R2 = .2786
lm.test1 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + Year, data=trw.data)
summary(lm.test1)

# R2 = .3837
lm.test2 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + Year + Plot, data=trw.data)
summary(lm.test2)

hist(trw.data$BAI); abline(v=median(trw.data$BAI), lty="dashed", col="red", lwd=5)
hist(resid(lm.test2)); abline(v=0, lty="dashed", col="red", lwd=5)
plot(resid(lm.test2) ~ predict(lm.test2)); abline(h=0, lty="dashed", col="red", lwd=2)
plot(predict(lm.test2) ~ trw.data$BAI); abline(a=0, b=1, col="red", lty="dashed", lwd=2)

# --------------------------------------

# An example of how to make a better model by building detrending into the model

# R2 = .274
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot), data=trw.data)
summary(gam1)
plot(gam1) 

# R2 = .318
gam2 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag), data=trw.data)
summary(gam2)
plot(gam2)

# GAMM example (with random effects!); replace Plot, Tree & Core with whatever the appropraite column names are

gamm.example <- gamm(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot), random=list(Tag=~1, Core=~1), data=trw.data)

# Error in MEestimate(lmeSt, grps) : 
# Singularity in backsolve at level 0, block 1

# --------------------------------------
# Comparing average growth rates over the 17 year chunk

trw.avg <- aggregate(trw.data$BAI,
                     trw.data[,c("Plot", "Species", "Tag", "Core", "FireCount")],
  
                                        mean)
names(trw.avg)[6] <- "AvgBAI"

ggplot(trw.avg) +
  geom_point(aes(x=FireCount, y=AvgBAI, color = Species)) +
  geom_smooth(aes(x=FireCount, y=AvgBAI, color = Species), method="lm")

# R2 = .4743
lm.test3 <- lm(AvgBAI ~ FireCount*Species + Plot, data=trw.avg)
summary(lm.test3)

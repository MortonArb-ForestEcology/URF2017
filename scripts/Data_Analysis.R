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
# path.wd <- "~/Github/URF2017/" # Sierra
path.wd <- "~/Desktop/Research/URF2017_Lopazelles/" # Christy

# Path to East Woods Google Drive folder
path.google <- "C:/Users/macmo/Google Drive/Morton Summer 2017/East Woods/" # Sierra
# path.google <- "~/Google Drive/East Woods/" # Christy

# --------------------------------------
# Load libraries, define working directory
# --------------------------------------

# Libraries
library(lubridate); library(car); library(ggplot2)

# Setting a working directory
setwd(path.wd)

# --------------------------------------
# Reading in data
# --------------------------------------

trw.data <- read.csv(file.path(path.wd,"data/CombinedData"))

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


lm.test1 <- lm(RingWidth ~ FireCount, data=trw.data)
summary(lm.test1)

lm.test2 <- lm(RingWidth ~ FireCount+Year, data=trw.data)
summary(lm.test2)

lm.test3 <- lm(RingWidth ~ Precip+Temp+FireCount, data=trw.data)
summary(lm.test3)

lm.test4 <- lm(RingWidth ~ Precip+Temp+FireCount+Year, data=trw.data)
summary(lm.test4)

lm.test5 <- lm(RingWidth ~ Precip+Temp+FireCount+Year+Species, data=trw.data)
summary(lm.test5)

# Should improve with BAI rather then ring widths

hist(trw.data$RingWidth); abline(v=median(trw.data$RingWidth), lty="dashed", col="red", lwd=5)
hist(resid(lm.test4)); abline(v=0, lty="dashed", col="red", lwd=5)
plot(resid(lm.test4) ~ predict(lm.test4)); abline(h=0, lty="dashed", col="red", lwd=2)
plot(predict(lm.test4) ~ trw.data$RingWidth[1:846]); abline(a=0, b=1, col="red", lty="dashed", lwd=2)

hist(log(trw.data$RingWidth)); abline(v=median(log(trw.data$RingWidth)), lty="dashed", col="red", lwd=5)

# -----------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=BAI)) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm")

lm.test6 <- lm(BAI ~ FireCount, data=trw.data)
summary(lm.test6)

lm.test7 <- lm(BAI ~ FireCount+Year, data=trw.data)
summary(lm.test7)

lm.test8 <- lm(BAI ~ Precip+Temp+FireCount, data=trw.data)
summary(lm.test8)

lm.test9 <- lm(BAI ~ Precip+Temp+FireCount+Year, data=trw.data)
summary(lm.test9)

lm.test10 <- lm(BAI ~ Precip+Temp+FireCount+Year+Species, data=trw.data)
summary(lm.test10)

# An example of how to make a better model by building detrending into the model
# Plot will need to be changed to whatever the name of the column is that tells you 
# what tree things came from; if by=Plot runs quickly, try changing it to Tag 
# (or whatever indicates the unique tree)
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot), data=trw.data)
summary(gam1)
plot(gam1) # hit escape to stop graphing, but think about what this is actually showing

# GAMM example (with random effects!); replace Plot, Tree & Core with whatever the appropraite column names are
library(mgcv)
gamm.example <- gamm(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot), random=list(Tree=~1, Core=~1), data=trw.data)

hist(trw.data$BAI); abline(v=median(trw.data$BAI), lty="dashed", col="red", lwd=5)
hist(resid(lm.test10)); abline(v=0, lty="dashed", col="red", lwd=5)
plot(resid(lm.test10) ~ predict(lm.test10)); abline(h=0, lty="dashed", col="red", lwd=2)
plot(predict(lm.test10) ~ trw.data$BAI); abline(a=0, b=1, col="red", lty="dashed", lwd=2)

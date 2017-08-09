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
# path.wd <- "~/Desktop/Research/URF2017_Lopazalles/" # Christy

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

ggplot(trw.data) +
  geom_point(aes(x=Year, y=RingWidth, color=as.factor(FireCount))) +
  geom_smooth(aes(x=Year, y=RingWidth, color=as.factor(FireCount)), method="lm")

# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=BAI, color = Species)) +
  geom_smooth(aes(x=FireCount, y=BAI, color = Species), method="lm")

ggplot(trw.data) +
  geom_point(aes(x=Year, y=BAI, color=as.factor(FireCount))) +
  geom_smooth(aes(x=Year, y=BAI, color=as.factor(FireCount)), method="lm")


# R2 = .2786
lm.test1 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + as.factor(Year), data=trw.data)
summary(lm.test1)

# R2 = .3837
lm.test2 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + as.factor(Year) + Plot, data=trw.data)
summary(lm.test2)

lm.test2b <- lm(BAI ~ (Precip*Temp + FireCount)*Species + as.factor(Year) + Plot + Tag*Core, data=trw.data)
summary(lm.test2b)
anova(lm.test2b)
# ------------------------------
# Linear Mixed-Effects Modeling Option:
# ------------------------------
library(lme4)
lme.test3 <- lmer(BAI ~ (Precip*Temp + FireCount)*Species + as.factor(Year) + (1|Core/Tag/Plot), data=trw.data)
summary(lme.test3)

lme.test4a <- lmer(BAI ~ (Precip*Temp*Species + FireCount) + as.factor(Year) + (1|Core/Tag/Plot), data=trw.data)
summary(lme.test4a)

lme.test4b <- lmer(BAI ~ (Precip*Temp + FireCount*Species) + as.factor(Year) + (1|Core/Tag/Plot), data=trw.data)
summary(lme.test4b)

# We can use an anova to see if removing effects (interactions) had significant impact on model performance
# with mixed effects models, it compares the models of model fit that penalize the number of parameters; 
# in general differences >=2 in AIC are considered significant
anova(lme.test3, lme.test4a, lme.test4b)
# This tells us that model 4b (with a fire*Species interaction is the worst fit and the species effect is 
# just with climate responses)
# ------------------------------



# ------------------------------
# Things to think about: 
# ------------------------------
# 1. You're assigning the same fire count to all years of growth for a plot.  
#    - Is this an accurate representation?  What might be some alternatives?
#    - If you're going to give one fire count data, I think you're going to want
#      to work with *average* growth for the period over which the fire count is 
#      calculated
##### CR Update: Okay, I see you realized this and did it how it should be done at the end
#                Good job!
#    - Now think about how you can deal with fire in the actual years.  Some suggestions:
#      1. binary fire factor (yes/no) for a year
#      2. Time since fire.  What would you expect with a factor of time since fire?
# ------------------------------



hist(trw.data$BAI); abline(v=median(trw.data$BAI), lty="dashed", col="red", lwd=5)
hist(resid(lm.test2)); abline(v=0, lty="dashed", col="red", lwd=5)
plot(resid(lm.test2) ~ predict(lm.test2)); abline(h=0, lty="dashed", col="red", lwd=2)
plot(predict(lm.test2) ~ trw.data$BAI); abline(a=0, b=1, col="red", lty="dashed", lwd=2)

# --------------------------------------

# An example of how to make a better model by building detrending into the model

# R2 = .274
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot) + Plot + Tag*Core, data=trw.data)
summary(gam1)
anova(gam1)
# plot(gam1) 

# R2 = .318
gam2 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data)
summary(gam2)
anova(gam2)
# plot(gam2)

# We can try fitting the year spline by core, but that gets computationally expensive
# gam3 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Core) + Plot + Tag*Core, data=trw.data)
# summary(gam3)
# anova(gam3)

# GAMM example (with random effects!); replace Plot, Tree & Core with whatever the appropraite column names are
# This ends up having stability issues -- we have too many random effects with too little replication
# gamm.example <- gamm(BAI ~  s(Year, by=Plot), random=list(Plot=~1, Tag=~1), data=trw.data)

# Error in MEestimate(lmeSt, grps) : 
# Singularity in backsolve at level 0, block 1

# --------------------------------------
# Comparing average growth rates over the 17 year chunk

# Dropping the core to reduce pseudoreplication
trw.avg <- aggregate(trw.data$BAI,
                     trw.data[,c("Plot", "Species", "Tag", "FireCount")],
                     mean)
names(trw.avg)[ncol(trw.avg)] <- "AvgBAI"

ggplot(trw.avg) +
  geom_point(aes(x=FireCount, y=AvgBAI, color = Species, fill=Species)) +
  geom_smooth(aes(x=FireCount, y=AvgBAI, color = Species, fill=Species), method="lm")

# R2 = .4743
lm.test3 <- lm(AvgBAI ~ FireCount*Species + Plot, data=trw.avg)
summary(lm.test3)
anova(lm.test3)

lm.test4 <- lm(AvgBAI ~ FireCount*Species + Plot + Tag, data=trw.avg)
summary(lm.test4)
anova(lm.test4)
# lme.avg <- lmer(AvgBAI ~ FireCount*Species + (1|Tag) + (1|Plot), data=trw.avg)
# summary(lme.avg)
# anova(lme.avg)

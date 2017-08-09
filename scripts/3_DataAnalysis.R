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

trw.data <- read.csv(file.path(path.wd,"data/CombinedData"))

# --------------------------------------
# Data Wrangling
# --------------------------------------

# Removing weird first column
trw.data <- trw.data[,2:length(trw.data)]

# --------------------------------------
# Analysis and graphs
# --------------------------------------

# GAMM example (with random effects!); replace Plot, Tree & Core with whatever the appropraite column names are

gamm.example <- gamm(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Plot), random=list(Tag=~1, Core=~1), data=trw.data)

# Error in MEestimate(lmeSt, grps) : 
# Singularity in backsolve at level 0, block 1

# --------------------------------------
# Total number of fires per plot
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=BAI, color = Species)) +
  geom_smooth(aes(x=FireCount, y=BAI, color = Species), method="lm")

# R2 = .3837
lm.test1 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + Year + Plot, data=trw.data)
summary(lm.test1)

# R2 = .318
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag), data=trw.data)
summary(gam1)
plot(gam1)

# Seperating by species

# R2 = .3599
lm.test1QUMA <- lm(BAI ~ Precip*Temp + FireCount + Year + Plot, data=trw.data[trw.data$Species == "QUMA",])
summary(lm.test1QUMA)

# R2 = .348
gam1QUMA <- gam(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), data=trw.data[trw.data$Species == "QUMA",])
summary(gam1QUMA)
plot(gam1)

# --------------------------------------
# Number of burns in the past 3 years
# --------------------------------------

ggplot(trw.data) +
  geom_jitter(aes(x=BurnIn3, y=BAI, color = Species)) +
  geom_smooth(aes(x=BurnIn3, y=BAI, color = Species), method="lm")

# R2 = .294
gam3 <- gam(BAI ~ (Precip*Temp + BurnIn3)*Species + s(Year, by=Tag), data=trw.data)
summary(gam3)

# R2 = .3699
lm.test3 <- lm(BAI ~ (Precip*Temp + BurnIn3)*Species + Year + Plot, data=trw.data)
summary(lm.test3)

# Seperating by species

# R2 = .3599
lm.test3QUMA <- lm(BAI ~ Precip*Temp + BurnIn3 + Year + Plot, data=trw.data[trw.data$Species == "QUMA",])
summary(lm.test3QUMA)

# R2 = .182
gam3QUMA <- gam(BAI ~ Precip*Temp + BurnIn3 + s(Year, by=Tag), data=trw.data[trw.data$Species == "QUMA",])
summary(gam3QUMA)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_point(aes(x=BurnIn3, y=BAI, color = Species)) +
  geom_smooth(aes(x=BurnIn3, y=BAI, color = Species), method="lm")
# --------------------------------------
# Years since last burn, with 10 as max
# --------------------------------------

ggplot(trw.data) +
  geom_jitter(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# R2 = .3
gam2 <- gam(BAI ~ (Precip*Temp + SinceBurn)*Species + s(Year, by=Tag), data=trw.data)
summary(gam2)

# R2 = .374
lm.test2 <- lm(BAI ~ (Precip*Temp + SinceBurn)*Species + Year + Plot, data=trw.data)
summary(lm.test2)

# --------------------------------------
# Seperating by species

# QUMA ---------------------------------

# R2 = .3607
lm.test2QUMA <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "QUMA",])
summary(lm.test2QUMA)

# R2 = .247 ***
gam2QUMA <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Plot), data=trw.data[trw.data$Species == "QUMA",])
summary(gam2QUMA)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# QUAL ---------------------------------

# R2 = .5316 .
lm.test2QUAL <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "QUAL",])
summary(lm.test2QUAL)

# R2 = .377 
gam2QUAL <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "QUAL",])
summary(gam2QUAL)

ggplot(trw.data[trw.data$Species == "QUAL",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# QURU ---------------------------------

# R2 = .08703
lm.test2QURU <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "QURU",])
summary(lm.test2QURU)

# R2 = .0573 **
gam2QURU <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "QURU",])
summary(gam2QURU)

ggplot(trw.data[trw.data$Species == "QURU",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# ACSA ---------------------------------

# R2 = .1737
lm.test2ACSA <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "ACSA",])
summary(lm.test2ACSA)

# R2 = .0932
gam2ACSA <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "ACSA",])
summary(gam2ACSA)

ggplot(trw.data[trw.data$Species == "ACSA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# TIAM ---------------------------------

# R2 = .5716
lm.test2TIAM <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "TIAM",])
summary(lm.test2TIAM)

# R2 = .477
gam2TIAM <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "TIAM",])
summary(gam2TIAM)

ggplot(trw.data[trw.data$Species == "TIAM",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# --------------------------------------

# Oaks ---------------------------------

# R2 = .2526 *
lm.test2QU <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Genus == "QU",])
summary(lm.test2QU)

# R2 = .0998 ***
gam2QU <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Genus == "QU",])
summary(gam2QU)

ggplot(trw.data[trw.data$Genus == "QU",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

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
trw.data <- trw.data[,2:length(trw.data)]

# Categorical for boxplots

trw.data.cat <- trw.data
trw.data.cat$BurnIn3 <- as.factor(trw.data$BurnIn3)
trw.data.cat$SinceBurn <- as.factor(trw.data$SinceBurn)
trw.data.cat$FireCount <- as.factor(trw.data$FireCount)

# --------------------------------------
# Analysis and graphs
# --------------------------------------

library(lme4)
lmer.test1QUMA <- lmer(BAI ~ Precip*Temp + FireCount + as.factor(Year) + Plot + (1|Core/Tag), data=trw.data[trw.data$Species == "QUMA",])
lmer.test1QUMA2 <- lmer(BAI ~ Precip*Temp + as.factor(Year) + Plot + (1|Core/Tag), data=trw.data[trw.data$Species == "QUMA",])
summary(lmer.test1QUMA)
summary(lmer.test1QUMA2)
anova(lmer.test1QUMA, lmer.test1QUMA2)

# --------------------------------------
# Total number of fires per plot
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=FireCount, y=BAI, color = Species)) +
  geom_smooth(aes(x=FireCount, y=BAI, color = Species), method="lm")

ggplot() +
  geom_jitter(aes(x=FireCount, y=BAI, color = Species), alpha=.3, data=trw.data.cat) +
  geom_boxplot(aes(x=FireCount, y=BAI, color = Species), data=trw.data.cat) 
  #geom_smooth(aes(x=FireCount, y=BAI), method="lm", data=trw.data)


# R2 = .77 ***
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag) + Tag*Core, data=trw.data)
summary(gam1)
anova(gam1)
plot(gam1)

anova(gamm1QUMA$lme, gamm1QUMA2$lme)
plot(gam1)


# --------------------------------------
# Number of burns in the past 3 years
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=BurnIn3, y=BAI, color=Species)) +
  geom_smooth(aes(x=BurnIn3, y=BAI, color = Species), method="lm")

# R2 = .771
gam3 <- gam(BAI ~ (Precip*Temp + BurnIn3)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data)
summary(gam3)

# Seperating by species

# R2 = .812
gam3QUMA <- gam(BAI ~ (Precip*Temp + BurnIn3) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUMA",])
summary(gam3QUMA)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_point(aes(x=BurnIn3, y=BAI, color = Species)) +
  geom_smooth(aes(x=BurnIn3, y=BAI, color = Species), method="lm")

# --------------------------------------
# Years since last burn, with 10 as max
# --------------------------------------

ggplot(trw.data) +
  geom_jitter(aes(x=SinceBurn, y=BAI, color=Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color=Species), method="lm")

# R2 = .773
gam2 <- gam(BAI ~ (Precip*Temp + SinceBurn)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data)
summary(gam2)

# --------------------------------------
# Seperating by species

# QUMA ---------------------------------

# R2 = .35 . (-437)
gamm1QUMA <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUMA",])
summary(gamm1QUMA$gam)

# R2 = .812 (184)
gam1QUMA <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUMA",])
summary(gam1QUMA)

# R2 = .816 . (50)
gam2QUMA <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUMA",])
summary(gam2QUMA)

# R2 = .0793 . (50)
gamm2QUMA <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUMA",])
summary(gamm2QUMA$gam)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# QUAL ---------------------------------

# R2 = .774 *** (5048)
gam1QUAL <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUAL",])
summary(gam1QUAL)

# R2 = .397 (109)
gamm1QUAL <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUAL",])
summary(gamm1QUAL$gam)

# R2 = .781 *** (59)
gam2QUAL <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUAL",])
summary(gam2QUAL)

# R2 = .364 ** (52)
gamm2QUAL <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUAL",])
summary(gamm2QUAL$gam)

ggplot(trw.data.cat[trw.data$Species == "QUAL",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Species)) 

# QURU ---------------------------------

# R2 = .797 (-1)
gam1QURU <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QURU",])
summary(gam1QURU)

# R2 = .0596 (-137)
gamm1QURU <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QURU",])
summary(gamm1QURU$gam)

# R2 = .797 (55)
gam2QURU <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QURU",])
summary(gam2QURU)

# R2 = .0426 (60)
gamm2QURU <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QURU",])
summary(gamm2QURU$gam)

ggplot(trw.data[trw.data$Species == "QURU",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# ACSA ---------------------------------

# R2 = .545 (915)
gam1ACSA <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "ACSA",])
summary(gam1ACSA)

# R2 = .0922 (-56)
gamm1ACSA <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "ACSA",])
summary(gamm1ACSA$gam)

# R2 = .544 (7)
gam2ACSA <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "ACSA",])
summary(gam2ACSA)
 
# R2 = .0865 (8)
gamm2ACSA <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "ACSA",])
summary(gamm2ACSA$gam)

ggplot(trw.data[trw.data$Species == "ACSA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# TIAM ---------------------------------

# R2 = .623 (-52)
gam1TIAM <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "TIAM",])
summary(gam1TIAM)

# R2 = .467 (-23)
gamm1TIAM <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "TIAM",])
summary(gamm1TIAM$gam)

# R2 = .622 (1)
gam2TIAM <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "TIAM",])
summary(gam2TIAM)

# R2 = .469 (7)
gamm2TIAM <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "TIAM",])
summary(gamm2TIAM$gam)

ggplot(trw.data[trw.data$Species == "TIAM",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# --------------------------------------

# Oaks ---------------------------------

gamm2QU <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Genus == "QU",])
summary(gamm2QU$lme)
summary(gamm2QU$gam)

ggplot(trw.data[trw.data$Genus == "QU",]) +
  geom_point(aes(x=SinceBurn, y=BAI)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# --------------------------------------

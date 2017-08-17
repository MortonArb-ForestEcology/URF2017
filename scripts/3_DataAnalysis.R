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
library(mgcv); library(lme4)

# Setting a working directory
setwd(path.wd)

# --------------------------------------
# Reading in data
# --------------------------------------

trw.data <- read.csv(file.path(path.wd,"data/CombinedData.csv"))
trw.late <- read.csv(file.path(path.wd,"data/CombinedData_Late.csv"))

# --------------------------------------
# Data wrangling
# --------------------------------------

trw.data <- trw.data[, 2:ncol(trw.data)]
trw.late <- trw.late[, 2:ncol(trw.late)]

# --------------------------------------
# Analysis and graphs
# --------------------------------------

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

# R2 = .779
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag) + Tag*Core, data=trw.data)
summary(gam1)
anova(gam1)
plot(gam1)

anova(gamm1QUMA$lme, gamm1QUMA2$lme)
plot(gam1)

# R2 = .69 *** (-666)
gam1L <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag) + Tag*Core, data=trw.late)
summary(gam1L)


# --------------------------------------
# Number of burns in the past 3 years
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=BurnIn3, y=BAI, color=Species)) +
  geom_smooth(aes(x=BurnIn3, y=BAI, color = Species), method="lm")

# R2 = .78
gam3 <- gam(BAI ~ (Precip*Temp + BurnIn3)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data)
summary(gam3)

# R2 = .691 . (-160)
gam3L <- gam(BAI ~ (Precip*Temp + BurnIn3)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.late)
summary(gam3L)

# Seperating by species

# R2 = .826
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

ggplot(trw.late) +
  geom_jitter(aes(x=SinceBurn, y=BAI, color=Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color=Species), method="lm")

# R2 = .78
gam2 <- gam(BAI ~ (Precip*Temp + SinceBurn)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data)
summary(gam2)

# R2 = .694 ** (53)
gam2L <- gam(BAI ~ (Precip*Temp + SinceBurn)*Species + s(Year, by=Tag) + Plot + Tag*Core, data=trw.late)
summary(gam2L)

# --------------------------------------
# Seperating by species

# QUMA ---------------------------------

# R2 = .35 . (-485)
gamm1QUMA <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUMA",])
summary(gamm1QUMA$gam)

# R2 = .827 ** (-215)
gam1QUMA <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUMA",])
anova(gam1QUMA)
summary(gam1QUMA)

# R2 = .827  (19)
gam2QUMA <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUMA",])
summary(gam2QUMA)

# R2 = .0269  (19)
gamm2QUMA <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUMA",])
summary(gamm2QUMA$gam)

# --- Latewood ---
 
# R2 = .237 (-274)
gamm1LQUMA <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.late[trw.late$Species == "QUMA",])
summary(gamm1LQUMA$gam)

# R2 = .706 ** (-200)
gam1LQUMA <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.late[trw.late$Species == "QUMA",])
anova(gam1LQUMA)
summary(gam1LQUMA)

# R2 = .704  (0)
gam2LQUMA <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.late[trw.late$Species == "QUMA",])
summary(gam2LQUMA)

# R2 = .00988  (5)
gamm2LQUMA <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.late[trw.late$Species == "QUMA",])
summary(gamm2LQUMA$gam)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# QUAL ---------------------------------

# R2 = .784 *** (-281)
gam1QUAL <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUAL",])
summary(gam1QUAL)

# R2 = .406 (147)
gamm1QUAL <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUAL",])
summary(gamm1QUAL$gam)

# R2 = .788 * (44)
gam2QUAL <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QUAL",])
summary(gam2QUAL)

# R2 = .365 (21)
gamm2QUAL <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QUAL",])
summary(gamm2QUAL$gam)

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, group = SinceBurn)) 

# QURU ---------------------------------

# R2 = .803 (0)
gam1QURU <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QURU",])
summary(gam1QURU)

# R2 = .0624 (-162)
gamm1QURU <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QURU",])
summary(gamm1QURU$gam)

# R2 = .805 (71)
gam2QURU <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "QURU",])
summary(gam2QURU)

# R2 = .0505 * (82)
gamm2QURU <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "QURU",])
summary(gamm2QURU$gam)

ggplot(trw.data[trw.data$Species == "QURU",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# ACSA ---------------------------------

# R2 = .556 * (-2)
gam1ACSA <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "ACSA",])
summary(gam1ACSA)

# R2 = .0811 (-31)
gamm1ACSA <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "ACSA",])
summary(gamm1ACSA$gam)

# R2 = .556 (5)
gam2ACSA <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "ACSA",])
summary(gam2ACSA)
 
# R2 = .08 (7)
gamm2ACSA <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "ACSA",])
summary(gamm2ACSA$gam)

ggplot(trw.data[trw.data$Species == "ACSA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# TIAM ---------------------------------

# R2 = .622 (-3)
gam1TIAM <- gam(BAI ~ (Precip*Temp + FireCount) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "TIAM",])
summary(gam1TIAM)

# R2 = .468 (-27)
gamm1TIAM <- gamm(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), random=list(Plot=~1,Tag=~1, Core=~1), data=trw.data[trw.data$Species == "TIAM",])
summary(gamm1TIAM$gam)

# R2 = .621 (-4)
gam2TIAM <- gam(BAI ~ (Precip*Temp + SinceBurn) + s(Year, by=Tag) + Plot + Tag*Core, data=trw.data[trw.data$Species == "TIAM",])
summary(gam2TIAM)

# R2 = .469 (3)
gamm2TIAM <- gamm(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Species == "TIAM",])
summary(gamm2TIAM$gam)

ggplot(trw.data[trw.data$Species == "TIAM",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# --------------------------------------

# Oaks ---------------------------------

gamm2QU <- gamm(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag), random=list(Plot=~1, Tag=~1, Core=~1), data=trw.data[trw.data$Genus == "QU",])
summary(gamm2QU$lme)
summary(gamm2QU$gam)

ggplot(trw.data[trw.data$Genus == "QU",]) +
  geom_point(aes(x=SinceBurn, y=BAI)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# --------------------------------------



# --------------------------------------
# Adding an example of looking at average growth to fit
# --------------------------------------
growth.agg <- aggregate(trw.data$BAI,
                        by=trw.data[,c("Plot", "Tag", "Species", "DBH", "FireCount")],
                        FUN=mean)
names(growth.agg)[ncol(growth.agg)] <- "BAI"
summary(growth.agg)

growth.lme <- lmer(BAI ~ FireCount*Species - FireCount + (1|Plot), data=growth.agg)
summary(growth.lme)

# An example with looking at average growth
lme.qual0 <- lmer(BAI ~  (1|Plot), data=growth.agg[growth.agg$Species=="QUMA",])
lme.qual1 <- lmer(BAI ~ FireCount + (1|Plot), data=growth.agg[growth.agg$Species=="QUMA",])
anova(lme.qual0, lme.qual1)
summary(lme.qual1)

ggplot(growth.agg) +
  geom_jitter(aes(x=DBH, y=BAI), width = .1) +
  geom_smooth(aes(x=DBH, y=BAI), method="lm")

ggplot(growth.agg) +
  geom_jitter(aes(x=log(DBH), y=BAI), width = .1) +
  geom_smooth(aes(x=log(DBH), y=BAI), method="lm")
  
# --------------------------------------


?sea

growth.mean <- aggregate(trw.data[trw.data$Genus == "QU" & trw.data$FireCount == 6, "BAI"],
                        list(trw.data[trw.data$Genus == "QU" & trw.data$FireCount == 6, "Year"]),
                        FUN=mean)
names(growth.mean) <- c("Year", "BAI")

growth.mean <- growth.mean$BAI

growth.mean <- data.frame(growth.mean)

rownames(growth.mean) <- seq(2000, 2016)

event.years <- c(2006, 2007, 2012, 2013, 2015)

trw.sea <- sea(growth.mean, event.years, lag = 5, resample = 1000)

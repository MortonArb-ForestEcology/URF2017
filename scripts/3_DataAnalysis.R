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

# Categorical for boxplots

trw.data.cat <- trw.data
trw.data.cat$BurnIn3 <- as.factor(trw.data$BurnIn3)
trw.data.cat$SinceBurn <- as.factor(trw.data$SinceBurn)
trw.data.cat$FireCount <- as.factor(trw.data$FireCount)

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

ggplot() +
  geom_jitter(aes(x=FireCount, y=BAI, color = Species), alpha=.3, data=trw.data.cat) +
  geom_boxplot(aes(x=FireCount, y=BAI, color = Species), data=trw.data.cat) 
  #geom_smooth(aes(x=FireCount, y=BAI), method="lm", data=trw.data)

# R2 = .3837
lm.test1 <- lm(BAI ~ (Precip*Temp + FireCount)*Species + Year + Plot, data=trw.data)
summary(lm.test1)

# R2 = .318
gam1 <- gam(BAI ~ (Precip*Temp + FireCount)*Species + s(Year, by=Tag), data=trw.data)
summary(gam1)
plot(gam1)

# Seperating by species

# R2 = .3599 ***
lm.test1QUMA <- lm(BAI ~ Precip*Temp + FireCount + Year + Plot, data=trw.data[trw.data$Species == "QUMA",])
summary(lm.test1QUMA)

# R2 = .348 ***
gam1QUMA <- gam(BAI ~ Precip*Temp + FireCount + s(Year, by=Tag), data=trw.data[trw.data$Species == "QUMA",])
summary(gam1QUMA)
plot(gam1)

# --------------------------------------
# Number of burns in the past 3 years
# --------------------------------------

ggplot(trw.data) +
  geom_point(aes(x=BurnIn3, y=BAI, color=Species)) +
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
  geom_jitter(aes(x=SinceBurn, y=BAI, color=Species)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color=Species), method="lm")

# R2 = .3
gam2 <- gam(BAI ~ (Precip*Temp + SinceBurn)*Species + s(Year, by=Tag), data=trw.data)
summary(gam2)

# R2 = .374 *
lm.test2 <- lm(BAI ~ (Precip*Temp + SinceBurn)*Species + Plot + Year, data=trw.data)
summary(lm.test2)

# R2 = .4735 
lm.test22 <- lm(BAI ~ (Precip*Temp + SinceBurn)*Species*Plot + Year, data=trw.data)
summary(lm.test22)

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
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# QUAL ---------------------------------

# R2 = .5316 .
lm.test2QUAL <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "QUAL",])
summary(lm.test2QUAL)

# R2 = .377 
gam2QUAL <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "QUAL",])
summary(gam2QUAL)

ggplot(trw.data.cat[trw.data$Species == "QUAL",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Species)) 

# QURU ---------------------------------

# R2 = .08703
lm.test2QURU <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "QURU",])
summary(lm.test2QURU)

# R2 = .0573 **
gam2QURU <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "QURU",])
summary(gam2QURU)

ggplot(trw.data[trw.data$Species == "QURU",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# ACSA ---------------------------------

# R2 = .1737
lm.test2ACSA <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "ACSA",])
summary(lm.test2ACSA)

# R2 = .0932
gam2ACSA <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "ACSA",])
summary(gam2ACSA)

ggplot(trw.data[trw.data$Species == "ACSA",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# TIAM ---------------------------------

# R2 = .5716
lm.test2TIAM <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Species == "TIAM",])
summary(lm.test2TIAM)

# R2 = .477
gam2TIAM <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Species == "TIAM",])
summary(gam2TIAM)

ggplot(trw.data[trw.data$Species == "TIAM",]) +
  geom_point(aes(x=SinceBurn, y=BAI, color = Plot)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Plot), method="lm")

# --------------------------------------

# Oaks ---------------------------------

# R2 = .2526 *
lm.test2QU <- lm(BAI ~ Precip*Temp + SinceBurn + Year + Plot, data=trw.data[trw.data$Genus == "QU",])
summary(lm.test2QU)

# R2 = .0998 ***
gam2QU <- gam(BAI ~ Precip*Temp + SinceBurn + s(Year, by=Tag), data=trw.data[trw.data$Genus == "QU",])
summary(gam2QU)

ggplot(trw.data[trw.data$Genus == "QU",]) +
  geom_point(aes(x=SinceBurn, y=BAI)) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")


# --------------------------------------
# Unburned plot comparison
# --------------------------------------

trw.unburned <- trw.data[trw.data$SinceBurn == 10,]

# R2 = .3937 
lm.plot <- lm(BAI ~ (Precip*Temp)*Species*Plot + Year + Tag,
              data=trw.unburned)
summary(lm.plot)

lm.a128 <- lm(BAI ~ Species + Tag,
              data=trw.unburned[trw.unburned$Plot == "R109",])
summary(lm.a128)

ggplot(trw.data.cat) +
  geom_boxplot(aes(x=Species, y=BAI, color=Plot))

# --------------------------------------

bai.diff <- trw.data[,c("Tag", "Core", "Species", "Plot", "Year")]


for (i in 1:nrow(trw.data)){
  if (trw.data[i, "LastBurn"] == trw.data[i, "Year"]){
    now.bai.tmp <- trw.data[i, "BAI"]
    before.bai.tmp <- trw.data[trw.data$Year == trw.data[i, "Year"] - 1 & trw.data$Core == trw.data[i, "Core"], "BAI"]
    bai.diff[bai.diff$Core == trw.data[i, "Core"] & bai.diff$Year == trw.data[i, "Year"],"BurnYear"] <- now.bai.tmp
    bai.diff[bai.diff$Core == trw.data[i, "Core"] & bai.diff$Year == trw.data[i, "Year"],"BeforeBurn"] <- before.bai.tmp
  
  }
}

bai.diff$Difference <- bai.diff$BurnYear - bai.diff$BeforeBurn

bai.diff <- bai.diff[!is.na(bai.diff$Difference),]

bai.order <- order(bai.diff$Tag)

bai.diff <- bai.diff[bai.order,]

bai.diff <- unique(bai.diff)

bai.diff$Percent <- bai.diff$Difference / bai.diff$BeforeBurn

mean(bai.diff[bai.diff$Species == "QUMA","Difference"])
sd(bai.diff$Difference)

mean(bai.diff$Percent)
sd(bai.diff$Percent)

# --------------------------------------

trw.oneyear <- trw.data[trw.data$SinceBurn <= 1,]

ggplot(trw.oneyear) +
  geom_jitter(aes(x=SinceBurn, y=BAI, color = Species), width = .1) +
  geom_smooth(aes(x=SinceBurn, y=BAI, color = Species), method="lm")

# R2 = .3665 *

lm.oneyear <- lm(BAI ~ (Precip+Temp)*Species + SinceBurn + Year + Plot + Tag,
              data=trw.oneyear)
summary(lm.oneyear)

gam.oneyear <- gam(BAI ~ (Precip+Temp)*Species+ SinceBurn + s(Year, by=Plot), data=trw.oneyear)
summary(gam.oneyear)

# --------------------------------------

ggplot(trw.data) +
  geom_boxplot(aes(x=FireCount, y=BAI, color = Plot))

ggplot(trw.data) +
  geom_boxplot(aes(x=BurnIn3, y=BAI, color = Plot))

ggplot(trw.data) +
  geom_boxplot(aes(x=SinceBurn, y=BAI))

ggplot(trw.data[trw.data$Species == "QUMA",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Plot))

ggplot(trw.data[trw.data$Species == "QUAL",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Plot)) 

ggplot(trw.data[trw.data$Species == "QURU",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Plot))

ggplot(trw.data[trw.data$Genus == "QU",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI))

ggplot(trw.data[trw.data$Species == "ACSA",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Plot))

ggplot(trw.data[trw.data$Species == "TIAM",]) +
  geom_boxplot(aes(x=SinceBurn, y=BAI, color = Plot))



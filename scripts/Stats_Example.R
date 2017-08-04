# --------------------------------------
# --------------------------------------
# --------------------------------------

# --------------------------------------
# Load libraries, define file paths
# --------------------------------------
# Libraries
library(raster); library(rgdal); library(rgeos)
library(lubridate)

# File paths
path.local <- "~/Desktop/Research/URF2017_Lopazelles/"
path.ew <- "~/Desktop/Research/EastWoods-MonitoringPlots/"
path.google <- "~/Google Drive/East Woods/"
# --------------------------------------

tree.dat <- read.csv(file.path(path.ew, "TreeCensus_2017/data/TreeData-raw_data.csv"))
height.dat <- read.csv(file.path(path.ew, "TreeCensus_2017/data/TreeHeights-raw_data.csv"))


summary(tree.dat)
summary(height.dat)

tree.dat <- merge(tree.dat, height.dat[,c("Plot", "Tag", "Height")])
summary(tree.dat)

hist(tree.dat$Height); abline(v=median(tree.dat$Height), lty="dashed", col="red", lwd=5)
hist(log(tree.dat$DBH)); abline(v=median(log(tree.dat$DBH)), lty="dashed", col="red", lwd=5)

lm.height <- lm(Height ~ log(DBH)-1, data=tree.dat)
summary(lm.height)

hist(resid(lm.height)); abline(v=0, lty="dashed", col="red", lwd=5)
plot(resid(lm.height) ~ predict(lm.height)); abline(h=0, lty="dashed", col="red", lwd=2)
plot(predict(lm.height) ~ tree.dat$Height); abline(a=0, b=1, col="red", lty="dashed", lwd=2)

Height = 0.24*DBH + 6.3


lm.test <- lm(Height ~ log(DBH)*Sp_code-1, data=tree.dat)
summary(lm.test)
anova(lm.test)

lm.test2 <- lm(Height ~ log(DBH)+Sp_code-1, data=tree.dat)
summary(lm.test2)
anova(lm.test2)

# Fitting a mixed-effects model, where species effects are in the background
library(nlme)
lm.test3 <- lme(Height ~ log(DBH), random=list(Plot=~1), data=tree.dat)
summary(lm.test3)

summary(lm.test)
summary(lm.test2)
summary(lm.test3)


aov.spp <- aov(Height ~ Sp_code, data=tree.dat)
summary(aov.spp)

TukeyHSD(aov.spp)


t.test(tree.dat[tree.dat$Sp_code=="ACSA","DBH"], tree.dat[tree.dat$Sp_code=="QUMA","DBH"])

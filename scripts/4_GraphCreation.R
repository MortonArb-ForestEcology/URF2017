# --------------------------------------
# Graphs
# Sierra Lopezalles, slopezal@caltech.edu
# August 11, 2017
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
library(mgcv); library(car)

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

# Removing the black walnut and black cherry
trw.data <- trw.data[!trw.data$Species == "JUNI" & !trw.data$Species == "PRSE",]

trw.data$Tag <- as.factor(trw.data$Tag)

trw.data$Species <- recode(trw.data$Species, " 'ACSA'='A. saccharum'; 'QUAL'='Q. alba'; 'QUMA'='Q. macrocarpa'; 'QURU'='Q. rubra'; 'TIAM' = 'T. americana' ")

trw.tag <- aggregate(trw.data[,c("BAI", "RingWidth")],
                     trw.data[,c("Tag", "Year", "Species", "SinceBurn")],
                     mean)

growth.agg <- aggregate(trw.data[,c("BAI", "RingWidth")],
                        by=trw.data[,c("Plot", "Tag", "Species", "DBH", "FireCount", "Genus")],
                        mean)

# Distribution of data -----------------

ggplot(trw.tag[trw.tag$Species == "Q. alba",]) +
  geom_line(aes(x=Year, y=RingWidth, color = Tag, size = 2)) +
  geom_point(aes(x=Year, y=RingWidth, color = Tag, size = 3)) +
  labs(x="Year", y = "Ring width (mm)") +
  theme(legend.position="none")  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))
  
# Overall ------------------------------

ggplot(growth.agg) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount), width = .1, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm") +
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  theme(axis.text=element_text(size=20),
  axis.title=element_text(size=25))

# By species ---------------------------

ggplot(growth.agg) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount, color = Species), width = .1, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI, color = Species), method="lm", se = FALSE)+
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"), 
        legend.text = element_text(size=17,face="italic"),
        legend.title = element_text(size=17))

# Oaks

ggplot(growth.agg[growth.agg$Genus == "QU",]) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount), width = .07, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm")+
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  ggtitle("Quercus spp.") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        plot.title = element_text(face="italic", size=27))

# QUMA

ggplot(growth.agg[growth.agg$Species == "Q. macrocarpa",]) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount), width = .07, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm")+
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  ggtitle("Quercus macrocarpa") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        plot.title = element_text(face="italic", size=27)) +
  coord_cartesian(ylim = c(0, 10000))

# ACSA

ggplot(growth.agg[growth.agg$Species == "A. saccharum",]) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount), width = .07, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm")+
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  ggtitle("Acer saccharum") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        plot.title = element_text(face="italic", size=27))  +
  coord_cartesian(ylim = c(0, 3500))

# TIAM

ggplot(growth.agg[growth.agg$Species == "T. americana",]) +
  geom_jitter(aes(x=FireCount, y=BAI, group = FireCount), width = .07, size = 4) +
  geom_smooth(aes(x=FireCount, y=BAI), method="lm")+
  labs(x="Number of burns since 2000", 
       y = expression("Average growth per year (mm"^2*")")) +
  ggtitle("Tilia americana") + 
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"),
        plot.title = element_text(face="italic", size=27))

# Since Burn ---------------------------

ggplot(trw.tag[trw.tag$Tag == "1427",]) +
  geom_line(aes(x=Year, y=BAI, color = Tag, size = 2)) +
  geom_point(aes(x=Year, y=BAI, color = Tag, size = 3)) +
  labs(x="Year", y = expression("Growth (mm"^2*")")) +
  theme(legend.position="none")  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

ggplot(trw.tag[trw.tag$Tag ==  "1427",]) +
  geom_jitter(aes(x=SinceBurn, y=BAI), width = .1, size = 4) +
  geom_smooth(aes(x=SinceBurn, y=BAI), method="lm") +
  labs(x="Number of years since last burned", 
       y = expression("Growth (mm"^2*")")) +
  scale_x_continuous(breaks = c(seq(0, 10)),
                     labels = c(seq(0,9), "\u2265 10"),
                     limits = c(-.1, 10.1)) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))

ggplot(trw.data[trw.data$Species == "Q. macrocarpa",]) +
  geom_jitter(aes(x=SinceBurn, y=BAI, group = FireCount), width = .1, size = 4) +
  geom_smooth(aes(x=SinceBurn, y=BAI), method="lm")+
  labs(x="Number of years since last burned", 
       y = expression("Growth (mm"^2*")")) +
  ggtitle("Quercus macrocarpa") + 
  theme(plot.title = element_text(face="italic", size=27)) +
  scale_x_continuous(breaks = c(seq(0, 10)),
                     labels = c(seq(0,9), "\u2265 10"),
                     limits = c(-.1, 10.1)) +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))


# 1200, 650

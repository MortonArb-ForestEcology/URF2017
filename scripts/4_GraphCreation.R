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

# Path to Tree Census 2017 Github folder
path.ew <- "~/GitHub/EastWoods-MonitoringPlots/TreeCensus_2017/" # Sierra

# Path to East Woods Google Drive folder
path.google <- "C:/Users/macmo/Google Drive/Morton Summer 2017/East Woods/" # Sierra
# path.google <- "~/Google Drive/East Woods" # Christy


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

# 2017 permanent plot survey data

survey1.perm <-read.csv(file.path(path.ew,"data/TreeData-raw_data.csv"), na.strings="")

heights.perm <- read.csv(file.path(path.ew,"data/TreeHeights-raw_data.csv"), na.strings="")

survey.temp <-read.csv(file.path(path.ew, "data/URF_2017_AdditionalOakData-raw_data.csv"), na.strings="", colClasses=c("Tag"="character"))

trw.data <- read.csv(file.path(path.wd,"data/CombinedData.csv"))

fire.data <- read.csv(file.path(path.google,"URF_2017_Rollinson/URF2017_BurnInfo.csv"))

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

# Survey data

survey1.perm$Sp_code <- as.factor(substr(survey1.perm$Sp_code, 1, 4))

survey.perm <- merge(survey1.perm, heights.perm[c("Tag","Height")], by = "Tag")

survey.perm$Plot <- recode(survey.perm$Plot, " 'A1'='N115'; 'B5'='U134'; 'C6'='HH115'; 'D1'='B127' ")

names(survey.perm)[6] <- "Species"

survey.perm[,"Tag"] <- as.character(survey.perm[,"Tag"])
survey.temp[,"Tag"] <- as.character(survey.temp[,"Tag"])

survey <- rbind(survey.perm[,c("Plot","Tag","Species", "DBH", "Height", "Canopy")], survey.temp[,c("Plot","Tag","Species", "DBH", "Height", "Canopy")])                     

# Fire data

# Removing month and day from date
fire.data$Burn_Date <- substr(fire.data$Burn_Date, 1, 4)

# Resaving as numeric
fire.data$Burn_Date <- as.numeric(as.character(fire.data$Burn_Date))

# Fixing the 2013's
fire.data[fire.data$NOTES == "2013 unsure on date" & !is.na(fire.data$NOTES),"Burn_Date"] <- 2013

# Removing all rows without a burn date
fire.data <- fire.data[!is.na(fire.data[,"Burn_Date"]),]

# Removing 2017 since only partial year
fire.data <- fire.data[fire.data$Burn_Date != "2017",]

# Fixing column name

names(fire.data)[9] <- "Plot"

# Removing the hyphen to allow a merge

fire.data$Plot <- paste0(substr(fire.data$Plot, 1, nchar(paste(fire.data$Plot))-4), substr(fire.data$Plot, nchar(paste(fire.data$Plot))-2,nchar(paste(fire.data$Plot))))

fire.count <- aggregate(fire.data$Burn_Date,
                        list(fire.data$Plot),
                        length)

names(fire.count) <- c("Plot", "FireCount")

# Merging fire counts
survey <- merge(survey, fire.count, "Plot", all=TRUE)

# Setting all NA fire counts to zero
survey[is.na(survey$FireCount),"FireCount"] <- 0

# Setting levels

survey$Canopy <- factor(survey$Canopy, levels=c("D", "CD", "I", "U", NA))

survey$Species <- factor(survey$Species, 
                         levels=c("ACSA", "FRAM", "JUNI", "OSVI", "QURU", "QUAL", "QUMA", "PRSE", "TIAM", "ULRU", "UNKN", "unkn", NA))

# --------------------------------------
# Figures
# --------------------------------------

# Canopy Distributions based on plot

ggplot(survey) +
  geom_bar(aes(Canopy, fill=Species)) +
  facet_wrap(~ Plot)

# Canopy Distributions based on regime

ggplot(survey) +
  geom_bar(aes(Canopy, fill = Species)) +
  facet_wrap(~ FireCount)

# Species distribution based on plot

ggplot(survey) +
  geom_bar(aes(Species, fill=Species)) +
  facet_wrap(~ Plot)

ggplot(survey) +
  geom_bar(aes(Plot, fill=Species))

# Species distribution based on regime

ggplot(survey) +
  geom_bar(aes(Species, fill=Species)) +
  facet_wrap(~ FireCount)

# --------------------------------------
# Morton Symposium Presentation Figures
# --------------------------------------

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

fire.date <- data.frame(c(2012, 2017))

ggplot(trw.tag[trw.tag$Tag == "1427",]) +
  geom_line(aes(x=Year, y=BAI, color = Tag, size = 2)) +
  geom_point(aes(x=Year, y=BAI, color = Tag, size = 3)) +
  labs(x="Year", y = expression("Growth (mm"^2*")")) +
  theme(legend.position="none")  +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=25,face="bold"))+
  geom_vline(xintercept = c(2012, 2017), aes(color = ))

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

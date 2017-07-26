# Combine files for DCC partial ring widths
# CHG w/ RLB 12-14-16

library(dplR)
#setwd('~/Navajo Dendroclimate/Sites/CrossCanyon/')

files <- Sys.glob('West_peak/EW_LW/*.rwl')

# pulling remeasured files
files.redo <- Sys.glob('West_peak/EW_LW/redo/*.rwl')

# Total ring widths -------------------------------------------------------

trw.files <- files[grep('ring width', files)]

trw.obj <- NA
for(i in trw.files) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_t', sep='')
  trw.obj <- append(trw.obj, nam)
  assign(nam, aa)
}

trw.obj <- na.omit(trw.obj)

all.trw <- combine.rwl(get(trw.obj[1]), get(trw.obj[2]))

for(i in 3:length(trw.obj)){
  aa <- get(trw.obj[i])
  all.trw <- combine.rwl(all.trw, aa)
}

write.tucson(all.trw, "west_peak_trw.rwl", prec=0.001, long.names=TRUE)


# Pulling remeasured files
trw.files <- files.redo[grep('ring width', files.redo)]

trw.obj <- NA
for(i in trw.files) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_t', sep='')
  trw.obj <- append(trw.obj, nam)
  assign(nam, aa)
}

trw.obj <- na.omit(trw.obj)

redo.trw <- combine.rwl(get(trw.obj[1]), get(trw.obj[2]))

for(i in 3:length(trw.obj)){
  aa <- get(trw.obj[i])
   redo.trw <- combine.rwl(redo.trw, aa)
}

write.tucson(redo.trw, "west_peak_redo_trw.rwl", prec=0.001, long.names=TRUE)



summary(all.trw)
# Earlwood widths ---------------------------------------------------------

erw.files <- files[grep('earlywood width', files)]

erw.obj <- NA
for(i in erw.files) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_e', sep='')
  erw.obj <- append(erw.obj, nam)
  assign(nam, aa)
}

erw.obj <- na.omit(erw.obj)

all.erw <- combine.rwl(get(erw.obj[1]), get(erw.obj[2]))

for(i in 3:length(erw.obj)){
  aa <- get(erw.obj[i])
  all.erw <- combine.rwl(all.erw, aa)
}

write.tucson(all.erw, "west_peak_ew.rwl", prec=0.001, long.names=TRUE)

# Redo EW
erw.redo <- files.redo[grep('earlywood width', files.redo)]

erw.obj <- NA
for(i in erw.redo) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_e', sep='')
  erw.obj <- append(erw.obj, nam)
  assign(nam, aa)
}

erw.obj <- na.omit(erw.obj)

redo.erw <- combine.rwl(get(erw.obj[1]), get(erw.obj[2]))

for(i in 3:length(erw.obj)){
  aa <- get(erw.obj[i])
  redo.erw <- combine.rwl(redo.erw, aa)
}

write.tucson(redo.erw, "west_peak_ew_redo.rwl", prec=0.001, long.names=TRUE)


# Latewood widths ---------------------------------------------------------

lrw.files <- files[grep('latewood width', files)]

lrw.obj <- NA
for(i in lrw.files) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_l', sep='')
  lrw.obj <- append(lrw.obj, nam)
  assign(nam, aa)
}

lrw.obj <- na.omit(lrw.obj)

all.lrw <- combine.rwl(get(lrw.obj[1]), get(lrw.obj[2]))

for(i in 3:length(lrw.obj)){
  aa <- get(lrw.obj[i])
  all.lrw <- combine.rwl(all.lrw, aa)
}

write.tucson(all.lrw, "west_peak_lww.rwl", prec=0.001, long.names=TRUE)


# LW redo

lrw.redo <- files.redo[grep('latewood width', files.redo)]

lrw.obj <- NA
for(i in lrw.redo) {
  aa <- read.rwl(i)
  nam <- paste(names(aa), '_l', sep='')
  lrw.obj <- append(lrw.obj, nam)
  assign(nam, aa)
}

lrw.obj <- na.omit(lrw.obj)

redo.lrw <- combine.rwl(get(lrw.obj[1]), get(lrw.obj[2]))

for(i in 3:length(lrw.obj)){
  aa <- get(lrw.obj[i])
  redo.lrw <- combine.rwl(redo.lrw, aa)
}

write.tucson(redo.lrw, "west_peak_lww_redo.rwl", prec=0.001, long.names=TRUE)


# Chronology stats --------------------------------------------------------



west.peak.det <- detrend(all.trw, method="Spline")
#ocw.ids <- read.ids(ocw.det, stc=c(3, ))
rwi.stats(west.peak.det)


ocw.chron <- chron(west.peak.det, pref='PWP')

plot(ocw.chron)

seg.plot(west.peak.det)

test <- corr.rwl.seg(all.trw)

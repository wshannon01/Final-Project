library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# load data
# set directory

setwd("../data")

dat.15 <- read.csv("2015crimelog.csv", stringsAsFactors = FALSE)
dat.16 <- read.csv("2016crimelog.csv", stringsAsFactors = FALSE)
dat.17 <- read.csv("2017crimelog.csv", stringsAsFactors = FALSE)

# remove blank entries
dat.15 <- dat.15[!dat.15$Crime == "",]

dat.15$time <- sapply(dat.15$Date.Time.Occurred, function(x) strsplit(as.character(x), " ")[[1]][2]) #time without AM, PM
dat.15$ap <- sapply(dat.15$Date.Time.Occurred, function(x) strsplit(as.character(x), " ")[[1]][3]) # AM, PM


dat.15$timeap <- paste(dat.15$time, dat.15$ap) # combine time and AM, PM
# convert to military time
dat.15$mtime <- substr(strptime(dat.15$timeap, "%I:%M %p"), 11,19) 
# remove white space
dat.15$mtime <- trimws(dat.15$mtime)

dat.15$ctime <- hour(hms(dat.15$mtime)) 
#dat.15$ttimes <- times(dat.15$mtime) 
# categorize times into time of day (tod)
tbreaks <- hour(hm("00:00", "5:00", "11:00", "17:00", "23:59"))
tlabels <- c("Night", "Morning", "Afternoon", "Evening")
#dat.15$tod <- cut(x=dat.15$ttime, breaks = tbreaks, labels = tlabels, include.lowest = TRUE)
dat.15$tod <- cut(x=dat.15$ctime, breaks = tbreaks, labels = tlabels, include.lowest = TRUE)

# table crimes by tod
crime.tod.15 <- as.data.frame(table(dat.15$Crime, dat.15$tod))

# date, day, month, quarter
dat.15$date <- sapply(dat.15$Date.Time.Reported, function(x) strsplit(as.character(x), " ")[[1]][1]) %>% mdy()
dat.15$day <- weekdays(dat.15$date)
dat.15$month <- months(dat.15$date)
dat.15$quarter <- quarters(dat.15$date)
dw15 <- as.data.frame(table(dat.15$Crime, dat.15$day))
dwm15 <- melt(dw15, c("Var1", "Var2"))
g.dwm15 <- dwm15 %>% group_by(Var2) %>% summarise(n = sum(value))
dg.dwm15 <- as.data.frame(g.dwm15) 
dg.dwm15$Var2 <- factor(dg.dwm15$Var2, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

dg.dwm15[order(dg.dwm15$Var2),]

dw15m <- as.data.frame(table(dat.15$Crime, dat.15$month))
dwm15m <- melt(dw15m, c("Var1", "Var2"))
g.dwm15m <- dwm15m %>% group_by(Var2) %>% summarise(n = sum(value))

dg.dwm15m <- as.data.frame(g.dwm15m) 
dg.dwm15m$Var2 <- factor(dg.dwm15m$Var2, levels = month.name)




# format data and create variables
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# load data
# set directory

# setwd()

dat.15 <- read.csv("2015crimelog.csv")
dat.16 <- read.csv("2016crimelog.csv")
dat.17 <- read.csv("2017crimelog.csv")


# format data and create variables
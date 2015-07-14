# Basic script to load all the repetitive information
#28/6/2015
#
require(dplyr)
require(magrittr)
library(maps)
library(mapproj)
library(mapdata)
oldpar <- par()

#####
#rm(shkdir)
shkdir <- "C:/Projects/SHK-indicators-2015/"
dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
setwd("C:/Projects/SHK-indicators-2015/")

dat.dir <-"C:/Projects/DATA_2015/"

# make colors for the main species - use previous
hues=c("royalblue","gray","red","mediumspringgreen","sienna", "orange", "purple" ) ; mycol<- hues
huenames=c("Blue","Mako","OCS","Silky","Thresher", "HHD", "POR")
huecodes=c("BSH","MAK","OCS","FAL","THR","HHD", "POR")
# make names and other init declarations because they are called by various names.
#
spec<- c("BSH", "MAK", "OCS","FAL", "THR", "HHD", "POR"); nspec<- length(spec)
scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
#timeframe of analysis
s.yr <- 1995
e.yr <- 2014

# 
nyears <- length(s.yr:e.yr)

#
#
nreg <- 6
#
eez <- read.table(file=paste("C:/Projects/SHK-indicators-2015_backup/DATA/EZNEW2.txt", sep=""), sep="",header=F)
#

count <- function(x, n){ length((which(x == n))) }
perc <- function(x, n){ 100*length((which(x == n))) / length(x) }





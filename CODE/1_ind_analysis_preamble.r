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
#dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
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
eez <- read.csv(file="C:/Projects/SHK-indicators-2015/DATA/eez-contour.csv") # added to data
#

count <- function(x, n){ length((which(x == n))) }
perc <- function(x, n){ 100*length((which(x == n))) / length(x) }

#region.polys <- list(list(x=c(120,210,210,120),y=c(50,50,20,20)),
#                     list(x=c(



draw.regions <- function(lwd=2, ...) {

    #horizontal lines
    lines(c(120,210),c(50,50),lwd=lwd, ...)
    lines(c(120,210),c(20,20),lwd=lwd, ...)
    lines(c(210,230),c(-4,-4),lwd=lwd, ...)
    lines(c(120,230),c(-10,-10),lwd=lwd, ...)
    lines(c(141,150),c(-55,-55),lwd=lwd, ...)
    lines(c(150,230),c(-60,-60),lwd=lwd, ...)
    #vertical lines
    lines(c(120,120),c(50,-10),lwd=lwd, ...)
    lines(c(141,141),c(-10,-55),lwd=lwd, ...)
    lines(c(150,150),c(-55,-60),lwd=lwd, ...)
    lines(c(180,180),c(50,20),lwd=lwd, ...)
    lines(c(170,170),c(20,-60),lwd=lwd, ...)
    lines(c(210,210),c(50,-4),lwd=lwd, ...)
    lines(c(230,230),c(-4,-60),lwd=lwd, ...)
}

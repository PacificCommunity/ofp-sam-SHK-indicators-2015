

#
#  Distribution maps  based on observer data.
#
#
#

#
library(maps)
library(mapproj)
library(mapdata)
################################


#load( file= "C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata"  ) # loads shk_all  #
# load( file= "C:/Projects/SHK-indicators-2015/DATA/lldata_03JULY2015.rdata"  )# loads sets and catch sets should be about equal to shk_all
#load(file=paste(shkdir, "DATA/Shark_Operational_processed.rdata", sep='') ) # loads shklllog  but this is from January 2013
#head(shkLLlog)

load(paste0(dat.dir,"lldata_11JULY2015.rdata"))
#---------------------------------------    map for the effort --------------------------------##
#
#----------------------------------------- inital declarations  -------------------------------##
#
par(mar=c(4, 4, 4, 2) + 0.1)
par(mai=c(0.65,0.65 ,0.25,0.1))
par(mfrow=c(1,1))
mapxlim<-c(110,260) # this defines how far out we make the map, the eastern boundary of region 6 is at 230

scalecex <- 0.75 # the character expansion for the text on the scale
legcex   <- 1.25  # size of the legend
ptcex    <- 0.5

speclong<- c("blue", "mako", "ocs","silky", "thresher", 'hammerhead', 'porbeagle'); nspec<- length(speclong)

cntrylst<- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela", "Cuba", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii", "Canada", "USSR")

nams <- map("world", namesonly=TRUE, plot=FALSE)

ptcol    <-  1
ptbg     <- rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)
myorange <- rgb(red=255, green=165, blue=0, alpha=75, maxColorValue=255)
myred    <- rgb(red=255, green=2, blue=0, alpha=75, maxColorValue=255)
mygrey   <-rgb(red=220,green=220, blue=220, alpha=75, maxColorValue=255)
myblue   <-rgb(red=0,green=205, blue=255, alpha=175, maxColorValue=255)
mygold   <-rgb(red=255,green=215, blue=0, alpha=105, maxColorValue=255)
mygrey2   <- rgb(red=61,green=61, blue=61, alpha=75, maxColorValue=255)   # col2rgb("purple")


hues=c("royalblue","gray","red","mediumspringgreen","sienna", 'orange', 'purple') ; mycol<- hues
#col2rgb(mycol)
mymapcol<-  c (   rgb(red=65,green=105, blue=225, alpha=75, maxColorValue=255),
                  rgb(red=255,green=192, blue=203, alpha=75, maxColorValue=255) ,   # changed to pink to see it better against the grey bg
                  rgb(red=255,green= 0, blue=0, alpha=75, maxColorValue=255)   ,
                  rgb(red=0,green=250, blue=154, alpha=75, maxColorValue=255)  ,
                  rgb(red=160,green=82, blue=45, alpha=75, maxColorValue=255)  ,
                  rgb(red=255, green=165, blue=0, alpha=75, maxColorValue=255) ,
                  rgb(red=255, green=215, blue=0, alpha=75, maxColorValue=255)
                  )

#col2rgb("pink")


# make colors for the main species - use previous
hues=c("royalblue","gray","red","mediumspringgreen","sienna", "orange", "purple" ) ; mycol<- hues
huenames=c("Blue","Mako","OCS","Silky","Thresher", "HHD", "POR")
huecodes=c("BSH","MAK","OCS","FAL","THR","HHD", "POR")
# make names and other init declarations because they are called by various names.
#
spec<- c("BSH", "MAK", "OCS","FAL", "THR", "HHD", "POR"); nspec<- length(spec)

 ##---------------------------------------------------------------------------
#
#  start plot
#

shk_all<-sets
shk_all <- shk_all[!is.na(shk_all$lat1d),] #lat1d lon1d
shk_all <- shk_all[!is.na(shk_all$lon1d),]
for( i in 1:nspec){
png(file=paste(shkdir,"GRAPHICS/LL_spec_dist_", spec[i], ".png",sep=''))
#
par(mfrow=c(1,1),mar=c(4,4,2,1),omi=c(0.5,0.5,0.25,0.25))
#
plot(1,1, ylab="Longitude", xlab="Lattitude"  ,xlim=c(110,240),ylim=c(-60,50),col="white" , las=1 )
#
lines(eez[,1], eez[,2], col=1) #
#
#points(shk_all$newlon, shk_all$newlat, pch=21, col=mygrey2, bg=mygrey, cex=ptcex)
#

points(sets$lon1d, sets$lat1d, pch=21, col=mygrey2, bg=mygrey, cex=ptcex)
#
i=1
  pnt <- sets[,spec[i]] >0; head(pnt) # sum(pnt, na.rm=T)
#points(shk_all[pnt, "newlon"],shk_all[pnt, "newlat"], pch=21, col=mymapcol[i], bg=mymapcol[i], cex=ptcex )
#
points(sets[pnt, "lon1d"],sets[pnt, "lat1d"], pch=21, col="blue", bg=mymapcol[i], cex=ptcex )
#
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region =c(cntrylst), fill=T, add=T, yaxt="n", xaxt="n", col= grey(0.5))
#pull in the region lines only
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")
box("plot")
#
legend('bottomleft',  legend=c(  "Observed Set", paste("Obsereved",spec[i],"Catch") ), pch=21,col=c(mygrey2, mymapcol[i]),  pt.bg=c(mygrey, mymapcol[i]) , pt.cex=1, cex=0.8)

dev.off()

}

# rm(sets,shk_all)
rm(shk_all)

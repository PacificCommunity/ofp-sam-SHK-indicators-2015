#
#  LL fishery map
#
#
library(maps)
library(mapproj)
library(mapdata)
################################


load( file= "C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata"  ) # loads shk_all  # 
load(file=paste(shkdir, "DATA/Shark_Operational_processed.rdata", sep='') ) # loads shklllog  but this is from January 2013
head(shkLLlog)

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
ptcex    <- 0.35
 

cntrylst<- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela", "Cuba", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii", "Canada", "USSR")

nams <- map("world", namesonly=TRUE, plot=FALSE)

ptcol    <-  1
ptbg     <- rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)  
myorange <- rgb(red=255, green=165, blue=0, alpha=75, maxColorValue=255) 
myred    <- rgb(red=255, green=2, blue=0, alpha=75, maxColorValue=255) 
mygrey   <-rgb(red=220,green=220, blue=220, alpha=75, maxColorValue=255)  
myblue   <-rgb(red=0,green=205, blue=255, alpha=175, maxColorValue=255)
mygold   <-rgb(red=255,green=215, blue=0, alpha=105, maxColorValue=255)
 


##---------------------------------------------------------------------------
#
#  start plot
#


par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0,0))
 
plot(1,1, ylab="Longitude", xlab="Lattitude"  ,xlim=c(110,240),ylim=c(-60,50),col="white"  )
#
lines(eez[,1], eez[,2], col=1) #  
# 
points(shkLLlog$newlon ,shkLLlog$newlat ,type="p",pch=21, col=mygrey2, bg=mygrey,  cex=ptcex )   
# 
points(shk_all$newlon, shk_all$newlat, pch=21, col=myorange, bg=myorange, cex=ptcex)
#
shk_all$cmbshk <- rowSums(shk_all[,c('totalshk', "othershk")])
 
points(shk_all[shk_all$cmbshk >0, "newlon"],shk_all[shk_all$cmbshk >0, "newlat"], pch=21, col=myred, bg=myred, cex=ptcex*.8)
# 

map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region =c(cntrylst), fill=T, add=T, yaxt="n", xaxt="n", col='black')
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")

box("plot")
legend('topright',  legend=c("Set", "Observed Set", "Obsereved Shark Catch"), pch=21,col=c(myblue, myorange,myred ),  pt.bg=c(myblue, myorange,myred ) , pt.cex=1.5)


 
dev.off()



   





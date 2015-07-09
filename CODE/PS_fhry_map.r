


##################### PURSE SEINE
#   
# plot the observed effort and catch. 
# as well as the reported PS effort
# maybe a dot where there was a shark caught.
#
# 
# load data
load( file= "C:/Projects/SHK-indicators-2015/DATA/PSObs16Jun2015.RData"  ) # loads PSObsShk  # 



#
##---------------------------------------    map for the effort--------------------------------##
par(mar=c(4, 4, 4, 2) + 0.1)
par(mai=c(0.65,0.65 ,0.25,0.1))
par(mfrow=c(1,1))
mapxlim<-c(110,260) # this defines how far out we make the map, the eastern boundary of region 6 is at 230 

scalecex <- 0.75 # the character expansion for the text on the scale
legcex   <- 1.25  # size of the legend
ptcex    <- 0.75

cntrylst <- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela","Canada",  "Cuba", "USSR", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii")
nams <- map("world", namesonly=TRUE, plot=FALSE)

ptcol    <-  1
ptbg     <- rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)  
myorange <- rgb(red=255, green=165, blue=0, alpha=75, maxColorValue=255) 
myred    <- rgb(red=255, green=2, blue=0, alpha=75, maxColorValue=255) 



##set up plot region 
#png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_sets.png",sep='') )  
# plot(180,0,type="n", xlim=mapxlim,ylim=c(-60,50), cex=1, las=1, ylab="Latitude", xlab="Longitude", main="Observed Longline Sets")    
# 
# image(as.numeric(rownames(llsurf)),as.numeric( colnames(llsurf)), log(llsurf+1), col=rev( heat.colors(15) ), add=T) 
# #equator and int. date line
# abline(h=0, lty=1, lwd=2, col=grey(0.8))     
# abline(v=180, lty=1, lwd=2, col=grey(0.8)) 
# # Add Points
# #  points( shk$newlon,shk$newlat, col=ptcol,pch=21, bg=ptbg   ,lwd=1.5,cex=1.5 )
# # Map and lines
# 
# #text( mapxlim[2]-10,5,"# of Sets \n Observed", pos=3)   #bad
# #map.scale(110, -55,ratio=F, cex=scalecex)
# #arrows(x0=110,y0=-56,x1=110,y1=-46, length=0.15)
# #text(110,-44,"N", cex=1.5)
# box("plot")
# # box("figure")
# #plot the map
#  source("C:/Projects/SHK-indicators-2015/CODE/map_lines.r")





psagg <- read.table(file="C:/Projects/SHK-indicators-2015/DATA/PSEFFORT_1X1.TXT", header=TRUE, sep=',')
#head(psagg); dim(psagg)
psagg<-psagg[psagg$sets>0,]
#head(psagg); dim(psagg)

png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_sets.png",sep='')) 
# 
plot(1,1, ylab="Longitude", xlab="Lattitude"  ,xlim=c(110,240),ylim=c(-60,50),col="white"  )
lines(eez[,1], eez[,2], col=1) #  
# plot the aggregate reported data
with(psagg, points(    lond,   latd ,pch=21, col=myblue, bg=myblue, cex=ptcex))
#plot observed set
with(PSObsShk, points(    newlon,   newlat ,pch=21, col=myorange, bg=myorange, cex=ptcex))
# plot observed catch
with(PSObsShk[PSObsShk$totalshk>0,], points( newlon,  newlat , pch=21, col=myred ,bg=myred , cex=ptcex )    )



map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region =c(cntrylst), fill=T, add=T, yaxt="n", xaxt="n", col=grey(0.001))

#  Add title 
#title("Observed Shark Catch and Effort \nPurse Seine") 
 


#source("C:/Projects/SHK-indicators-2015/CODE/map_lines.r") # this also puts
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")

box("plot")
legend('topright',  legend=c("Set", "Observed Set", "Obsereved Shark Catch"), pch=21,col=1,  pt.bg=c(myblue, myorange,myred ) , pt.cex=1.5)


dev.off()

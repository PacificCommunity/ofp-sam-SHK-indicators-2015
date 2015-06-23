

load( file= "C:/Projects/SHK-indicators-2015/DATA/PSObs16Jun2015.RData"  ) # loads PSObsShk  # 
#make a set field to get the flag sums by
PSObsShk$set<- 1
#  head(PSObsShk)
pstop4<-  names(sort(with(PSObsShk, tapply(set, list(flag_id), sum)) ,decreasing=TRUE))[1:4]     #  get the overall top 4 recorders of sharks (ignoring region for the moment)
#  pstop4

 

ptcol<-1
ptbg<-rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)  

#____________________________________________________________________________________Start Plots.
oldpar<-par()



# Sum by flag and year for the top 4   
place_set <-with(PSObsShk[PSObsShk$flag_id%in%c(pstop4),],tapply(  set,list( flag_id, yy, region),sum) )   # hooks for the top 4
#adjust names
#pstop4<-rownames(place_set)
#All the sets
allset<-with(PSObsShk, tapply(set, list(yy,region), sum))      #total sets by region
#calc total top 4 sets
totalset <-colSums(place_set,na.rm="T")    #total top 4 hooks by region
#calc other sets
otherset<-allset[,c("3","4", "5", "6")]-totalset[,c("3","4", "5", "6")]                #total other hooks
#make final data list   
pDat<-list()   
for(i in c(1:4)){ 
  j<-i 
  pDat[[j]]<-  rbind(place_set[pstop4,,i],OT=t(otherset[,i])) 
  pDat[[j]]<- ifelse(is.na(pDat[[j]]),0,pDat[[j]]) 
  rownames(pDat[[j]])<-c(pstop4,"OT") 
  pDat[[j]]<-round(pDat[[j]] ) }
# pDat
pssurf<-with(PSObsShk,tapply(set,list(lon5, lat5), FUN=length))
pssurf<-ifelse(is.na(pssurf),0,pssurf)
# pssurf


hues<-c( gray(0), gray(10/40), gray(20/40), gray(30/40), gray(1))                #                 gray(0)=black;    gray(1)=white
huenames <-  c(pstop4,"OT")
#par(mar = par("mar")/1.5)        #1.6414141 1.6414141 1.1363636 0.2525252  ,-mar, and mai0.21666667 0.21666667 0.15000000 0.03333333

par(mar=c(1.6,1.6,1.13,0.2))



#
 #

#nf<-layout( matrix(c(15,1,2,15,3,4,15,5,6,15,7,7,16,8,9,16,10,11,16,12,13,16,14, 14),8,3,byrow=TRUE), widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
nf<-layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))

scalecex<-0.75 # the character expansion for the text on the scale
legcex<-1.25  # size of the legend
#
# 
layout.show(nf)# 

#loop over the areas 
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_eMAP_sets.png",sep='') )     # THIS ONLY SAVES THE LAST PLOT?
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
bpltylim<-c(0,20000)
par(mar=c(2, 2, 2, 1) + 0.1)
# par(mai=c(0.65,0.65,0.25,0.1))
plot.new()    # "draws" a blank space in reg 1
plot.new()    # "draws" a blank space in reg 2
bpltylim<-c(0,20000)
for (i in c(1:4)) {   #loop over areas
  if(i==3){bpltylim<-c(0,300)}  
  barplot(pDat[[i]][which(rowSums(pDat[[i]])>0),],ylab="",xlab="",names.arg=colnames(pDat[[i]]),col=hues,ylim=bpltylim)
  mtext(side=3,paste("Region ", as.character(i+2)),line=-0.2, cex=0.8)
  if(i==2){ mtext(side=2,outer=F,"Number of sets observed",line=2,cex=0.75)     }
}


plot.new()
legend("center",legend=huenames ,fill=hues ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=T)    #bty="n" (no box);

 dev.off()

#-----------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------

##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##
par(mar=c(4, 4, 4, 2) + 0.1)
par(mai=c(0.65,0.65 ,0.25,0.1))


This one is still kinda fucked.

# plot.new()         # do panels C and A seperately  and stitch them together.
plot(180,0,type="n", xlim=mapxlim,ylim=c(-60,50), cex=1, las=1, ylab="Latitude", xlab="Longitude", main="Observed purse seine sets")   # 
image(as.numeric(rownames(pssurf)),as.numeric( colnames(pssurf)), log(pssurf+1), col=rev(heat.colors(15)), add=T) # greyscale grey(1:15/15)
abline(h=0, lty=1, lwd=2, col=grey(0.8))     
abline(v=180, lty=1, lwd=2, col=grey(0.8)) 
#  add points
#  points( PSObsShk$newlon,PSObsShk$newlat, col='white',pch=21, bg=rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)   ,lwd=1.5,cex=1.5 )
#map and lines
source("C:/Projects/SHK-indicators-2015/CODE/map_lines.r")


#scale bar & arrow and & key  

text(  mapxlim[2]-10,5,"# of Sets \n Observed", pos=3)
map.scale(110, -55,ratio=F, cex=scalecex)
arrows(x0=110,y0=-56,x1=110,y1=-46, length=0.15)
text(110,-44,"N", cex=1.5)
box("plot")



#scale bar   
rhs<-240
top<-4
btm<-c(-55)
off<-4
myvals <- 1:15
image(x=c( rhs-off,rhs-0.5) ,y=c(round(seq(  btm,top, length.out=15))), z=t(myvals), col = rev(grey(1:15/15)),   add=T)
axis(4, at=c(round(seq( btm,top, length.out=7))),    pos=c(rhs+1,top),  labels=c(seq(0,6000,by=1000)), las=1, cex=0.65 )

lines(x=c( rhs-off-2,rhs+1), y=c(btm-2,btm-2))  
lines(x=c( rhs-off-2,rhs+1), y=c(top+2,top+2))  

lines(x=c( rhs-off-2,rhs-off-2), y=c(btm-2,top+2))   
lines(x=c( rhs+1 ,rhs+1 ), y=c(btm-2,top+2))  

# image.plot( zlim=c( 0,round(max(pssurf, na.rm=T))),legend.only=T, col=rev(grey(1:15/15)), legend.shrink=0.5,smallplot = c(0.83,0.86,0.18,0.6) , graphics.reset=FALSE)   #small plot is in x=[0,1] X y=[0,1]


#place Panel text  & lines


pushViewport(viewport())
grid.lines(x = c(0,1), y = 0.5, gp = gpar(col = "black", lwd=2))
grid.lines(x = 0.5, y =c(0,1), gp = gpar(col = "black", lwd=2))

grid.text("(a)", x = unit(0.0155, "npc"), y = unit(0.99, "npc")) 
grid.text("(b)", x = unit(1-0.0155, "npc"), y = unit(0.99, "npc")) 
grid.text("(c)", x = unit(0.0155, "npc"), y = unit(0.49, "npc")) 
grid.text("(d)", x = unit(1-0.0155, "npc"), y = unit(0.49, "npc")) 






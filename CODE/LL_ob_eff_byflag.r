
#############################################
#  Joel Rice
# this code produces a regional barplot of the top 4 fishing nations in each region.
#
#



library(maps)
library(mapproj)
library(mapdata)

library(grid)
library(gridBase)


shk <- shk_all
top4<-  names(sort(with(shk, tapply(hook_est, list(flag_id), sum)) ,decreasing=TRUE)[1:4])     #  get the overall top 4 recorders of sharks (ignoring region for the moment)

placehook <-with(shk[shk$flag_id%in%c(top4),],tapply(  hook_est,list( flag_id, yy, region),sum) )   # hooks for the top 4
top4<-rownames(placehook)
allhook<-with(shk, tapply(hook_est, list(yy,region), sum))      #total hooks by region

totalhook <-colSums(placehook,na.rm="T")    #total top 4 hooks 
otherhook<-allhook-totalhook                #total other hooks

Dat<-list()   #make final data list    will have totals in 1000's of hooks
for(i in 1:dim(placehook) [3]){ Dat[[i]]<-  rbind(placehook[,,i],OT=t(otherhook[,i])) 
                                Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                rownames(Dat[[i]])<-c(top4,"OT") 
                                Dat[[i]]<-round(Dat[[i]]/1000,2) }
#Dat
shk$set<-1
llsurf<-with(shk,tapply(set,      list(lon5, lat5), FUN=length))
llsurf<-ifelse(is.na(llsurf),0,llsurf)


#PLOTTING (in units of Thousands of hooks)
options(scipen=10)   #suppresses scientific notation on the y axis

#
top4<-  names(sort(with(shk, tapply(hook_est, list(flag_id), sum)) ,decreasing=TRUE)[1:4])     #  get the overall top 4 
r2p<- match(top4, rownames(Dat[[1]]), )
 #rownames(Dat[[1]])[r2p]

huenames <-  c(rownames(Dat[[1]])[r2p],"OT")
hues<-c( gray(0), gray(10/40), gray(20/40), gray(30/40), gray(1))                #                 gray(0)=black;    gray(1)=white


par(mar =c(1.6414141, 1.6414141, 1.1363636, 0.2525252)
   
nf<-layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))

scalecex<-0.75 # the character expansion for the text on the scale
legcex<-1.25  # size of the legend
#
# 
layout.show(nf)# 

#loop over the areas 

#loop over the areas 
png(file=paste(shkdir,"GRAPHICS/FIG_xx_LLeff_FLAG.png",sep='') )  
par(mar=c( 2.55,2.05, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#loop over the areas 
for (i in c(1:6)) {   #loop over areas
  barplot(Dat[[i]][huenames,],ylab="",xlab="",names.arg=colnames(Dat[[i]]),col=hues,ylim=c(0,6500))
  mtext(side=3,paste("Region ", as.character(i)),line=1, cex=0.8)
  # if(i==1){text(par("usr")[1] ,par("usr")[4] , "(b)", col=1, cex=1.5, pos=1)  }
  if(i%in%3:4){mtext(side=2,outer=F,"Total Hooks Observed (Thousand Hooks)",line=2,cex=0.75)}
}

par(mar = par("mar")/2)
plot.new()
legend("center",legend=huenames ,fill=hues ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=T)    #bty="n" (no box);
#mtext("(b)", side=3,outer=T,  adj = 0, padj=1 , cex=1.5 )          # FIX THIS 
# savePlot("P:/shark/Ind_Shk_Paper/IndPaper_Fig1_panB",type="pdf")    
#mtext("Observed longline sets by year & flag", side=3,outer=T, line=-1 ) 
#box("inner")

dev.off()
 
 

##---------------------------------------    map for the effort--------------------------------##
par(mar=c(4, 4, 4, 2) + 0.1)
par(mai=c(0.65,0.65 ,0.25,0.1))
par(mfrow=c(1,1))
mapxlim<-c(110,260) # this defines how far out we make the map, the eastern boundary of region 6 is at 230 

scalecex<-0.75 # the character expansion for the text on the scale
legcex<-1.25  # size of the legend
cntrylst<- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela", "Cuba", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii")
nams <- map("world", namesonly=TRUE, plot=FALSE)

ptcol<-1
ptbg<-rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)  

##set up plot region 
png(file=paste(shkdir,"GRAPHICS/FIG_xx_obs_ll_sets.png",sep='') )  
plot(180,0,type="n", xlim=mapxlim,ylim=c(-60,50), cex=1, las=1, ylab="Latitude", xlab="Longitude", main="Observed Longline Sets")   # 
image(as.numeric(rownames(llsurf)),as.numeric( colnames(llsurf)), log(llsurf+1), col=rev( heat.colors(15) ), add=T) 
#equator and int. date line
abline(h=0, lty=1, lwd=2, col=grey(0.8))     
abline(v=180, lty=1, lwd=2, col=grey(0.8)) 
# Add Points
#  points( shk$newlon,shk$newlat, col=ptcol,pch=21, bg=ptbg   ,lwd=1.5,cex=1.5 )
# Map and lines

text( mapxlim[2]-10,5,"# of Sets \n Observed", pos=3)   #bad
map.scale(110, -55,ratio=F, cex=scalecex)
arrows(x0=110,y0=-56,x1=110,y1=-46, length=0.15)
text(110,-44,"N", cex=1.5)
box("plot")
# box("figure")
#plot the map
source("C:/Projects/SHK-indicators-2015/CODE/map_lines.r")

max(llsurf, na.rm=T)

#scale bar   
rhs<-240
top<-4
btm<-c(-55)
off<-4
myvals <- 1:15
image(x=c( rhs-off-1.5,rhs+1) ,y=c(round(seq(  btm,top, length.out=15))), z=t(myvals), col = rev(heat.colors(15) ),   add=T)#  grey color scheme is grey(1:15/15)
axis(4, at=c(round(seq( btm,top, length.out=8))),    pos=c(rhs+1,top),  labels=c(seq(0,7000,by=1000)), las=1, cex=0.65 )

lines(x=c( rhs-off-2,rhs+1), y=c(btm-2,btm-2))  
lines(x=c( rhs-off-2,rhs+1), y=c(top+2,top+2))  

lines(x=c( rhs-off-2,rhs-off-2), y=c(btm-2,top+2))   
lines(x=c( rhs+1 ,rhs+1 ), y=c(btm-2,top+2))   

dev.off()

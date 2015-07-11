



# Figure 1 in response to the reviews comments (follows)
# but no Kriging

#Figures.
#
#The figures need a lot of work to fully communicate the scale and significance of the authors' findings. The figures should stand alone and the reader should be able to 'get' them without reading the text or legend.
#
#I strongly recommend that all hatching is removed from all graphs. Hatching creates a visually disturbing moiré effect which distracts the reader from the point of the graph. Instead use shade of grey or question what additional is added by the shading
#
#Figure 1. The extent of this study is not quite clear, to clarify the vast area of this study please consider adding a tiny bit more of South America and maybe consider plotting the equator and dateline (in a visually subordinate shade of grey.
#
#There is considerable over plotting; some form of density map, say produced by kriging or some other smoother, is required.
#
#The link between the right hand and left-hand panels could be made clearer if the histograms were arranged around the maps.
#
#The maps are cluttered and would be improved by downplaying the shading of countries using a mid-grey shade.
#
#Are the shadings on histograms necessary?  Given that you offer no key, it suggests to me that they are not. 
#
#What do the hieroglyphics of US=JP=FJ=PF=OT mean?
#
#Drop region 1, 2, 5, 6 from d. 
#
#


rm(list = ls(all = TRUE))
gc()

memory.limit(size=NA)
#memory.limit(size=4000)
memory.limit(size=NA)

      
library(maps)
library(mapproj)
library(mapdata)

library(grid)
library(gridBase)



# load data and do prelim calcs
# ---------------------Longline_______________________________________________--#
load("H:/SC8_shark assessments/SC7_shark work/DATA_clean/SharkLLObs.Rdata")
# load("P:\\WCPFC Shark\\SharkLLObs.Rdata")
 top4<-  names(sort(with(shk, tapply(hook_est, list(flag_id), sum)) ,decreasing=TRUE)[1:4])     #  get the overall top 4 recorders of sharks (ignoring region for the moment)

placehook <-with(shk[shk$flag_id%in%c(top4),],tapply(  hook_est,list( flag_id, yy, region),sum) )   # hooks for the top 4
 top4<-rownames(placehook)
allhook<-with(shk, tapply(hook_est, list(yy,region), sum))      #total hooks by region

totalhook <-colSums(placehook,na.rm="T")    #total top 4 hooks by region
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
   #  llsurf
#     dim(llsurf)
#length(unique(shk$lon5))     
#length(unique(shk$lat5))
#par(mfrow=c(1,1))
#image(as.numeric(rownames(llsurf)),as.numeric( colnames(llsurf)), llsurf)
#contour(as.numeric(rownames(llsurf)),as.numeric( colnames(llsurf)), llsurf, add=T)
#
  
# -----------------------------------Purse Seine--------------------------------#
  
#load("P:\\WCPFC Shark\\SharkPSObs.RData")  
load("H:/SC8_shark assessments/SC7_shark work/DATA_clean/SharkPSObs.Rdata")

#make a set field to get the flag sums by
  PSObsShk$set<- 1
#  head(PSObsShk)
  pstop4<-  names(sort(with(PSObsShk, tapply(set, list(flag_id), sum)) ,decreasing=TRUE)[1:4])     #  get the overall top 4 recorders of sharks (ignoring region for the moment)
#  pstop4
   
# Sum by flag and year for the top 4   
place_set <-with(PSObsShk[PSObsShk$flag_id%in%c(pstop4),],tapply(  set,list( flag_id, yy, region),sum) )   # hooks for the top 4
#adjust names
pstop4<-rownames(place_set)
#All the sets
allset<-with(PSObsShk, tapply(set, list(yy,region), sum))      #total hooks by region
#calc total top 4 sets
totalset <-colSums(place_set,na.rm="T")    #total top 4 hooks by region
#calc other sets
otherset<-allset-totalset                #total other hooks
  #make final data list   
 pDat<-list()   
  for(i in c(1:4)){ 
  j<-i 
  pDat[[j]]<-  rbind(place_set[,,i],OT=t(otherset[,i])) 
  pDat[[j]]<- ifelse(is.na(pDat[[j]]),0,pDat[[j]]) 
  rownames(pDat[[j]])<-c(pstop4,"OT") 
  pDat[[j]]<-round(pDat[[j]] ) }
 # pDat
     pssurf<-with(PSObsShk,tapply(set,list(lon5, lat5), FUN=length))
     pssurf<-ifelse(is.na(pssurf),0,pssurf)
    # pssurf
#     dim(pssurf)
#length(unique(PSObsShk$lon5))     
#length(unique(PSObsShk$lat5))
#par(mfrow=c(1,1))
#image(as.numeric(rownames(pssurf)),as.numeric( colnames(pssurf)), pssurf)
#contour(as.numeric(rownames(pssurf)),as.numeric( colnames(pssurf)), pssurf, add=T)
#
#____________________________________________________________________________________initial settings

eez <-read.table("P:\\WCPFC Shark\\EZNEW2.txt", sep="", header=F)
 
  
 mapxlim<-c(110,260) # this defines how far out we make the map, the eastern boundary of region 6 is at 230 
 
 scalecex<-0.75 # the character expansion for the text on the scale
  legcex<-1.25  # size of the legend
cntrylst<- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela", "Cuba", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii")
nams <- map("world", namesonly=TRUE, plot=FALSE)

ptcol<-1
ptbg<-rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)  

#____________________________________________________________________________________Start Plots.
     oldpar<-par()
 
 par(omi=c(0,0,0.2,0))
 par(oma=c(0,0,1,0))
 #
 #
 #
 #
 
 windows(11,14)                                                                                             
# nf<-layout( matrix(c(1,2,3,1,4,5,1,6,7,1,8,8,9,10,11,9,12,13,9,14,15, 9,16,16),8,3,byrow=TRUE) , widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
 nf<-layout( matrix(c(15,1,2,15,3,4,15,5,6,15,7,7,16,8,9,16,10,11,16,12,13,16,14, 14),8,3,byrow=TRUE), widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
 
layout.show(nf)# 
 

#par(new=F)    #  a line that forces R to stay on the same graph       IF TRUE
 #par(bty='n')
#par( pty = "s")# square plotting region
# Create the base plotting window
# Set the background color to "white"
#par(bg = "white")

par(new=F)        
   #  par(oma=c(0.1,0.1,0.1,0.1)) # this calls a new plot window    unless new is true
   # par(oma=c(0.5,3,0.5,0.5))
    par(mar=c(4, 4, 4, 2) + 0.1.)
    par(mai=c(0.65,0.65,0.45,0.1))


 #  plot.new()

##---PANEL B----##---PANEL B----##---PANEL B----##---PANEL B----##---PANEL B----##---PANEL B----##---PANEL B----##---PANEL B----##
 

#PLOTTING (in units of Thousands of hooks)
  options(scipen=10)   #suppresses scientific notation on the y axis
  
 
 hues<-c( gray(0), gray(10/40), gray(20/40), gray(30/40), gray(1))                #                 gray(0)=black;    gray(1)=white
 huenames <-  c(top4,"OT")
 
 par(mar = par("mar")/1.5)        #1.6414141 1.6414141 1.1363636 0.2525252  ,-mar, and mai0.21666667 0.21666667 0.15000000 0.03333333



 #loop over the areas 
      for (i in c(1:6)) {   #loop over areas
          barplot(Dat[[i]],ylab="",xlab="",names.arg=colnames(Dat[[i]]),col=hues,ylim=c(0,5000))
          mtext(side=3,paste("Region ", as.character(i)),line=-0.2, cex=0.8)
         # if(i==1){text(par("usr")[1] ,par("usr")[4] , "(b)", col=1, cex=1.5, pos=1)  }
          if(i==4){mtext(side=2,outer=F,"Total Hooks Observed (Thousand Hooks)",line=2,cex=0.75)}
          }
           par(mar = par("mar")/2)
        plot.new()
         legend("center",legend=huenames ,fill=hues ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=T)    #bty="n" (no box);
          #mtext("(b)", side=3,outer=T,  adj = 0, padj=1 , cex=1.5 )          # FIX THIS 
        # savePlot("P:/shark/Ind_Shk_Paper/IndPaper_Fig1_panB",type="pdf")    
         mtext("Observed longline sets by year & flag", side=3,outer=F, line=31 ) 
         box("inner")
         
 ##---PANEL D----##---PANEL D----##---PANEL D----##---PANEL D----##---PANEL D----##---PANEL D----##---PANEL D----##---PANEL D----##
 
 hues<-c( gray(0), gray(10/40), gray(20/40), gray(30/40), gray(1))                #                 gray(0)=black;    gray(1)=white
 huenames <-  c(pstop4,"OT")
      
    par(mar=c(2, 2, 2, 1) + 0.1)
   # par(mai=c(0.65,0.65,0.25,0.1))
     plot.new()    # "draws" a blank space in box 7 and then the legend will plot over this blank
     plot.new()  # "draws" a blank space in box 7 and then the legend will plot over this blank
     bpltylim<-c(0,5000)
      for (i in c(1:4)) {   #loop over areas
          if(i==3){bpltylim<-c(0,75)}  
          barplot(pDat[[i]],ylab="",xlab="",names.arg=colnames(pDat[[i]]),col=hues,ylim=bpltylim)
          mtext(side=3,paste("Region ", as.character(i+2)),line=-0.2, cex=0.8)
          if(i==2){ mtext(side=2,outer=F,"Number of sets observed",line=2,cex=0.75)     }
          }
    plot.new()
          legend("center",legend=huenames ,fill=hues ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=T)    #bty="n" (no box);

        #  mtext("(d)", side=3,outer=T,  adj = 0 , padj= .5 , cex=1.5 )          # FIX THIS 
   
   
  
  mtext("Observed purse seine sets by year & flag", side=3,outer=F, line=31 )   





 
##---PANEL A----##---PANEL A----##---PANEL A----##---PANEL A----##---PANEL A----##---PANEL A----##---PANEL A----##---PANEL A----##
     par(mar=c(4, 4, 4, 2) + 0.1)
     par(mai=c(0.65,0.65 ,0.25,0.1))

    ##set up plot region 
       plot(180,0,type="n", xlim=mapxlim,ylim=c(-60,50), cex=1, las=1, ylab="Latitude", xlab="Longitude", main="Observed Longline Sets")   # 
      image(as.numeric(rownames(llsurf)),as.numeric( colnames(llsurf)), log(llsurf+1), col=rev(grey(1:15/15)), add=T) 
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
     source("H:/SC8_shark assessments/Ind_Shk_Paper/map_lines.r")
   
   
   
  #scale bar   
   rhs<-240
  top<-4
  btm<-c(-55)
  off<-4
  image(x=c( rhs-off,rhs-0.5) ,y=c(round(seq(  btm,top, length.out=15))), z=t(myvals), col = rev(grey(1:15/15)),   add=T)
  axis(4, at=c(round(seq( btm,top, length.out=5))),    pos=c(rhs+1,top),  labels=c(seq(0,2000,by=500)), las=1, cex=0.65 )
  
    lines(x=c( rhs-off-2,rhs+1), y=c(btm-2,btm-2))  
    lines(x=c( rhs-off-2,rhs+1), y=c(top+2,top+2))  
  
  lines(x=c( rhs-off-2,rhs-off-2), y=c(btm-2,top+2))   
   lines(x=c( rhs+1 ,rhs+1 ), y=c(btm-2,top+2))   
   ##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##---PANEL C----##
     par(mar=c(4, 4, 4, 2) + 0.1)
   par(mai=c(0.65,0.65 ,0.25,0.1))




 # plot.new()         # do panels C and A seperately  and stitch them together.
  plot(180,0,type="n", xlim=mapxlim,ylim=c(-60,50), cex=1, las=1, ylab="Latitude", xlab="Longitude", main="Observed purse seine sets")   # 
  image(as.numeric(rownames(pssurf)),as.numeric( colnames(pssurf)), log(pssurf+1), col=rev(grey(1:15/15)), add=T) 
  abline(h=0, lty=1, lwd=2, col=grey(0.8))     
  abline(v=180, lty=1, lwd=2, col=grey(0.8)) 
  #  add points
  #  points( PSObsShk$newlon,PSObsShk$newlat, col='white',pch=21, bg=rgb(red=105,green=105, blue=75, alpha=75, maxColorValue=255)   ,lwd=1.5,cex=1.5 )
  #map and lines
  source("H:/SC8_shark assessments/Ind_Shk_Paper/map_lines.r")
  #scale bar & arrow and & key  
 
  text(  mapxlim[2]-10,5,"# of Sets \n Observed", pos=3)
  map.scale(110, -55,ratio=F, cex=scalecex)
  arrows(x0=110,y0=-56,x1=110,y1=-46, length=0.15)
  text(110,-44,"N", cex=1.5)
   box("plot")
  
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


popViewport()
#   plot.new()  
 
 ######################
 
 
 
  box("outer")
 
    savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig1_",type="pdf") 
   # savePlot("P:/shark/Ind_Shk_Paper/IndPaper_Fig1_Combined_temp",type="png") 
    savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig1_",type="eps") 
       
#########################################
   
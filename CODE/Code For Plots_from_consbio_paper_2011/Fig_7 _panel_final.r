

# loads fate and condition data (pre processed) and plots a 4 panel figure 7 for the shark indicators paper

#library(grid)
#library(gridBase)
#


#rm(list=ls(all=TRUE))
#gc()
#
#memory.limit(size=NA)
## Load fate and condition final  data
# load(file="C:/Users/joelr/Dropbox/clarke_shark_consbiol/Code For Plots/ObsDta/LL_FateCond_by_yr.rdata")  # loads objects fate and Combo   both are longline
# load(file="C:/Users/joelr/Dropbox/clarke_shark_consbiol/Code For Plots/ObsDta/PS_FateCond_by_yr_NEW.rdata"  ) # not really new I just changed the names from combo & fate to pscombo and psfate 
#
 #save par
# oldpar<-par()
 
 par(omi=c(0,0,0.2,0))
 par(oma=c(0,0,1,0))
 par(las=1)
 #
 #
 #
 par()
 
 windows(11,14)                                                                                             
 nf<-layout( matrix(c(1,2,3,1,4,5,1,6,7,1,8,8,9,10,11,9,12,13,9,14,15, 9,16,16),8,3,byrow=TRUE) , widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
# nf<-layout( matrix(c(15,1,2,15,3,4,15,5,6,15,7,7,16,8,9,16,10,11,16,12,13,16,14, 14),8,3,byrow=TRUE), widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))

layout.show(nf)# 

  figmar<-c(4, 4, 4, 2) + 0.1
  figmai<-c(0.65,0.65,0.45,0.1)
  
  legmar<-c(0,0,0,0)
  
# par(mar=figmar)
 par(mai=figmai)    
     par("mai")
  hues<-c( gray(1), gray(25/30), gray(11/30), gray(0))                #                 gray(0)=black;    gray(1)=white
  huenames <- c("escaped","retained","discarded","finned")
 
    

#------Panel A
 # par(mfrow=c(1,1),mar=c(3,2,2,1),omi=c(0.5,0.5,1,0),ask=TRUE)
  barplot(combo,ylab="",xlab="",names.arg=c(1995:2010),col=hues,ylim=c(0,40000), las=1)         
  legend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.35,horiz=F,bty="n", ncol=1)
  mtext(side=2,outer=F,"Number Recorded by Observers",line=3.75,cex=1 )
  mtext(side=3,outer=F,"Fate of Observed Sharks - Longline",line=1.2,cex=1 )                     #WCPO
  mtext(side=1, outer=F, "Year", line=2)
 

#panel B

# Plot the fate condition year by year in each region
 
 
  par(mar = figmar/1.5) 
           par("mai")
  #plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Area 1 would be
      for (i in c(1:6)) {   #loop over areas
          barplot(fate[[i]]/1000,ylab="",xlab="",names.arg=c(1995:2010),col=hues,ylim=c(0,18000)/1000, las=1, cex.axis=1)   
          mtext(side=3,paste("Region ", as.character(i)),line=-0.4, cex=0.8)
          if(i%in%c( 4)){   mtext(side=2,outer=F,"Number Recorded by Observers (Thousands)",line=2.25,cex=1 )}
          mtext(side=1, outer=F, "Year",cex=0.8, line=2)
          }
         
        par(mai=legmar)  
        plot.new()
          legend("center",legend=huenames[1:4],fill=hues[1:4],cex=1.5,horiz=F,bty="n", ncol=2)     #bty="n" (no box);
           mtext("Fate by Region, Longline", side=3,outer=F, line=29) 



 #------------------Panel C  -    Purse seine fate and condition
 
# par(mar=figmar)
 par(mai=figmai)
  barplot(pscombo,ylab="",xlab="",names.arg=c(1995:2010),col=hues,ylim=c(0,3750), las=1)           #ylim=c(0,40000)                         #JR note new scale w/ 2010
  legend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.35,horiz=F,bty="n", ncol=1)
  mtext(side=2,outer=F,"Number Recorded by Observers",line=3.75,cex=1 )
  mtext(side=3,outer=F,"Fate of Observed Sharks - Purse Seine",line=1,cex=1 )                     #WCPO
   mtext(side=1, outer=F, "Year", line=2)
#------------------Panel D
# plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Area 1 would be
  par(mar = figmar/1.5) 
  plot.new()
  plot.new()
      for (i in c(3:4)) {   #loop over areas
          barplot(psfate[[i]]/1000,ylab="",xlab="",names.arg=c(1995:2010),col=hues,ylim=c(0,2500)/1000, las=1)   
          mtext(side=3,paste("Region ", as.character(i)),line=-0.2, cex=0.8)
          if(i%in%c( 4)){   mtext(side=2,outer=F,"Number Recorded by Observers (Thousands)     ",line=2.25,cex=1 )}
            mtext(side=1, outer=F, "Year",cex=0.8, line=2)
          }
           
  plot.new()
  plot.new()          
          #
          par(mai=legmar)  
        plot.new()
          legend("bottom",legend=huenames[1:4],fill=hues[1:4],cex=1.5,horiz=F,bty="n", ncol=2)    #bty="n" (no box);

           mtext("Fate by Region, Purse Seine", side=3,outer=F, line=28.5) 
#------------------    place Panel text  & lines
 

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
 
 
 
 # box("outer")
 
    savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig_7final",type="pdf") 
  # savePlot("P:/shark/Ind_Shk_Paper/IndPaper_Fig1_Combined_temp",type="png") 
   savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig_7final",type="eps") 
       
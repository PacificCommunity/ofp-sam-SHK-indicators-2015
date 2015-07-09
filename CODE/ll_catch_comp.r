
# this plots the shark species composition by year for LL 
# as a percentage
#  

#  Plot the the proportional catch of main sharks  annually 
#  w/total shark
#
par(xpd=NA)
par(las=1,   omi=c(1,1,0.2,0.1) )
#init calcs : sharks per 1000 hooks by region & yr
teff <-   tapply(shk_all$hook_est,  list(shk_all$region, shk_all$yy), sum)
#
tshark <- tapply(shk_all$othershk, list(shk_all$region, shk_all$yy), sum) 
tbsh <- tapply(shk_all$blue, list(shk_all$region, shk_all$yy), sum) 
tmak <- tapply(shk_all$mako, list(shk_all$region, shk_all$yy), sum) 
tocs <- tapply(shk_all$ocs,  list(shk_all$region, shk_all$yy), sum) 
tfal <- tapply(shk_all$silky,list(shk_all$region, shk_all$yy), sum) 
tthr <- tapply(shk_all$thresher,  list(shk_all$region, shk_all$yy), sum) 



#start calcs and plot

png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_llshks_pcnt.png",sep='')) 
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
par(mar=c(3, 2, 2, 1) + 0.1)
par(las=1,   oma=c(2,2,3.5,1) )
 
for(i in 1:nreg){
 
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,])
  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"white") , main=paste("Region", i), las=1 )
  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
} 
plot.new()
#par(mar=c(0.5,0.5,0.5,0.5)  )
legend("center",legend=c(spec, 'Other Shark' ) ,fill=c(mycol, 'white' ) , cex=1, horiz=TRUE,bty="n",  title="Species",xpd=NA)     

dev.off()
#
#---------------------------------------


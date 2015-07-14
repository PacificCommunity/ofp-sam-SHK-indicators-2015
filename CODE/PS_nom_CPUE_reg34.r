

# Plot the nomial PS cpue for the sharks in region 3 and 4 


# this is the newest but psobs <- read.csv("C:/Projects/DATA_2015/PS/ps_obs_set_shk.csv", header=TRUE ) ...not 'processed
# psobs <-psobs[psobs$yy %in% 1995:2014,]
head(psobs)
table(psobs$POR>0)

rm(psobs)
load("C:/Projects/SHK-indicators-2015_backup/DATA/PSObs16Jun2015.RData")
 

head(PSObsShk)

HHD <- c('SPN','SPZ','SPL','SPK','EUB')
PSObsShk$HHD <- rowSums(  PSObsShk[,HHD  ] )

table(PSObsShk$HHD>0)

FALSE   TRUE 
152486    141 
 table(PSObsShk$POR>0)
FALSE 
152627

# so basically no HHD or POR beagle?
head(PSObsShk)
table(PSObsShk$ez_id)
t

par(xpd=NA)
par(las=1,   omi=c(1,1,0.2,0.1) )

PSObsShk<- PSObsShk[PSObsShk$yy %in% 1995:2014,]
tps <- PSObsShk[PSObsShk$region %in% 3:4, ] 
 
#spec<- c("BSH", "MAK", "OCS","FAL", "THR", "HHD", "POR"); nspec<- length(spec)
tbsh <- tapply(tps$blue, list(tps$region, tps$yy), mean) 
tmak <- tapply(tps$mako, list(tps$region, tps$yy), mean)  
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), mean)  
tfal <- tapply(tps$silky,list(tps$region, tps$yy), mean)  
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), mean)
thhd <- tapply(tps$HHD, list(tps$region, tps$yy), mean)
thhd <- tapply(tps$HHD, list(tps$region, tps$yy), mean)

table(PSObsShk$HHD>0)
table(PSObsShk$POR>0)

tshark <- tapply(tps$othershk, list(tps$region, tps$yy), mean)  



#plot file
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg3.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
  plot(colnames(tbsh), tbsh[1,], col=mycol[1],type='b', pch=16,las=1,  main= paste('BSH (n=', sum(tps[tps$region==3, 'blue']), ')', sep=''), ylab='', xlab='')
  plot(colnames(tmak), tmak[1,], col=mycol[2],type='b', pch=16,las=1,  main= paste('MAK (n=', sum(tps[tps$region==3, 'mako']), ')', sep=''), ylab='', xlab='')
  plot(colnames(tocs), tocs[1,], col=mycol[3],type='b', pch=16,las=1,  main= paste('OCS (n=', sum(tps[tps$region==3, 'ocs']), ')', sep=''), ylab='', xlab='')
  plot(colnames(tfal), tfal[1,], col=mycol[4],type='b', pch=16,las=1,  main= paste('FAL (n=', sum(tps[tps$region==3, 'silky']), ')', sep=''), ylab='', xlab='')
  plot(colnames(tthr), tthr[1,], col=mycol[5],type='b', pch=16,las=1,  main= paste('THR (n=', sum(tps[tps$region==3, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - All Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 3",line=-1.5,cex=1 , las=1 )


dev.off()

#------------------------------------------------------
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg4.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
plot(colnames(tbsh), tbsh[2,], col=mycol[1],type='b', pch=16, las=1, main= paste('BSH (n=', sum(tps[tps$region==4, 'blue']), ')', sep=''), ylab='', xlab='')
plot(colnames(tmak), tmak[2,], col=mycol[2],type='b', pch=16, las=1, main= paste('MAK (n=', sum(tps[tps$region==4, 'mako']), ')', sep=''), ylab='', xlab='')
plot(colnames(tocs), tocs[2,], col=mycol[3],type='b', pch=16, las=1, main= paste('OCS (n=', sum(tps[tps$region==4, 'ocs']), ')', sep=''), ylab='', xlab='')
plot(colnames(tfal), tfal[2,], col=mycol[4],type='b', pch=16, las=1, main= paste('FAL (n=', sum(tps[tps$region==4, 'silky']), ')', sep=''), ylab='', xlab='')
plot(colnames(tthr), tthr[2,], col=mycol[5],type='b', pch=16, las=1, main= paste('THR (n=', sum(tps[tps$region==4, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - All Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 4",line=-1.5,cex=1 , las=1 )


dev.off()

#---------------------------------------------------------------------
#---------------------------------------------------------------------
#--------------                Associated            -----------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------
 

tps <- PSObsShk[PSObsShk$region %in% 3:4, ] 
tps <- tps[tps$asso=='A',]

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), mean) 
tmak <- tapply(tps$mako, list(tps$region, tps$yy), mean)  
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), mean)  
tfal <- tapply(tps$silky,list(tps$region, tps$yy), mean)  
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), mean)  
tshark <- tapply(tps$othershk, list(tps$region, tps$yy), mean)  



#plot file
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg3_assoc.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
plot(colnames(tbsh), tbsh[1,], col=mycol[1],type='b', pch=16,las=1,  main= paste('BSH (n=', sum(tps[tps$region==3, 'blue']), ')', sep=''), ylab='', xlab='')
plot(colnames(tmak), tmak[1,], col=mycol[2],type='b', pch=16,las=1,  main= paste('MAK (n=', sum(tps[tps$region==3, 'mako']), ')', sep=''), ylab='', xlab='')
plot(colnames(tocs), tocs[1,], col=mycol[3],type='b', pch=16,las=1,  main= paste('OCS (n=', sum(tps[tps$region==3, 'ocs']), ')', sep=''), ylab='', xlab='')
plot(colnames(tfal), tfal[1,], col=mycol[4],type='b', pch=16,las=1,  main= paste('FAL (n=', sum(tps[tps$region==3, 'silky']), ')', sep=''), ylab='', xlab='')
plot(colnames(tthr), tthr[1,], col=mycol[5],type='b', pch=16,las=1,  main= paste('THR (n=', sum(tps[tps$region==3, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - Associated Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 3",line=-1.5,cex=1 , las=1 )


dev.off()

#------------------------------------------------------
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg4_assoc.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
plot(colnames(tbsh), tbsh[2,], col=mycol[1],type='b', pch=16, las=1, main= paste('BSH (n=', sum(tps[tps$region==4, 'blue']), ')', sep=''), ylab='', xlab='')
plot(colnames(tmak), tmak[2,], col=mycol[2],type='b', pch=16, las=1, main= paste('MAK (n=', sum(tps[tps$region==4, 'mako']), ')', sep=''), ylab='', xlab='')
plot(colnames(tocs), tocs[2,], col=mycol[3],type='b', pch=16, las=1, main= paste('OCS (n=', sum(tps[tps$region==4, 'ocs']), ')', sep=''), ylab='', xlab='')
plot(colnames(tfal), tfal[2,], col=mycol[4],type='b', pch=16, las=1, main= paste('FAL (n=', sum(tps[tps$region==4, 'silky']), ')', sep=''), ylab='', xlab='')
plot(colnames(tthr), tthr[2,], col=mycol[5],type='b', pch=16, las=1, main= paste('THR (n=', sum(tps[tps$region==4, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - Associated Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 4",line=-1.5,cex=1 , las=1 )


dev.off()

#---------------------------------------------------------------------
#---------------------------------------------------------------------
#--------------                UNassociated            -----------------
#---------------------------------------------------------------------
#---------------------------------------------------------------------


tps <- PSObsShk[PSObsShk$region %in% 3:4, ] 
tps <- tps[tps$asso=='U',]

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), mean) 
tmak <- tapply(tps$mako, list(tps$region, tps$yy), mean)  
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), mean)  
tfal <- tapply(tps$silky,list(tps$region, tps$yy), mean)  
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), mean)  
tshark <- tapply(tps$othershk, list(tps$region, tps$yy), mean)  



#plot file
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg3_UNASS.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
plot(colnames(tbsh), tbsh[1,], col=mycol[1],type='b', pch=16,las=1,  main= paste('BSH (n=', sum(tps[tps$region==3, 'blue']), ')', sep=''), ylab='', xlab='')
plot(colnames(tmak), tmak[1,], col=mycol[2],type='b', pch=16,las=1,  main= paste('MAK (n=', sum(tps[tps$region==3, 'mako']), ')', sep=''), ylab='', xlab='')
plot(colnames(tocs), tocs[1,], col=mycol[3],type='b', pch=16,las=1,  main= paste('OCS (n=', sum(tps[tps$region==3, 'ocs']), ')', sep=''), ylab='', xlab='')
plot(colnames(tfal), tfal[1,], col=mycol[4],type='b', pch=16,las=1,  main= paste('FAL (n=', sum(tps[tps$region==3, 'silky']), ')', sep=''), ylab='', xlab='')
plot(colnames(tthr), tthr[1,], col=mycol[5],type='b', pch=16,las=1,  main= paste('THR (n=', sum(tps[tps$region==3, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - Unassociated Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 3",line=-1.5,cex=1 , las=1 )


dev.off()

#------------------------------------------------------
png(file=paste(shkdir,"GRAPHICS/cpue_psnom_reg4_UNASS.png",sep='')) 
# layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
# par(mar=c(3, 2, 2, 1) + 0.1)
# par(las=1,   oma=c(2,2,3.5,1) )
par(mfrow=c(3,2))
plot(colnames(tbsh), tbsh[2,], col=mycol[1],type='b', pch=16, las=1, main= paste('BSH (n=', sum(tps[tps$region==4, 'blue']), ')', sep=''), ylab='', xlab='')
plot(colnames(tmak), tmak[2,], col=mycol[2],type='b', pch=16, las=1, main= paste('MAK (n=', sum(tps[tps$region==4, 'mako']), ')', sep=''), ylab='', xlab='')
plot(colnames(tocs), tocs[2,], col=mycol[3],type='b', pch=16, las=1, main= paste('OCS (n=', sum(tps[tps$region==4, 'ocs']), ')', sep=''), ylab='', xlab='')
plot(colnames(tfal), tfal[2,], col=mycol[4],type='b', pch=16, las=1, main= paste('FAL (n=', sum(tps[tps$region==4, 'silky']), ')', sep=''), ylab='', xlab='')
plot(colnames(tthr), tthr[2,], col=mycol[5],type='b', pch=16, las=1, main= paste('THR (n=', sum(tps[tps$region==4, 'thresher']), ')', sep=''), ylab='', xlab='')
plot.new()
legend("center",legend=c(spec ) ,fill=c(mycol ) , cex=1.5,horiz=FALSE, ncol=2,bty="n",  title="Species",xpd=NA)     

mtext(side=2,outer=T,"Nominal CPUE - Unassociated Sets",line=-1.5,cex=1, las=0 )
mtext(side=3,outer=T,"REGION 4",line=-1.5,cex=1 , las=1 )


dev.off()
###################################################################

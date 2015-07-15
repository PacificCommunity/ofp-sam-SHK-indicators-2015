
# this plots the shark species composition by year for LL
# as a percentage
#  as well as the otehr bar plots of catch rates, and absolute numbers, by LL and by shallow and deep sets
#----------------------------------------------------------------------------------------------------------
#  Plot the the proportional catch of main sharks  annually
#  w/total shark

#  load(file=paste0(dat.dir, "lldata_11JULY2015.rdata"))
#
par(xpd=NA)
par(las=1,   omi=c(1,1,0.2,0.1) )
#init calcs : sharks per 1000 hooks by region & yr
shk.labs <- c(main.sharks, "SHK")
mycol.flag <- c(BSH="#4F94CD",THR="steelblue1",FAL="seagreen3",MAK="olivedrab2",HHD="goldenrod1",OCS="tomato1",
           OCS="sienna3",POR="orange",SHK="grey90")
ldat <- data.frame(count=unlist(sets[,shk.labs]),
                   sp=rep(shk.labs,each=nrow(sets)),
                   region=sets$region,
                   yy=sets$yy)
start.timer();
if(!exists("cc.bpmat")) cc.bpmat <- with(ldat, tapply(count, list(region, sp, yy), sum));
stop.timer()
#tbsh <- tapply(sets$BSH,  list(sets$region, sets$yy), sum)
#tmak <- tapply(sets$MAK,  list(sets$region, sets$yy), sum)
#tocs <- tapply(sets$OCS,  list(sets$region, sets$yy), sum, na.rm=T)
#tfal <- tapply(sets$FAL,  list(sets$region, sets$yy), sum, na.rm=T)
#tthr <- tapply(sets$THR,  list(sets$region, sets$yy), sum)
#thhd <- tapply(sets$HHD,  list(sets$region, sets$yy), sum)
#tpor <- tapply(sets$POR,  list(sets$region, sets$yy), sum)
#tshark <- tapply(sets$SHK, list(sets$region, sets$yy), sum)

bp.catch.comp <- function() {


reg.funk <- function(i) {

    tmat <- cc.bpmat[i,names(mycol),]

    colv <- mycol[rownames(tmat)]

    bp<-  barplot(100*prop.table(tmat,2), col=mycol , width=colSums(tmat, na.rm=TRUE),
                  main=paste("Region", i), las=1 ,
                  axisnames=ifelse(i %in% 5:6,TRUE,FALSE), border=NA, las=1,
                  yaxt=ifelse(i %in% c(1,3,5),"t","n"))
    box()

}


par(mfrow=c(3,2), mai=rep(0.15, 4), omi=c(0.3,0.65,1,0.1))
dmm <- sapply(1:6, reg.funk)
mtext(side=2,outer=TRUE,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )
par(mar = par("mar")/2)
lx <- grconvertX(0,from="nic")
ly <- grconvertY(0.985,from="ndc")
legend(lx, ly, legend=names(mycol), col=mycol, xpd=NA,
       pch=15, bty='n',pt.cex=3 , ncol=3, cex=1.5)

#png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_llshks_pcnt.png",sep=''))
#
#par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
#layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#par(las=1,   oma=c(2,2,3.5,1) )

#for(i in 1:nreg){
#
#  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,], tshark[i,])
#  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"white") ,border=1,space=0,  main=paste("Region", i), las=1 )
#  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
#}
#par(mar = par("mar")/2)
#plot.new()
#legend('center',  legend = c(spec,'OtherShark'), fill=c(mycol, "white"),   bty='n',cex=1.5, ncol=4)
#
#dev.off()
########################################
# no other shark

#png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_llshks_pcnt_keyshark.png",sep=''))
#
#par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
#layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#par(las=1,   oma=c(2,2,3.5,1) )

#for(i in 1:nreg){

#  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,] )
#  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"white") ,border=1,space=0, main=paste("Region", i), las=1 )
#  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
#}
#par(mar = par("mar")/2)
#plot.new()
#legend('center',  legend = c(spec ), fill=c(mycol ),   bty='n',cex=1.5 , ncol=4)

#
#dev.off()
}


run.rest <- FALSE## ADDED BY LTB
if(run.rest) {

################3
#  Plot the the cummulative CPUE (annually) for sharks of interest
# and all other sharks
# based on observer data
# head (sets)


#init calcs : sharks per 1000 hooks by region & yr
teff <-   tapply(sets$hook_est,  list(sets$region, sets$yy), sum)
#
tshark <- tapply(sets$SHK, list(sets$region, sets$yy), sum) /(teff/1000)
# BSH FAL OCS MAK THR HHD SHK SKJ POR
tbsh <- tapply(sets$BSH,  list(sets$region, sets$yy), sum) /(teff/1000)
tmak <- tapply(sets$MAK,  list(sets$region, sets$yy), sum) /(teff/1000)
tocs <- tapply(sets$OCS,  list(sets$region, sets$yy), sum, na.rm=T) /(teff/1000)
tfal <- tapply(sets$FAL,  list(sets$region, sets$yy), sum, na.rm=T) /(teff/1000)
tthr <- tapply(sets$THR,  list(sets$region, sets$yy), sum) /(teff/1000)
thhd <- tapply(sets$HHD,  list(sets$region, sets$yy), sum) /(teff/1000)
tpor <- tapply(sets$POR,  list(sets$region, sets$yy), sum) /(teff/1000)
#


#huecodes=c("BSH","MAK","OCS","FAL","THR","HHD", "POR")
#start calcs and plot

png(file=paste(shkdir,"GRAPHICS/FIG_xx_shksP1000Hooks.png",sep='')) #
#
#
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,], tshark[i,])
  # start plot
  barplot(tmat, col=c(mycol, "white"), , main=paste("Region", i) )
  if(i %in% c(3:4)) mtext( "Sharks per 1000 hooks" , side=2, line=2.5, outer=F, las=0 )
}
par(mar = par("mar")/2)
plot.new()
#
legend('center',  legend = c(spec,'OtherShark'), fill=c(mycol, "white"),   bty='n',cex=1 , ncol=4)

dev.off()

#------------------------------------------------------------------------------------------
# Now just the observed
#------------------------------------------------------------------------------------------
tbsh <- tapply(sets$BSH,  list(sets$region, sets$yy), sum)
tmak <- tapply(sets$MAK,  list(sets$region, sets$yy), sum)
tocs <- tapply(sets$OCS,  list(sets$region, sets$yy), sum, na.rm=T)
tfal <- tapply(sets$FAL,  list(sets$region, sets$yy), sum, na.rm=T)
tthr <- tapply(sets$THR,  list(sets$region, sets$yy), sum)
thhd <- tapply(sets$HHD,  list(sets$region, sets$yy), sum)
tpor <- tapply(sets$POR,  list(sets$region, sets$yy), sum)
tshark <- tapply(sets$SHK, list(sets$region, sets$yy), sum)

png(file=paste(shkdir,"GRAPHICS/FIG_xx_obs_shks.png",sep='')) #
#
#
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,], tshark[i,])
  # start plot
  barplot(tmat/1000, col=c(mycol, "white"), , main=paste("Region", i) )
  if(i %in% c(3:4)) mtext( "Sharks Observed (1000's)" , side=2, line=2.5, outer=F, las=0 )
}
par(mar = par("mar")/2)
plot.new()
#
legend('center',  legend = c(spec,'OtherShark'), fill=c(mycol, "white"),   bty='n',cex=1 , ncol=4)
#
#
dev.off()


###########################################################################################################



#
#------------------------ Shallow and Deep Sets.
#

sets$HPBCAT <- ifelse( sets$hk_bt_flt>11,"D", "S")
table(sets$HPBCAT, useNA='ifany')
s_llobs <- sets[sets$HPBCAT=="S",]; dim(s_llobs)
#init calcs : sharks per 1000 hooks by region & yr  for SHALLOW SETS
teff <-   tapply(s_llobs$hook_est,  list(s_llobs$region, s_llobs$yy), sum)
#head(sets)

tbsh <- tapply(s_llobs$BSH, list(s_llobs$region, s_llobs$yy), sum)
tmak <- tapply(s_llobs$MAK, list(s_llobs$region, s_llobs$yy), sum)
tocs <- tapply(s_llobs$OCS,  list(s_llobs$region, s_llobs$yy), sum)
tfal <- tapply(s_llobs$FAL,list(s_llobs$region, s_llobs$yy), sum)
tthr <- tapply(s_llobs$THR,  list(s_llobs$region, s_llobs$yy), sum)
thhd <- tapply(s_llobs$HHD,  list(s_llobs$region, s_llobs$yy), sum)
tpor <- tapply(s_llobs$POR,  list(s_llobs$region, s_llobs$yy), sum)

tshark <- tapply(s_llobs$SHK, list(s_llobs$region, s_llobs$yy), sum)



png(file=paste(shkdir,"GRAPHICS/FIG_xx_shks_obs_shallow.png",sep=''))
#
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,], tshark[i,])
  # start plot
  barplot(tmat/1000, col=c(mycol, "white"), , main=paste("Region", i) , ylim=c(0,15  ) )
  if(i %in% c(3:4)) mtext( "Sharks Observed (1000s)"  , side=2, line=2.5, outer=F, las=0 )
}
par(mar = par("mar")/2)
plot.new()
#
legend('center',  legend = c(spec,'OtherShark'), fill=c(mycol, "white"),   bty='n',cex=1 , ncol=4)
#
dev.off()



#---------------------------Deep Sets
d_llobs <- sets[sets$HPBCAT=="D",]
#init calcs : sharks per 1000 hooks by region & yr  for SHALLOW SETS
teff <-   tapply(d_llobs$hook_est,  list(d_llobs$region, d_llobs$yy), sum)
#head(sets)

tbsh <- tapply(d_llobs$BSH, list(d_llobs$region, d_llobs$yy), sum)
tmak <- tapply(d_llobs$MAK, list(d_llobs$region, d_llobs$yy), sum)
tocs <- tapply(d_llobs$OCS,  list(d_llobs$region, d_llobs$yy), sum)
tfal <- tapply(d_llobs$FAL,list(d_llobs$region, d_llobs$yy), sum)
tthr <- tapply(d_llobs$THR,  list(d_llobs$region, d_llobs$yy), sum)
thhd <- tapply(d_llobs$HHD,  list(d_llobs$region, d_llobs$yy), sum)
tpor <- tapply(d_llobs$POR,  list(d_llobs$region, d_llobs$yy), sum)
tshark <- tapply(d_llobs$SHK, list(d_llobs$region, d_llobs$yy), sum)


ymaxs <-c(25,5,30,5,5)

png(file=paste(shkdir,"GRAPHICS/FIG_xx_shks_obs_deep.png",sep=''))
#
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,1)) #
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
plot.new()

for(i in 1:(nreg-1)){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,],thhd[i,], tpor[i,], tshark[i,])
  # start plot
  # barplot(tmat/1000, col=c(mycol, "white"), , main=paste("Region", i+1 ) , ylim=c(0,ymaxs[i]  ) )
  barplot(tmat/1000, col=c(mycol, "white"), , main=paste("Region", i+1 )  )

  if(i %in% c(2:3)) mtext( "Sharks Observed (1000's)"  , side=2, line=2.5, outer=F, las=0 )
}
par(mar = par("mar")/2)
plot.new()
#
legend('center',  legend = c(spec,'OtherShark'), fill=c(mycol, "white"),   bty='n',cex=1 , ncol=4)
#

dev.off()

}

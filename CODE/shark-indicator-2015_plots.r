## shark-indicator-2015_data-prep.r
## Reading in already extracted data from observer database and massaged a bit.
## 
## makes simple plots and descriptions of data
##
## -------------------------------------------------------
## Author: Joel Rice (joelrice@uw.edu)
## Written on: June 5, 2015  (updated a bit since)
 


options(stringsAsFactors=FALSE)

# load the combined (SPC and HW) and cleaned observer data
load( file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" )   #loads shk_all
head(shk_all)
dim(shk_all)
#ENSURE TIME FRAME IS CORRECT.
shk_all <- shk_all[shk_all$yy > 1994, ]
shk_all <- shk_all[shk_all$yy < 2015, ]
shk_all <- shk_all[shk_all$region %in% 1:6,]
dim(shk_all)
#
#

#######################################################3
#
#  Plot 1
#

png(file=paste(shkdir,"GRAPHICS/FIG_1_MAP.png",sep='')) 

par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0,0))
plot(1,1,type="n",ylab="",xlab="",xlim=c(110,240),ylim=c(-60,50),col="cadetblue",cex=0.1)

lines(eez[,1], eez[,2], col=1) # draw these boundaries over the set locations (obscure misplaced sets!)
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), fill=T, add=T, yaxt="n", xaxt="n", col="black")

#Source the region lines
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")
# Make Region Names.
text( c(160,200,160,200,160,200 ), y=c(25,25,0,0,-30,-30) , labels=1:6, cex=4, pos=3)
dev.off()




############################3
#
#  FIG XX number of hooks set by region- annually  1995-2014
#
#
library(plotrix)

load( file="C:/Projects/SHK-indicators-2015/DATA/agg_eff_by_flag.rdata")
head(aggr)
agg2 <- with(aggr, tapply(hhooks, list(yy, region), sum))
head(agg2)
agg2 <- agg2/10000 # effort in million hooks now

# 
dev.off()
png(file=paste(shkdir,"GRAPHICS/FIG_xx_agg_eff.png",sep='')) 
par(mfrow=c(1,1))
  stackpoly( agg2,main="",xlab="Year",ylab="Total Effort (Millions of Hooks)" , stack=TRUE, xaxlab=rownames(agg2), las=1, ylim=c(0,1200), axis4=FALSE)
  
legend('topleft', legend=c(paste("Region", 1:6)), fill=rainbow(6), bty='n' )
 
dev.off()




########################### 
#
#  FIG XX number of hooks OBSERVED by region- annually  1995-2014
#
#
require(plotrix)
#CALC EFFORT
obs_eff<- with(shk_all, tapply(hook_est, list(yy, region), sum))
obs_eff<- obs_eff/1e6
obs_eff <- ifelse (is.na(obs_eff), 0, obs_eff)
# 
# MAKE FILE
png(file=paste(shkdir,"GRAPHICS/LL_OBS_eff_poly.png",sep='')) 

par(mfrow=c(1,1))
stackpoly( obs_eff ,main="",xlab="Year",ylab="Total Observed Effort (Millions of Hooks)" , stack=TRUE, xaxlab=rownames(obs_eff), las=1, ylim=c(0,20), axis4=FALSE)
 
legend('topleft', legend= paste("Region", 1:6),  fill=rainbow(6) , bty='n'    )
 
dev.off()


##################################################################################
# SPECIES SPECIFIC 
#  annual %positive by region
#
#
 tdat <-tdat <- sets
 
# table(tdat[tdat$BSH>0, 'region'])


for(j in 1:nspec){
  
temp <-  table( tdat[,scpue[j]] >0, tdat$yy,tdat$region)

pcnt_pos <- matrix(NA, nrow=6, ncol=length(s.yr:e.yr) , dimnames=list(1:6, s.yr:e.yr))
for ( i in 1:6) {
  pcnt_pos[i,]<- round(temp[1,,i]/ colSums(temp[,,i]) ,3)
  
   }
 
png(file=paste(shkdir,"GRAPHICS/FIG_xx_pcntpos_reg_", spec[j], ".png",sep='')) 
par(mfrow=c(3,2))
for(k in 1:6 ){
plot(colnames(pcnt_pos),  pcnt_pos[k,]*100, type='b', lwd=2,  col=mycol[j], lty=1, ylim=c(0,100), xlab='Year', pch=16, ylab="", las=1 , main=paste("Region", k) )
if(k %in% 3:4){mtext("Percent Positive Sets" , side=2, line=3, outer=F )}

}

#matplot(colnames(pcnt_pos), t(pcnt_pos)*100, type='b', lwd=2, pch=rownames(pcnt_pos), col=mycol[j], lty=1, ylim=c(0,100), xlab='Year', ylab="Percent of Positive sets", las=1, )
dev.off()
}
 
##########################
##########################
##########################
# plots nominal mean CPUE by region for each species
# load() file=paste0(dat.dir, "lldata_11JULY2015.rdata")

 tdat <- sets
 
scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
tdat[,scpue] <- 0; head(tdat[,scpue])
tdat[,scpue] <- tdat[,spec] /(tdat[,"hook_est"] /1000) 

 
for(j in 1:nspec){
  
 # temp <-   tapply( shk_all[,scpue[j]] >0, list( shk_all$yy,shk_all$region), mean )
  temp <-   tapply(tdat[,scpue[j]] >0, list( tdat$yy, tdat$region), mean )
  

png(file=paste(shkdir,"GRAPHICS/FIG_xx_nomCPUE_reg_", spec[j], ".png",sep='')) 
  par(mfrow=c(3,2))
  for(i in 1:nreg){
  plot(rownames(temp), temp[,i], type='b', lwd=2, pch=16, col=mycol[j], lty=1, ylim=c(0,1.25*(max(temp[,i], na.rm=T))), xlab='Year', ylab=" ", main=paste("Region", i) , las=1 )
  if(i %in% c(3:4)) mtext("Mean Nominal CPUE (/1000 hooks)" , side=2, line=3, outer=F )
  
  }
    dev.off()
}
dev.off()

rm(tdat)
###############################
#
#
#  diagnostic histograms for size of the catch
#
#
dev.off()

for(j in 1:nspec){
png(file=paste(shkdir,"GRAPHICS/FIG_xx_HIST_CPUE_", spec[j], ".png",sep='')) 

par(mfrow=c(3,2))
for(i in 1:nreg){
  
   tdat <- shk_all[ shk_all[,scpue[j]]>0 &  shk_all$region==i,scpue[j]] 
 #
  if( length(tdat)==0 )   plot( 1,1, type='n', ylim=c(0,1), xlim=c(0,10), ylab='Density', xlab='CPUE',  col=mycol[j], main=paste("Region", i) , las=1)
  if( length(tdat)>0 )  hist( tdat    , xlab='CPUE', freq=F, col=mycol[j], nclass=50, main=paste("Region", i) , las=1  )
  
  
  rm(tdat)
 
}
 mtext(paste(spec[j]) , 3,outer=T, line=-2)

 dev.off()
}


###########################
# SPECIES DIST Section
#  high cpue 
#
# # this is the stat that needs to get worked out....by region
#length(which(tdat[,10] > 1)) / length(which(!is.na(tdat[,10] ) )   )


scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
sets[,scpue] <- 0; head(sets[,scpue])
sets[,scpue] <- sets[,spec] /(sets[,"hook_est"] /1000) 

  tdat2 <-  sets; head(tdat2)
  rm(tdat); str(tdat2)

tdat <- tapply( tdat2[ tdat2$region==i,scpue[j] ] , list(tdat2[  tdat2$region==i,"cell"], tdat2[ tdat2$region==i,'yy']), mean, na.rm=T) 
 
dim(tdat)  
head(tdat)  
# for BSH it was 1/1000hks
 


#make storage
spec_thres <- c( 1, 1,1,1,1,1,1 ) # could make this an array so that the different regions have different thresholds.
hicpue <- array(data=NA, dim=c(length(s.yr:e.yr), nreg, nspec), dimnames=list(1995:2014, 1:nreg, spec))               #make storage
# hicpue[1:5,1:3,]

#start calcs and plot
for(j in 1:nspec){

  png(file=paste(shkdir,"GRAPHICS/FIG_xx_HIGH_CPUE_", spec[j], ".png",sep='')) 
  par(mfrow=c(3,2))
  for(i in 1:nreg){
   
    tdat <- tapply( tdat2[ tdat2$region==i,scpue[j] ] , list(tdat2[  tdat2$region==i,"cell"], tdat2[ tdat2$region==i,'yy']), mean) 
   
    tvec <- c()
    for( k in 1:dim(tdat)[2] ) tvec <- c(tvec,length(which(tdat[,k] > spec_thres[j])) / length(which(!is.na(tdat[,k] ) )   ))
     #
    tvec <-   ifelse(as.character(tvec)=="0", NA, tvec)
     hicpue[1:length(tvec), i,j] <- tvec # store
     # ~~~~ Plot 
     plot(1995:(1995+length(tvec)-1),   hicpue[1:length(tvec), i,j],  type='o', lwd=2, pch=16, col=mycol[j], lty=1, xlim=c(1995,2014), ylim=c(0,1),
          xlab="Year", ylab="Proportion HiGH CPUE", las=1, main=paste("Region", i))
  } #over each region
  mtext(paste(spec[j]) , 3,outer=T, line=-2)
 
  dev.off()
   
} # over each species
  
 



#########################3
#  Plot the  Proportion of sets by month , annually
#
#  OBSERVER DATA
#
 load("C:/Projects/SHK-indicators-2015_backup/DATA/lldata_03JULY2015.rdata") # loads catch and sets # think this is right
sets <- sets[sets$yy %in% s.yr:e.yr,]
bcol<- rainbow(25)[seq(2,24,2)]
mnths <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
# make calc
tobs <- table(sets$mm, sets$yy, sets$region)
#plot file
png(file=paste(shkdir,"GRAPHICS/FIG_xx_obsBY_mm.png",sep='')) 
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
 
for(i in 1:nreg){
barplot(prop.table(tobs[,,i], 2), col=bcol,  legend.text = mnths,  args.legend = list(x =10, y=-.8, bty='n',cex=0.7, ncol=6,xjust=.5, yjust=0), main=paste("Region", i) )

}
par(mar = par("mar")/2)
plot.new()
legend("center",legend=mnths ,fill=bcol, cex=1 , ncol=6, bty="n",  title="Month", xpd=T)     
#
dev.off()
 

#########################3
#  Plot the  Proportion of sets by month , annually
#  in each region
#  LOGSHEET DATA
############

#now load the dataframe with the LL logsheet data
load(file="C:/Projects/DATA_2015/logbook/LL_oper_processed_10July2015.rdata")  # loads shklog
head(shklog); dim(shklog)

# init dec.
bcol<- rainbow(25)[seq(2,24,2)]
mnths <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
# make calc
tlog <- table(shklog$mm, shklog$yy, shklog$region)
#plot file
png(file=paste(shkdir,"GRAPHICS/FIG_xx_LOGSHEET_mm.png",sep='')) 
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
#
for(i in 1:nreg){
bp<-  barplot(prop.table(tlog[,,i], 2), col=bcol   , main=paste("Region", i), las=1 )
   text( x=bp, y=rep(1.09, dim(tlog)[2] ) ,labels=round(colSums(tlog[,,i]) /1000, 0), cex=0.7 , pos=3 )
if(i%in%3:4){mtext(side=2,outer=F,"Proportion Effort By Month",line=2.5,cex=0.75)}
}
# plot the legend
par(mar = par("mar")/2)
plot.new()
legend("center",legend=mnths ,fill=bcol, cex=1 , ncol=6, bty="n",  title="Month", xpd=T)     
#
dev.off()
#
#
#########################3
#  Plot the  cummulative difference in the proportion of sets by month , annually
#  in each region
#    abs( prop ObsData - prop LOGSHEET DATA)
#
############
png(file=paste(shkdir,"GRAPHICS/FIG_xx_obsDIFFlog_mm.png",sep='')) 
par(mfrow=c(3,2))
par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  

for(i in 1:nreg){

 tdif  <- colSums(   abs(prop.table(tobs[,,i] ,2) -   prop.table(tlog[,,i], 2)))
 tdif<- ifelse(is.na(tdif), NA, tdif)
 
 plot(names(tdif), tdif, type='o', lwd=2, pch=16, col=3, lty=1, xlim=c(1995,2014), ylim=c(0,2),
 xlab="Year", ylab="", las=1, main=paste("Region", i))
  
 if(i %in% c(3:4)) mtext("Cumulative Difference In Data Coverage" , side=2, line=2.7, outer=F )
 }
 
dev.off() 
 




#############################  #############################################################
#
# PURSE SEINE CATCH COMPOSITON 

# TOTAL first then  by set type.
range(PSObsShk$yy)
PSObsShk<- PSObsShk[PSObsShk$yy > 1994,]
PSObsShk<- PSObsShk[PSObsShk$yy < 2015,]
#init calcs : sharks per 1000 hooks by region & yr
 
tbsh <- tapply(PSObsShk$blue, list(PSObsShk$region, PSObsShk$yy), sum)  
       tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(PSObsShk$mako, list(PSObsShk$region, PSObsShk$yy), sum)  
    tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(PSObsShk$ocs,  list(PSObsShk$region, PSObsShk$yy), sum)  
  tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(PSObsShk$silky,list(PSObsShk$region, PSObsShk$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(PSObsShk$thresher,  list(PSObsShk$region, PSObsShk$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(PSObsShk$othershk, list(PSObsShk$region, PSObsShk$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )
 


#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_set.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
   tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
    barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,25000),  main=paste("Region", i) )
   if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,  legend=c(spec,'OtherShark'), fill=c(mycol, "white")) 
   if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}

dev.off()
#-------------------------------------------------------------------------

#  NOW For ASSOCIATED AND UNASSOCIATED
tps <- PSObsShk[PSObsShk$asso =="A",]
dim(tps)

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)  
tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)  
tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)  
tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )



#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_ASSO.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,20000),  main=paste("Region", i) )
  if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,    legend=c(spec,'OtherShark'),   fill=c(mycol, "white")) 
  if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}
dev.off()

#____________________________________________
#_______ Un-Associated                      #   
#____________________________________________

#  NOW For ASSOCIATED AND UNASSOCIATED
tps <- PSObsShk[PSObsShk$asso =="U",]
dim(tps)

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)  
tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)  
tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)  
tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )



#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_UNAS.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,15000),  main=paste("Region", i) )
  if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,   legend=c(spec,'OtherShark'), col=1, fill=c(mycol, "white")) 
  if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}
dev.off()




#
#
#





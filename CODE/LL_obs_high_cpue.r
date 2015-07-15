

##########################
# SPECIES DIST Section
#  high cpue 
#
# # this is the stat that needs to get worked out....by region
#length(which(tdat[,10] > 1)) / length(which(!is.na(tdat[,10] ) )   )
 load("C:/Projects/DATA_2015/LL/lldata_11JULY2015.rdata") # loads
head(sets)

scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
sets[,scpue] <- 0 
head(sets[,scpue])
sets[,scpue] <- sets[,spec] /(sets[,"hook_est"] /1000) 
head(sets)
sets$lat5 <- floor(sets$lat1d/5)*5 +2.5
sets$lon5 <- floor(sets$lon1d/5)*5 +2.5
#sets$cell <- paste(round(as.numeric(as.character(sets$lat5))),round(as.numeric(as.character(sets$lon5))),sep="")
sets$cell <- as.character(paste(round(sets$lat5),round(sets$lon5),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8
sets$cell<- ifelse(nchar(sets$cell)==5 & substr(sets$cell,1,2)=="-2",paste("-02",substr(sets$cell,3,5),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==4 & substr(sets$cell,1,1)=="2",paste("02",substr(sets$cell,2,4),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==5 & substr(sets$cell,1,2)=="-8",paste("-08",substr(sets$cell,3,5),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==4 & substr(sets$cell,1,1)=="8",paste("08",substr(sets$cell,2,4),sep=""),sets$cell)


for(aa in 1:nspec){
  print(scpue[aa])
 
matrix(unlist(tapply( sets[ ,scpue[aa]] , sets[ , "region"], quantile, probs=c(0.025, 0.5, 0.75), na.rm=T, simplify=T )),  byrow=T,nrow=6)
}
, dimnames=list(1:6, c(25,50,75))
aa
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

head(sets)

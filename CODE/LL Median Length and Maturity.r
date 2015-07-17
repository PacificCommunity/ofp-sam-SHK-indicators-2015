#  plots the median length and maturity based on observed longline data
# updated for different data /data sources for the 2015 shark indicator papers
# 
#gc()

# source the reg info
source("C:/Projects/SHK-indicators-2015/CODE/1_ind_analysis_preamble.r")
#
#object name is shkbio; note that the length processing is done here, not in the data processing file
# load(file="C:/Projects/SHK-indicators-2015_backup/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )
# load( file=paste0(dat.dir, "lldata_11JULY2015.rdata"))

#load(  file="C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata" )  
load( file="C:/Projects/DATA_2015/LL/reconciled_catch.rdata" ) # loads catch
# nrow(sets); nrow(catch)
# pntr  <- match( catch$l_set_id, sets$l_set_id ); sum(is.na(pntr))
# max(pntr)==nrow(sets)# should be the same as nrow(sets)
catch$yy <- sets$yy[pntr]

names(catch)
# table(shkbio$obs_prg_id, shkbio$yy)
# head(shkbio); dim(shkbio)
c("obstrip_id", "l_set_id", "catch_time", "sp_code", "sp_category", 
  "hk_bt_flt", "hook_no", "condition_land", "condition_release", 
  "fate_code", "len", "len_code", "sex_code", "condition_use", 
  "hook_pos", "yy")

#file="C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata" )  

str(catch)

  catch$program<-factor(shkbio$obs_prg_id)
#
shkbio <- shkbio[ shkbio$sex_id %in% c("M", "F"),  ]
shkbio$sex_id  <-  factor(shkbio$sex_id)

table(shkbio$obs_prg_id, shkbio$yy, shkbio$sex_id)
    head(shkbio); dim(shkbio)  # 

table(shkbio$group, shkbio$yy, shkbio$sex_id, shkbio$region)

#c("BSH","MAK","OCS","FAL","THR","HHD", "POR")
#data for size at maturity  #note this is the same, north and south only difference is sex
 sizeMatFN=c(168,275,144,173,203,210  ,175)                      #size in FL (UF), for females in the North Pacific:  blue, mako, OWT, silky, thresher
 sizeMatFS=c(168,275,144,173,203,210  ,175)                      #females in the South Pacific                         
 sizeMatMN=c(168,180,138,175,168,198  ,145)                      #males in the North Pacific
 sizeMatMS=c(168,180,138,175,168, 198 ,145)                      #males in the South Pacific

# Porbeagle We therefore adopted the estimates of median length at maturity for both sexes produced earlier by Francis & Duffy (2005), i.e. 145 cm for males and 175 cm for females.   ......in FL
# HHD Estimated age at maturity was 4.lyr (210cmTL) for females and-3:8yr (198cm-TL) for males, based on the von Bertalanffy growth equation from back-calculated data. 
# table(catch[catch$sp_code %in% HHD, c("sp_code", "len_code")])
# 
# SPK  SPL  SPN  SPZ 
# 240  810 1803  168
# SPN is generic hammerhead,  SPL is the Scalloped

# Maximum total length about 370 to 420 cm, males maturing at 140 to 165 cm and reaching at least 295 cm, females maturing at about 212 cm and reaching at least 309 cm; size at birth 42 to 55 cm.
# it is atlantinc, but thats okay ish http://www.nefsc.noaa.gov/publications/tm/tm110/tm110.pdf
#  
sexid<- c("Male", "Female")
 group_names <-c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher", "Hammerhead", "Porbeagle") 
gender <- c("M", "F")
species <- c("BSH","MAK","OCS","FAL","THR","HHD", "POR") # spec also apparently
catch$convFL  <- as.numeric(as.character(catch$convFL ))
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  Subset data and plot
for( i in 1:nspec){  # over 7 species
  
 for (k in 1:2 ) {       #two sexes in "gender"
 #tdat <- catch[catch$sp_category ==species[i] & catch$sex_code==gender[k]  & catch$region %in%2:6, ]
   
 tdat <- catch2[catch2$sp_category ==species[i] & catch2$sex_code==gender[k]  & catch2$region %in%2:6, ]
# 
t_low<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.05 ),na.rm=T, simplify=TRUE) 
t_med<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.5  ),na.rm=T, simplify=TRUE)   
t_upp<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.95 ),na.rm=T, simplify=TRUE) 

png(file= paste(shkdir,"GRAPHICS/bio_len_", sexid[k],"_", spec[i], ".png",sep='') ) 
#
par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=FALSE, las=1)
 plot.new() # for region 1
  for (j in c(1:5)) {   #loop over areas with data drawing males
    if(sum(dim(t_med))==0 |i==7 & j %in% 1:2 ){
        plot.new()
        } else{
    plot(as.numeric(as.character(rownames(t_med))),    t_med[,j]    ,ylim=c(50,275),type="o",pch=21,cex=1.1,bg=hues[i],col="black",lwd=2,lty=1,xlim=c(s.yr,e.yr),ylab="",xlab="", las=1)  #plots one species only
    lines(rownames(t_low),  t_low[,j], col=hues[i],lwd=3,lty=3)
    lines(rownames(t_upp),  t_upp[,j], col=hues[i],lwd=3,lty=3)
  
    mtext(side=3,paste("Region ", as.character(j+1)),line=0.3)
    legend("bottomleft",legend=  c(paste(spec[i],gender[k],"n=",table(tdat$region)[j] )),  col=hues[i],lwd=rep(2,5),cex=0.8)
   
    if ( k==1) {
      abline(h=sizeMatMN[i],col=hues[i])
    } else {   
      abline(h=sizeMatFS[i],col=hues[i])
    }     
  } }
 par(las=0)
  #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
  mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=1.5)
  mtext(side=3,outer=T,paste(group_names[i]," Shark ", sexid[k],sep=""),line=1,cex=1.5)   #

dev.off()
 } 
}

 ## porbeagle
i<-7
for (k in 1:2 ) {       #two sexes in "gender"
  tdat <- catch[catch$sp_category ==species[i] & catch$sex_code==gender[k]  & catch$region %in%2:6, ]
  # 
  t_low<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.05 ),na.rm=T, simplify=TRUE) 
  t_med<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.5  ),na.rm=T, simplify=TRUE)   
  t_upp<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.95 ),na.rm=T, simplify=TRUE) 
  
  png(file=paste(shkdir,"GRAPHICS/bio_len_", sexid[k],"_", spec[i], ".png",sep='')) 
  #
  par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=FALSE, las=1)
  plot.new() # for region 1
  plot.new() # for region 2
  plot.new() # for region 3
  plot.new() # for region 3
  
  for (j in c(1:2)) {   #loop over areas with data drawing males
    if(sum(dim(t_med))==0 ){
      plot.new()
    } else{
      plot(as.numeric(as.character(rownames(t_med))),    t_med[,j]    ,ylim=c(50,275),type="o",pch=21,cex=1.1,bg=hues[i],col="black",lwd=2,lty=1,xlim=c(s.yr,e.yr),ylab="",xlab="", las=1)  #plots one species only
      lines(rownames(t_low),  t_low[,j], col=hues[i],lwd=3,lty=3)
      lines(rownames(t_upp),  t_upp[,j], col=hues[i],lwd=3,lty=3)
      
      mtext(side=3,paste("Region ", as.character(j+4)),line=0.3)
      legend("bottomleft",legend=  c(paste(spec[i],gender[k],"n=",table(tdat$region)[j] )),  col=hues[i],lwd=rep(2,5),cex=0.8)
      
      if ( k==1) {
        abline(h=sizeMatMN[i],col=hues[i])
      } else {   
        abline(h=sizeMatFS[i],col=hues[i])
      }     
    } }
  par(las=0)
  #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
  mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=1.5)
  mtext(side=3,outer=T,paste(group_names[i]," Shark ", sexid[k],sep=""),line=1,cex=1.5)   #
  
  dev.off()
}
   
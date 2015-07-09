#  plots the median length and maturity based on observed longline data
# updated for different data /data sources for the 2015 shark indicator papers
# 
#gc()

# source the reg info
source("C:/Projects/SHK-indicators-2015/CODE/1_ind_analysis_preamble.r")
#
#object name is shkbio; note that the length processing is done here, not in the data processing file
load(file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )

  table(shkbio$obs_prg_id, shkbio$yy)
head(shkbio); dim(shkbio)


  shkbio <- shkbio[!shkbio$obs_prg_id %in% c("AUOB", "NZOB"),  ]
  shkbio$obs_prg_id<-factor(shkbio$obs_prg_id)
#
shkbio <- shkbio[ shkbio$sex_id %in% c("M", "F"),  ]
shkbio$sex_id  <-  factor(shkbio$sex_id)

table(shkbio$obs_prg_id, shkbio$yy, shkbio$sex_id)
    head(shkbio); dim(shkbio)  # removing AU and NZ ob programs cost us 115121-68184= 46837 records of length between 1995 and 2014

table(shkbio$group, shkbio$yy, shkbio$sex_id, shkbio$region)


#data for size at maturity  #note this is the same, north and south only difference is sex
 sizeMatFN=c(168,275,144,173,203)                      #size in FL (UF), for females in the North Pacific:  blue, mako, OWT, silky, thresher
 sizeMatFS=c(168,275,144,173,203)                      #females in the South Pacific                         
 sizeMatMN=c(168,180,138,175,168)                      #males in the North Pacific
 sizeMatMS=c(168,180,138,175,168)                      #males in the South Pacific
    
#  
sexid<- c("Male", "Female")
group_names <-c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher") 
gender <- c("M", "F")
#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  Subset data and plot
for( i in 1:nspec){  # over five species
 for (k in 1:2 ) {       #two sexes in "gender"
 tdat <- shkbio[shkbio$group==huenames[i] & shkbio$sex_id==gender[k]  & shkbio$region %in%2:6, ]
# 
t_low<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.05 ),na.rm=T, simplify=TRUE) 
t_med<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.5  ),na.rm=T, simplify=TRUE)   
t_upp<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.95 ),na.rm=T, simplify=TRUE) 

png(file=paste(shkdir,"GRAPHICS/bio_len_", sexid[k],"_", spec[i], ".png",sep='')) 
#
par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=FALSE)
 plot.new() # for region 1
  for (j in c(1:5)) {   #loop over areas with data drawing males
    if(sum(dim(t_med))==0 ){
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

  #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
  mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=1.5)
  mtext(side=3,outer=T,paste(group_names[i]," Shark ", sexid[k],sep=""),line=1,cex=1.5)   #

dev.off()
 } 
}

  
 
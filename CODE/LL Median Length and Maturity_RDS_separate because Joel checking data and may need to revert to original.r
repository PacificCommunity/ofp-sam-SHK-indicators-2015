#  plots the median length and maturity based on observed longline data
# updated for different data /data sources for the 2015 shark indicator papers
# 
#gc()

# source the reg info
#source("C:/Projects/SHK-indicators-2015/CODE/1_ind_analysis_preamble.r")

# do this manually
source("C:/wcpfc/shark indicators/shk-indicators-2015/CODE/1_ind_analysis_preamble.r")


#
#object name is shkbio; note that the length processing is done here, not in the data processing file
#load(file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )
load(file="C:/wcpfc/shark indicators/shk-indicators-2015/DATA/ll_obs_bio_280615_processed_allsharks.rdata")


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



### latest data from Joel - 17 July 
load(file="C:/wcpfc/shark indicators/shk-indicators-2015/DATA/reconciled_catch.rdata")


#data for size at maturity  #note this is the same, north and south only difference is sex
 sizeMatFN=c(168,275,144,173,203)                      #size in FL (UF), for females in the North Pacific:  blue, mako, OWT, silky, thresher
 sizeMatFS=c(168,275,144,173,203)                      #females in the South Pacific                         
 sizeMatMN=c(168,180,138,175,168)                      #males in the North Pacific
 sizeMatMS=c(168,180,138,175,168)                      #males in the South Pacific
 



#  
sexid<- c("Male", "Female")
group_names <-c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher", "Hammerhead", "Porbeagle") 
gender <- c("M", "F")
#sppNames <- c("OWT","Blue","Thresher","Mako","Silky")
sppNames <- c('BSH','OCS','THR','MAK','FAL','HHD','POR')

sizeMat <- array(c(c(168,144,203,275,173,210,175),
                   c(168,138,168,180,175,198,145)), dim=c(7,2), dimnames=list(sppNames,c("F","M")))

#---------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------------
#  Subset data and plot
res.df   <- data.frame()
nrecords <- NULL
for( i in 1:length(sppNames)){  # over five species
 for (k in 1:2 ) {       #two sexes in "gender"
 #tdat <- shkbio[shkbio$group==sppNames[i] & shkbio$sex_id==gender[k]  & shkbio$region %in%2:6, ]
 tdat <- catch2[catch2$sp_category==sppNames[i] & catch2$sex_code==gender[k]  & catch2$region %in%2:6, ]
 tdat$convFL <- as.numeric(tdat$convFL)
# 
t_low<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.05 ),na.rm=T, simplify=TRUE) 
t_med<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.5  ),na.rm=T, simplify=TRUE) 
t_upp<- tapply(tdat$convFL,list(tdat$yy,tdat$region),quantile,probs=c( 0.95 ),na.rm=T, simplify=TRUE) 

res.df<- rbind(res.df, data.frame(year  =as.numeric(rownames(t_med)),
                                  region=rep(paste("Region",as.numeric(colnames(t_med))),each=dim(t_med)[1]),
                                  spp   =sppNames[i],
                                  sex   =gender[k],
                                  type  =rep(c('median','lower','upper'), each=dim(t_med)[1]*dim(t_med)[2]),
                                  dat   =c(c(t_med), c(t_low), c(t_upp))),
                        data.frame(year =1995, region=paste("Region",1), spp=sppNames[i], sex=gender[k],type='median',dat=NA))
  nrecords <- rbind(nrecords, table(tdat$region))

  }
}
nrecords <- cbind('1'=0, nrecords)
rownames(nrecords) <- unlist(lapply(as.list(sppNames),function(x) paste(x, c('F','M'),sep="_") ))
res.df[res.df$sex=='M','Sex']<- 'Males'
res.df[res.df$sex=='F','Sex']<- 'Females'


sp <- sppNames[7]
nyrs <- length(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='lower')))/5

poly.df <- data.frame(Region=rep(c(2:6,6:2), each=nyrs),sex=rep(c('F','M'),each=nyrs*10), 
                      panelct=c(rep(c(3,5,7,9,11,11,9,7,5,3),each=nyrs),rep(c(4,6,8,10,12,12,10,8,6,4),each=nyrs)),
                      yvals =   c(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='lower')),
                              rev(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='upper'))),
                                  with(res.df, subset(dat,  spp==sp & sex=='M' & type=='lower')),
                              rev(with(res.df, subset(dat,  spp==sp & sex=='M' & type=='upper')))),
                      xvals =   c(with(res.df, subset(year, spp==sp & sex=='F' & type=='lower')),
                              rev(with(res.df, subset(year, spp==sp & sex=='F' & type=='upper'))),
                                  with(res.df, subset(year, spp==sp & sex=='M' & type=='lower')),
                              rev(with(res.df, subset(year, spp==sp & sex=='M' & type=='upper')))))

#for regions and sexes with NAs work out how many polygons need to be drawn and number them appropriateley
poly.df$poly.count <- 1
poly.df$poly.count[is.na(poly.df$yvals)] <- NA

for(rgg in 2:6){
  for(sxx in c('F','M')){
    kk <- subset(poly.df, Region==rgg & sex==sxx)$poly.count[1:nyrs]
    kk.orig <- kk
    for(ii in 1:nyrs){
      if(is.na(kk)[ii]){
        kk[(ii+1):nyrs] <- ii
        kk[is.na(kk.orig)] <- NA
      }
    }
    kk <- kk[1:nyrs]
    poly.df[poly.df$Region==rgg & poly.df$sex==sxx,]$poly.count <- c(kk, rev(kk))
  }
}


filename <- paste("FIG_XX_bio_len_",sp,"_RDS.png", sep="")
png(paste("C:/wcpfc/shark indicators/shk-indicators-2015/GRAPHICS/",filename,sep=""), width=900, height=700)


pfun <- function(x, y, Lmat, nrec, polygons, ...){
  ppp <- polygons[polygons$panelct==panel.number(),]
  for(pcounter in 1:20){ #max(ppp$poly.count)){
    xxx <- ppp[ppp$poly.count==pcounter,]$xvals
    yyy <- ppp[ppp$poly.count==pcounter,]$yvals
    panel.polygon(xxx[!is.na(xxx)], yyy[!is.na(yyy)], col='lightgrey',lty='n',border=NA,...)
  }

  panel.abline(h=Lmat[panel.number()], col='darkgrey', lty=2)
  panel.xyplot(x,y,col=c('lightgrey','black','lightgrey'),...)
  panel.text(1995,290, paste("n =",nrec[panel.number()]), col='black')
}

sb <- trellis.par.get("strip.background")
sb$col[c(1,2)] <- c('ivory2','ivory3')
trellis.par.set("strip.background", sb)

xyplot(dat~year|Sex*as.character(region), groups=type, data=res.df[res.df$spp==sp,], type='l', lty=c(2,1,2),
       layout=c(2,6),as.table=T, Lmat=rep(sizeMat[sp,],6), 
       panel=pfun, polygons=poly.df,
       nrec=c(nrecords[c(paste(sp,'M',sep='_'),paste(sp,'F',sep='_')),]), 
       ylab="Median Upper Jaw-Fork Length", xlab="", scales=list(x=list(alternating=F)),ylim=c(0,350))

dev.off()



########################
##
## PORBEAGLE ONLY
##

sp <- sppNames[7]
nyrs <- length(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='lower')))/2

poly.df <- data.frame(Region=rep(c(5:6,6:5), each=nyrs),sex=rep(c('F','M'),each=nyrs*2), 
                      panelct=c(rep(c(3,5,7,9,11,11,9,7,5,3),each=nyrs),rep(c(4,6,8,10,12,12,10,8,6,4),each=nyrs)),
                      yvals =   c(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='lower')),
                                  rev(with(res.df, subset(dat,  spp==sp & sex=='F' & type=='upper'))),
                                  with(res.df, subset(dat,  spp==sp & sex=='M' & type=='lower')),
                                  rev(with(res.df, subset(dat,  spp==sp & sex=='M' & type=='upper')))),
                      xvals =   c(with(res.df, subset(year, spp==sp & sex=='F' & type=='lower')),
                                  rev(with(res.df, subset(year, spp==sp & sex=='F' & type=='upper'))),
                                  with(res.df, subset(year, spp==sp & sex=='M' & type=='lower')),
                                  rev(with(res.df, subset(year, spp==sp & sex=='M' & type=='upper')))))

#for regions and sexes with NAs work out how many polygons need to be drawn and number them appropriateley
poly.df$poly.count <- 1
poly.df$poly.count[is.na(poly.df$yvals)] <- NA

for(rgg in :6){
  for(sxx in c('F','M')){
    kk <- subset(poly.df, Region==rgg & sex==sxx)$poly.count[1:nyrs]
    kk.orig <- kk
    for(ii in 1:nyrs){
      if(is.na(kk)[ii]){
        kk[(ii+1):nyrs] <- ii
        kk[is.na(kk.orig)] <- NA
      }
    }
    kk <- kk[1:nyrs]
    poly.df[poly.df$Region==rgg & poly.df$sex==sxx,]$poly.count <- c(kk, rev(kk))
  }
}


filename <- paste("FIG_XX_bio_len_",sp,"_RDS.png", sep="")
png(paste("C:/wcpfc/shark indicators/shk-indicators-2015/GRAPHICS/",filename,sep=""), width=900, height=700)


pfun <- function(x, y, Lmat, nrec, polygons, ...){
  ppp <- polygons[polygons$panelct==panel.number(),]
  for(pcounter in 1:20){ #max(ppp$poly.count)){
    xxx <- ppp[ppp$poly.count==pcounter,]$xvals
    yyy <- ppp[ppp$poly.count==pcounter,]$yvals
    panel.polygon(xxx[!is.na(xxx)], yyy[!is.na(yyy)], col='lightgrey',lty='n',border=NA,...)
  }
  
  panel.abline(h=Lmat[panel.number()], col='darkgrey', lty=2)
  panel.xyplot(x,y,col=c('lightgrey','black','lightgrey'),...)
  panel.text(1995,290, paste("n =",nrec[panel.number()]), col='black')
}

sb <- trellis.par.get("strip.background")
sb$col[c(1,2)] <- c('ivory2','ivory3')
trellis.par.set("strip.background", sb)

xyplot(dat~year|Sex*as.character(region), groups=type, data=res.df[res.df$spp==sp,], type='l', lty=c(2,1,2),
       layout=c(2,6),as.table=T, Lmat=rep(sizeMat[sp,],6), 
       panel=pfun, polygons=poly.df,
       nrec=c(nrecords[c(paste(sp,'M',sep='_'),paste(sp,'F',sep='_')),]), 
       ylab="Median Upper Jaw-Fork Length", xlab="", scales=list(x=list(alternating=F)),ylim=c(0,350))

dev.off()























#png(file=paste(shkdir,"GRAPHICS/bio_len_", sexid[k],"_", spec[i], ".png",sep='')) 
#
par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=FALSE)
 plot.new() # for region 1
  for (j in c(1:5)) {   #loop over areas with data drawing males
    if(sum(dim(t_med))==0 ){
        plot.new()
        } else{
    plot(as.numeric(as.character(rownames(t_med))),    t_med[,j] ,ylim=c(50,275),type="o",pch=21,cex=1.1,bg=hues[i],col="black",lwd=2,lty=1,xlim=c(s.yr,e.yr),ylab="",xlab="", las=1)  #plots one species only
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

#dev.off()
 } 
}

  
 
#
#####This is similar to median length vs maturity plots except that the length data are standardized
#####the predict command is used to generate each annual value
#####originally  created in March 2011 and updated in June 2011 with new data & now re-worked in 2015 for the updated indicators paper
#####only LL data are used as there are not enough PS length data to apply

#object name is shkbio; note that the length processing is done here, not in the data processing file
#load(file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )
 # dim(shkbio)
 # dim

dat.dir <- "C:/WCPFC/shark indicators/SHK-indicators-2015/DATA/"

load(paste(dat.dir, "ll_obs_bio_280615_processed_allsharks.rdata", sep=""))
load(paste(dat.dir, "ll_obs_CATCH_11JULY_processed.rdata", sep=""))
      
    head(catch)  

  
species<-c("Blue","Mako","OWT","Silky","Thresher")
spcodes<-c("BSH","MAK","OCS","FAL","THR")

spec<- c("BSH", "MAK", "OCS","FAL", "THR", "HHD", "POR"); nspec<- length(spec)
scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
#timeframe of analysis
s.yr <- 1995
e.yr <- 2014

  table(catch$sp_category)
#    BSH    FAL    HHD    MAK    OCS    POR    SHK    SKJ    THR 
# 212025  52589   3003  13394  12871    438  18618  31255  13704 
 
table(shkbio[shkbio$group==species[1], 'lat10'] , shkbio[shkbio$group==species[1], 'lon10'],   shkbio[shkbio$group==species[1], 'yy']   )

tdat <- shkbio[shkbio$sex_id %in% c('M',"F"),]; tdat$sex_id <- factor(tdat$sex_id)
with(tdat ,table(group,yy, sex_id, hemi))
# we are slightly data deficient in the North pacific ....
        # , , sex_id = F, hemi = N
        # 
        # yy
        # group      1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
        # Blue      308  568  657  625  436  401  195   38   74  173  210  580  560   88   13    2    0    1   11    2
        # Mako       14   13   32   58   25   36   33    4    2    3    7   49   75    8   11    0    0    1    2    3
        # OWT        55   91   66   80   65   84   51    0    0   28   12   89  188   20    6    1    0    5   16    2
        # Silky     106   83   93   72  100   85   70   10    7  134  121  357  714  197   69    1    1   11   25   21
        # Thresher   28    8   13   34   17   34   25    1    0   52   61  496  487   40   16    0    0    0    3    6
        # 
        # , , sex_id = M, hemi = N
        # 
        # yy
        # group      1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
        # Blue      277  621 1434  821  502  357  223   39   35  115  134  540  674   75   17    3    0   21   10    5
        # Mako       11   16   20   37   23   24   42    1    0    1    3   43   66    6   17    0    0    1    4    1
        # OWT        44   43   49   66   44   69   45    1    0   32    8   57  145   18    4    1    0    1    7    3
        # Silky      96   36   43   40   74  106   41    1   12   79   69  300  671  159   37    2    0   14   23    6
        # Thresher   29   19    8   69   24   64   35    0    0   40   36  472  478   47   13    0    0    5    4    9
        # 
        # , , sex_id = F, hemi = S
        # 
        # yy
        # group      1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
        # Blue      891 1693 2737 1053 2740 1331 1086  849  942 1254 1409 1435 3059  934 2159 1110 1004   24  484  120
        # Mako      106  120  326  104   78   14   59   56  152  106  122   97  176  182  150  112  107    9  235   16
        # OWT        28   35   96   47  362   15   34   87   82  141  174  220   75  125   59   26   33   19  144   16
        # Silky      17   39  111  311 1247  517  228 1002   66  899  247  483  185  288   75   65  759  228 2722  246
        # Thresher    7   20   22    5    3   11    5   12   12   20   13   12   22    4   14    7   20    8   42   12
        # 
        # , , sex_id = M, hemi = S
        # 
        # yy
        # group      1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014
        # Blue      614 1147 2102  549 1131  418  286  327  547  670  946  916 1643  603 1132  641  521   61  380  129
        # Mako      121  104  342  104  157   58   89   75  203  137  146  120  213  199  140   80  123   18  202   23
        # OWT        27   31   85   40  342   12   34  111   79  106  123  170   59   83   43   11   14    9   97   13
        # Silky      16   25   88  423 1068  667  199 1042   68  891  233  474  178  258   98   48  658  265 2130  157
        # Thresher   11   21   22   16    7    7    2   24   20   19   30   23   33   10   23    7   26   17   35   10
        # 
        # 

  
 
catch$lon5 <- floor(catch$lon1d /5)*5 +2.5 
catch$lat5 <- floor(catch$lat1d /5)*5 +2.5 


#make a factor which is a combination of lat5 and lon5

catch$cell <- as.character(paste(round(catch$lat5),round(catch$lon5),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8
catch$cell<- ifelse(nchar(catch$cell)==5 & substr(catch$cell,1,2)=="-2",paste("-02",substr(catch$cell,3,5),sep=""),catch$cell)
catch$cell<- ifelse(nchar(catch$cell)==4 & substr(catch$cell,1,1)=="2",paste("02",substr(catch$cell,2,4),sep=""),catch$cell)
catch$cell<- ifelse(nchar(catch$cell)==5 & substr(catch$cell,1,2)=="-8",paste("-08",substr(catch$cell,3,5),sep=""),catch$cell)
catch$cell<- ifelse(nchar(catch$cell)==4 & substr(catch$cell,1,1)=="8",paste("08",substr(catch$cell,2,4),sep=""),catch$cell)
catch$cell <- as.factor(catch$cell)
table(catch$cell)
head(catch)  
dim(catch) 




# so north and south for mako, and BSH then just one for thr, fal and ocs and HHD and POR (and SKJ)
# by sex,  so that is 7 x2=14  models
#


#MODEL for BLUE and Mako

hemis <- c('N', 'S')


genderlong <- c('Male', 'Female')
gender     <- c('M', 'F')
 
estout <- NULL #s.yr:e.yr
seout  <- NULL #s.yr:e.yr 
#
#
for(  i in 1:2 ) {  # species
  for(j in 1:2)   {  # hemi
    for( k in 1:2 ){  #gender

      # subset the data
tdat <- shkbio[shkbio$group==species[i] & shkbio$hemi==hemis[j] &shkbio$sex_id==gender[k],  ]  
tdat$cell<-factor(tdat$cell) 

#dim(tdat)
#
glmshk<-glm( len ~ as.factor( yy ) + cell , family=gaussian(link=log), data=tdat, na.action=na.omit)
shkSUM <- summary(glmshk)

 newdat  <-   expand.grid( yy  = as.factor(unique(tdat$yy)), 
                         cell = as.factor(  names( sort( table(tdat$cell), decreasing=TRUE ) )[1]  )    )

newdat <- newdat[order(as.numeric(as.character(newdat$yy))),]; newdat
#
newdat <- cbind(newdat,   predict(glmshk, newdat,  type='response',  se.fit = TRUE, MC = 2500, conf = .95)   )
 

#png(file=paste(shkdir,"GRAPHICS/CPUE_std/len_stdz_",  species[i], "_", hemis[j], '_Hemi_', gender[k],".png",sep='')   )  

plot(as.numeric(as.character(newdat$yy)), newdat$fit, type='n', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(0,300), las=1, xlab='Year', ylab='Standardized Length' )
segments(x0= as.numeric(as.character(newdat$yy)), y0= newdat$fit+1.96*newdat$se.fit, x1=as.numeric(as.character(newdat$yy)), y1=newdat$fit-1.96*newdat$se.fit, col=1, lwd=0.7 )
points( as.numeric(as.character(newdat$yy)), newdat$fit, type='p', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(150,300)  )
title( paste( species[i], " Shark ", hemis[j], ' Hemisphere,  ', genderlong[k], sep='' ), )

#dev.off()

 
 
estout <-  cbind( estout ,  newdat$fit[ match(s.yr:e.yr, newdat$yy  )]  )  
#
seout <-  cbind( seout ,  newdat$se.fit[ match(s.yr:e.yr, newdat$yy  )]  )  
#
}}}



### RDS plotting stuff

fit.df <- data.frame(year=s.yr:e.yr, hemisphere=rep(c('Northern','Southern'), each=20*2), sex=rep(c('Males','Females'),each=20),
                     spp=rep(c("BLUE","MAKO"),each=20*2*2), est=c(estout), se=c(seout))

fit.df2<- rbind(cbind(fit.df[,c(-5,-6)], dat=fit.df$est, dtype="mean"),
                cbind(fit.df[,c(-5,-6)], dat=fit.df$est+1.96*fit.df$se, dtype=paste("std",fit.df$year,sep="_")),
                cbind(fit.df[,c(-5,-6)], dat=fit.df$est-1.96*fit.df$se, dtype=paste("std",fit.df$year,sep="_")))

pfun <- function(x,y,...){
  panel.xyplot(x,y,...)
}

# BLUE
png("C:/wcpfc/shark indicators/shk-indicators-2015/graphics/len_stdz_Blue.png", width=500, height=500)

sb <- trellis.par.get("strip.background")
sb$col[c(1,2)] <- c('ivory2','ivory3')
trellis.par.set("strip.background", sb)

pfun <- function(x,y,...){
  panel.xyplot(x,y,...)
  panel.lines(loess.smooth(x,y), col='lightgrey')
}

xyplot(dat~year|sex*hemisphere, groups=dtype, data=fit.df2[fit.df2$spp=='BLUE',], type=c('l','p'), 
       lty=c(0,rep(1,20)), pch=c(1,rep(NA,20)), cex=1.2,
       layout=c(2,2), as.table=T, ylim=c(0,300), panel=pfun, col='black', ylab="Standardised Length", xlab="",
       scales=list(alternating=F))

dev.off()


# MAKO
png("C:/wcpfc/shark indicators/shk-indicators-2015/graphics/len_stdz_Mako.png", width=500, height=500)

sb <- trellis.par.get("strip.background")
sb$col[c(1,2)] <- c('ivory2','ivory3')
trellis.par.set("strip.background", sb)

pfun <- function(x,y,...){
  panel.xyplot(x,y,...)
  panel.lines(loess.smooth(x,y), col='lightgrey')
}

xyplot(dat~year|sex*hemisphere, groups=dtype, data=fit.df2[fit.df2$spp=='MAKO',], type=c('l','p'), 
       lty=c(0,rep(1,20)), pch=c(1,rep(NA,20)), cex=1.2,
       layout=c(2,2), as.table=T, ylim=c(0,300), panel=pfun, col='black', ylab="Standardised Length", xlab="",
       scales=list(alternating=F))

dev.off()






fnames <- 'yy'
for(  i in 1:2 ) {  # species
  for(j in 1:2)   {  # hemi
    for( k in 1:2 ){  #gender
 fnames <- c(fnames, paste( species[i], "_", hemis[j], "_", gender[k],  sep='')) 
    }}}
fnames

colnames(estout) <- fnames
colnames(seout)  <- fnames  
 
dim(seout)
seout <- as.data.frame(seout)


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
# for thr, fal and ocs
# species

estout2 <- s.yr:e.yr
seout2  <- s.yr:e.yr 
#
genderlong <- c('Male', 'Female')


#
for(  i in 3:5 ) {  # species
      for( k in 1:2 ){  #gender
      
      # subset the data
      tdat <- shkbio[shkbio$group==species[i] & shkbio$sex_id==gender[k],  ]  
      tdat$cell<-factor(tdat$cell) 
      
      #dim(tdat)
      #
      glmshk<-glm( len ~ as.factor( yy ) + cell , family=gaussian(link=log), data=tdat, na.action=na.omit)
     # shkSUM <- summary(glmshk)
      
      newdat  <-   expand.grid( yy  = as.factor(unique(tdat$yy)), 
                                cell = as.factor(  names( sort( table(tdat$cell), decreasing=TRUE ) )[1]  )    )
      
      newdat <- newdat[order(as.numeric(as.character(newdat$yy))),]; newdat
      #
      newdat <- cbind(newdat,   predict(glmshk, newdat,  type='response',  se.fit = TRUE, MC = 2500, conf = .95)   )
      #      
      png(file=paste(shkdir,"GRAPHICS/CPUE_std/len_stdz_",  species[i], "_", gender[k],".png",sep='')   )  
      #----------------------------------------------------------------------------------------------------------------------------------
      #----------------------------------------------------------------------------------------------------------------------------------
      plot(as.numeric(as.character(newdat$yy)), newdat$fit, type='n', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(0,300), las=1, xlab='Year', ylab='Standardized Length' )
      segments(x0= as.numeric(as.character(newdat$yy)), y0= newdat$fit+1.96*newdat$se.fit, x1=as.numeric(as.character(newdat$yy)), y1=newdat$fit-1.96*newdat$se.fit, col=1, lwd=0.7 )
      points( as.numeric(as.character(newdat$yy)), newdat$fit, type='p', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(150,300)  )
      title( paste( species[i], " Shark ", genderlong[k], sep='' ), )
      #
      dev.off()
      #----------------------------------------------------------------------------------------------------------------------------------
      #----------------------------------------------------------------------------------------------------------------------------------
      #
      estout2 <-  cbind( estout2 ,  newdat$fit[ match(s.yr:e.yr, newdat$yy  )]  )  
      #
      seout2 <-  cbind( seout2 ,  newdat$se.fit[ match(s.yr:e.yr, newdat$yy  )]  )  
      #
    }
  } 

fnames <- 'yy'
for(  i in 3:5 ) {  # species
 
    for( k in 1:2 ){  #gender
      fnames <- c(fnames, paste( species[i], "_",   gender[k],  sep='')) 
    }}
fnames

colnames(estout2) <- fnames
colnames(seout2)  <- fnames  
# make list and save if we want to do something else
length_output <- list()
length_output[["estimates"]]<- cbind(estout, estout2[,-1])
length_output[["se.fit"]]<- cbind(seout, seout2[,-1])
length_output

save(length_output, file="C:/Projects/SHK-indicators-2015/DATA/output/stdz_length.rdata")

    # 
    # par(mfrow=c(3,2))
    #  plot(s.yr:e.yr, length_output[[1]][,2], type='p', col=mycol[1], pch="N",   cex=1.5,lty=1 , las=1, xlab='Year', ylab='Standardized Length', ylim=c(0,275))
    #  points(s.yr:e.yr, length_output[[1]][,4], type='p', col=mycol[1], pch="S", cex=1.5)
    # 
    #  points(s.yr:e.yr, length_output[[1]][,3], type='p', col=1, pch=21, bg=3, cex=1.5)
    #  points(s.yr:e.yr, length_output[[1]][,5], type='p', col=1, pch=21, bg=5, cex=1.5)


#segments(x0= as.numeric(as.character(newdat$yy)), y0= newdat$fit+1.96*newdat$se.fit, x1=as.numeric(as.character(newdat$yy)), y1=newdat$fit-1.96*newdat$se.fit, col=1, lwd=0.7 )


# don't re do all of it, just porbeagle, HHD and SKJ, all in the soutehrn hemi
with(catch[ catch$sp_category =="POR",], table( lat5))
with(catch[ catch$sp_category =="HHD",], table( lat5))
with(catch[ catch$sp_category =="SKJ",], table( lat5))


#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
# for POR, HS
# species
estout3 <- s.yr:e.yr
seout3  <- s.yr:e.yr 
#
genderlong <- c('Male', 'Female')
gender <-c("M", "F")
species <- c(spec, "SKJ")
#
for(  i in 6:8 ) {  # species
  for( k in 1:2 ){  #gender
    
    # subset the data
    tdat <- catch[catch$sp_category==species[i] & catch$sex_code==gender[k],  ]  
    tdat$cell<-factor(tdat$cell) 
    
    #dim(tdat)
    #
    glmshk<-glm( len ~ as.factor( yy ) + cell , family=gaussian(link=log), data=tdat, na.action=na.omit)
    # shkSUM <- summary(glmshk)
    
    newdat  <-   expand.grid( yy  = as.factor(unique(tdat$yy)), 
                              cell = as.factor(  names( sort( table(tdat$cell), decreasing=TRUE ) )[1]  )    )
    
    newdat <- newdat[order(as.numeric(as.character(newdat$yy))),]; newdat
    #
    newdat <- cbind(newdat,   predict(glmshk, newdat,  type='response',  se.fit = TRUE, MC = 2500, conf = .95)   )
    # shkdir2 <- "C:/Projects/SHK-indicators-2015_backup/"     
    png(file=paste(shkdir2,"GRAPHICS/CPUE_std/len_stdz_",  species[i], "_", gender[k],".png",sep='')   )  
    #----------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------
    plot(as.numeric(as.character(newdat$yy)), newdat$fit, type='n', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(0,300), las=1, xlab='Year', ylab='Standardized Length' )
    segments(x0= as.numeric(as.character(newdat$yy)), y0= newdat$fit+1.96*newdat$se.fit, x1=as.numeric(as.character(newdat$yy)), y1=newdat$fit-1.96*newdat$se.fit, col=1, lwd=0.7 )
    points( as.numeric(as.character(newdat$yy)), newdat$fit, type='p', col=1, pch=21, bg=mycol[i], cex=1.5,lty=1, ylim=c(150,300)  )
    title( paste( species[i], " Shark ", genderlong[k], sep='' ), )
    #
    dev.off()
    #----------------------------------------------------------------------------------------------------------------------------------
    #----------------------------------------------------------------------------------------------------------------------------------
    #
    estout3 <-  cbind( estout3 ,  newdat$fit[ match(s.yr:e.yr, newdat$yy  )]  )  
    #
    seout3<-  cbind( seout3 ,  newdat$se.fit[ match(s.yr:e.yr, newdat$yy  )]  )  
    #
  }
}

#
fnames <- 'yy'
for(  i in 6:8 ) {  # species
  
  for( k in 1:2 ){  #gender
    fnames <- c(fnames, paste( species[i], "_",   gender[k],  sep='')) 
  }}
fnames

colnames(estout3) <- fnames
colnames(seout3)  <- fnames  
# make list and save if we want to do something else

load( file="C:/Projects/SHK-indicators-2015_backup/DATA/output/stdz_length.rdata")
 
 
length_output[["estimates"]]<- cbind(length_output[["estimates"]], estout3[,-1] )
length_output[["se.fit"]]<- cbind(length_output[["se.fit"]], seout3[,-1] )
length_output_final <- length_output

save(length_output, file="C:/Projects/SHK-indicators-2015_backup/DATA/output/stdz_length_final.rdata")

#
t(apply(x, 2, function(x.col) lm(y~x.col)$coef))

# should really do one more on SKJ all sexes combined
ests <- length_output[["estimates"]][,-1]
y<-  length_output[["estimates"]][,1]

 
coefout <- t(apply(ests, 2, function(x.col) lm(y~x.col, na.action="na.exclude" )$coef))
coefout2<-  head(coefout, -2)
#


png(file=paste(shkdir,"GRAPHICS/len_stdz_coef_out.png",sep='')) 
par(mar=c(5.1, 4.1, 2.1, 2.1))

plot( 1:nrow(coefout2), coefout2[,2], ylim=c(-1,1), pch=16, cex=1.5, col= c(mycol[rep(1,4)], mycol[rep(2,4)], mycol[ rep(3:7, each=2)] ), 
    ylab="Annual change in length (cm) over the study period" , xlab='', xaxt='n' )
abline(h=0, lwd=2, col=grey(.7), lty=3)
axis(side=1, at = 1:nrow(coefout2), labels = rownames(coefout2),  las = 2, cex.axis=0.7)
 dev.off() 
  







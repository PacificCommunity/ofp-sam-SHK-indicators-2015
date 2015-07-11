



#
#Figure 2.  Longline catch rates for blue, mako, oceanic whitetip and silky sharks standardized using a quasi-Poisson formulation of a generalized linear model (see Table 1), and unstandardized (nominal) purse seine catch rates for oceanic whitetip (n=5,344) and silky (n=57,197) sharks in the WCPO.  Analyses for blue and mako sharks were split into northern and southern hemispheres based on these species' primarily temperate distributions.  Confidence intervals were generated for the standardized series only.  
#
#
#
##
#Figure 2. I can't read the axis labels and barely can read the axis scales. Make y-axis labels horizontal (las=). The ordering of the figures is far from satisfactory. It takes a while to figure out that longline is at the top, then the upper part of columns is populations of blue and mako and then the lower panels are larger aggregations and then you have purse seine. Mostly I can see heterogeneity. However my confidence that this is the only message you wish to convey is colored by the weird (counterintuitive) ordering and aggregation of data. Can you go the extra mile and figure out exactly what message you wish to convey and present the material more clearly?
#
#Consider making dots bigger, consider presenting the CI as smoothed grey envelope might help considerably. 
#At some point are you going to present the overall trends and model fits?
#
#






 

# load data  - longline
#load( file="C:/Users/joelr/Dropbox/clarke_shark_consbiol/RegressionCoeff_CPUE/LLtrends.rdata")       #loads trendlist
load( file="C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/RegressionCoeff_CPUE/LLtrends.rdata")       #loads trendlist
# load data - purseseine
#load( file= "C:/Users/joelr/Dropbox/clarke_shark_consbiol/RegressionCoeff_CPUE/psnom.rdata" )         #loads psnom
load( file= "C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/RegressionCoeff_CPUE/psnom.rdata" )         #loads psnom


# set up layout  / margins and other pars

 
 
 
 windows(11,14)   
 lymat<-matrix(c(1,1,2,2,3,4,5,5,6,7,8,8,9,10,11,11,12,13),9,2,byrow=TRUE) 
                                                                                          
 nf<-layout(lymat , widths=c(4,4), heights=c( 1,1,4,1,4,1,4,1,4))
 
    
#layout.show(nf)# 
  
 par(omi=c(0,0,0,0))
 par(oma=c(0,0,0,0))
 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))
 
figmar<-c(2, 2, 2, 1) + 0.1
figmai<-c(0.45,0.65 ,0.25,0.055)   
 
 
 #PLOT THE FIRST TITLES>>>>>>>>>>>>>>>>>>>>>
 plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F , axes=F)
 text(1,1,"Standardized Longline CPUE", cex=2,font=2 )
 #
  plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
  text(1,1,"Blue Shark CPUE Trends", cex=2,font=1 )
     
#
    par(mar=figmar)
     par(mai=figmai)
     par(las=1)  
 
#PLOT BLUE SHARK TRENDS
for(i in 1:6) {
  a <- trendlist[[i]]
  a <- a[a[,1]!=2010,]
  trendlist[[i]] <- a
}
obj<-trendlist[["BlueNorth"]]     
plot(obj, main='North Pacific', ylab="Catch per 1000 hooks",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
 mtext("Year", 1, line=2, cex=.8)  
 
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16,)   
             
obj<-trendlist[["BlueSouth"]]
plot(obj, main='South Pacific', ylab="Catch per 1000 hooks",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
  mtext("Year", 1, line=2, cex=.8)
   
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16,)   


# PLOT THE TITLE FOR MAKO SHARKS
 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))
 
   plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
  text(1,1,"Mako Shark CPUE Trends", cex=2,font=1 )
     
     par(mar=figmar)
     par(mai=figmai)

# plot the Mako Shark trends       
obj<-trendlist[["MakoNorth"]]     
plot(obj, main='North Pacific', ylab="Catch per 1000 hooks",xlab='',type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
  mtext("Year", 1, line=2, cex=.8)    
   
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16,)   
          
obj<-trendlist[["MakoSouth"]]
plot(obj, main='South Pacific', ylab="Catch per 1000 hooks",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
  mtext("Year", 1, line=2, cex=.8)
   
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16)   

 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))

  plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
  
 # text(0.55,.16,"Oceanic Whitetip Shark CPUE Trend", cex=2,font=1 , xpd=T )
 # text(1.65,.16,"Silky Shark CPUE Trend", cex=2,font=1 )
    
    
     par(mar=figmar)
     par(mai=figmai-c(0,0, 0.1,0  ))
                
     
obj<-trendlist[["OCS"]]     
plot(obj, main=' ', ylab="Catch per 1000 hooks",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
 mtext("Year", 1, line=2, cex=.8)               
 mtext( "Oceanic Whitetip Shark CPUE Trend", 3, cex=1.35,font=1 ,line=1)
  
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16)   
  

obj<-trendlist[["FAL"]]
plot(obj, main=' ', ylab="Catch per 1000 hooks",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj[,2], na.rm=T)))
 mtext("Year", 1, line=2, cex=.8)
 mtext( "Silky Shark CPUE Trend", 3, cex=1.35,font=1 ,line=1)
   
   yrs<-obj[,1]
   ylow<-obj[,'lwCI']   
   yhigh<-obj[,'upCI'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(obj,type='b', lwd=2, pch=16,)   

 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))

 ################Purse Seine Catch Rates
 #text
  plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
  text(1,1,"Nominal Purse Seine CPUE Trend", cex=2,font=2)
 
 #figures
      par(mar=figmar)
      par(mai=figmai)
  
 
obj<-psnom[["psOCS"]]     
plot(psnom[["years"]], obj, main='Oceanic Whitetip Shark', ylab="Number per set",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj, na.rm=T)))
 mtext("Year", 1, line=2, cex=.8)  
 # nominal so no CI's
obj<-psnom[["psFAL"]]     
plot(psnom[["years"]], obj,main='Silky Shark', ylab="Number per set",xlab='', type='b', lwd=2, pch=16, ylim=c(0,1.25* max(obj, na.rm=T)))
 mtext("Year", 1, line=2, cex=.8)
  
    savePlot("C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/IndPaper_Fig2_final2",type="pdf") 
   # savePlot("P:/shark/Ind_Shk_Paper/IndPaper_Fig1_Combined_temp",type="png") 
    savePlot("C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/IndPaper_Fig2_final2",type="eps") 
       
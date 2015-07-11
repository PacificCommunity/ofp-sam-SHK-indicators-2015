rm(list=ls()) 
    #library(grid)
#    library(gridBase)
#       load(file="C:/Users/joelr/Dropbox/clarke_shark_consbiol/RegressionCoeff_CPUE/ps_fork_len.rdata")#loads    scc_ps_len
#       load(file="C:/Users/joelr/Dropbox/clarke_shark_consbiol/RegressionCoeff_CPUE/ll_forklen.rdata")# loads   scc_lens
#       load(file="C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/RegressionCoeff_CPUE/ps_fork_len.rdata")#loads    scc_ps_len
#       load(file="C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/RegressionCoeff_CPUE/ll_forklen.rdata")# loads   scc_lens
#
    years<-c(1995:2009)
    
  windows(11,14)   
  lymat<-matrix(c(1,1,2,2,3,4,5,5,6,7,8,8,9,10),7,2,byrow=TRUE) 
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
 text(1,1,"Sizes of Sharks Caught in Longline Fisheries", cex=2,font=2 )
 #
  plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
  text(1,1,"Oceanic Whitetip Shark", cex=2,font=1 )
     
#
    par(mar=figmar)
     par(mai=figmai)
     par(las=1)  
 #####################
# par(mfrow=c(3,2))
 
# par(omi=c(0,0,0,0))
# par(oma=c(0,0,0,0))
# par(mar=c(0,0,0,0))
# par(mai=c(0,0,0,0))
# 
          ssizev_vec<-c(scc_lens$s_size_F_reg3[3], scc_lens$m_size_F_reg3[3],scc_lens$s_size_F_reg3[4],scc_lens$m_size_F_reg3[4])
          matsize<- c(scc_ps_len$size_a_mat[c(1,1,2,2)])

 
 
  i<-1
 matplot(rownames(scc_lens[[1]]),scc_lens[[i]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Females") 
   obj<-scc_lens[[i]]
   yrs<-years
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs[1:2],rev(yrs[1:2])), y=c(ylow[1:2],rev(yhigh[1:2])),border=FALSE,col=gray(.75))
    polygon(x=c(yrs[4:8],rev(yrs[4:8])), y=c(ylow[4:8],rev(yhigh[4:8])),border=FALSE,col=gray(.75))
     polygon(x=c(yrs[10:14],rev(yrs[10:14])), y=c(ylow[10:14],rev(yhigh[10:14])),border=FALSE,col=gray(.75))
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
   legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
   legend('topleft', legend=c(paste( "n=",ssizev_vec[i]) ), bty='n' )
   abline(h=matsize[i], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 
################################################################## 
 i<-2
 matplot(rownames(scc_lens[[1]]),scc_lens[[i]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Males") 
   obj<-scc_lens[[i]]
   yrs<-years
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs[1:8],rev(yrs[1:8])), y=c(ylow[1:8],rev(yhigh[1:8])),border=FALSE,col=gray(.75))
     polygon(x=c(yrs[10:14],rev(yrs[10:14])), y=c(ylow[10:14],rev(yhigh[10:14])),border=FALSE,col=gray(.75))
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
   legend('topleft', legend=c(paste( "n=",ssizev_vec[i]) ), bty='n' )
    legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
 
   abline(h=matsize[i], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 
   
##########Plot Text________________________________________________     
 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))
 
 
   plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F, axes=F )
   text(1.05,1,"Silky Shark ", cex=2,font=1 )
     par(mar=figmar)
     par(mai=figmai)
     
 #    
 # ______________do silky shark plots____________________________________
 #    
 
 i<-3
 matplot(rownames(scc_lens[[1]]),scc_lens[[i]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Females") 
   obj<-scc_lens[[i]]
   yrs<-years
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
    legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
 
   legend('topleft', legend=c(paste( "n=",ssizev_vec[i]) ), bty='n' )
   abline(h=matsize[i], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 
###################################################################
 i<-4
 matplot(rownames(scc_lens[[1]]),scc_lens[[i]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Males") 
   obj<-scc_lens[[i]]
   yrs<-years
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
    legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
  
   legend('topleft', legend=c(paste( "n=",ssizev_vec[i]) ), bty='n' )
   abline(h=matsize[i], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 

 par(mar=c(0,0,0,0))
 par(mai=c(0,0,0,0))
 
  plot(1,1,type='n', xlim=c(0,2),ylim=c(0,2), ann=F , axes=F)
 text(1,1.05,"Sizes of Sharks Caught in Purse Seine Fisheries", cex=2,font=2 )
 
 #OCS
     par(mar=figmar)
     par(mai=figmai)
     i<-1  # different object
   matplot(years,scc_ps_len[[1]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Oceanic Whitetip Shark") 
   obj<-scc_ps_len[[i]]
   
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs[3:11],rev(yrs[3:11])), y=c(ylow[3:11],rev(yhigh[3:11])),border=FALSE,col=gray(.75))
   polygon(x=c(yrs[13]-0.25, yrs[13]+0.25,rev(c(yrs[13]-0.25, yrs[13]+0.25))), y=c(ylow[c(13,13)],rev(yhigh[c(13, 13)])),border=FALSE,col=gray(.75))
  
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
   legend('topleft', legend=c(paste( "n=",scc_ps_len$samplesize[1]) ), bty='n' )
   legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
 
   abline(h=matsize[i], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 
 #SILKY
 i<-2
   matplot(years,scc_ps_len[[i]],  ylim=c(50,250),type="n",pch=16,cex=1.1, col="black",lwd=2, lty=c(3,1,3), xlim=c(1995,2009),ylab="",xlab="", main="Silky Shark") 
   obj<-scc_ps_len[[i]]
   
   ylow<-obj[,'5%']   
   yhigh<-obj[,'95%'] 
   polygon(x=c(yrs,rev(yrs)), y=c(ylow,rev(yhigh)),border=FALSE,col=gray(.75))
   lines(years,obj[,'50%'],type='b', lwd=2, pch=16)   
   legend('topleft', legend=c(paste( "n=",scc_ps_len$samplesize[2]) ), bty='n' )
   legend('topright', legend=c("Size at maturity", "Median Size","90% Quantiles")  , lty=c(1,1,1), pch=c(NA,16, NA),col=c(1,1,grey(0.75)), bty='n',lwd=c(1,2,5))
 
   abline(h=matsize[3], lwd=1) 
   mtext("Year", 1, line=2, cex=.8) 
 
     par(mar=figmar)
     par(mai=figmai)
     mtext(side=2,outer=T,"Median Fork Length (cm)",line=-2,cex=1.5, las=0)

  
savePlot("C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/IndPaper_Fig4_final2",type="pdf") 
savePlot("C:/Users/simonh/Dropbox/papers/clarke_shark_consbiol/IndPaper_Fig4_final2",type="eps") 
#savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig4_final2",type="pdf") 
#savePlot("C:/Users/joelr/Dropbox/clarke_shark_consbiol/IndPaper_Fig4_final2",type="eps") 
       
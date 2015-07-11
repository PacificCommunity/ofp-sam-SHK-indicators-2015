

###########################################################################################################
     # This plots the slope of a linear model fit to the standardized size data.  Thre are threee sources of size data, the LL data, the PS data and the JPN data.

###################################### #composite plot  #####################################################

#first load the data
loc <- "C:/Users/simonh/Dropbox/Papers/"
 
       szdta<-read.csv(paste(loc,"clarke_shark_consbiol/RegressionCoeff_CPUE/ChgStdSize_LL.csv",sep=""), header=T)
       szdta
       psdata<-read.csv(paste(loc,"clarke_shark_consbiol/RegressionCoeff_CPUE/ChgNominalSizePS.csv",sep=""),  header=T)
       psdata
       load( file=paste(loc,"clarke_shark_consbiol/RegressionCoeff_CPUE/JPNsizeChg.rdata",sep=""))
       p.info
###########################
 #REMOVE THRESHERS
   szdta<-szdta[szdta$species!="THR",]
   
      
   #assign color vectors     
     hues=c("royalblue","gray","red","mediumspringgreen","sienna")
     #make semi-transparent vectors for the nonsignificant ones.
         nrb<-rgb(red=65,green=105, blue=225, alpha=75, maxColorValue=255)    
         ngray<-rgb(red=190,green=190, blue=190, alpha=75, maxColorValue=255)    
         nred<-rgb(red=255,green=05, blue=0, alpha=75, maxColorValue=255)  
         nmsg<-rgb(red=0,green=250, blue=154, alpha=75, maxColorValue=255)  
         nsie<-rgb(red=160,green=82, blue=45, alpha=75, maxColorValue=255)  
         hues2<- c(nrb, ngray, nred, nmsg, nsie)   # makes slightly transparent colors just like the others ones.
         mygrey<-rgb(red=181, green=181, blue=181, alpha=150,   maxColorValue = 255) # specific grey 
   #get package for axis.break
   library(plotrix)  
          
#set up plotting details
    # lambda<-c(1.401, 1.141, 1.117, 1.108, 0.996)   based on Cortes 2002  ....demographic models...
    #nm.ordr<-c(BSH, MAK, OCS, FAL, THR)
    rvals<-c(0.286,0.018, 0.094,0.063,0.01)
    pch.vec<-rep(c(21,24), each=16) #male is 21 female 24                           # with thresher it is each =20
    l.vec<-c(rep(rvals[1:4],each=4),rep(rvals[1:4],each=4))
    nxlim<-c(-0.1,0.35)  
    nylim<-c(-6,5) 
    
#make plot    
    windows(height=8, width=10)
      a.loc<-c(-6)
     plot(l.vec, szdta$slope, xlim=nxlim, ylim=nylim, type='n', ylab="Estimated annual change in length (cm)", xlab="",xaxt='n')
      # axis(1, at=rvals[1:4] , tick=F, labels=c("BSH", "MAK", "OCS", "FAL"), cex.axis=0.9,line=1,las=1, col=4)
       axis(1, at=rvals[1:4] , tick=F, labels=c("Blue", "Mako", "Oceanic \n Whitetip", "Silky"), cex.axis=0.85,line=1.00,las=1, col=4, padj=.5)
       axis(1, at=rvals[1:4], tick=T, labels=rvals[1:4],cex.axis=0.8,las=1)
     arrows(x0=0.05, y0=a.loc, x1=-0.05,y1= a.loc,  lwd=1.5, col=1,cex=0.5, length=1/8)
     arrows(x0=0.2, y0=a.loc, x1=0.3,y1= a.loc,  lwd=1.5, col=1,cex=0.5, length=1/8)
     text(x=-0.05, y=a.loc, labels="Lower", pos=2)
     text(x=0.3, y= a.loc, labels="Higher", pos=4)
     text(x=0.12, y=a.loc, labels="Productivity")
        mtext("Intrinsic Rate of Increase", line=4,side=1, cex=1.25 )     
   
     box() 
     # axis(2,at=c(5,0,-5,-7.5,-9.5), labels=c("5","0","-5","-15","-20"), las=1 )
     # axis.break(2,-6.5)   
     #plot non significant ones with transparent colors
     #  col.vec<-rep(c(hues2, hues2), each=4) 
     #
     #
     for(i in 1:dim(szdta)[1]){ if(szdta$pval[i]>=0.05){points(jitter(l.vec[i], 0.85), szdta$slope[i],  pch=pch.vec[i],col=1, bg=NA,cex=1.7, lwd=1.85)}else{}       }
     #plot   significant ones with opaque colors 
     #col.vec<-  col.vec<-rep(c(hues, hues), each=4) 
    # for(i in 1:dim(szdta)[1]){ if(szdta$pval[i]<0.05){points(jitter(l.vec[i],5), szdta$slope[i],  pch=pch.vec[i],col=1, bg=mygrey,cex=1.7, lwd=2)}else{}       }
    xtra<-c(-0.005,0.005)
     for(i in 1:dim(szdta)[1]){ if(szdta$pval[i]<0.05){points( jitter(l.vec[i]+xtra[szdta$sex[i]], 2), szdta$slope[i],  pch=pch.vec[i],col=1, bg=mygrey,cex=1.7, lwd=2)}else{}       }
     #use a bit of foreknowledge here to scale the plots       # currently #18 and #38 are too big (>10 )
     #for(i in 1:dim(szdta)[1]){if(szdta$slope[i]<(-10)){points(jitter(l.vec[i], 0.85),  szdta$slope[i]/2 +0.51, pch=pch.vec[i],col=gray(0.05), bg=col.vec[i],cex=1.7, lwd=1.85)}else{} }
     # Japanese data  
     # plot(l.vec, szdta$slope, xlim=nxlim, ylim=c(-0.015,0.005), type='n', ylab="Slope a Linear fit to Standardized Size (cm)", xlab="",xaxt='n')
     # axis(1, at=rvals, tick=T, labels=c("BSH", "MAK", "OCS", "FAL", "THR"), line=0,las=2, cex=0.8)
     # axis(side=1, at=lambda, tick=T, labels=c("1.401", "1.141", "1.117", "1.108", "0.996"), cex.axis=0.8, las=2)  
     t.vec<-rvals[c( 1, 1, 1, 1, 2,2, 5, 5, 1, 1, 4,4,2,2,3,3, 5, 5) ]
     ord<-c( 1, 1, 1, 1, 2,2, 5, 5, 1, 1, 4,4,2,2,3,3, 5, 5) 
     #col.vec<-hues[ord ]
             # pch.vec2 <-rep(c(21,24),9) #males and females every other one
              p.info<-p.info[p.info$species!="THR",]
             # if p.info comes in as a data frame with  factors instead of numbers  use these lines
             if(is.factor( p.info$p.value)){p.info$p.value<- as.numeric(levels(p.info$p.value))[p.info$p.value] }else{}
             if(is.factor( p.info$slope)){p.info$slope<- as.numeric(levels(p.info$slope))[p.info$slope] }else{}  
            
             #plot non significant ones with transparent colors
             #col.vec<-hues2[ord ]
             for(i in 1:dim(p.info)[1]){ if(as.numeric(p.info$p.value[i])>=0.05){points(jitter(t.vec[i], 1.2),as.numeric(p.info$slope[i]),  pch=9,col= grey(0.5),cex=1.7, lwd=1.85)}else{}       }
              #plot   significant ones with opaque colors 
              #     col.vec<-hues[ord ]
        for(i in 1:dim(p.info)[1]){if(as.numeric(p.info$p.value[i])<0.05){points(jitter(t.vec[i], 1.2),as.numeric(p.info$slope[i]),  pch=9,col=1,cex=1.7, lwd=2)}else{}       }
     
        nxval<-jitter(rvals[c(3,3,4,4)],0.9)  # in case you want a background for your points then use this 
        #plot purse seine data, 
        #points( nxval, psdata[,3], pch=22, col=c(grey(0.05),grey(0.5)  ,grey(0.05),grey(0.9)), bg=c(hues[3], hues2[3], hues[4], hues[2])) 
         points( nxval, psdata[,3], pch=22, col=1, bg=c(NA, mygrey,NA, mygrey),lwd=c(1,2,1,2), cex=1.5) 
        # points( nxval, psdata[,3], pch=8, col=c(hues[3], hues2[3], hues[4], hues2[4]), cex=c(1.5,0.7,1.5,0.7) )
                   
#          mtext("Intrinsic Rate of Growth (jittered to show points)", line=3.5,side=1, cex=1.25 )     
  
        abline(h=0, lty=3, lwd=0.8, col=gray(0.5) )
    #    abline(h=-6)
        
        legend("topleft", legend=c("Longline Males", "Longline Females", "Purse Seine", "Japanese Research/Training Vessel", "Sig. p <0.05"),pt.cex=1.5, pt.lwd=c(1,1,1,1,2),pch=c(21,24,22, 9,22), col=c(1,1,1,1,1),bg="white", pt.bg=c(NA, NA,NA,NA, mygrey))
         #
      filecap <- paste(loc,"clarke_shark_consbiol/IndPaper_fig6_final1",sep="")   
      # filecap <- paste("C:/Users/joelr/Desktop/Ind_Shk_Paper/IndPaper_fig6",sep="")
       # savePlot(filename=filecap,type="png")   
         savePlot(filename=filecap,type="eps")
         savePlot(filename=filecap,type="pdf")   
#####################################################################################################
       
   

        
    
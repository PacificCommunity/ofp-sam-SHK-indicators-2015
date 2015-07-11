

#plots for the Shark Indicator Paper, 18/01/12
#
#
#  this starts with analysis from C:/Users/joelr/Desktop/Shark Stock Assessment/R_CODE_shark work/LLSharkLength_lm_stdSZ.r
#         # but then is updated to run the plots from txt_loc rather than the reg.out list
#
# or you could just 
load(file="C:/Users/joelr/Desktop/Shark Stock Assessment/LengthData/SharkLNdata.rdata")
#
#
#############################################################################################################

      #the Windshield wiper Plot 
      szdta<-read.csv("C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\LLSizeModelCoeffs\\ChgStdSize_LL.csv", header=T)
      szdta

     hues=c("royalblue","gray","red","mediumspringgreen","sienna")
     col.vec<-c(rep(hues, each=4))
     index.vec2<-c(rep(c(1,3,5,7,9), each=4) )
     txt_loc<-read.table("C:/Users/joelr/Desktop/Shark Stock Assessment/LengthData/txtlocation.csv",sep=",", header=T)    
     greyvec<- c(gray(c(0.7,0)))
     
     #start Plot
     #windows()
                                                                                                                           # 
        plot(c(1,2), c(1,2)*reg.out[[1]][2], type='n', col=2, lwd=2, xlim=c(0,8.55), ylim=c(-3,3), ylab='',   xlab='', yaxt='n',xaxt='n', main='' )
             axis(1, at= c(1,3,5,7,9)+0.5, tick=T, labels=c("BSH", "MAK", "OCS", "FAL", "THR"))
             axis(2, at=c(-2,0,2), tick=F, labels=c("-", "0", "+"), cex.axis=1.3, las=1)
             axis(2, at=c(0), tick=F, labels=c("Slope"), line=1.25)    
            abline(h=0, lty=3, lwd=0.8, col=gray(0.5) )
         #plot the longline             
         for(i in 1:40){
                      #lines(c( index.vec2[i], index.vec2[i]+1), c( 0, 1*reg.out[[i]][2]), type='l', col=greyvec[txt_loc$sig[i]+1], lwd=2)
                      lines(c( txt_loc$plot.index[i], txt_loc$plot.index[i]+1), c( 0, 1*txt_loc$slope[i]), type='l',lty=txt_loc$sex[i],  col=greyvec[txt_loc$sig[i]+1], lwd=2) } 
         #plot the females                   
        # for(i in 21:40){
#                      lines(c( index.vec2[i-20], index.vec2[i-20]+1), c( 0, 1*reg.out[[i]][2]), lty=2, col=greyvec[txt_loc$sig[i]+1], lwd=2)}        #col=col.vec[i-20]      for colors
#        
         #hardwire the PURSE SEINE data
                  lines( x=c(4.975,6), y=c(0, -5.08008658), lty=3,  col=greyvec[2])
                  lines( x=c(5,6), y=c(0, -5.108108108), lty=3, col=greyvec[1])
                  lines( x=c(7,8), y=c(0, -1.192307692), lty=3, col=greyvec[2])
                  lines( x=c(7,8), y=c(0, -2.270979021), lty=3, col=greyvec[2])
                  
                  
                                                             #gray(0) is black
       #      abline(v=4.75, col=gray(0.71))
        
        
         #block out the extra long ones           
         polygon( c(0,11,11,0), y=c(-1.5,-1.5,-6,-6),border = NA, col ="white")
         polygon( c(0,11,11,0), y=c(1.5,1.5,6,6),border = NA, col ="white")
         box()  #replace
         #legend("topleft", legend=c( "Males", "Females", "Purse Seine"), lty=c(1,2,3), lwd=c(2,2,1),  col=c(1)) 
          legend("topleft", legend=c( "Males", "Females", "Purse Seine", "Region (Sig.)", "Region (Not Sig.)"), lty=c(1,2,3, NA, NA), lwd=c(2,2,1, NA,NA ),  col=c(1,1,1,1,greyvec[1]), pch=c(NA, NA,NA, '4', '4'),  text.col=c(1,1,1,1,greyvec[1])) 
          
  #  ?legend
         
         text(txt_loc$new.x, txt_loc$adjusted.y, labels=txt_loc$reg.num,font=1+txt_loc$sig, pos=1, col=greyvec[txt_loc$sig +1])
        
        # text(txt_loc$new.x, txt_loc$adjusted.y, labels=c,font=1+txt_loc$sig, pos=1,cex=2*txt_loc$sig, col=greyvec[txt_loc$sig +1])      #  these are poor attempts at circling the names.
        # points(txt_loc$new.x, txt_loc$adjusted.y,cex=txt_loc$sig,  col=greyvec[txt_loc$sig +1])                                         #
        
        #save 
            filecap <- paste("C:/Users/joelr/Desktop/Ind_Shk_Paper/IndPaper_fig3_v2",sep="")
          savePlot(filename=filecap,type="png")
         #  head(txt_loc)


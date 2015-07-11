
# 11/7/2011 this file takes the standardized year effects from the CPUE data, and fits a linear model to them, then plots and saves the output.
# 19 Jan 2012 update to blac and white, make more legible and ledgend changes, remove thresher sharks
# 24/04/2012   updated to fix graph as per reviewrs comments.

#####only LL data are used as there are not enough PS length data to apply     


    #rm(list=ls(all=TRUE))
#    gc()
#    
#    # memory.limit(size=NA)
#    # memory.limit(size=4000)
#    # memory.limit(size=NA)
#    # processing the new Hawaii data        if you need to re-do this just delet the hash mark  
#    
#    # new.hw<-read.table("C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\CPUE_DATA_stdzd\\HWdata.csv",sep=",",header=T)
#    #  new.hw
#    #    Year HW_DLN_OCS HW_ZIP_OCS HW_DLN_FAL HW_QP_FAL
#    #           cbind(  new.hw$Year, new.hw$HW_DLN_OCS)
#    #           
#    #  SJH_HWDLN[[1]]<-cbind(  new.hw$Year, new.hw$HW_DLN_OCS)
#    #    colnames(SJH_HWDLN[[1]])<-c("years", "HWDLN_OCS")
#    #       SJH_HWDLN[[1]]
#    #       
#    #           
#    #  SJH_HWDLN[[2]]<-cbind(  new.hw$Year, new.hw$HW_DLN_FAL)
#    #    colnames(SJH_HWDLN[[2]])<-c("years", "HWDLN_FAL")
#    #       SJH_HWDLN[[2]]
#    #      SJH_HWDLN   #to check
#    #      
#    #      
#    #      SJH_HWQP[[1]]<-cbind(  new.hw$Year, new.hw$HW_ZIP_OCS)
#    #          colnames(SJH_HWQP[[1]])<-c("years", "HW_ZIP_OCS")
#    #          SJH_HWQP[[1]]
#    #          
#    #           
#    #      SJH_HWQP[[2]]<-cbind(  new.hw$Year, new.hw$HW_QP_FAL)
#    #          colnames(SJH_HWQP[[2]])<-c("years", "HW_QP_FAL")
#    #          SJH_HWQP[[2]]
#    #          
#    #          SJH_HWQP
#    #          rm(new.hw)
#    #          ls()
#    #          
#    #     save( list=ls(), file="P:\\WCPFC Shark\\StatusPlotList4.RData")    
#    #    save( list=ls(), file="C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\CPUE_DATA_stdzd\\StatusPlotList4.RData")  
#    #    #
#    #####################################################################   
#    
#    
#    #load( file="P:\\WCPFC Shark\\StatusPlotList3.RData")
#     load( file="P:\\WCPFC Shark\\StatusPlotList4.RData")
#    #  load(file="C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\CPUE_DATA_stdzd\\StatusPlotList4.RData")  
#      ls()
#         d.sets<-ls()          # as long as StatusPlotList3.RData is a group of lists, this codeshould work, other wise overwrite with  
#         d.sets
#         #if d.sets is off just hard wire it to the lists you are using.
#           d.sets<-c("SJH_HWDLN",  "SJH_HWQP",   "SJH_JPLL1", "SJH_JPRTV2", "SJH_JPRTV4", "SJH_SPC")
#    
#                   
#    ################data analysis       _ go through the lists, do the regressions and save out the coefficients as elements of individual lists
#           data.out<-c()  
#          for(j in 1:length(d.sets)){
#           # j<-4
#            d.sets[j]
#    
#        wkngobj<-get(noquote(d.sets[j]))
#        #names(wkngobj)
#        no.fits<-length(wkngobj)
#        # no.fits
#       TEMPSUM<-list()   #make storage ....ideally this would just be one loop with this list commad somethigng like   get(paste(noquote(d.sets[j]),".sum<-list()", sep="")  and then referenced later
#       t.nm.vec<-c() #temp name vector
#       
#      for( i in 1:no.fits){
#        a <- wkngobj[[i]]
#           if( length(dim(a))>0){
#              t.nm<-colnames(a)      #temp names
#              if(d.sets[j] == "SJH_SPC") a <- a[a[,1] < 2010,]
#              t.lm<-lm( a[,2]~ a[,1]) #temp linear model
#              tlog.lm <- lm(log(a[,2])~ a[,1]) #temp linear model
#              #summary(t.lm)
#              t.nm.vec<-c(t.nm.vec, t.nm[2])
#              # TEMPSUM[[i]]<-coef(summary(t.lm)) 
#              TEMPSUM[[i]]<-coef(summary(tlog.lm))                                    # changed to an exponential decline model to give rates of decline 17 April 2012
#                    #here is where there is a problem with the variable name  matching.
#                            #
#                 #windows()
#                # plot( wkngobj[[i]][,1], wkngobj[[i]][,2], pch=20, cex=1.2, xlab="Year", ylab="Standardized CPUE", main=paste(d.sets[j],t.nm[2], sep=" "))
#                # abline( t.lm, col=4, lwd=2)
#                            }else{}
#                   names(TEMPSUM)<-t.nm.vec
#                   assign(noquote(paste("XX",d.sets[j],sep='')),TEMPSUM)
#                   }
#                   data.out<-c(data.out,  noquote(paste("XX",d.sets[j],sep='')))
#                        }
#                        
#           # so now we have lists of coeffecients
#           # co.lists<-c( "XXSJH_HWDLN",   "XXSJH_HWQP",    "XXSJH_JPLL1",   "XXSJH_JPRTV2",  "XXSJH_JPRTV4",  "XXSJH_SPC")    
#           # data.out might be the same names, but actual items.
#               
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_HWDLN)) {
#                          slp<-c(slp, XXSJH_HWDLN[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_HWDLN[[i]][8])}
#               HWDLN.dataout<- cbind(slp, p.vl)  
#               rownames(HWDLN.dataout)<-  names(XXSJH_HWDLN)
#             #
#             #          
#            
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_HWQP)) {
#                          slp<-c(slp, XXSJH_HWQP[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_HWQP[[i]][8])}
#               HWQP.dataout<- cbind(slp, p.vl)  
#               rownames(HWQP.dataout)<-  names(XXSJH_HWQP)
#             #
#             #        
#             
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_JPLL1)) {
#                          slp<-c(slp, XXSJH_JPLL1[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_JPLL1[[i]][8])}
#               JPLL1.dataout<- cbind(slp, p.vl)  
#               rownames(JPLL1.dataout)<-  names(XXSJH_JPLL1)
#             #
#             # 
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_JPRTV2)) {
#                          slp<-c(slp, XXSJH_JPRTV2[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_JPRTV2[[i]][8])}
#               JPRTV2.dataout<- cbind(slp, p.vl)  
#               rownames(JPRTV2.dataout)<-  names(XXSJH_JPRTV2)
#             #
#             #        
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_JPRTV4)) {
#                          slp<-c(slp, XXSJH_JPRTV4[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_JPRTV4[[i]][8])}
#               JPRTV4.dataout<- cbind(slp, p.vl)  
#               rownames(JPRTV4.dataout)<-  names(XXSJH_JPRTV4)
#             #
#             #        
#               slp<-c()
#               p.vl<-c()
#               for(i in 1:length(XXSJH_SPC)) {
#                          slp<-c(slp, XXSJH_SPC[[i]][2])
#                          p.vl<-c(p.vl,XXSJH_SPC[[i]][8])}
#               spc.dataout<- cbind(slp, p.vl)  
#               rownames(spc.dataout)<-  names(XXSJH_SPC)           
#                        
#                
#                #so all the data is stored in these, but they are all "different scale/ catchabilities so what we really need is -some standardization
#                # I tried dividing bythe mean, and max, and that changes the slope.
#                 HWDLN.dataout
#                 HWQP.dataout
#                 JPLL1.dataout        
#                 JPRTV2.dataout         
#                 JPRTV4.dataout          
#                 spc.dataout
#                 #the JPRTV2 silky shark data have slope of 0 with p.val=NAN...which gives errors.
#                 #better to    overwrite as 100
#                       JPRTV2.dataout[4,2]<-100
#                 
#      #           
#                  HWDLN.2 <-cbind( HWDLN.dataout , c("OCS","FAL"), c("HWDLN","HWDLN"),c("HW", "HW"))
#                  HWQP.2  <-cbind(  HWQP.dataout , c("OCS","FAL"),c("HW_ZIP","HW_QP"),c("HW", "HW")) 
#                  JPLL1.2  <-cbind( JPLL1.dataout , c("BSH","MAK", "OCS", "THR"), c(rep("JPLL1", 4)), c("N","N","N","N"))
#                  JPRTV2.2  <-cbind( JPRTV2.dataout , c("BSH","MAK", "OCS", "FAL","THR"), c(rep("JPRTV2", 5)),c(rep(2, 5)))    
#                  JPRTV4.2  <-cbind( JPRTV4.dataout,  c("BSH","MAK", "OCS", "FAL","THR"), c(rep("JPRTV2", 5)),c(rep(4, 5)))   
#                  spc.2    <-cbind( spc.dataout, c("BSH","BSH","MAK","MAK", "OCS", "FAL","THR"), c(rep("SPC", 7)),c("N","S","N","S",NA,NA,NA) )  
#    #             
#                  HWDLN.2  
#                  HWQP.2   
#                  JPLL1.2  
#                  JPRTV2.2      
#                  JPRTV4.2   
#                  spc.2  
#                  
#    #XXSJH_HWDLN 2
#    #XXSJH_HWQP  2
#    #XXSJH_JPLL1   4
#    #XXSJH_JPRTV2  5
#    #XXSJH_JPRTV4  5
#    #XXSJH_SPC     7
#    
#    slope_coefs <- matrix(NA,nrow=23,ncol=2)
#    i <- 0
#    a <- c(XXSJH_HWDLN,XXSJH_HWQP,XXSJH_JPLL1,XXSJH_JPRTV2,XXSJH_JPRTV4,XXSJH_SPC)
#    a$coeffsilky <- NULL
#    a[[14]] <- NULL
#    for (ln in a) {
#      n <- dim(ln)
#      i <- i+1
#      slope_coefs[i,] <- ln[2,1:2]
#      }
#    a <- data.frame(species=names(a),slope=slope_coefs[,1],sd=slope_coefs[,2])
#    a$trend <- exp(a$slope)-1
#    a$trend_upper <- exp(a$slope + 1.96 * a$sd)-1
#    a$trend_lower <- exp(a$slope - 1.96 * a$sd)-1
#    a$error <- (a$trend_upper - a$trend_lower)/2
#    summ_table <- a
#    mean(a$trend)
#    median(a$trend)
#                    
#     ############################################################################
#     #make and save out the final matrix
#     #
#     
#     temp.mat<-rbind( HWDLN.2, HWQP.2, JPLL1.2,JPRTV2.2, JPRTV4.2,spc.2) 
#     #nrow(temp.mat)
#       lambda<-c(1.401, 1.141, 1.117, 1.108, 0.996)        #lambda values from Cortes 2002
#       rvals<-c(0.286,0.018, 0.094,0.063,0.01)             #r values from Cortes 2010
#         nm.ordr<-c("BSH", "MAK", "OCS", "FAL", "THR")
#         sp.indx<-c(1:5)
#     
#     l.vec<-temp.mat[,3]
#        sp.index<-temp.mat[,3]
#        
#      for(i in 1:length(lambda) ){
#     l.vec[ c(which(l.vec== nm.ordr[i]))]<-  rvals[i]    
#      sp.index[ c(which(  sp.index== nm.ordr[i]))]<-  sp.indx[i]    
#       }
#         sp.index
#         
#         
#         
#        LLcpuechg<-cbind(exp(as.numeric(temp.mat[,1])), temp.mat, l.vec,sp.index)
#        colnames(LLcpuechg)<-c("rate of change","slope","p.val","species","d.set","area","l.vec", "sp.index")
#        LLcpuechg
#        # one of these was NA and 0 earlier...
    #LLcpuechg[10,2]<-1
    
   #  save(LLcpuechg, file="C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\CPUE_DATA_stdzd\\LLcpuechg3.Rdata" )       "P:\\WCPFC Shark\\             
   #  save(LLcpuechg, file="P:\\Shark\\Ind_Shk_Paper\\CPUE_DATA_stdzd\\LLcpuechg3.Rdata" )       
     
    #load(   file="P:\\Shark\\Ind_Shk_Paper\\CPUE_DATA_stdzd\\LLcpuechg3.Rdata" )
    loc <- "C:/Users/simonh/Dropbox/Papers/"

      load(   file=paste(loc,"clarke_shark_consbiol/RegressionCoeff_CPUE/LLcpuechg3.Rdata",sep="") )
   
################################################################################################################        
#COLORS 
 hues=c("royalblue","gray","red","mediumspringgreen","sienna")
   #make new colors    - like the old but more  transparent governed by ALPHA
   a.level<-100
   nrb<-rgb(red=65,green=105, blue=225, alpha=a.level, maxColorValue=255)    
   ngray<-rgb(red=190,green=190, blue=190, alpha=a.level, maxColorValue=255)    
   nred<-rgb(red=255,green=05, blue=0, alpha=a.level, maxColorValue=255)  
   nmsg<-rgb(red=0,green=250, blue=154, alpha=a.level, maxColorValue=255)  
   nsie<-rgb(red=160,green=82, blue=45, alpha=a.level, maxColorValue=255)  
   hues2<- c(nrb, ngray, nred, nmsg, nsie)
      plot(1:10, 1:10, pch=21, col=1, bg=c(hues, hues2), cex=3) 
################################################################################################################     plot 1
         #      plot(1:30,1:30,col=rainbow(30),pch=1:30, cex=3)
 
    pch.vec<- c(20,20,21,21,rep(22,3),rep(c(24,25),each=4),rep(23, 6))
    no.fits<- nrow(LLcpuechg)   
    mygrey<-rgb(red=181, green=181, blue=181, alpha=150,   maxColorValue = 255) # specific grey 
          #remove Thresher sharks
      LLcpuechg<-as.data.frame(LLcpuechg)
      LLcpuechg<-LLcpuechg[LLcpuechg$species!="THR",]
       no.fits<- nrow(LLcpuechg)   
    
     rvals<-c(0.286,0.018, 0.094,0.063,0.01)    
     #plot limits
#     nxlim<-c(-0.1,0.35)  
     nxlim<-c(0,0.35)  
     nylim<-c(-0.3,0.25) 
        LLcpuechg[,7]<-as.numeric(as.character(LLcpuechg[,7]))
        LLcpuechg[,1]<-as.numeric(as.character(LLcpuechg[,1]))
        LLcpuechg[,3]<-as.numeric(as.character(LLcpuechg[,3]))
        LLcpuechg[,2] <- as.numeric(as.character(LLcpuechg[,2]))
    
    a.loc<- c(-0.3)# arrow location in yaxis scale  
     
     #start plot   
    windows(height=8, width=10)
    par(las=1)
     plot( LLcpuechg[,6], LLcpuechg[,1], xlim=nxlim, ylim=nylim,pch=20, col=1, type='n', bg=hues[2], cex=1.7, ylab="Annual Rate of Change in Standardized Catch Rate", xlab="",xaxt='n')
     #write Axes  text and set up plot
# axis(1, at=rvals[1:4] , tick=F, labels=c("Blue", "Mako", "Oceanic \n Whitetip", "Silky"), cex.axis=0.85,line=1.25,las=1, col=4)
 axis(1, at=rvals[1:4] , tick=F, labels=c("Blue", "Mako", "Oceanic \n Whitetip", "Silky"), cex.axis=0.85,line=1,las=1, col=4, padj=0.5)
  #   axis(1, at=rvals[1:4], tick=F, labels=c("BSH", "MAK", "OCS", "FAL"), cex.axis=0.9,line=1.5,las=1)
#     axis(1, at=rvals[1:4], tick=T, labels=rvals[1:4],cex.axis=0.8,las=1)
     axis(1, at=rvals[1:4], tick=T, labels=rvals[1:4],cex.axis=0.8,las=1)
     arrows(x0=0.12, y0= a.loc, x1=0.05,y1= a.loc,  lwd=1.5, col=1,cex=0.5, length=1/8)
     arrows(x0=0.2, y0= a.loc, x1=0.3,y1=  a.loc,  lwd=1.5, col=1,cex=0.5, length=1/8)
     text(x=0.05, y= a.loc, labels="Lower", pos=2)
     text(x=0.30, y= a.loc, labels="Higher", pos=4)
     text(x=0.16, y= a.loc, labels="Productivity")
     abline(h=0, col=gray(0.65), lty=3, lwd=0.7)
     box() 
          jamt<-0.0
     #plot non significant ones with grey colors                                                                               bg=hues2[as.numeric(LLcpuechg[i,7])]
#      for(i in 1:no.fits){ if(as.numeric(LLcpuechg[i,2])>=0.05){points(jitter(LLcpuechg[i,6]), LLcpuechg[i,1], pch=pch.vec[i],col=gray(0.04), bg=NA,cex=1.7, lwd=1)} else{} }
      for(i in c(1:10,12:21)){ if(as.numeric(LLcpuechg[i,3])>=0.05){points(jitter(as.numeric(LLcpuechg[i,7]),amount=jamt), LLcpuechg[i,1]-1, pch=pch.vec[i],col=gray(0.04), bg=NA,cex=1.7, lwd=1)} else{} }
        #plot significant ones with opaque
#     for(i in 1:no.fits){ if(as.numeric(LLcpuechg[i,2])<0.05){points(jitter(LLcpuechg[i,6] ), LLcpuechg[i,1], pch=pch.vec[i],col=1, bg=mygrey,cex=1.7, lwd=2)} else{} }
     for(i in c(1:10,12:21)){ 
     if(LLcpuechg[i,"species"]=="OCS"){jamt<-0.01}else{jamt<-0}
     if(as.numeric(LLcpuechg[i,3])<0.05){points(jitter(LLcpuechg[i,7], amount=jamt ), LLcpuechg[i,1]-1, pch=pch.vec[i],col=1, bg=mygrey,cex=1.7, lwd=2)} else{} }
        
      # legend("topleft", legend=c("HWDLN", "HWQP", "JPLL1", "JPRTV2", "JPRTV4","This Study", "Sig. p<0.05"),pt.cex=1.5, pch=c(20,21,22,24,25,23, 22), col=1, pt.lwd=c(rep(1,6),2),  pt.bg=c(NA,NA, NA, NA,NA,NA, gray(0.71)),ncol=4  )
       l.text<-c("Hawaii Delta-Lognormal", "Hawaii Quasi-Poisson", "Japanese Longline", "Japanese RTV #2","Japanese RTV #4","This Study", "Sig. p<0.05")
       legend("top", legend=l.text,pt.cex=1.5, pch=c(20,21,22,24,25,23, 22), col=1, pt.lwd=c(rep(1,6),2),  pt.bg=c(NA,NA, NA, NA,NA,NA, gray(0.71)),ncol=4  )
 
 
 #     text(x=as.numeric(LLcpuechg[LLcpuechg$d.set=="SPC",6]),y=as.numeric(LLcpuechg[LLcpuechg$d.set=="SPC",1]), labels=as.character(LLcpuechg[LLcpuechg$d.set=="SPC",5]),pos=4)
       text(x=as.numeric(LLcpuechg[LLcpuechg$d.set=="SPC",7]),y=as.numeric(LLcpuechg[LLcpuechg$d.set=="SPC",1])-1, labels=as.character(LLcpuechg[LLcpuechg$d.set=="SPC",6]),pos=4)
        #mtext("Slope of a Linear Model fit to Standardized CPUE", line=2,side=3, cex=1.5)        #title?            
 #     mtext("Intrinsic Rate of Growth (jittered to show points)", line=3.5,side=1, cex=1.25 )     
      mtext("Intrinsic Rate of Increase", line=4,side=1, cex=1.25 )     

         

    filecap <- paste(loc,"clarke_shark_consbiol/IndPaper_fig5_final3",sep="")
     # filecap <- paste("IndPaper_fig5",sep="")
       savePlot(filename=filecap,type="pdf")
       savePlot(filename=filecap,type="eps")   
   
##################################################################

#for(j in 1:length(d.sets)){
#  wkngobj<-get(noquote(d.sets[j]))
#  no.fits<-length(wkngobj)
#  windows(14,10); par(mfrow=c(3,3))
#  for(i in 1:no.fits) { 
#    dat <- wkngobj[[i]][1:14,]
#    plot(dat[,1],dat[,2],ylim=c(0,max(dat[,2],na.rm=T)),main=names(dat)[2])
#    m <- lm(log(dat[,2]) ~ dat[,1])
#    y <- exp(m$coef[2] * dat[,1] + m$coef[1])
#    lines(dat[,1],y,lty=1)
#    }
#  title(d.sets[j],outer=T,line=-1)  
#  }
#
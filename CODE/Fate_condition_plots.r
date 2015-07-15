
# 13July 2015
# library(grid)
# library(gridBase)
#

 #save par
  
 par(omi=c(0,0,0.2,0))
 par(oma=c(0,0,1,0))
 par(las=1)
 #
 #
 #
 load("C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata")
 head(catch)
 #windows(11,14)                                                                                             
#  nf<-layout( matrix(c(1,2,3,1,4,5,1,6,7,1,8,8,9,10,11,9,12,13,9,14,15, 9,16,16),8,3,byrow=TRUE) , widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
# # nf<-layout( matrix(c(15,1,2,15,3,4,15,5,6,15,7,7,16,8,9,16,10,11,16,12,13,16,14, 14),8,3,byrow=TRUE), widths=c(4,2,2), heights=c( 2,2,2,1,2,2,2,1))
# 
# layout.show(nf)# 

  figmar<-c(4, 4, 4, 2) + 0.1
  figmai<-c(0.65,0.65,0.45,0.1)
  
  legmar<-c(0,0,0,0)
  
# par(mar=figmar)
 par(mai=figmai)    
     par("mai")
   
  huenames <- c("Escaped","Retained","Discarded","Finned") 
 # hues<-c( gray(1), gray(25/30), gray(11/30), gray(0))                #                 gray(0)=black;    gray(1)=white
 hues <- c('white', 'firebrick1', 'deepskyblue', 'black')
 
  
 
 #R=retained; D=discarded; E=escaped  
 #DFR=discarded trunk, fins retained
 #DPA=species of special interest discarded alive
 #DPD=species of special interest discarded dead
 #DPU=species of special interest discarded condition unknown
 #DOR=discarded, other reason
 #RPT=retained partial
 
 #> table(catch$fate_code)
 
 #    A0     A1     A2     A3      D      U 
 #141703  73175  15127  14947  47399  69676 
 
 #A0=alive
 #A1=alive, healthy  
 #A2=Alive, injured or distressed
 #A3=Alive, but dying
 #D=dead
 #U=condition unknown
 
 

#------ Regional plot of the data
 # par(mfrow=c(1,1),mar=c(3,2,2,1),omi=c(0.5,0.5,1,0),ask=TRUE)
#  with(catch, table(yy, retained, escaped))
#  head(catch)
#  spec
#  rm(tdat)
 tdat <- catch[catch$sp_category %in%spec, ]; nrow(tdat)
 #c("escaped","retained","discarded","finned")
reg_res<- cbind(  tapply(tdat$escaped,      tdat$yy, sum)
     ,tapply(tdat$retained,     tdat$yy, sum)
     ,tapply(tdat$discardednf,  tdat$yy, sum)
     ,tapply(tdat$finned,       tdat$yy, sum) )
 reg_res  <- t(reg_res)
 colnames(reg_res) <- c("Escaped","Retained","Discarded","Finned")

 
 png(file=paste0(shkdir,"GRAPHICS/fate_ll_obs_regional.png" )) 
 # 
  par(mfrow=c(1,1),,mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)) 

 #
 barplot( reg_res /1000,ylab="",xlab="",names.arg=c(1995:2014),col=hues,ylim=c(0,40 ), las=1)  
 
  legend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.15,horiz=F,bty="n", ncol=1)
  mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.3,cex=1 )
  mtext(side=3,outer=F,"Fate of Observed Sharks - Longline",line=2,cex=1 )                     #WCPO
  mtext(side=1, outer=F, "Year", line=2)
 #
 dev.off()
 
 table(catch$escaped>0)
 #-----------------------------
 # same but as a proportion
 png(file=paste0(shkdir,"GRAPHICS/fate_ll_obs_regional_prop.png" )) 
 # 
 # 
 par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE) 
  
 barplot(prop.table( reg_res,2 ) ,ylab="",xlab="",names.arg=c(1995:2014),col=hues,  las=1)         
 legend(-1, -.15,legend=huenames[1:4],fill=hues[1:4],cex=1,horiz=T,bty="n", ncol=1, xpd=NA)
 mtext(side=2,outer=F,"Proportion Recorded by Observers ",line=2.3,cex=1 )
 mtext(side=3,outer=F,"Fate of Observed Sharks - Longline",line=2,cex=1 )                     #WCPO
 mtext(side=1, outer=F, "Year", line=2)
 #
 dev.off()
 #
 rm(reg_res)
#
 #-------------------------------------------------------------------
#

# Plot the fate condition year by year in each region
 
tescape <-  tapply(tdat$escaped,       list(tdat$yy, tdat$region), sum)
tretain <-  tapply(tdat$retained,      list(tdat$yy, tdat$region), sum)
tdiscar <-  tapply(tdat$discardednf,   list(tdat$yy, tdat$region), sum)
tfinned <-  tapply(tdat$finned,        list(tdat$yy, tdat$region), sum) 
 
 png(file=paste(shkdir,"GRAPHICS/fate_ll_obs_by_region.png",sep='')) 
 #
 par(mar=c( 2.55,3.05, 2.05, 1.05), mgp=c(3, 1, 0), las=1, oma=c(1,1,1,.5)) #  
 layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
 #par(las=1,   oma=c(2,2,3.5,1) )
 
 for(i in 1:nreg){
   
   tmat <- rbind( tescape[,i], tretain[,i], tdiscar[,i], tfinned[,i]) 
   bp<-  barplot( tmat/1000, col=hues , main=paste("Region", i), las=1 )
  
   if(i%in% 3) { mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.5,cex=0.75, las=0 )  }
 } 
 
  par(mar = par("mar")/2)
 plot.new()
  legend('center',  legend=huenames[1:4],fill=hues[1:4],cex=1.5,horiz=F,bty="n", ncol=2, xpd=NA)
 
 #
 mtext(side=2,outer=T,"Number Recorded by Observers (1000s)",line=2.3,cex=1 )
 mtext(side=3,outer=T,"Fate of Observed Sharks - Longline",line=2,cex=1 )                     #WCPO
# mtext(side=1, outer=F, "Year", line=2)
  dev.off()
 
 
 #---------------------------------------------------------------------------------------------------------
 # species specific plots?
 head(catch)
 rm(reg_res)
speclongC <-  c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher", "Hammerhead", "Porbeagle" ) 
 
 for ( i in 1: length(spec)){
  # i<-3
   
   tdat <- catch[catch$sp_category==spec[i],]
   
  reg_res <-  cbind(  tapply(tdat$escaped,      tdat$yy, sum)
           ,tapply(tdat$retained,     tdat$yy, sum)
           ,tapply(tdat$discardednf,  tdat$yy, sum)
           ,tapply(tdat$finned,       tdat$yy, sum) )
   reg_res  <- t(reg_res)
   rownames(reg_res) <- c("Escaped","Retained","Discarded","Finned")
     
png( file=paste0(shkdir,"GRAPHICS/fate_ll_obs_", spec[i], ".png" ) ) 
   # 
  par(mfrow=c(1,1) ,mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)  
 
 #
 if(max (colSums(reg_res)) >1000){
 barplot( reg_res /1000 ,ylab="",xlab="", col=hues,  las=1)  
 mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.3,cex=1, las=0 )
 }else{ 
 barplot( reg_res  ,ylab="",xlab="", col=hues,  las=1)  
 mtext(side=2,outer=F,"Number Recorded by Observers",line=2.3,cex=1, las=0 )
 }
 
 legend("topright", legend=huenames[1:4],fill=hues[1:4],cex=.85,horiz=F,bty="n", ncol=1, xpd=T)
 mtext(side=3,outer=F,paste0("Fate of Observed ", speclongC[i] ," Sharks \nLongline"),line=2,cex=1 )                     #WCPO
 mtext(side=1, outer=F, "Year", line=2)
 #
 dev.off()
   
   
 }

table(catch$sp_category)
 par('usr')
 
 #---------
 
 
 
 #------------------     Purse seine fate  
 psbio <- read.csv("C:/Projects/DATA_2015/PS/ps_shk_catch_FATE_july_2015.csv", header=TRUE, stringsAsFactors=FALSE) # note there is no length here...
 head(psbio) ; nrow(psbio) #56809
 psbio <- psbio[psbio$yy %in% s.yr:e.yr, ]
 psbio$region<- 0
 psbio$region <- ifelse(psbio$lat1 >= 20 & psbio$lat1 <= 50 & psbio$lon1 >= 120 & psbio$lon1 < 180, 1, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= 20 & psbio$lat1 <= 50 & psbio$lon1 >= 180 & psbio$lon1 < 210, 2, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -10 & psbio$lat1 < 20 & psbio$lon1 >= 120 & psbio$lon1 < 170, 3, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -10 & psbio$lat1 < 20 & psbio$lon1 >= 170 & psbio$lon1 < 210, 4, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -10 & psbio$lat1 < -4 & psbio$lon1 >= 210 & psbio$lon1 < 230, 4, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -40 & psbio$lat1 < -10 & psbio$lon1 >= 141 & psbio$lon1 < 170, 5, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -55 & psbio$lat1 < -40 & psbio$lon1 >= 141 & psbio$lon1 < 150, 5, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -60 & psbio$lat1 < -40 & psbio$lon1 >= 150 & psbio$lon1 < 170, 5, psbio$region)
 psbio$region <- ifelse(psbio$lat1 >= -60 & psbio$lat1 < -10 & psbio$lon1 >= 170 & psbio$lon1 < 230, 6, psbio$region)
 psbio <- psbio[psbio$region > 0,]
 nrow(psbio)   #  28043
 #
 
 table(psbio$sp_code )
 
 # individual species to include
 sp <- c('FAL','OCS','BSH') # silky, oceanic whitetip , blue
 THR <- c('THR','BTH','PTH','ALV')
 MAK <- c('MAK','SMA','LMA')
 HHD <- c('SPN','SPZ','SPL','SPK','EUB')
 
 # make my own categories using sp_category
 # psbio$sp_category[psbio$sp_code %in% sp] <- psbio$sp_code[psbio$sp_code %in% sp]
 # psbio$sp_category[psbio$sp_code %in% THR] <- "THR"
 # psbio$sp_category[psbio$sp_code %in% MAK] <- "MAK"
 # psbio$sp_category[psbio$sp_code %in% HHD] <- "HHD"
 
 psbio$sp_category <- as.character(psbio$sp_code )
 psbio$sp_code <- as.character(psbio$sp_code)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% sp, psbio$sp_code, psbio$sp_category)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% MAK, "MAK", psbio$sp_category)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% THR, "THR", psbio$sp_category)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% HHD, "HHD", psbio$sp_category)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% "SKJ", "SKJ", psbio$sp_category)
 psbio$sp_category  <- ifelse(psbio$sp_code %in% "POR", "POR", psbio$sp_category)
 
 
 

 nrow(psbio) 
 # psbio2<-psbio[psbio$len>0,]      ; nrow(psbio2) #  NO length in this data set...
 
 #note "retained = yes" means retained for more than just fins
 psbio$retained <- ifelse(substr(psbio$fate_code,1,1)=="R",1,0)
 psbio$escaped <- ifelse(psbio$fate_code %in% c("ESC"),1,0)
 psbio$discardednf <- ifelse(substr(psbio$fate_code,1,1)=="D" & psbio$fate_code!="DFR",1,0)
 psbio$finned <- ifelse(psbio$fate_code %in% c("DFR"),1,0)
 
 str(psbio)
 psbio$Individuals <- as.numeric(psbio$Individuals ) ; sum(is.na(psbio$Individuals))
 psbio<- psbio[!is.na(psbio$Individuals),]
 
 dim(psbio)
 psbio$escaped <- psbio$escaped * psbio$Individuals
 psbio$retained <- psbio$retained * psbio$Individuals
 psbio$discardednf <- psbio$discardednf * psbio$Individuals
 psbio$finned <- psbio$finned * psbio$Individuals
  head(psbio)
 
 tdat <- psbio[psbio$sp_code  %in%spec, ]; nrow(tdat)
 
 psres<- cbind(    tapply(tdat$escaped,      tdat$yy, sum)
                   ,tapply(tdat$retained,     tdat$yy, sum)
                   ,tapply(tdat$discardednf,  tdat$yy, sum)
                   ,tapply(tdat$finned,       tdat$yy, sum) )
 psres  <- t(psres)
 colnames(psres) <-c("Escaped","Retained","Discarded","Finned")
 psres
 # hues<-c( gray(1), gray(25/30), gray(11/30), gray(0))                #                 gray(0)=black;    gray(1)=white
 hues <- c('white', 'firebrick1', 'deepskyblue', 'black')
 
 huenames <- c("Escaped","Retained","Discarded","Finned")
 
 
 #-------------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------------
 png(file=paste0(shkdir,"GRAPHICS/fate_PS_obs_regional.png" )) 
 # 
 par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)
  #
 barplot( psres /1000,ylab="",xlab="",names.arg=c(1995:2014),col=hues,ylim=c(0,5.5), las=1)  
 #
 legend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.15,horiz=F,bty="n", ncol=1)
 mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.3,cex=1 )
 
 mtext(side=3,outer=F,"Fate of Observed Sharks - Purse Seine",line=1,cex=1 )                     #WCPO
 mtext(side=1, outer=F, "Year", line=2)
 #
 dev.off()
 table(psbio$region)
 #-------------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------------
 #-------------------------------------------------------------------------------------
 
 
#------------------regionally 
 
# plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Area 1 would be
 
 tdat <- psbio[psbio$sp_code  %in%spec &psbio$region== 3, ]; nrow(tdat)
 
 
 psres<- cbind(    tapply(tdat$escaped,      tdat$yy, sum)
                   ,tapply(tdat$retained,     tdat$yy, sum)
                   ,tapply(tdat$discardednf,  tdat$yy, sum)
                   ,tapply(tdat$finned,       tdat$yy, sum) )
 psres <- t(psres)
 png(file=paste0(shkdir,"GRAPHICS/fate_PS_obs_regional.png" )) 
 # 
 par(mfrow=c(1,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)
 #
 barplot( psres /1000,ylab="",xlab="",names.arg=c(1995:2014),col=hues,ylim=c(0,45), las=1)  
 #
 legend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.15,horiz=F,bty="n", ncol=1)
 mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.3,cex=1 )
 mtext(side=3,outer=F,"Region 3",line=1,cex=1 )                     #WCPO
 mtext(side=1, outer=F, "Year", line=2)
 #
 tdat <- psbio[psbio$sp_code  %in%spec &psbio$region== 4 ,]; nrow(tdat)
 
 
 psres<- cbind(    tapply(tdat$escaped,      tdat$yy, sum)
                   ,tapply(tdat$retained,     tdat$yy, sum)
                   ,tapply(tdat$discardednf,  tdat$yy, sum)
                   ,tapply(tdat$finned,       tdat$yy, sum) )
psres <- t(psres)
 barplot( psres /1000,ylab="",xlab="", col=hues,ylim=c(0,45), las=1)  
 #
  #'egend("topleft",legend=huenames[1:4],fill=hues[1:4],cex=1.15,horiz=F,bty="n", ncol=1)
# mtext(side=2,outer=F,"Number Recorded by Observers (1000s)",line=2.3,cex=1 )
 mtext(side=3,outer=F,"Region 4",line=1,cex=1 )                     #WCPO
 mtext(side=1, outer=F, "Year", line=2)
 
 
 
 dev.off()
 
#----  OCS LL and PS ----------  OCS LL and PS ----------  OCS LL and PS ----------  OCS LL and PS ----------  OCS LL and PS ----------  OCS LL and PS ------

# note the data (esc, disc, retain,  finned) order is different than above.
# spec
 speclongC <-  c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher", "Hammerhead", "Porbeagle" )  
 i<-3 
 hues <- c('white',  'deepskyblue', 'firebrick1','black')
 
 
  tdat <- catch[catch$sp_category==spec[i],]
  reg_res <-  cbind(  tapply(tdat$escaped,      tdat$yy, sum)                    
                     ,tapply(tdat$discardednf,  tdat$yy, sum)
                     ,tapply(tdat$retained,     tdat$yy, sum)
                     ,tapply(tdat$finned,       tdat$yy, sum) )
 reg_res  <- t(reg_res)
 rownames(reg_res) <- c("Escaped","Discarded","Retained","Finned")
 #--------------------------------------------------------------------------------------------
# calc purse sein data 
 tdat <- psbio[psbio$sp_code =="OCS", ]; nrow(tdat)
 
 psres<- cbind(    tapply(tdat$escaped,      tdat$yy, sum)
                  ,tapply(tdat$discardednf,  tdat$yy, sum)
                  ,tapply(tdat$retained,     tdat$yy, sum)
                  ,tapply(tdat$finned,       tdat$yy, sum) )
 psres <- t(psres)
 rownames(psres)<- c("Escaped","Discarded","Retained","Finned")
 psres <- cbind(psres ,  c(NA,NA,NA,NA))
 colnames(psres) <- 1995:2014
 #######################make the figure
 
 
  png( file=paste0(shkdir,"GRAPHICS/fate_ll_ps_obs_", spec[i], ".png" ) ) 
 #
 layout( matrix(c(1,2,3),3,1,byrow=TRUE), widths=c(4,4,4), heights=c( 4,4,1))
  #par(mfrow=c(2,1) )
  par(  mar = figmar, mai=  figmai)
       
       
 par(mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)  
  barplot(prop.table( reg_res,2)  ,ylab="",xlab="", col=hues,  las=1)  
   #mtext(side=2,outer=F,"Number Recorded by Observers",line=2.3,cex=1, las=0 )
   # mtext(side=3,outer=F,paste0("Fate of Observed ", speclongC[i] ," Sharks \nLongline"),line=1,cex=1 )                    
    mtext(side=3,outer=F,paste0("Longline"),line=1,cex=1 )   
   # mtext(side=1, outer=F, "Year", line=2)

 #par(mar=c(2.5,2,0,1),omi=c(0.5,0.5,0.05,0.15),ask=FALSE)  
 barplot(prop.table( psres,2)  ,ylab="",xlab="", col=hues,  las=1)  
 mtext(side=3,outer=F,paste0("Purse Seine"),line=.7,cex=1 )   
# text(side=1, outer=F, "Year", line=1.5)
 mtext(side=2,outer=T,"Proportion Observed",line=.8 ,cex=1.5, las=0 )
 par(mar=legmar)
 plot.new()
 legend("center", legend= rownames(reg_res),fill=hues[1:4],cex=1.4,horiz=T,bty="n", ncol=1, xpd=T)

 #
 # 
 mtext(side=3,outer=T,paste0("Oceanic Whitetip Fate"),line=1,cex=1.5 )   
 dev.off()
 #______________________________________________________________________________________________________________
 #
 #__________________single panel comparing the fate and condion in LL and PS, all Key Species___________________
 #
 #______________________________________________________________________________________________________________
 speclongC <-  c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher", "Hammerhead", "Porbeagle" )  
 i<-3 
 hues <- c('white',  'deepskyblue', 'firebrick1','black')
 
 
 tdat <- catch[catch$sp_category%in%spec,] ; nrow(tdat)
 reg_res <-  cbind(  tapply(tdat$escaped,      tdat$yy, sum)                    
                     ,tapply(tdat$discardednf,  tdat$yy, sum)
                     ,tapply(tdat$retained,     tdat$yy, sum)
                     ,tapply(tdat$finned,       tdat$yy, sum) )
 reg_res  <- t(reg_res)
 rownames(reg_res) <- c("Escaped","Discarded","Retained","Finned")
 #--------------------------------------------------------------------------------------------
 # calc purse sein data 
 tdat <- psbio[psbio$sp_code%in%spec, ]; nrow(tdat)
 
 psres<- cbind(    tapply(tdat$escaped,      tdat$yy, sum)
                   ,tapply(tdat$discardednf,  tdat$yy, sum)
                   ,tapply(tdat$retained,     tdat$yy, sum)
                   ,tapply(tdat$finned,       tdat$yy, sum) )
 psres <- t(psres)
 rownames(psres)<- c("Escaped","Discarded","Retained","Finned")
# psres <- cbind(psres ,  c(NA,NA,NA,NA))
#colnames(psres) <- 1995:2014
 #######################make the figure
 
 
  png( file=paste0(shkdir,"GRAPHICS/fate_ll_ps_obs_all_keysharks_wcpo.png" ) ) 
 #
 layout( matrix(c(1,2,3),3,1,byrow=TRUE), widths=c(4,4,4), heights=c( 4,4,1))
 #par(mfrow=c(2,1) )
 par(  mar = figmar, mai=  figmai)
 
 
 par(mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.15),ask=FALSE)  
 barplot(prop.table( reg_res,2)  ,ylab="",xlab="", col=hues,  las=1)  
 mtext(side=3,outer=F,paste0("Longline"),line=.7,cex=1 )   
 # mtext(side=1, outer=F, "Year", line=2)
 
 #par(mar=c(2.5,2,0,1),omi=c(0.5,0.5,0.05,0.15),ask=FALSE)  
 barplot(prop.table( psres,2)  ,ylab="",xlab="", col=hues,  las=1)  
 mtext(side=3,outer=F,paste0("Purse Seine"),line=.7,cex=1 )   
 # text(side=1, outer=F, "Year", line=1.5)
 mtext(side=2,outer=T,"Proportion Observed",line=.8 ,cex=1.5, las=0 )
 par(mar=legmar)
 plot.new()
 legend("center", legend= rownames(reg_res),fill=hues[1:4],cex=1.4,horiz=T,bty="n", ncol=1, xpd=T)
 
 #
 # 
# mtext(side=3,outer=T,paste0("Oceanic Whitetip Fate"),line=1,cex=1.5 )   
 dev.off()
 #______________________________________________________________________________________________________________
 #
 #_________________ ~~  ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~~ ~ ~ ~  ~               ~~~~~~~~~~~~~~~~~~~~______________
 #
 #______________________________________________________________________________________________________________
 
 # note that in E:\NOUMEA\H_drive_JoelR$ (loche)\SC8_shark assessments\Ind_Shk_Paper\ObsDta\SharkLLObsBio.Rdata there were more escaped sharks....
 # but not in the file  E:\NOUMEA\H_drive_JoelR$ (loche)\SC8_shark assessments\SC7_shark work\DATA_cleanSharkLLObsBio_convFL.Rdata
 # check
 #    with(shkbio[!shkbio$group=='error',], table(yy, fate_id) )
 #
 
#   par(mar = figmar/1.5) 
#   plot.new()
#   plot.new()
#       for (i in c(3:4)) {   #loop over areas
#           barplot(psfate[[i]]/1000,ylab="",xlab="",names.arg=c(1995:2010),col=hues,ylim=c(0,2500)/1000, las=1)   
#           mtext(side=3,paste("Region ", as.character(i)),line=-0.2, cex=0.8)
#           if(i%in%c( 4)){   mtext(side=2,outer=F,"Number Recorded by Observers (Thousands)     ",line=2.25,cex=1 )}
#             mtext(side=1, outer=F, "Year",cex=0.8, line=2)
#           }
#            
#   plot.new()
#   plot.new()          
#           #
#           par(mai=legmar)  
#         plot.new()
#           legend("bottom",legend=huenames[1:4],fill=hues[1:4],cex=1.5,horiz=F,bty="n", ncol=2)    #bty="n" (no box);
# 
#            mtext("Fate by Region, Purse Seine", side=3,outer=F, line=28.5) 
# 
#        
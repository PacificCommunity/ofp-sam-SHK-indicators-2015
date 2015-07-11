#
#   Figure 4.  Median length (in fork length) for males and females (longline fishery only; both sexes combined in the purse seine fishery) for oceanic whitetip and silky sharks in Region 3, 1995-2009. The 5th and 95th percentiles of the data are shown with dashed lines.  Size at maturity is represented by the solid horizontal line.  Years for which n ? 20 are omitted.  The total sample size is shown in the inset to each plot.  Slopes are significant (p ? 0.05) in each case except for the male oceanic whitetip sharks in the longline fishery.  
#
#

  
rm(list = ls(all = TRUE))
gc()

memory.limit(size=NA)
memory.limit(size=4000)
memory.limit(size=NA)


load("P:\\WCPFC Shark\\SharkLLObsBio.RData")        #object name is shkbio; note that the length processing is done here, not in the data processing file


#remove all zero lengths (reduces the dataset from 362,074 to 152,635 records)
shkbio <- shkbio[shkbio$len>0,]     


#use TL_flag to flag and screen those TL which are outside the range measured to create the conversion factor (and thus won't convert credibility)(removes 669 records) 
#note:  don't have such a range for OWT so no OWT are screened (these are Kohler, Casey and Turner (1996) data--see Length Conversion Factors.xslx)
shkbio$TL_flag1 <- rep("N",length(shkbio$yy))
shkbio$TL_flag <- ifelse((shkbio$sp_id=="BSH" & shkbio$len_id=="TL" & (shkbio$len<64|shkbio$len>337)) |
                             (shkbio$sp_id %in% c("SMA","MAK","LMA") & shkbio$len_id=="TL" & (shkbio$len<70|shkbio$len>368)) |
                              (shkbio$sp_id=="FAL" & shkbio$len_id=="TL" & (shkbio$len<90|shkbio$len>258)) |
                               (shkbio$sp_id %in% c("ALV","BTH","PTH","THR") & shkbio$len_id=="TL" & (shkbio$len<155|shkbio$len>450)),"N","Y")
shkbio<-shkbio[shkbio$TL_flag=="Y",]

  
#convert TLs to UFs (all "dodgy" TLs have already been removed(using field TL_flag)); here OWT conversion factor comes from FishBase; BSH and MAK are from Francis and Duffy (2005); THR is BTH
 sharkgroup=c("Blue","Mako","OWT","Silky","Thresher")
 afact=c(0.831,0.911,0.822,0.8388,0.5598)
 bfact=c(1.39,0.821,0,-2.651,17.666)
 shkbio$convFL <- rep(NA,length(shkbio$obstrip_id))
     for (i in c(1:5)) {                             
       shkbio$convFL <- ifelse(shkbio$len_id=="TL" & shkbio$group==sharkgroup[i], afact[i]*shkbio$len+bfact[i],shkbio$convFL)       
     } 
 shkbio$convFL <- ifelse(shkbio$len_id=="UF", shkbio$len,shkbio$convFL)  

#use len_flag1 to screen based on whether each observation is above the smallest known free-swimming size for each species   (doesn't screen any)
shkbio$len_flag1 <- rep("N",length(shkbio$yy))
shkbio$len_flag1 <- ifelse((shkbio$sp_id=="BSH" & shkbio$len>34)|(shkbio$sp_id=="SMA" & shkbio$len>62)| 
                              (shkbio$sp_id=="MAK" & shkbio$len>62)|(shkbio$sp_id=="LMA" & shkbio$len>122)|
                              (shkbio$sp_id=="FAL" & shkbio$len>63)|(shkbio$sp_id=="OCS" & shkbio$len>65)|
                              (shkbio$sp_id=="ALV" & shkbio$len>116)|(shkbio$sp_id=="BTH" & shkbio$len>129)|
                              (shkbio$sp_id=="PTH" & shkbio$len>136)|(shkbio$sp_id=="THR" & shkbio$len>116),"Y","N")
shkbio$len_flag1 <- rep("Y",length(shkbio$obstrip_id))

#screen out all records for which each length observation is below the low end of the range of size at birth for pelagic sharks (SOTOO p. 38)(screens 306 recs, n=151,660)
shkbio <- shkbio[shkbio$len>34,]

#remove those sharks which are not key species
shkbio <- shkbio[shkbio$group!="error",]           #(removes 22542 records, n=129118)

#data for size at maturity
 sizeMatFN=c(168,275,144,173,203)                      #size in FL (UF), for females in the North Pacific:  blue, mako, OWT, silky, thresher
 sizeMatFS=c(168,275,144,173,203)                      #females in the South Pacific                         
 sizeMatMN=c(168,180,138,175,168)                      #males in the North Pacific
 sizeMatMS=c(168,180,138,175,168)                      #males in the South Pacific

               
#add a field for northern/southern hemisphere
shkbio$hemi <- rep(0,length(shkbio$yy))
shkbio$hemi <- ifelse(shkbio$lat1 > 0,"N","S")
 
 
      head(shkbio)
 LL_len_lst<-list()
   
 for(i in c("OCS", "FAL")){
 for(j in c("F","M")){
   aaa<-shkbio[shkbio$sp_id==i & shkbio$sex_id%in%c(j) &shkbio$region==3, ]
   lenout<- with(aaa, tapply(convFL, list(yy ), quantile,probs=c(0.05, 0.5, 0.95), na.rm=T, simplify=T))
   LL_len_lst[[paste(i, "_", j, sep='')]]<- do.call(rbind, lenout)
   }}
 LL_len_lst  

 with(shkbio[shkbio$sp_id=="OCS" &shkbio$sex_id==2 &shkbio$region==3, ]  ,tapply(yy, list(mm),length))
 
 
  head(shkbio)
  par(mfrow=c(2,2)) ;  for(i in 1:4){  matplot(rownames(LL_len_lst[[i]]),LL_len_lst[[i]], pch=16,lty=c(2,1,2) ,col=1, ylim=c(50,275), type='b', lwd=c(1,2,1),xlab='Year', ylab='Fork Length (cm)', main=names(LL_len_lst)[i])} 
    
#COLORS as of 20 September
 hues=c("royalblue","gray","red","mediumspringgreen","sienna")
 huenames=c("Blue","Mako","OWT","Silky","Thresher")
 huecodes=c("BSH","MAK","OCS","FAL","THR")
 

      legsamsizeM <- list()      #sample size for males
      legsamsizeF <- list()    #sample size for females
      medUFM <- list()        #median upper fork length for males
      medUFF <- list()        #median upper fork length for females 
      upperUFM <- list()
      upperUFF <- list()
      lowerUFM <- list()
      lowerUFF <- list()                                           
  
      a<-c(-2,rep(-1,14))    #this builds a matrix showing the records which are dummies and need to be subtracted off sample size  Years=15
      b<-c(-1,rep(0,14))      #median calculation doesn't need this because the dummies hold NAs
      dummies<-cbind(a,b,b,b,b)
      gender=c("M","F")

#note to the next user of this code:
#I know the dummies method is awkward and I've replaced it in other script with match() but if its still here its because I 
#couldn't justify the time to re-work it using match(), given that it works perfectly well the way it is
  
            windows(record = TRUE) 
#once all the necessary fields are created in shkbio, then subset:

shkbio1<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==1,]
shkbio2<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==2,]
shkbio3<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==3,]
shkbio4<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==4,]
shkbio5<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==5,]
shkbio6<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==6,]
    #writes each region into its own dataset (note that region 1 has no data)   
 
 #Note:  if you want to run with just "UF" use $len_code=="UF" in the creation of lenUF and then use tapply on field len 
 #if you want to run with real and converted "UF"s then use len_code of UF or TL to subset into lenUF, and use tapply on field convFL
 
#Analyze                      # huenames=c("Blue","Mako","OWT","Silky","Thresher")

                                                                                     
  for (s in c(1:2)) {               #two sexes                                        s<-1
     for (i in c(1:6)) {                             # six regions                   i<-3
     a <-  get(paste("shkbio",i,sep=""))     #each time through make "a" the name of the source file
     temp <- a[a$sex_id==gender[s],c(65,40,27,50)] # column numbers for convFL,Sex,Year,Group
     dumarray1 <- data.frame(convFL=rep(NA,5),sex_id=rep(gender[s],5),yy=rep(1995,5),group=c("Blue","Mako","OWT","Silky","Thresher"))   
     dumarray2 <- data.frame(convFL=rep(NA,15),sex_id=rep(gender[s],15),yy=c(1995:2009),group=rep("Blue",15))   
     temp <- rbind(temp,dumarray1,dumarray2)      #the dummy method makes sure that medtemp (etc) array has column for every shark and a row for every year
                                                  #there were(are) no F silkies in Region 2 and this resulted in a shorter matrix for Region 2 
     
     medtemp <- tapply(temp$convFL,list(temp$yy,temp$group),median,na.rm=T)
     uppertemp <- tapply(temp$convFL,list(temp$yy,temp$group),quantile,probs=c(0.95),na.rm=T)   
     lowertemp <- tapply(temp$convFL,list(temp$yy,temp$group),quantile,probs=c(0.05),na.rm=T)   
     
     counttemp <- table(temp$yy,temp$group)        #this is the actual sample size in each year by group (but includes the dummies); will always be 15 x 5
     counttemp <- counttemp+dummies                                               #get rid of the dummies:  1:one shark each group for 1995; 2: one blue shark in every year                                                                                                                                                         
     truetemp <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))  #make an empty array of the correct dimensions rows=years, columns=5 (shark groups)
     trueupper <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
     truelower <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
     effsamsize <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))
            for (x in c(1:dim(counttemp)[1])) {
            for (y in c(1:dim(counttemp)[2]))  {
                  truetemp[x,y] <- ifelse(counttemp[x,y]<20,NA,medtemp[x,y]) #loop over each cell of the empty array, assigning the median length if sample size>20
                  trueupper[x,y] <- ifelse(counttemp[x,y]<20,NA,uppertemp[x,y])
                  truelower[x,y] <- ifelse(counttemp[x,y]<20,NA,lowertemp[x,y])
  
                  effsamsize[x,y] <- ifelse(counttemp[x,y]<20,0,counttemp[x,y]) #assign the effective sample size for the legend (i.e. <20 becomes zero)
                  }
     }
     rownames(effsamsize) <- rownames(counttemp)
     colnames(effsamsize) <- colnames(counttemp)
     if (gender[s]=="M") {
                medUFM[[i]] <- truetemp
                upperUFM[[i]] <- trueupper
                lowerUFM[[i]] <- truelower
                legsamsizeM[[i]] <- effsamsize
                } else {   
                medUFF[[i]] <- truetemp
                upperUFF[[i]] <- trueupper
                lowerUFF[[i]] <- truelower
                legsamsizeF[[i]] <- effsamsize           # save the sample size for writing in the legend
                }   
                            
     }
    }
 
 #looped to plot on 6 pages, one for each species with males and females on the same page               huenames=c("Blue","Mako","OWT","Silky","Thresher")
 #Plotting for males 
  
   par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)
  for (S in c(1:5)) { #BIG LOOP HERE FOR SPECIES
  plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Area 1 would be
       for (i in c(2:6)) {   #loop over areas with data drawing males
       
       
          plot(rownames(legsamsizeM[[i]]),medUFM[[i]][,S],ylim=c(50,275),type="o",pch=21,cex=1.1,bg=hues[S],col="black",lwd=2,lty=1,xlim=c(1995,2009),ylab="",xlab="")  #plots one species only
          lines(rownames(legsamsizeM[[i]]),upperUFM[[i]][,S],col=hues[S],lwd=3,lty=3)
          lines(rownames(legsamsizeM[[i]]),lowerUFM[[i]][,S],col=hues[S],lwd=3,lty=3)
          mtext(side=3,paste("Region ", as.character(i)),line=0.3)
          legend("bottomleft",legend=c(paste(huenames[S],"males, n=",ifelse(sum(legsamsizeM[[i]][,huenames[S]])==0,"NA",sum(legsamsizeM[[i]][,huenames[S]])))),col=hues[S],lwd=rep(2,5),cex=0.8)
           if (i<5) {
                abline(h=sizeMatMN[S],col=hues[S])
                } else {   
                abline(h=sizeMatMS[S],col=hues[S])
                }     
        }   
      #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
      mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=2.0)
      mtext(side=3,outer=T,paste(huenames[S]," Males",sep=""),line=1,cex=2.0)  #NEED TITLES ON EACH PAGE FOR MALE AND FEMALE
      #Save to file
      #fileML <- paste("P:\\WCPFC Shark\\Graphical Output\\MedLenSexAreaSppM","-",huenames[S],sep ="")   #Specify the location plots sent to
     # savePlot(filename=fileML, type="png")   
  
  }
  
  malemedfork<-medUFM[[3]][,c(3,4)]
  femalemedfork<-medUFF[[i]][,c(3,4)]    
  
   
  #now plot females 
 par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)
  for (S in c(1:5)) { #BIG LOOP HERE FOR SPECIES
  plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Area 1 would be
       for (i in c(2:6)) {   #loop over areas with data drawing males
          plot(rownames(legsamsizeF[[i]]),medUFF[[i]][,S],ylim=c(50,275),type="o",pch=21,cex=1.1,bg=hues[S],col="black",lwd=2,lty=1,xlim=c(1995,2009),ylab="",xlab="")  #plots one species only
          lines(rownames(legsamsizeF[[i]]),upperUFF[[i]][,S],col=hues[S],lwd=3,lty=3)
          lines(rownames(legsamsizeF[[i]]),lowerUFF[[i]][,S],col=hues[S],lwd=3,lty=3)
          mtext(side=3,paste("Region ", as.character(i)),line=0.3)
          legend("bottomleft",legend=c(paste(huenames[S],"females, n=",ifelse(sum(legsamsizeF[[i]][,huenames[S]])==0,"NA",sum(legsamsizeF[[i]][,huenames[S]])))),col=hues[S],lwd=rep(2,5),cex=0.8)
           if (i<5) {
                abline(h=sizeMatFN[S],col=hues[S])
                } else {   
                abline(h=sizeMatFS[S],col=hues[S])
                }     
        }   
      #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
      mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=2.0)
      mtext(side=3,outer=T,paste(huenames[S]," Females",sep=""),line=1,cex=2.0)  #NEED TITLES ON EACH PAGE FOR MALE AND FEMALE
      #Save to file
      fileML <- paste("P:\\WCPFC Shark\\Graphical Output\\MedLenSexAreaSppF","-",huenames[S],sep ="")   #Specify the location plots sent to
     # savePlot(filename=fileML, type="png")   
  
  }
 
 scc_lens<-list()
 
  scc_lens[['OCS_M']]<-  cbind( "5%"=lowerUFM[[3]][,3],  "50%"=medUFM[[3]][,3], "95%"=upperUFM[[3]][,3]   )
  scc_lens[['OCS_F']]<-  cbind( "5%"=lowerUFF[[3]][,3],  "50%"=medUFF[[3]][,3], "95%"=upperUFF[[3]][,3]   )
    #
  scc_lens[['FAL_M']]<-  cbind( "5%"=lowerUFM[[3]][,4],  "50%"=medUFM[[3]][,4], "95%"=upperUFM[[3]][,4]   )
  scc_lens[['FAL_F']]<-  cbind( "5%"=lowerUFF[[3]][,4],  "50%"=medUFF[[3]][,4], "95%"=upperUFF[[3]][,4]   )

for(i in 1:length(scc_lens)){ rownames( scc_lens[[i]])<- rownames(legsamsizeF[[i]])}
 
    scc_lens[['s_size_F_reg3']]<-  colSums(legsamsizeF[[3]][, ])
    scc_lens[['m_size_F_reg3']]<-  colSums(legsamsizeM[[3]][, ])
         scc_lens
 save(scc_lens,file="H:/SC8_shark assessments/Ind_Shk_Paper/RegressionCoeff_CPUE/ll_forklen.rdata")
 
 #  Purse Seine
 #
 #
 #
 #
 #
 
    
    
     
 
load("P:\\WCPFC shark\\SharkPSObsBioLen.Rdata")  #object name is PSshkbioLen

#COLORS as of 20 September
 hues=c("red","mediumspringgreen")
 huenames=c("OWT","Silky")
 huecodes=c("OCS","FAL")


#VARIOUS DATA SCREENS:
PSshkbioLen<-PSshkbioLen[PSshkbioLen$len>0,]      #first, remove all zero lengths   (leaves only n=1490)
#n=533 silky sharks and n=563 uid sharks and n=260 owt
#decision:  just use silky and owt records
PSshkbioLen <- PSshkbioLen[PSshkbioLen$sp_id %in% c("FAL","OCS"),]
#decision:  only use data from regions 3 and 4
PSshkbioLen <- PSshkbioLen[PSshkbioLen$region %in% c("3","4"),]
#note:  after screening, the only years with n>20 are 1995-1999

#PSshkbioLen$len_flag1<-rep(NA,length(PSshkbioLen$obstrip_id))  
PSshkbioLen$len_flag2<-rep(NA,length(PSshkbioLen$obstrip_id))

#use len_flag1 to screen based on whether each observation is above the smaPSshkbioest known free-swimming size for each species  
#PSshkbioLen$len_flag1<-ifelse((PSshkbioLen$sp_id=="BSH" & PSshkbioLen$len>34)|(PSshkbioLen$sp_id=="SMA" & PSshkbioLen$len>62)| 
#                              (PSshkbioLen$sp_id=="MAK" & PSshkbioLen$len>62)|(PSshkbioLen$sp_id=="LMA" & PSshkbioLen$len>122)|
#                              (PSshkbioLen$sp_id=="FAL" & PSshkbioLen$len>63)|(PSshkbioLen$sp_id=="OCS" & PSshkbioLen$len>65)|
#                              (PSshkbioLen$sp_id=="ALV" & PSshkbioLen$len>116)|(PSshkbioLen$sp_id=="BTH" & PSshkbioLen$len>129)|
#                              (PSshkbioLen$sp_id=="PTH" & PSshkbioLen$len>136)|(PSshkbioLen$sp_id=="THR" & PSshkbioLen$len>116),"Y","N")
#use len_flag2 to screen based on whether each observation is above the low end of the range of size at birth for pelagic sharks (SOTOO p. 38) (flag 74 records as "N")
PSshkbioLen$len_flag2<-ifelse(PSshkbioLen$len>34,"Y","N")
PSshkbioLen <- PSshkbioLen[PSshkbioLen$len_flag2=="Y",]

  
#IMPORTANT NOTE:  NO CONVERSION NECESSARY BECAUSE ALL PURSE SEINE LENGTHS FOR SHARK [SHOULD BE] IN FORK LENGTH  

      legsamsize <- list()      #sample size for males
      medUF <- list()        #median upper fork length for males
      upperUF <- list()
      lowerUF <- list()
  
      a<-c(-2,rep(-1,14))    #this builds a matrix showing the records which are dummies and need to be subtracted off sample size  (1995-2004)
      b<-c(-1,rep(0,14))      #median calculation doesn't need this because the dummies hold NAs
      dummies<-cbind(a,b)
      #gender=c("M","F")

#note to the next user of this code:
#I know the dummies method is awkward and I've replaced it in other scripts with match() but if its still here its because I 
#couldn't justify the time to re-work it using match(), given that it works perfectly well the way it is
  
  
#once all the necessary fields are created in shkbio, then subset:

#shkbio1<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==1,]
#shkbio2<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==2,]
PSshkbioLen3 <- PSshkbioLen[(!is.na(PSshkbioLen$lat1)|!is.na(PSshkbioLen$lon1))& PSshkbioLen$region==3,]
PSshkbioLen4 <- PSshkbioLen[(!is.na(PSshkbioLen$lat1)|!is.na(PSshkbioLen$lon1))& PSshkbioLen$region==4,]
#shkbio5<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==5,]
#shkbio6<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==6,]
    #writes each region into its own dataset (note that region 1 has no data)   
 
 #Note:  if you want to run with just "UF" use $len_code=="UF" in the creation of lenUF and then use tapply on field len 
 #if you want to run with real and converted "UF"s then use len_code of UF or TL to subset into lenUF, and use tapply on field convFL

#data for size at maturity
 sizeMatN=c(144,175)                      #took the bigger of male and female (OCS,FAL)
 sizeMatS=c(144,175)                      #same values for North and South due to lack of data (only OCS and FAL are used anyway)

  

 
#Analyze
 # for (s in c(1:2)) {               #sexes have to be combined as there is no data on sex
     for (i in c(3:4)) {                             # six regions
     a <-  get(paste("PSshkbioLen",i,sep=""))     #each time through make "a" the name of the source file
     temp <- a[,c(5,31,42)] # column numbers for len,yy,group
     dumarray1 <- data.frame(len=rep(NA,2),yy=rep(1995,2),group=c("OWT","Silky"))   
     dumarray2 <- data.frame(len=rep(NA,15),yy=c(1995:2009),group=rep("OWT",15))   
     temp <- rbind(temp,dumarray1,dumarray2)      #the dummy method makes sure that the medtemp (etc) array has column for every shark and a row for every year
                                               #there were(are) no F silkies in Area 2 and this results in a shorter matrix which messes up the legend
     
     medtemp <- tapply(temp$len,list(temp$yy,temp$group),median,na.rm=T)
     uppertemp <- tapply(temp$len,list(temp$yy,temp$group),quantile,probs=c(0.95),na.rm=T)   
     lowertemp <- tapply(temp$len,list(temp$yy,temp$group),quantile,probs=c(0.05),na.rm=T)   
     
     counttemp <- table(temp$yy,temp$group)        #this is the actual sample size in each year by group (but includes the dummies); will always be 15 x 5
     counttemp <- counttemp+dummies                                               #get rid of the dummies:  1:one shark each group for 1995; 2: one blue shark in every year                                                                                                                                                         
     truetemp <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))  #make an empty array of the correct dimensions rows=years, columns=5 (shark groups)
     trueupper <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
     truelower <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
     effsamsize <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))
            for (x in c(1:dim(counttemp)[1])) {
            for (y in c(1:dim(counttemp)[2]))  {
                  truetemp[x,y] <- ifelse(counttemp[x,y]<20,NA,medtemp[x,y]) #loop over each cell of the empty array, assigning the median length if sample size>20
                  trueupper[x,y] <- ifelse(counttemp[x,y]<20,NA,uppertemp[x,y])
                  truelower[x,y] <- ifelse(counttemp[x,y]<20,NA,lowertemp[x,y])
  
                  effsamsize[x,y] <- ifelse(counttemp[x,y]<20,0,counttemp[x,y]) #assign the effective sample size for the legend (i.e. <20 becomes zero)
                  }
     }
     rownames(effsamsize) <- rownames(counttemp)
     colnames(effsamsize) <- colnames(counttemp)
    
                medUF[[i]] <- truetemp
                upperUF[[i]] <- trueupper
                lowerUF[[i]] <- truelower
                legsamsize[[i]] <- effsamsize
  
                            
     }
#    }
 
 #Plotting for both sexes
  layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE),widths=c(2,2), heights=c(1.5,3,1.5))
  #layout.show(dc)
  #par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)
  par(mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)

  for (S in c(1:2)) { #BIG LOOP HERE FOR SPECIES  (OCS=1 and FAL=2) (not 1:5)
        plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Region 1 would be
        plot(1, ann = FALSE, axes = FALSE, type = "n")  # blank for Region 2

        for (i in c(3:4)) {   #loop over areas with data  (Regions 3 and 4 only, not 1:6)
          
          plot(rownames(legsamsize[[i]]),medUF[[i]][,S],ylim=c(50,250),type="o",pch=21,cex=1.1,bg=hues[S],col="black",lwd=2,lty=1,xlim=c(1995,2009),ylab="",xlab="")  #plots one species only
          lines(rownames(legsamsize[[i]]),upperUF[[i]][,S],col=hues[S],lwd=3,lty=3)
          lines(rownames(legsamsize[[i]]),lowerUF[[i]][,S],col=hues[S],lwd=3,lty=3)
          mtext(side=3,paste("Region ", as.character(i)),line=0.3)
          legend("topright",legend=c(paste(huenames[S],"n=",ifelse(sum(legsamsize[[i]][,huenames[S]])==0,"NA",sum(legsamsize[[i]][,huenames[S]])))),col=hues[S],lwd=rep(2,5),cex=0.8)
           if (i<5) {
                abline(h=sizeMatN[S],col=hues[S])
                } else {   
                abline(h=sizeMatS[S],col=hues[S])
                }     
        }   
      #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
      plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Region 5 would be
      plot(1, ann = FALSE, axes = FALSE, type = "n")  # blank for Region 6
      
      mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=2.0)
      mtext(side=3,outer=T,paste(huenames[S]," ",sep=""),line=1,cex=2.0)  #NEED TITLES ON EACH PAGE FOR MALE AND FEMALE
      #Save to file
      fileML <- paste("P:\\WCPFC Shark\\Graphical Output\\PSLengthvsMat","-",huenames[S],sep ="")   #Specify the location plots sent to
    #  savePlot(filename=fileML, type="png")   
  
  }





  
          plot(rownames(legsamsize[[i]]),medUF[[i]][,S],ylim=c(50,250),type="o",pch=21,cex=1.1,bg=hues[S],col="black",lwd=2,lty=1,xlim=c(1995,2009),ylab="",xlab="")  #plots one species only
          lines(rownames(legsamsize[[i]]),upperUF[[i]][,S],col=hues[S],lwd=3,lty=3)
          lines(rownames(legsamsize[[i]]),lowerUF[[i]][,S],col=hues[S],lwd=3,lty=3)



       scc_ps_len<-list()
        scc_ps_len[['OCS_ps_lens']]<-  cbind( "5%"=lowerUF[[3]][,1],  "50%"=medUF[[3]][,1], "95%"=upperUF[[3]][,1]   )
        scc_ps_len[['FAL_ps_lens']]<-  cbind( "5%"=lowerUF[[3]][,2],  "50%"=medUF[[3]][,2], "95%"=upperUF[[3]][,2]   )

         names(sizeMatN)<-c('OCS', 'FAL')
        scc_ps_len[['size_a_mat']]<-  sizeMatN

        scc_ps_len[['samplesize']]<-   colSums(legsamsize[[3]] )


     save(scc_ps_len,file="H:/SC8_shark assessments/Ind_Shk_Paper/RegressionCoeff_CPUE/ps_fork_len.rdata")
 
 ###################################################################################################################
 ###################################################################################################################
 ###################################################################################################################
 ###################################################################################################################
 ###################################################################################################################


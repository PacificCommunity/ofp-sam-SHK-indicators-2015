
#
# read in the biodata, do some minimal processing and make plots of length for SILKY and OCS

psbio <- read.csv("C:/Projects/DATA_2015/PS/ps_obs_bio_shk_june_2015.csv", header=TRUE, stringsAsFactors=FALSE)
head(psbio) ; nrow(psbio) #55432
 
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
nrow(psbio)   #  32039
#

table(psbio$sp_code )

psbio <- psbio[psbio$yy %in% s.yr:e.yr, ]
nrow(psbio) 
psbio<-psbio[psbio$len>0,]      ; nrow(psbio) # 
# #data for size at maturity
#  sizeMatN=c(144,175)                      #took the bigger of male and female (OCS,FAL)
#  sizeMatS=c(144,175)                      #same values for North and South due to lack of data (only OCS and FAL are used anyway)
sizeMat=c(144,175) 
#   
# 

 hues2=c("red","mediumspringgreen")
 ps.spec1=c("Oceanic Whitetip","Silky")
ps.spec=c("OCS","FAL")


#  Subset data and plot
for( i in 1:length(ps.spec)){  # over 2 species
  # do the calc once per shark
    tdat <- psbio[psbio$sp_code==ps.spec[i]  & psbio$region %in% 3:4, ] # nrow(tdat)
    # 
    t_low<- tapply(tdat$len,list(tdat$yy,tdat$region),quantile,probs=c( 0.05 ),na.rm=T, simplify=TRUE) 
    t_med<- tapply(tdat$len,list(tdat$yy,tdat$region),quantile,probs=c( 0.5  ),na.rm=T, simplify=TRUE)   
    t_upp<- tapply(tdat$len,list(tdat$yy,tdat$region),quantile,probs=c( 0.95 ),na.rm=T, simplify=TRUE) 
    #t_med
    png(file=paste0(shkdir,"GRAPHICS/ps_bio_len_",   ps.spec[i], ".png"  )  ) 
    
    par(mfrow=c(1,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=FALSE)
    for( j in 3:4 ){
        plot(as.numeric(as.character(rownames(t_med))),    t_med[,j-2]    ,ylim=c(25,250),type="o",pch=21,cex=1.1,bg=hues2[i],col="black",lwd=2,lty=1,xlim=c(s.yr,e.yr),ylab="",xlab="", las=1)  #plots one species only
        lines(rownames(t_low),  t_low[,j-2], col=hues2[i],lwd=3,lty=3)
        lines(rownames(t_upp),  t_upp[,j-2], col=hues2[i],lwd=3,lty=3)
        
        mtext(side=3,paste("Region ", as.character(j)),line=0.3)
        legend("bottomleft",legend=  c(paste(ps.spec[i], "n=",table(tdat$region)[j-2] )),  col=hues2[i],lwd=rep(2,5),cex=0.8)
        
       abline(h=sizeMat[i], col=hues2[i])
       
      }  
    t_med
    #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
    mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=.75,cex=1.5)
    mtext(side=3,outer=T,paste(ps.spec1[i]," Shark ",sep=""),line=0.5,cex=1.5)   #
    
    dev.off()
  } 
 


# catption
#         Median length (in fork length) for both sexes (combined) of oceanic whitetip and silky sharks in Regions 3 and 4
#         based on samples taken from the purse seine fishery, 1996-2009. The 5th and 95th percentiles of the data are shown
#         with dashed lines. Size at maturity is represented by the solid horizontal line. The sample size is shown in the inset
#         to each plot.

# #VARIOUS DATA SCREENS:
# PSshkbioLen<-PSshkbioLen[PSshkbioLen$len>0,]      #first, remove all zero lengths   (leaves only n=1490)
# #n=533 silky sharks and n=563 uid sharks and n=260 owt
# #decision:  just use silky and owt records
# PSshkbioLen <- PSshkbioLen[PSshkbioLen$sp_id %in% c("FAL","OCS"),]
# #decision:  only use data from regions 3 and 4
# PSshkbioLen <- PSshkbioLen[PSshkbioLen$region %in% c("3","4"),]
# #note:  after screening, the only years with n>20 are 1995-1999
# 
# #PSshkbioLen$len_flag1<-rep(NA,length(PSshkbioLen$obstrip_id))  
# PSshkbioLen$len_flag2<-rep(NA,length(PSshkbioLen$obstrip_id))
# 
# #use len_flag1 to screen based on whether each observation is above the smaPSshkbioest known free-swimming size for each species  
# #PSshkbioLen$len_flag1<-ifelse((PSshkbioLen$sp_id=="BSH" & PSshkbioLen$len>34)|(PSshkbioLen$sp_id=="SMA" & PSshkbioLen$len>62)| 
# #                              (PSshkbioLen$sp_id=="MAK" & PSshkbioLen$len>62)|(PSshkbioLen$sp_id=="LMA" & PSshkbioLen$len>122)|
# #                              (PSshkbioLen$sp_id=="FAL" & PSshkbioLen$len>63)|(PSshkbioLen$sp_id=="OCS" & PSshkbioLen$len>65)|
# #                              (PSshkbioLen$sp_id=="ALV" & PSshkbioLen$len>116)|(PSshkbioLen$sp_id=="BTH" & PSshkbioLen$len>129)|
# #                              (PSshkbioLen$sp_id=="PTH" & PSshkbioLen$len>136)|(PSshkbioLen$sp_id=="THR" & PSshkbioLen$len>116),"Y","N")
# #use len_flag2 to screen based on whether each observation is above the low end of the range of size at birth for pelagic sharks (SOTOO p. 38) (flag 74 records as "N")
# PSshkbioLen$len_flag2<-ifelse(PSshkbioLen$len>34,"Y","N")
# PSshkbioLen <- PSshkbioLen[PSshkbioLen$len_flag2=="Y",]
# 
#   
# #IMPORTANT NOTE:  NO CONVERSION NECESSARY BECAUSE ALL PURSE SEINE LENGTHS FOR SHARK [SHOULD BE] IN FORK LENGTH  
# 
#       legsamsize <- list()      #sample size for males
#       medUF <- list()        #median upper fork length for males
#       upperUF <- list()
#       lowerUF <- list()
#   
#       a<-c(-2,rep(-1,14))    #this builds a matrix showing the records which are dummies and need to be subtracted off sample size  (1995-2004)
#       b<-c(-1,rep(0,14))      #median calculation doesn't need this because the dummies hold NAs
#       dummies<-cbind(a,b)
#       #gender=c("M","F")
# 
# #note to the next user of this code:
# #I know the dummies method is awkward and I've replaced it in other scripts with match() but if its still here its because I 
# #couldn't justify the time to re-work it using match(), given that it works perfectly well the way it is
#   
#   
# #once all the necessary fields are created in shkbio, then subset:
# 
# #shkbio1<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==1,]
# #shkbio2<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==2,]
# PSshkbioLen3 <- PSshkbioLen[(!is.na(PSshkbioLen$lat1)|!is.na(PSshkbioLen$lon1))& PSshkbioLen$region==3,]
# PSshkbioLen4 <- PSshkbioLen[(!is.na(PSshkbioLen$lat1)|!is.na(PSshkbioLen$lon1))& PSshkbioLen$region==4,]
# #shkbio5<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==5,]
# #shkbio6<-shkbio[(!is.na(shkbio$lat1)|!is.na(shkbio$lon1))& shkbio$region==6,]
#     #writes each region into its own dataset (note that region 1 has no data)   
#  
#  #Note:  if you want to run with just "UF" use $len_code=="UF" in the creation of lenUF and then use tapply on field len 
#  #if you want to run with real and converted "UF"s then use len_code of UF or TL to subset into lenUF, and use tapply on field convFL
# 
# #data for size at maturity
#  sizeMatN=c(144,175)                      #took the bigger of male and female (OCS,FAL)
#  sizeMatS=c(144,175)                      #same values for North and South due to lack of data (only OCS and FAL are used anyway)
# 
#   
# 
#  
# #Analyze
#  # for (s in c(1:2)) {               #sexes have to be combined as there is no data on sex
#      for (i in c(3:4)) {                             # six regions
#      a <-  get(paste("PSshkbioLen",i,sep=""))     #each time through make "a" the name of the source file
#      temp <- a[,c(5,31,42)] # column numbers for len,yy,group
#      dumarray1 <- data.frame(len=rep(NA,2),yy=rep(1995,2),group=c("OWT","Silky"))   
#      dumarray2 <- data.frame(len=rep(NA,15),yy=c(1995:2009),group=rep("OWT",15))   
#      temp <- rbind(temp,dumarray1,dumarray2)      #the dummy method makes sure that the medtemp (etc) array has column for every shark and a row for every year
#                                                #there were(are) no F silkies in Area 2 and this results in a shorter matrix which messes up the legend
#      
#      medtemp <- tapply(temp$len,list(temp$yy,temp$group),median,na.rm=T)
#      uppertemp <- tapply(temp$len,list(temp$yy,temp$group),quantile,probs=c(0.95),na.rm=T)   
#      lowertemp <- tapply(temp$len,list(temp$yy,temp$group),quantile,probs=c(0.05),na.rm=T)   
#      
#      counttemp <- table(temp$yy,temp$group)        #this is the actual sample size in each year by group (but includes the dummies); will always be 15 x 5
#      counttemp <- counttemp+dummies                                               #get rid of the dummies:  1:one shark each group for 1995; 2: one blue shark in every year                                                                                                                                                         
#      truetemp <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))  #make an empty array of the correct dimensions rows=years, columns=5 (shark groups)
#      trueupper <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
#      truelower <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))
#      effsamsize <- array(0,c(dim(counttemp)[1],dim(counttemp)[2]))#dim(countlenUF)[2]))
#             for (x in c(1:dim(counttemp)[1])) {
#             for (y in c(1:dim(counttemp)[2]))  {
#                   truetemp[x,y] <- ifelse(counttemp[x,y]<20,NA,medtemp[x,y]) #loop over each cell of the empty array, assigning the median length if sample size>20
#                   trueupper[x,y] <- ifelse(counttemp[x,y]<20,NA,uppertemp[x,y])
#                   truelower[x,y] <- ifelse(counttemp[x,y]<20,NA,lowertemp[x,y])
#   
#                   effsamsize[x,y] <- ifelse(counttemp[x,y]<20,0,counttemp[x,y]) #assign the effective sample size for the legend (i.e. <20 becomes zero)
#                   }
#      }
#      rownames(effsamsize) <- rownames(counttemp)
#      colnames(effsamsize) <- colnames(counttemp)
#     
#                 medUF[[i]] <- truetemp
#                 upperUF[[i]] <- trueupper
#                 lowerUF[[i]] <- truelower
#                 legsamsize[[i]] <- effsamsize
#   
#                             
#      }
# #    }
#  
#  #Plotting for both sexes
#   layout(matrix(c(1,2,3,4,5,6),3,2,byrow=TRUE),widths=c(2,2), heights=c(1.5,3,1.5))
#   #layout.show(dc)
#   #par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)
#   par(mar=c(2.5,2,2,1),omi=c(0.5,0.5,0.5,0.5),ask=TRUE)
# 
#   for (S in c(1:2)) { #BIG LOOP HERE FOR SPECIES  (OCS=1 and FAL=2) (not 1:5)
#         plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Region 1 would be
#         plot(1, ann = FALSE, axes = FALSE, type = "n")  # blank for Region 2
# 
#         for (i in c(3:4)) {   #loop over areas with data  (Regions 3 and 4 only, not 1:6)
#           
#           plot(rownames(legsamsize[[i]]),medUF[[i]][,S],ylim=c(50,250),type="o",pch=21,cex=1.1,bg=hues[S],col="black",lwd=2,lty=1,xlim=c(1995,2009),ylab="",xlab="")  #plots one species only
#           lines(rownames(legsamsize[[i]]),upperUF[[i]][,S],col=hues[S],lwd=3,lty=3)
#           lines(rownames(legsamsize[[i]]),lowerUF[[i]][,S],col=hues[S],lwd=3,lty=3)
#           mtext(side=3,paste("Region ", as.character(i)),line=0.3)
#           legend("topright",legend=c(paste(huenames[S],"n=",ifelse(sum(legsamsize[[i]][,huenames[S]])==0,"NA",sum(legsamsize[[i]][,huenames[S]])))),col=hues[S],lwd=rep(2,5),cex=0.8)
#            if (i<5) {
#                 abline(h=sizeMatN[S],col=hues[S])
#                 } else {   
#                 abline(h=sizeMatS[S],col=hues[S])
#                 }     
#         }   
#       #mtext(side=1,outer=T,"Year",line=1,cex=2.0)
#       plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Region 5 would be
#       plot(1, ann = FALSE, axes = FALSE, type = "n")  # blank for Region 6
#       
#       mtext(side=2,outer=T,"Median Upper Jaw-Fork Length",line=1,cex=2.0)
#       mtext(side=3,outer=T,paste(huenames[S]," ",sep=""),line=1,cex=2.0)  #NEED TITLES ON EACH PAGE FOR MALE AND FEMALE
#       #Save to file
#       fileML <- paste("P:\\WCPFC Shark\\Graphical Output\\PSLengthvsMat","-",huenames[S],sep ="")   #Specify the location plots sent to
#       savePlot(filename=fileML, type="png")   
#   
#   }

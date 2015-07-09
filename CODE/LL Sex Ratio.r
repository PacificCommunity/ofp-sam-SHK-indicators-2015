
#  Updated from the old shk indicators work
#
#
#

# source the reg info
source("C:/Projects/SHK-indicators-2015/CODE/ind_analysis_preamble.r")
#
#object name is shkbio; note that the length processing is done here, not in the data processing file
load(file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )
dim(shkbio)


#remove those sharks which are not key species
shkbio <- shkbio[shkbio$group!="error",]           # 115121 -98906  (removes 16215 )


sexid<- c("Male", "Female")
group_names <-c("Blue", "Mako", "Oceanic Whitetip", "Silky", "Thresher") 

SharkBio1 <- shkbio[shkbio$region==1,]
SharkBio2 <- shkbio[shkbio$region==2,]
SharkBio3 <- shkbio[shkbio$region==3,]
SharkBio4 <- shkbio[shkbio$region==4,]
SharkBio5 <- shkbio[shkbio$region==5,]
SharkBio6 <- shkbio[shkbio$region==6,]
    #writes each region into its own dataset (note that region 1 has no data) 

 

#ANALYZE
  sexratio<-list()                        #the only matrix that is output repeatedly is this one, so it needs to be a list
  samplesize<-list()
  for (i in c(2:6)) {                               #don't run for Region 1 as there are not enough data
     a <-  get(paste("SharkBio",i,sep=""))          #each time through make "a" the name of the source file
     sexed <- a[a$sex_id %in% c("M","F"),]          #writes a subset of the key spp. in Area i which are male or female
     sexedmat <- table(sexed$yy,sexed$group)        #writes a matrix of these (males + females) by year and group (e.g. makos)   (sample size)
     sexedmat[sexedmat<50] <- 0                     #assign a zero to any cell in the matrix which is less than 50 (WILL THIS BE A PROBLEM WHEN DIVIDING BY ZERO??)
     samplesize[[i]] <- sexedmat                    #save the sample size for writing in the legend
     sexedF <- a[a$sex_id=="F",]                       #writes a subset of the key spp in Area i which are female
     sexedmatF <- table(sexedF$yy,sexedF$group)     #writes a matrix of these females by year and group
     sexratio[[i]] <- sexedmatF/sexedmat            #writes a matrix of females/(males+females) by year and group
     }

#PLOT
png(file=paste(shkdir,"GRAPHICS/LLSexRatio.png", sep="" ) )

  par(mfrow=c(3,2),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0,0))
  plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space where Region 1 would be
      for (i in c(2:6)) {   #loop over regions 
          plot(rownames(sexratio[[i]]),sexratio[[i]][,1],ylim=c(0,1),type="o",pch=19,lwd=2,col=hues[1],lty=1,xlim=c(s.yr,e.yr),ylab="",xlab="", las=1)
           for (j in c(2:5)) {   #draw lines for makos,OWT,silky and thresher
             lines(rownames(sexratio[[i]]),sexratio[[i]][,j],col=hues[j],lwd=2,lty=1,type="o",pch=19)
             abline(h=0.5,lwd=2,col="black")
             mtext(side=3,paste("Region ", as.character(i)),line=0.3)
             legend("bottomright",legend=c(paste("Blue n=",sum(samplesize[[i]][,1])),paste("Makos n=",sum(samplesize[[i]][,2])),paste("OWT n=",sum(samplesize[[i]][,3])),paste("Silky n=",sum(samplesize[[i]][,4])),paste("Threshers n=",sum(samplesize[[i]][,5]))),col=hues[1:5],lwd=rep(2,5),cex=0.6)
             
              }   
          } 
          
          
          mtext(side=2,outer=T,"Percent Female",line=1,cex=1.5)
   
 dev.off()  
                                                                                                          }
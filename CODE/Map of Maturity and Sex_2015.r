#updated (conversion factors and age at maturity; and corrected (some calculation errors) on 14 June 2011
#need to modify by hand for end year or mid year

#rm(list = ls(all = TRUE))
#gc()

memory.limit(size=NA)
memory.limit(size=4000)
memory.limit(size=NA)


#load("P:\\WCPFC Shark\\SharkLLObsBio.RData")        #object name is shkbio; note that the length processing is done here, not in the data processing file
#source("http://www.phaget4.org/R/myImagePlot.R") 
load("C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata")  
shkbio <- catch
# head(shkbio)
# #remove all zero lengths (reduces the dataset from 422472 to 159057 records)
# shkbio<-shkbio[shkbio$len>0,]      
# shkbio$TL_flag<-rep("Y",length(shkbio$obstrip_id))
# 
# 
# #screen out all records for which each length observation is below the low end of the range of size at birth for pelagic sharks (SOTOO p. 38) 
# shkbio<-shkbio[shkbio$len>34,]
# 
# #use TL_flag to flag and screen those TL which are outside the range measured to create the conversion factor (and thus won't convert credibility) 
# #note:  don't have such a range for OWT so no OWT are screened (these are Kohler, Casey and Turner (1996) data--see Length Conversion Factors.xslx)
# shkbio$TL_flag <- ifelse((shkbio$sp_id=="BSH" & shkbio$len_id=="TL" & (shkbio$len<64|shkbio$len>337)) |
#                              (shkbio$sp_id %in% c("SMA","MAK","LMA") & shkbio$len_id=="TL" & (shkbio$len<70|shkbio$len>368)) |
#                               (shkbio$sp_id=="FAL" & shkbio$len_id=="TL" & (shkbio$len<90|shkbio$len>258)) |
#                                (shkbio$sp_id %in% c("ALV","BTH","PTH","THR") & shkbio$len_id=="TL" & (shkbio$len<155|shkbio$len>450)),"N","Y")
# shkbio<-shkbio[shkbio$TL_flag=="Y",]


  
#convert TLs to UFs (all "dodgy" TLs have already been removed(using field TL_flag)); here OWT conversion factor comes from FishBase; BSH and MAK are from Francis and Duffy (2005); THR is BTH
#  sharkgroup=c("Blue","Mako","OWT","Silky","Thresher")
#  afact=c(0.8313,0.911,0.822,0.8388,0.5598)
#  bfact=c(1.39,0.821,0,-2.651,17.666)
#  shkbio$convFL <- rep(NA,length(shkbio$obstrip_id))
#      for (i in c(1:5)) {                             
#        shkbio$convFL <- ifelse(shkbio$len_id=="TL" & shkbio$group==sharkgroup[i], afact[i]*shkbio$len+bfact[i],shkbio$convFL)       
#      } 
#  shkbio$convFL <- ifelse(shkbio$len_id=="UF", shkbio$len,shkbio$convFL)       
 
sharkgroup<- c("BSH","MAK","OCS","FAL","THR","HHD", "POR")
#add field for Maturity and Sex:  AF,AM,JF,JM (adult/juvenile; male/female)
#key is ordered blueF,blueM,makoF,makoM, etc.   (not enough data to justify a North Pacific-South Pacific split
groupv <- c(rep(sharkgroup[1],4),rep(sharkgroup[2],4),rep(sharkgroup[3],4),rep(sharkgroup[4],4),rep(sharkgroup[5],4),rep(sharkgroup[6],4),rep(sharkgroup[7],4))
shkkey <- c(rep(sharkgroup[1],2),rep(sharkgroup[2],2),rep(sharkgroup[3],2),rep(sharkgroup[4],2),rep(sharkgroup[5],2),rep(sharkgroup[6],2),rep(sharkgroup[7],2))
#hemiv<-rep(c(rep("N",2),rep("S",2)),5)
sexv<- rep(c("F","M"),7)
matv<-c(168,168,275,180,144,138,173,175,203,168, 210,198,175,145)      #data in fork length
look<-paste(shkkey,sexv,sep="")
key<-as.data.frame(cbind(look,matv))

shkbio$group <- shkbio$sp_category
shkbio$sex <- shkbio$sex_code


#add field for date of set  -->assigned to a quarter
shkbio$period<-ifelse(shkbio$mm %in% c("11","12","1"),"BE",ifelse(shkbio$mm %in% c("5","6","7"),"MY","OT"))

shkbio$matlen<-rep(0,length(shkbio$obstrip_id))
shkbio$matlen<-as.numeric(as.character(key$matv[match(paste(shkbio$group,shkbio$sex,sep=""),key$look)]))    #read in the length at maturity for each shark
shkbio$matsex <- (ifelse(shkbio$sex=="F" & shkbio$convFL >= shkbio$matlen,"AF",
  ifelse(shkbio$sex=="F" & shkbio$convFL < shkbio$matlen,"JF",
    ifelse(shkbio$sex=="M" & shkbio$convFL >= shkbio$matlen,"AM",
      ifelse(shkbio$sex=="M" & shkbio$convFL <= shkbio$matlen,"JM","NA")))))    #matsex field separates into males and females, adult and juvenile
 
shkbio$lon5 <- floor(shkbio$lon1d/5)*5 +2.5
shkbio$lat5 <- floor(shkbio$lat1d/5)*5 +2.5

longitude<-sort(unique(shkbio$lon5))
latitude<-sort(unique(shkbio$lat5))


################DATA ANALYSIS

#GET NUMBER OF RECORDS IN EACH 5X5 SQUARE FOR EACH MATSEX CODE (CAN'T DO CPUE BECAUSE THIS IS A DATABASE OF OBSERVATIONS (W/ SEX AND LENGTH), NOT A CATCH RECORD
#CAN ADDITIONALLY BE SUBSET BY TIME PERIOD (not implemented in an automatic manner):  BE=Nov-Jan, MY=May-July (change by hand in subset statement below and filename output)
#e.g. MapBE Blue and MapMY Blue
 distmat<-list()
 samplesize<-c(rep(0,28))                                                 
 k<-rep(c("AF","AM","JF","JM"),7)
 
    for (i in c(1:28)) {       #order is Blue AF,AM,JF,JM  then Mako AF,AM,JF,JM, etc.  ==> produces a matrix for each 
       a <- shkbio[shkbio$group==groupv[i] & shkbio$matsex==k[i] & shkbio$period=="BE",]     #subsets for each of 20 categories (TIME PERIOD OPTION: lines 123-181)
       place<-array(0,c(length(longitude),length(latitude)))   #long in rows and lat in columns
       colnames(place)<-as.character(latitude)
       rownames(place)<-as.character(longitude)
       temp<-table(a$lon5,a$lat5)       #counts the number of records in each category (each record equals 1 shark)
       distmat[[i]]<-temp[match(rownames(place),rownames(temp)),match(colnames(place),colnames(temp))]
       distmat[[i]][is.na(distmat[[i]])==T]<-0   # use if you don't care to distinguish between zero and NA 
          #if you leave the NAs there the sum (e.g. sumblue) will be NA unless every life stage is represented (which isn't what you want -and- you will get a lot fewer colored cells)
       samplesize[i]<-sum(unlist(distmat[[i]]),na.rm=TRUE)   #total number of sharks in each of the 20 matrices (note:  before culling)
       }         

       
 #take each matrix of counts and convert to a proportion:  blue AF/blue; blue AM/blue; blue JF/blue and blue JM/blue; cull those <20
    sumblue<-distmat[[1]]+distmat[[2]]+distmat[[3]]+distmat[[4]]
    sumblue<-ifelse(sumblue>20,sumblue,NA)
    summako<-distmat[[5]]+distmat[[6]]+distmat[[7]]+distmat[[8]]
    summako<-ifelse(summako>20,summako,NA)
    sumowt<-distmat[[9]]+distmat[[10]]+distmat[[11]]+distmat[[12]]
    sumowt<-ifelse(sumowt>20,sumowt,NA)
    sumsilky<-distmat[[13]]+distmat[[14]]+distmat[[15]]+distmat[[16]]
    sumsilky<-ifelse(sumsilky>20,sumsilky,NA)
    sumthr<-distmat[[17]]+distmat[[18]]+distmat[[19]]+distmat[[20]]
    sumthr<-ifelse(sumthr>20,sumthr,NA)
    sumhhd<-distmat[[21]]+distmat[[22]]+distmat[[23]]+distmat[[24]]
    sumhhd<-ifelse(sumhhd>20,sumhhd,NA)
    sumpor<-distmat[[25]]+distmat[[26]]+distmat[[27]]+distmat[[28]]
    sumpor<-ifelse(sumpor>20,sumpor,NA)


    final=list()
    for (j in c(1:4)) {
        final[[j]] <- distmat[[j]]/sumblue
        }
    for (j in c(5:8)) {
        final[[j]] <- distmat[[j]]/summako
        }
    for (j in c(9:12)) {
        final[[j]] <- distmat[[j]]/sumowt
        }
    for (j in c(13:16)) {
        final[[j]] <- distmat[[j]]/sumsilky
        }
    for (j in c(17:20)) {
        final[[j]] <- distmat[[j]]/sumthr         #note:  adding the arrays for a given species doesn't give you 1 because of the culling of n<20
        }
    for (j in c(21:24)) {
      final[[j]] <- distmat[[j]]/sumhhd          
    }
    for (j in c(25:28)) {
      final[[j]] <- distmat[[j]]/sumpor         
    }


       int<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0)      #interval for the legend
       
       
####################PLOT
  library(maps)
  library(mapproj)
  library(mapdata)

#eez <-read.table("P:\\WCPFC Shark\\EZNEW2.txt", sep="", header=F)

stage<-rep(c("Adult Female","Adult Male","Juvenile Female", "Juvenile Male"),5)

greenpal<-c("#e5f2f2", "#cce5e5", "#b2d8d8","#99cbcb", "#7fbfbf", "#66b2b2", "#4ca5a5", "#339898",  "#198b8b", "#007f7f")    
  
    for (s in c(1:7)) { #five species on five pages
  
      png(file= paste0(shkdir,"GRAPHICS/Map_maturity_sex_",sharkgroup[s],  ".png")  ) 
      #
      layout(matrix(c(1,2,3,4,5,5),3,2,byrow=TRUE),width=c(50,50),heights=c(40,40,20),respect=TRUE)
      par(mar=c(3,3,3,3),bg="white",ask=FALSE)
 
       
      for (l in c(1:4)) {  #four life stages
        c<-(4*(s-1))+l
        image(longitude,latitude,final[[c]], breaks=int, xlim=c(132.5,227.5), ylim=c(-47.5,42.5),col=greenpal,xlab="lon",ylab="lat") 
          lines(eez[,1], eez[,2], col=1)
          map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
          map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",
                "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), fill=T, add=T, yaxt="n", xaxt="n", col="black")
          mtext(side=3,paste(sharkgroup[s]," ",stage[l]," (n =",samplesize[c],")",sep=""), line=0.50, cex=.7, font=2, col="black", adj=0.5)

          }  
      #legend:
      plot(0,0,xlim=c(0,int[11]),ylim=c(0,1),type="n",xaxt="n", yaxt="n",bty="n",ylab="# of sharks",xlab="")   #bty suppresses the border; ylab/xlab suppresses the legends on the key

        for (i in 1:11) {
          polygon(c(int[i],int[i],int[i+1],int[i+1]),c(0.3,0.5,0.5,0.3),col=greenpal[i])
            }
          text(int,0.15,int,cex=1) 
          
           
        mtext(side=1,"Proportion of Species Total (year-end)",outer=F,cex=0.8,line=1)
        #filename<-paste("P:\\WCPFC Shark\\Graphical Output\\MapBE ",sharkgroup[s],sep="")
        #savePlot(filename,type="png")        
     dev.off()
      }


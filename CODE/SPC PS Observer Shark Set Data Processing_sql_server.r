     #Read and Process SPC Observer Purse Seine Data, 
      # based on this file H:/SC8_shark assessments/SC7_shark work/R_CODE_shark work/DataProcessing/SPC PS Observer Shark Bio Data Processing JRedts.r
      # and data in 
      rm(list=ls(all=TRUE))
      gc()

      memory.limit(size=NA)
      memory.limit(size=4000)
      memory.limit(size=NA)
     # these two based on visual fox pro
     # PSObsShk <- read.csv("P:\\WCPFC Shark\\Updated Data from Colin late April\\ps_obs_set_data.csv", sep=",",header=TRUE,stringsAsFactors=FALSE)  
     # PSObsShk <- read.csv("C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\DATA_logsheets_shark_work\\ps_obs_set_data _V2.csv", sep=",",header=TRUE,stringsAsFactors=FALSE)  
     # based on sql server
     ##    PSObsShk <- read.csv("H:/ISSF_cpue_check/Data/PS_obserdata_wt.csv", header=T)
     # PS2 <- read.table("E:/NOUMEA/H_drive_JoelR$ (loche)/Shark Catch in PNA waters/2014/Data/ps_obsv_shk_2014_11_05_rev2.csv", sep=',', header=T )
      
     # psnames <-   names(PS2)
     # dput(psnames)
      psnames <- c("obstrip_id", "s_day_id", "s_alog_id", "setno", "actdate", 
                   "acttime", "lon_long", "lat_long", "ez_id", "fish_days", "s_act_id", 
                   "sch_id", "winddir", "wind_kts", "sea_id", "det_id", "vesselname", 
                   "flag_id", "gr_id", "obsv_id", "tripno", "o_dep_date", "o_ret_date", 
                   "shark", "sharktarget", "lon1", "lat1", "lon5", "lat5", "yy", 
                   "mm", "day", "qtr", "SKJ", "YFT", "BET", "DGX", "DUS", "WSH", 
                   "CCP", "BRO", "PTH", "ALV", "AML", "CCE", "TIG", "LMA", "STT", 
                   "BTH", "SPN", "BSH", "THR", "CCL", "SMA", "RMB", "MAK", "PLS", 
                   "OCS", "MAN", "FAL", "EAG", "GAG", "GUQ", "HXT", "ISB", "NTC", 
                   "POR", "RAJ", "SCK", "SHF", "SHK", "SHL", "PSK", "CYW", "STI", 
                   "ALS", "CZI", "SSQ", "CCG", "SPL", "BLR", "BAI", "SPZ", "SPK", 
                   "WST", "SRX", "CVX", "DGS", "LMD", "TRB", "RMJ", "BSK", "RMT", 
                   "CYU", "CYO", "SKH", "CCA", "HDQ", "TOE", "RMV", "YSM", "CCU", 
                   "GTF", "CYP", "OSF", "RDR", "RHN", "CNX", "YSA", "RSK", "ODH", 
                   "EUB", "CPS", "SYR", "SKX", "RUZ", "DOP", "DCA", "CCB")
      
      PSObsShk <- read.table("C:/Projects/SHK-indicators-2015/DATA/joel_ps_shk_june_2016.csv", sep=',', header=T )
       
      
      names(PSObsShk) <- psnames
      
      #prelim
 
      PSObsShk$lat1 <- as.numeric(as.character(PSObsShk$lat1)) # suspect
      PSObsShk$lon1 <- as.numeric(as.character(PSObsShk$lon1)) # suspect
            
      PSObsShk$lat5 <- as.numeric(as.character( PSObsShk$lat5 ))
      PSObsShk$lon5 <- as.numeric(as.character( PSObsShk$lon5 ))
      
      PSObsShk$yy <- as.numeric(as.character( PSObsShk$yy ))
      PSObsShk$mm <- as.numeric(as.character( PSObsShk$mm ))
      PSObsShk$day <- as.numeric(as.character( PSObsShk$day ))
      PSObsShk$qtr <- as.numeric(as.character( PSObsShk$qtr ))
      
      PSObsShk$sch_id <- as.numeric(as.character( PSObsShk$sch_id ))
      
      #convert lat/longs
      PSObsShk$newlat <- ifelse(substring(PSObsShk$lat_long,9,9)=="S",(-1*((as.numeric(substr(PSObsShk$lat_long,1,2)))+((as.numeric(substr(PSObsShk$lat_long,3,4)))/60))), ((as.numeric(substr(PSObsShk$lat_long,1,2)))+((as.numeric(substr(PSObsShk$lat_long,3,4)))/60)))
      PSObsShk$newlon <- ifelse(substring(PSObsShk$lon_long,10,10)=="E",(((as.numeric(substr(PSObsShk$lon_long,1,3)))+((as.numeric(substr(PSObsShk$lon_long,4,5)))/60))), (180+(180-(((as.numeric(substr(PSObsShk$lon_long,1,3)))+((as.numeric(substr(PSObsShk$lon_long,4,5)))/60))))))
      
      
      
      
 #1. Create a new region field in shk and trim non WCPFC Statistical Area data:

      PSObsShk$region<- rep(0,length(PSObsShk$obstrip_id))
      PSObsShk$region <- ifelse(PSObsShk$newlat >= 20 & PSObsShk$newlat <= 50 & PSObsShk$newlon >= 120 & PSObsShk$newlon < 180, 1, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= 20 & PSObsShk$newlat <= 50 & PSObsShk$newlon >= 180 & PSObsShk$newlon < 210, 2, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -10 & PSObsShk$newlat < 20 & PSObsShk$newlon >= 120 & PSObsShk$newlon < 170, 3, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -10 & PSObsShk$newlat < 20 & PSObsShk$newlon >= 170 & PSObsShk$newlon < 210, 4, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -10 & PSObsShk$newlat < -4 & PSObsShk$newlon >= 210 & PSObsShk$newlon < 230, 4, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -40 & PSObsShk$newlat < -10 & PSObsShk$newlon >= 141 & PSObsShk$newlon < 170, 5, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -55 & PSObsShk$newlat < -40 & PSObsShk$newlon >= 141 & PSObsShk$newlon < 150, 5, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -60 & PSObsShk$newlat < -40 & PSObsShk$newlon >= 150 & PSObsShk$newlon < 170, 5, PSObsShk$region)
      PSObsShk$region <- ifelse(PSObsShk$newlat >= -60 & PSObsShk$newlat < -10 & PSObsShk$newlon >= 170 & PSObsShk$newlon < 230, 6, PSObsShk$region)
      PSObsShk <- PSObsShk[PSObsShk$region > 0,]
      head(  PSObsShk)
  


# Ensure no NAs in the data
PSObsShk<- PSObsShk[is.na(PSObsShk$lat_long)==FALSE,]
PSObsShk<- PSObsShk[is.na(PSObsShk$lon_long)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$lat1)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$lon1)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$lat5)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$lon5)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$yy)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$mm)==FALSE,]
#PSObsShk <- PSObsShk[is.na(PSObsShk$sst)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$sch_id)==FALSE,]
PSObsShk <- PSObsShk[is.na(PSObsShk$flag_id)==FALSE,]
 dim(PSObsShk)   #62090
#PSObsShk <- PSObsShk[PSObsShk$yy > 1994 ,]        #there are limited observer records of sharks in 1993-1994 (and this is consistent with LL analysis)

#make groups "Blue, Silky, OWT, Mako and Thresher" and compute CPUE
PSObsShk$blue <- PSObsShk$BSH
PSObsShk$mako <- PSObsShk$SMA + PSObsShk$LMA + PSObsShk$MAK
PSObsShk$ocs <- PSObsShk$OCS
PSObsShk$silky <- PSObsShk$FAL
PSObsShk$thresher <- PSObsShk$BTH + PSObsShk$ALV + PSObsShk$PTH + PSObsShk$THR


#WILL NEED TO ADDRESS THE ISSUE AT SOME POINT OF WHAT CODES CONSTITUTE SHARKS
elasmo<-   c("DGX", "DUS", "WSH", "CCP", "BRO", "PTH", "ALV", "AML", "CCE", 
"TIG", "LMA", "STT", "BTH", "SPN", "BSH", "THR", "CCL", "SMA", 
"RMB", "MAK", "PLS", "OCS", "MAN", "FAL", "EAG", "GAG", "GUQ", 
"HXT", "ISB", "NTC", "POR", "RAJ", "SCK", "SHF", "SHK", "SHL", 
"PSK", "CYW", "STI", "ALS", "CZI", "SSQ", "CCG", "SPL", "BLR", 
"BAI", "SPZ", "SPK", "WST", "SRX", "CVX", "DGS", "LMD", "TRB", 
"RMJ", "BSK", "RMT", "CYU", "CYO", "SKH", "CCA", "HDQ", "TOE", 
"RMV", "YSM", "CCU", "GTF", "CYP", "OSF", "RDR", "RHN", "CNX", 
"YSA", "RSK", "ODH", "EUB", "CPS", "SYR", "SKX", "RUZ", "DOP", 
"DCA", "CCB")    #  

# the following codes are not sharks:  BAI,EAG,MAN,PLS,RAJ,RDR,RMB,RMJ,RMT,RMV,RUZ,SHF,SRX,STI,STT,TOE,WST --> remove them
# note that  RHN is a shark but removed (whale sharks...throw it all off)
sharks<-  c("ALS", "ALV", "AML", "BLR", "BRO", "BSH", "BSK", "BTH", "CCA", 
"CCB", "CCE", "CCG", "CCL", "CCP", "CCU", "CNX", "CPS", "CVX", 
"CYO", "CYP", "CYU", "CYW", "CZI", "DCA", "DGS", "DGX", "DOP", 
"DUS", "EUB", "FAL", "GAG", "GTF", "GUQ", "HDQ", "HXT", "ISB", 
"LMA", "LMD", "MAK", "NTC", "OCS", "ODH", "OSF", "POR", "PSK", 
"PTH", "RSK", "SCK", "SHK", "SHL", "SKH", "SKX", "SMA", "SPK", 
"SPL", "SPN", "SPZ", "SSQ", "SYR", "THR", "TIG", "TRB", "WSH", 
"YSA", "YSM")

            head(PSObsShk[,sharks])

 
#PSObsShk <- PSObsShk[,c(1:48,50:55,57,59,61,63:68,70,72:75,77:82,84:85,88:91,93,95:99,102:106,108:116,118:120,122:129)]
#PSObsShk$totshk <- rowSums(PSObsShk[,c(77:122)],na.rm=T)     
PSObsShk$totalshk <- rowSums(PSObsShk[,sharks])
PSObsShk$othershk <- rowSums(PSObsShk[,elasmo]) -rowSums(PSObsShk[,sharks])
 
    dim(PSObsShk)
PSObsShk <- PSObsShk[PSObsShk$blue >= 0,]            #removes negative catch numbers of which there are some
PSObsShk <- PSObsShk[PSObsShk$mako >= 0,]
PSObsShk <- PSObsShk[PSObsShk$ocs >= 0,]
PSObsShk <- PSObsShk[PSObsShk$silky >= 0,]
PSObsShk <- PSObsShk[PSObsShk$thresher >= 0,]
     dim(PSObsShk)
          

#2. Add additional fields and edits


#make a factor which is a combination of lat and lon
#PSObsShk$cell <- paste(round(as.numeric(as.character(PSObsShk$lat5))),round(as.numeric(as.character(PSObsShk$lon5))),sep="")
PSObsShk$cell <- as.character(paste(round(PSObsShk$lat5),round(PSObsShk$lon5),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8
PSObsShk$cell<- ifelse(nchar(PSObsShk$cell)==5 & substr(PSObsShk$cell,1,2)=="-2",paste("-02",substr(PSObsShk$cell,3,5),sep=""),PSObsShk$cell)
PSObsShk$cell<- ifelse(nchar(PSObsShk$cell)==4 & substr(PSObsShk$cell,1,1)=="2",paste("02",substr(PSObsShk$cell,2,4),sep=""),PSObsShk$cell)
PSObsShk$cell<- ifelse(nchar(PSObsShk$cell)==5 & substr(PSObsShk$cell,1,2)=="-8",paste("-08",substr(PSObsShk$cell,3,5),sep=""),PSObsShk$cell)
PSObsShk$cell<- ifelse(nchar(PSObsShk$cell)==4 & substr(PSObsShk$cell,1,1)=="8",paste("08",substr(PSObsShk$cell,2,4),sep=""),PSObsShk$cell)

#create a school ID field
PSObsShk$asso <- ifelse(PSObsShk$sch_id %in% c(1:2), "U",ifelse(PSObsShk$sch_id %in% c(3:7),"A","X"))


#save(PSObsShk,file="P:\\WCPFC Shark\\SharkPSObs.Rdata")    
#save(PSObsShk,file="C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\DATA_clean\\SharkPSObs.Rdata")      

  # save(PSObsShk,file="H:/DATAShark/PSObs.Rdata") 
      
    save(PSObsShk, file=paste( "C:/Projects/SHK-indicators-2015/DATA/PSObs",format(Sys.time(), "%d%b%Y" ), ".RData", sep='') )  
    
   #save(PSObsShk, file= paste("C:/Data/PS_obs_shk",format(Sys.time(), "%d%b%Y" ), ".RData", sep='') )  
                                         
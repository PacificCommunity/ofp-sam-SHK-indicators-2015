 

#
#
#
#  loads operational data and does basic formatting & checks
#  
# load( file="C:/Projects/SHK-indicators-2015_backup/DATA/Shark_Operational_processed.rdata") 
# head(shkLLlog); dim(shkLLlog)
#  mynames <- colnames(shkLLlog)
# dput(mynames)
mynames<-c("trip_id", "boat_id", "tripflag_id", "fleet_id", "dep_date", 
  "ret_date", "captain", "bait1_sp_id", "bait2_sp_id", "set_id", 
  "flag_id", "logdate", "lat", "lon", "ez_id", "set_start", "set_no", 
  "hook", "hk_bt_flt", "alb_n", "alb_n_est", "yft_n", "yft_n_est", 
  "bet_n", "bet_n_est", "oth_n", "oth_n_est", "lon1", "lat1", "lon5", 
  "lat5", "yy", "mm", "day", "qtr", "DGX", "DUS", "WSH", "CCP", 
  "BRO", "PTH", "ALV", "AML", "CCE", "TIG", "LMA", "STT", "BTH", 
  "SPN", "BSH", "THR", "CCL", "SMA", "RMB", "MAK", "PLS", "OCS", 
  "MAN", "FAL", "EAG", "GAG", "GUQ", "HXT", "ISB", "NTC", "POR", 
  "RAJ", "SCK", "SHF", "SHK", "SHL", "region", "HPBCAT", "newlat", 
  "newlon", "cell", "blue", "mako", "owt", "silky", "thresher", 
  "totshk")

shklog <- read.csv("C:/Projects/DATA_2015/logbook/ll_op_data_july_2015.txt", header=FALSE, stringsAsFactors=FALSE) 
      head(shklog) ; dim(shklog)      #   1785762      71     
      colnames(shklog) <- mynames[1:71]

     #  save(shkLLlog,file="C:\\Users\\joelr\\Desktop\\Shark Stock Assessment\\DATA_clean\\SharkLLLog.Rdata")  
     #  load(file="C:/Data/rawLL_operational_set170712.rdata")# loads oper
                      
   #1. Create a new region field in shklog and trim non WCPFC Statistical Area data:

      shklog$region<- rep(0,length(shklog$trip_id))
      shklog$region <- ifelse(shklog$lat1 >= 20 & shklog$lat1 <= 50 & shklog$lon1 >= 120 & shklog$lon1 < 180, 1, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= 20 & shklog$lat1 <= 50 & shklog$lon1 >= 180 & shklog$lon1 < 210, 2, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -10 & shklog$lat1 < 20 & shklog$lon1 >= 120 & shklog$lon1 < 170, 3, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -10 & shklog$lat1 < 20 & shklog$lon1 >= 170 & shklog$lon1 < 210, 4, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -10 & shklog$lat1 < -4 & shklog$lon1 >= 210 & shklog$lon1 < 230, 4, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -40 & shklog$lat1 < -10 & shklog$lon1 >= 141 & shklog$lon1 < 170, 5, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -55 & shklog$lat1 < -40 & shklog$lon1 >= 141 & shklog$lon1 < 150, 5, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -60 & shklog$lat1 < -40 & shklog$lon1 >= 150 & shklog$lon1 < 170, 5, shklog$region)
      shklog$region <- ifelse(shklog$lat1 >= -60 & shklog$lat1 < -10 & shklog$lon1 >= 170 & shklog$lon1 < 230, 6, shklog$region)
      shklog <- shklog[shklog$region > 0,]


# Ensure no NAs in the data
shklog <- shklog[is.na(shklog$lat1)==FALSE,]
shklog <- shklog[is.na(shklog$lon1)==FALSE,]


#2. Add additional fields and edits
shklog$HPBCAT <- ifelse(shklog$hk_bt_flt<11,"S","D")   #S=Shallow, D=Deep
#shklog$SST <- round(shklog$sst,1)


#convert lat/longs
shklog$newlat<- rep(0,length(shklog$trip_id))
shklog$newlon <- rep(0,length(shklog$trip_id))
shklog$newlat <- ifelse(substr(shklog$lat,5,5)=="S",(-1*((as.numeric(substr(shklog$lat,1,2)))+((as.numeric(substr(shklog$lat,3,4)))/60))),((as.numeric(substr(shklog$lat,1,2)))+((as.numeric(substr(shklog$lat,3,4)))/60)))
shklog$newlon <- ifelse(substr(shklog$lon,6,6)=="E",(((as.numeric(substr(shklog$lon,1,3)))+((as.numeric(substr(shklog$lon,4,5)))/60))), (180+(180-(((as.numeric(substr(shklog$lon,1,3)))+((as.numeric(substr(shklog$lon,4,5)))/60))))))



#make a factor which is a combination of lat and lon
#shklog$cell <- paste(round(as.numeric(as.character(shklog$lat5))),round(as.numeric(as.character(shklog$lon5))),sep="")
shklog$cell <- as.character(paste(round(shklog$lat1),round(shklog$lon1),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8 # shit this should be lat5
shklog$cell<- ifelse(nchar(shklog$cell)==5 & substr(shklog$cell,1,2)=="-2",paste("-02",substr(shklog$cell,3,5),sep=""),shklog$cell)
shklog$cell<- ifelse(nchar(shklog$cell)==4 & substr(shklog$cell,1,1)=="2",paste("02",substr(shklog$cell,2,4),sep=""),shklog$cell)
shklog$cell<- ifelse(nchar(shklog$cell)==5 & substr(shklog$cell,1,2)=="-8",paste("-08",substr(shklog$cell,3,5),sep=""),shklog$cell)
shklog$cell<- ifelse(nchar(shklog$cell)==4 & substr(shklog$cell,1,1)=="8",paste("08",substr(shklog$cell,2,4),sep=""),shklog$cell)


#make groups "Blue, Silky, OWT, Mako and Thresher HHD and POR" and compute CPUE
#shklog$blue <- shklog$BSH
shklog$MAK1 <- shklog$SMA + shklog$LMA + shklog$MAK
#shklog$owt <- shklog$OCS
#shklog$silky <- shklog$FAL
shklog$THR1 <- shklog$BTH + shklog$ALV + shklog$PTH + shklog$THR
 HHD <- c('SPN','SPZ','SPL','SPK','EUB')
shklog$HHD <- shklog$SPN # only one present

match(HHD,names(shklog) )
#Not sharks:  STT Dasyatis, RMB Manta, PLS Pelagic stingray, MAN Mantas, EAG Eagle Ray, RAJ Rajidae, SHF Anglerfish       head(shklog[,c(36:46,48:53,55,57,59,61:66,68,70:71)])
shklog$totshk<-rowSums(shklog[,c(36:46,48:53,55,57,59,61:66,68,70:71)],na.rm=T)

 shklog$BLUECPUE <- shklog$BSH/(shklog$hook/1000)
 shklog$MAKOCPUE <- shklog$MAK1/(shklog$hook/1000)
 shklog$OWTCPUE <- shklog$OCS/(shklog$hook/1000)
 shklog$SILKYCPUE <- shklog$FAL/(shklog$hook/1000)
 shklog$THRCPUE <- shklog$THR1/(shklog$hook/1000)
 shklog$HHDCPUE <- shklog$HHD/(shklog$hook/1000)
 shklog$PORCPUE <- shklog$POR/(shklog$hook/1000)

shklog <- shklog[shklog$yy > 1994 & shklog$yy < 2015,]
table(shklog$yy)
nrow(shklog)
 
#save(shkLLlog,file="P:\\WCPFC Shark\\SharkLLLog.Rdata")    
 save(shklog,file="C:/Projects/DATA_2015/logbook/LL_oper_processed_14July2015.rdata") 
#  oper <- shkLLlog  
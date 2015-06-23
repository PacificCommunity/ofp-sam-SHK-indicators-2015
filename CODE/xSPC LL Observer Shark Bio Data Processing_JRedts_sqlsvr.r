     
#######################
# Process data based on the new query that Corey put together on 26 Feb 2013.
#    
#
#
#     
 
#rm(list=ls(all=TRUE))
 gc()
#  memory.limit(size=NA)           #don't use if you have a 64 bit machine
#  memory.limit(size=4000)
#  memory.limit(size=NA)


#  new data as of 19 03 2013
     #  shkbio<- read.csv("C:/Data/ll_obs_bio260213.csv", header=T, stringsAsFactors=FALSE)  
     #  shkbio<- read.csv("C:/Data/ll_obs_bio_2013-03-19.csv", header=T, stringsAsFactors=FALSE)  
     #  shkbio_old <- read.csv("E:/NOUMEA/C_drive_JoelR/Data/ll_obs_bio_2013-03-19.csv", header=T, stringsAsFactors=FALSE) 
#
 
head(shkbio_old) ; dim(shkbio_old) # old was  534066     44
# the following names are the column names from the older files....som reason we've go an extra 2 columns in the new (june 2015 file)
cnames <- c("obstrip_id", "l_shaul_id", "l_cmon_id", "obs_prg_id", "vesselname", 
            "gr_id", "flag_id", "obsv_id", "tripno", "o_dep_date", "o_ret_date", 
            "setdate", "hk_bt_flt", "hook", "hook_set", "hook_est", "hook_calc", 
            "bask", "sharktarget", "lon_long", "lat_long", "ez_id", "lon1", 
            "lat1", "lon5", "lat5", "yy", "mm", "day", "quarter", "sp_id", 
            "hook_no", "ctime", "len", "len_id", "fate_id", "cond_id", "cond_2_id", 
            "wt_est", "sex_id", "sp_name", "sp_cat_id", "sp_sci_name", "sci_category" )                  



  shkbio <- read.csv("C:/Projects/SHK-indicators-2015/DATA/joel_ll_bio_june_2015.csv", header=F, stringsAsFactors=FALSE)
head(shkbio) ; dim(shkbio) # 565641     46  so it is slightly longer in the number of columns, and it has 


  xrow <- colnames(shkbio)
 rbind(c(cnames[1:18],"miss1","miss2", cnames[19:44]), shkbio[1,])

summary(shkbio[,19:20])

table(shkbio[,19])#                  1      2      3      4   NULL   # target speices?
                  #         214 114696   6206  29453  36234 378838 

table(shkbio[,20])#   target species ID?

       shkbio  <- shkbio[-1,]  
       shkbio2 <- shkbio   #  save a copy
   
         
# prelim calcs         
      shkbio$yy <- as.numeric(as.character( shkbio$yy ))     ; range(shkbio$yy, na.rm=T)
      shkbio$ez_id <- as.character(shkbio$ez_id)
      shkbio$lat1 <- as.numeric(as.character(shkbio$lat1))              
      shkbio$lon1 <- as.numeric(as.character(shkbio$lon1))
            
      shkbio$lat5 <- as.numeric(as.character( shkbio$lat5 ))
      shkbio$lon5 <- as.numeric(as.character( shkbio$lon5 ))
      
     table(shkbio$len, useNA='always') 
     table(shkbio$len_id, useNA='always')   
     head(shkbio)
     
       
   shkbio$len <- as.numeric(as.character(shkbio$len))
   shkbio <- shkbio[shkbio$len>0,]  
      

#0. create groups

#add field for key species group:   Blue, Mako, OWT, Silky, Thresher
shkbio$group <- rep(0,length(shkbio$obstrip_id))
shkbio$group <- ifelse(shkbio$sp_id=="BSH","Blue",
                    ifelse(shkbio$sp_id=="MAK"|shkbio$sp_id=="SMA"|shkbio$sp_id=="LMA","Mako",
                    ifelse(shkbio$sp_id=="FAL","Silky",
                    ifelse(shkbio$sp_id=="OCS","OWT",
                    ifelse(shkbio$sp_id=="ALV"|shkbio$sp_id=="BTH"|shkbio$sp_id=="PTH"|shkbio$sp_id=="THR","Thresher","error")))))
      
      
    
      
      
#use TL_flag to flag and screen those TL which are outside the range measured to create the conversion factor (and thus won't convert credibility)(removes 669 records) 
#note:  don't have such a range for OWT so no OWT are screened (these are Kohler, Casey and Turner (1996) data--see Length Conversion Factors.xslx)
shkbio$TL_flag1 <- rep("N",length(shkbio$yy))
shkbio$TL_flag <- ifelse((shkbio$sp_id=="BSH" & shkbio$len_id=="TL" & (shkbio$len<64|shkbio$len>337)) |
                             (shkbio$sp_id %in% c("SMA","MAK","LMA") & shkbio$len_id=="TL" & (shkbio$len<70|shkbio$len>368)) |
                              (shkbio$sp_id=="FAL" & shkbio$len_id=="TL" & (shkbio$len<90|shkbio$len>258)) |
                               (shkbio$sp_id %in% c("ALV","BTH","PTH","THR") & shkbio$len_id=="TL" & (shkbio$len<155|shkbio$len>450)),"N","Y")
shkbio<-shkbio[shkbio$TL_flag=="Y",]
#shkbio<-shk
 dim(shkbio)  #506757     46

       table( shkbio$len_id)   
       shkbio<-shkbio[shkbio$len_id %in% c("TL", "UF", "LF"),]
       dim(shkbio) 
       
       
       table(shkbio$len_id) #   table( shkbio$len_id=="TL")  
 #  par(mfrow=c(2,2));for(i in c("TL", "UF", "LF")){hist(shkbio$len[shkbio$len_id ==i&shkbio$group=="Silky"],col=rainbow(12)[3], main=i, xlab=i, xlim=c(0,350) )} 
 # convert TLs to UFs (all "dodgy" TLs have already been removed(using field TL_flag)); here OWT conversion factor comes from FishBase; BSH and MAK are from Francis and Duffy (2005); THR is BTH
 sharkgroup = c("Blue","Mako","OWT","Silky","Thresher")
 afact = c(0.831,0.911,0.822,0.8388,0.5598)
 bfact = c(1.39,0.821,0,-2.651,17.666)
 shkbio$convFL <- rep(0,length(shkbio$obstrip_id))
     for (i in c(1:5)) {                             
       shkbio$convFL <- ifelse(shkbio$len_id=="TL" & shkbio$group==sharkgroup[i], afact[i]*shkbio$len+bfact[i],shkbio$convFL)       
     } 
 shkbio$convFL <- ifelse(shkbio$len_id%in%c("UF","LF"), shkbio$len,shkbio$convFL)  
    table(shkbio$len_id) #   

 
 
#use len_flag1 to screen based on whether each observation is above the smallest known free-swimming size for each species   (doesn't screen any)
shkbio$len_flag1 <- rep("N",length(shkbio$yy))
shkbio$len_flag1 <- ifelse((shkbio$sp_id=="BSH" & shkbio$len>34)|(shkbio$sp_id=="SMA" & shkbio$len>62)| 
                              (shkbio$sp_id=="MAK" & shkbio$len>62)|(shkbio$sp_id=="LMA" & shkbio$len>122)|
                              (shkbio$sp_id=="FAL" & shkbio$len>63)|(shkbio$sp_id=="OCS" & shkbio$len>65)|
                              (shkbio$sp_id=="ALV" & shkbio$len>116)|(shkbio$sp_id=="BTH" & shkbio$len>129)|
                              (shkbio$sp_id=="PTH" & shkbio$len>136)|(shkbio$sp_id=="THR" & shkbio$len>116),"Y","N")
shkbio$len_flag1 <- rep("Y",length(shkbio$obstrip_id))
               #153219 rows now
#screen out all records for which each length observation is below the low end of the range of size at birth for pelagic sharks (SOTOO p. 38)(screens 306 recs, n=151,660)        -jrdts 152860
shkbio <- shkbio[shkbio$len>34,]
       dim(shkbio)

#convert lat/longs
       shkbio$newlat <- rep(0,length(shkbio$obstrip_id))
       shkbio$newlon <- rep(0,length(shkbio$obstrip_id))
       
        shkbio$newlat <- ifelse(substr(shkbio$lat_long,9,9)=="S",(-1*((as.numeric(substr(shkbio$lat_long,1,2)))+((as.numeric(substr(shkbio$lat_long,3,8)))/60))),((as.numeric(substr(shkbio$lat_long,1,2)))+((as.numeric(substr(shkbio$lat_long,3,8)))/60)))
              
      shkbio$newlon <- ifelse(substr(shkbio$lon_long,10,10)=="E",(((as.numeric(substr(shkbio$lon_long,1,3)))+((as.numeric(substr(shkbio$lon_long,4,9)))/60))), (180+(180-(((as.numeric(substr(shkbio$lon_long,1,3)))+((as.numeric(substr(shkbio$lon_long,4,9)))/60))))))
           dim(shkbio)
           head(shkbio)
 # to check       
    shkbio[shkbio$ez_id=='AU',c('ez_id','lon_long', 'lat_long', 'newlat', 'newlon','lat1', 'lon1', 'lat5', 'lon5') ][1:30,]
    shkbio[shkbio$ez_id=='PF',c('ez_id','lon_long', 'lat_long', 'newlat', 'newlon','lat1', 'lon1', 'lat5', 'lon5') ][1:30,]
        
# Convert         
         shkbio$oldlon5 <-  shkbio$lon5
         shkbio$oldlon1 <-  shkbio$lon1
        
           
        
         shkbio$lon1 <- round(shkbio$newlon) +0.5
         shkbio$lon5 <- floor(shkbio$newlon /5)*5 +2.5 
         
         shkbio$lat10 <- 10*floor(shkbio$newlat/10) 
         shkbio$lon10 <- 10*floor(shkbio$newlon/10)
         
         #
 # to check       
    shkbio[shkbio$ez_id=='AU',c('ez_id','lon_long', 'lat_long', 'newlat', 'newlon','lat1', 'lon1', 'lat5', 'lon5') ][1:30,]
    shkbio[shkbio$ez_id=='PF',c('ez_id','lon_long', 'lat_long', 'newlat', 'newlon','lat1', 'lon1', 'lat5', 'lon5') ][1:30,]
                                  



 #make a factor which is a combination of lat and lon
#shkbio$cell <- paste(round(as.numeric(as.character(shkbio$lat5))),round(as.numeric(as.character(shkbio$lon5))),sep="")
shkbio$cell <- as.character(paste(round(shkbio$lat5),round(shkbio$lon5),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8
shkbio$cell<- ifelse(nchar(shkbio$cell)==5 & substr(shkbio$cell,1,2)=="-2",paste("-02",substr(shkbio$cell,3,5),sep=""),shkbio$cell)
shkbio$cell<- ifelse(nchar(shkbio$cell)==4 & substr(shkbio$cell,1,1)=="2",paste("02",substr(shkbio$cell,2,4),sep=""),shkbio$cell)
shkbio$cell<- ifelse(nchar(shkbio$cell)==5 & substr(shkbio$cell,1,2)=="-8",paste("-08",substr(shkbio$cell,3,5),sep=""),shkbio$cell)
shkbio$cell<- ifelse(nchar(shkbio$cell)==4 & substr(shkbio$cell,1,1)=="8",paste("08",substr(shkbio$cell,2,4),sep=""),shkbio$cell)
 shkbio$cell <- as.factor(shkbio$cell)
 table(shkbio$cell)
 head(shkbio)  
   dim(shkbio)                   
 
 #1. Create a new region field in shk and trim non WCPFC Statistical Area data:

      shkbio$region<- rep(0,length(shkbio$obstrip_id))
      shkbio$region <- ifelse(shkbio$lat1 >= 20 & shkbio$lat1 <= 50 & shkbio$lon1 >= 120 & shkbio$lon1 < 180, 1, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= 20 & shkbio$lat1 <= 50 & shkbio$lon1 >= 180 & shkbio$lon1 < 210, 2, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < 20 & shkbio$lon1 >= 120 & shkbio$lon1 < 170, 3, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < 20 & shkbio$lon1 >= 170 & shkbio$lon1 < 210, 4, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < -4 & shkbio$lon1 >= 210 & shkbio$lon1 < 230, 4, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -40 & shkbio$lat1 < -10 & shkbio$lon1 >= 141 & shkbio$lon1 < 170, 5, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -55 & shkbio$lat1 < -40 & shkbio$lon1 >= 141 & shkbio$lon1 < 150, 5, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -60 & shkbio$lat1 < -40 & shkbio$lon1 >= 150 & shkbio$lon1 < 170, 5, shkbio$region)
      shkbio$region <- ifelse(shkbio$lat1 >= -60 & shkbio$lat1 < -10 & shkbio$lon1 >= 170 & shkbio$lon1 < 230, 6, shkbio$region)
      shkbio <- shkbio[shkbio$region > 0,]


#add a field for northern/southern hemisphere
#shkbio$hemi <- rep(0,length(shkbio$obstrip_id))
#shkbio$hemi <- ifelse(shkbio$newlat > 0,"N","S")

table(shkbio$yy, shkbio$group)
       dim(shkbio) 

#     shkbio <- shkbio[shkbio$yy > 1994 & shkbio$yy < 2012,]        #there are limited observer records before 1990      but I am skipping this bc might want to see the effect for blues

#2. Add additional fields and edits
shkbio$HPBCAT <- ifelse(as.numeric(as.character(shkbio$hk_bt_flt))<11,"S","D")   #S=Shallow, D=Deep

# if hook_est (the estimated number of effctive hooks ?) is zero or NULL take the number   set
   shkbio$hook_est <- ifelse( shkbio$hook_est=="NULL", shkbio$hook_set,shkbio$hook_est)
   shkbio$hook_est <- ifelse( shkbio$hook_est==0, shkbio$hook_set,shkbio$hook_est)
 dim(shkbio) 
  # table( shkbio$hook_est=="NULL"  , useNA='always')
   shkbio$hook_est <- as.numeric(as.character( shkbio$hook_est )) 
   shkbio <- shkbio[shkbio$hook_est > 500,] #   small sets are usually exploratory
    dim(shkbio) 
               
  
  
 



#> table(shkbio$fate_id)

# DCF   DDL   DFR   DFW   DGD   DOR   DPA   DPD   DPQ   DPS   DPU   DSD   DSO   DTS   DUS   DWD   ESC   RCC   RFL   RFR   RGG   RGO   RGT   RHG   RHT   ROR   RPT   RSD   RWD   RWW   UUU 
#    9    21 78425     2     4 21279    15    76    26     3   132    36    39  1267   818     7    76   169    11 72042   547     8    32  7805    27  3268  2464    86    18   856  3330 


keyspp <- shkbio[shkbio$sp_id %in% c("BSH","SMA","LMA","MAK","OCS","FAL","THR","PTH","BTH","ALV"),]
table(keyspp$sp_id,keyspp$fate_id %in% c("DFR","DOR","RFR","ROR","RPT"))

  
#R=retained; D=discarded; E=escaped  
#DFR=discarded trunk, fins retained
#DPA=species of special interest discarded alive
#DPD=species of special interest discarded dead
#DPU=species of special interest discarded condition unknown
#DOR=discarded, other reason
#RPT=retained partial
#RFR=retained both fins and trunk

#note "retained = yes" means retained for more than just fins
shkbio$retained <- ifelse(substr(shkbio$fate_id,1,1)=="R",1,0)

shkbio$escaped <- ifelse(shkbio$fate_id %in% c("ESC"),1,0)

shkbio$discardednf <- ifelse(substr(shkbio$fate_id,1,1)=="D" & shkbio$fate_id!="DFR",1,0)
shkbio$finned <- ifelse(shkbio$fate_id %in% c("DFR"),1,0)


#> table(shkbio$cond_id)

#    A0     A1     A2     A3      D      U 
#141703  73175  15127  14947  47399  69676 

#A0=alive
#A1=alive, healthy  
#A2=Alive, injured or distressed
#A3=Alive, but dying
#D=dead
#U=condition unknown

shkbio$alivestart <- ifelse(substr(shkbio$cond_id,1,1)=="A",1,0)
shkbio$healthystart <- ifelse(shkbio$cond_id %in% c("A0","A1"),1,0)
shkbio$deadstart <- ifelse(substr(shkbio$cond_id,1,1)=="D",1,0)
shkbio$uidcondstart <- ifelse(substr(shkbio$cond_id,1,1)=="U",1,0)

shkbio$aliveend <- ifelse(substr(shkbio$cond_2_id,1,1)=="A",1,0)
shkbio$healthyend <- ifelse(shkbio$cond_2_id %in% c("A0","A1"),1,0)
shkbio$deadend <- ifelse(substr(shkbio$cond_2_id,1,1)=="D",1,0)
shkbio$uidcondend <- ifelse(substr(shkbio$cond_2_id,1,1)=="U",1,0)

     head(shkbio)
     dim(shkbio) # 




 #  save(shkbio, file="C:/Data/ll_obs_bio_260213_processed_allsharks.rdata" )
    save(shkbio, file="C:/Data/ll_obs_bio_190313_processed_allsharks.rdata" )
   


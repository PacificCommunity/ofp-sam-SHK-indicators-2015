     
#######################
# Process data based on the new query that Corey put together on 26 Feb 2013.
#    
#
#
#     
 load(file=paste0(dat.dir, "lldata_11JULY2015.rdata")))
 
#    
# shkbio  <-  read.csv("C:/Projects/SHK-indicators-2015/DATA/joel_ll_bio_june_2015_for_shk_and_skj.csv", header=FALSE) ; head(shkbio)
# biohead <- read.csv("C:/Projects/SHK-indicators-2015/DATA/bio_header_rec.csv", header=FALSE,  stringsAsFactors=FALSE); head(biohead)

dput( names(catch) )
c("obstrip_id", "l_set_id", "catch_time", "sp_code", "sp_category",   "hk_bt_flt", "hook_no", "condition_land", "condition_release", 
  "fate_code", "len", "len_code", "sex_code", "condition_use",   "hook_pos", "yy")
 dim(shkbio) #raw 640354     46  # 74713 of which are SKJ
table(catch$sp_code)  # all of the individual codes
table(catch$sp_category)

#shkbio <- shkbio[shkbio$sp_cat_id=="SHK",]  # get rid of the devil rays/ skipjack/ and other ones
 dim(catch) #565213     46

table(catch[catch$sp_category=="SKJ","len_code" ]) #  over 31307 are UF also some NM

nrow(sets); nrow(catch)
pntr  <- match( catch$l_set_id, sets$l_set_id ); sum(is.na(pntr))
max(pntr)==nrow(sets)# should be the same as nrow(sets)
catch$yy <- sets$yy[pntr]
catch$mm <- sets$mm[pntr]
catch$lat1d <- sets$lat1d[pntr]
catch$lon1d <- sets$lon1d[pntr]
catch$flag  <- sets$flag[pntr]
catch$region <- sets$region[pntr]
catch$eez_code <- sets$eez_code[pntr]
catch$program_code <- sets$programcode[pntr]

# prelim calcs         
     str(catch)
      catch$len <- as.numeric(as.character(catch$len))
  nrow(catch) #659092      
  
catch <- catch[catch$len>0,]  
nrow(catch)  #578643

# this will change the dimensions w.r.t the  older data
catch <- catch[catch$yy %in%  1995:2014,]
nrow(catch)  # should have already done that 578643

#0. create groups

#add field for key species group:  c("BSH","MAK","OCS","FAL","THR","HHD", "POR") and SKJ were made in the ll_os_data_prep_sets_and_catch_11jULY2015.r
# 
# shkbio$group <- rep(0,length(shkbio$obstrip_id))
# shkbio$group <- ifelse(shkbio$sp_id=="BSH","Blue",
#                     ifelse(shkbio$sp_id=="MAK"|shkbio$sp_id=="SMA"|shkbio$sp_id=="LMA","Mako",
#                     ifelse(shkbio$sp_id=="FAL","Silky",
#                     ifelse(shkbio$sp_id=="OCS","OWT",
#                     ifelse(shkbio$sp_id=="ALV"|shkbio$sp_id=="BTH"|shkbio$sp_id=="PTH"|shkbio$sp_id=="THR","Thresher","error")))))
      
      
       
      
#use TL_flag to flag and screen those TL which are outside the range measured to create the conversion factor (and thus won't convert credibility)(removes 669 records) 
#note:  don't have such a range for OWT so no OWT are screened (these are Kohler, Casey and Turner (1996) data--see Length Conversion Factors.xslx)
catch$TL_flag1 <- rep("N",length(catch$yy))
catch$TL_flag <- ifelse((catch$sp_category=="BSH" & catch$len_code=="TL" & (catch$len<64|catch$len>337)) |
                             (catch$sp_category ==  "MAK"  & catch$len_code=="TL" & (catch$len<70|catch$len>368)) |
                              (catch$sp_category=="FAL" & catch$len_code=="TL" & (catch$len<90|catch$len>258)) |
                               (catch$sp_category=="THR" & catch$len_code=="TL" & (catch$len<155|catch$len>450))| 
                                 (catch$sp_category=="POR" & catch$len_code=="TL" & (catch$len<76|catch$len>291)), "N","Y")

 # info on HHD is tough to come by right now....its out there

catch<-catch[catch$TL_flag=="Y",]
nrow(catch) # 553880
#
# the only ones we can convert/use
catch<-catch[catch$len_code %in% c("TL", "UF", "LF"),]
       nrow(catch)#  358541
 
catch$len <- as.numeric(as.character(catch$len)) #sum(is.na(catch$len))=54
catch<- catch[!is.na(catch$len),]
nrow(catch)
 
 # convert TLs to UFs (all "dodgy" TLs have already been removed(using field TL_flag)); here OWT conversion factor comes from FishBase; BSH and MAK are from Francis and Duffy (2005); THR is BTH
 sharkgroup =  c("BSH","MAK","OCS","FAL","THR","HHD", "POR")  #FL=(a)TL+b
nrow(catch)#358487
# make names and other init declarations 
 afact = c(0.831,0.911,0.822,0.8388,0.5598, 0.7756   ,  0.893)
 bfact = c(1.39,0.821,0,-2.651,17.666,  -0.3132, -6.943 )
 catch$convFL <- rep(0,length(catch$obstrip_id))
     for (i in c(1:nspec)) {                             
       catch$convFL <- ifelse(catch$len_code=="TL" & catch$sp_category==sharkgroup[i], afact[i]*catch$len+bfact[i],catch$convFL)       
     } 
 catch$convFL <- ifelse(catch$len_code%in%c("UF","LF"), catch$len,catch$convFL)  
  

 dim(catch)
 
#use len_flag1 to screen based on whether each observation is above the smallest known free-swimming size for each species   (doesn't screen any)
# shkbio$len_flag1 <- rep("N",length(shkbio$yy))
# shkbio$len_flag1 <- ifelse((shkbio$sp_id=="BSH" & shkbio$len>34)|(shkbio$sp_id=="SMA" & shkbio$len>62)| 
#                               (shkbio$sp_id=="MAK" & shkbio$len>62)|(shkbio$sp_id=="LMA" & shkbio$len>122)|
#                               (shkbio$sp_id=="FAL" & shkbio$len>63)|(shkbio$sp_id=="OCS" & shkbio$len>65)|
#                               (shkbio$sp_id=="ALV" & shkbio$len>116)|(shkbio$sp_id=="BTH" & shkbio$len>129)|
#                               (shkbio$sp_id=="PTH" & shkbio$len>136)|(shkbio$sp_id=="THR" & shkbio$len>116),"Y","N")
# 
# shkbio$len_flag1 <- rep("Y",length(shkbio$obstrip_id))
                
#screen out all records for which each length observation is below the low end of the range of size at birth for pelagic sharks (SOTOO p. 38) 
catch <- catch[catch$len>34,]
nrow(catch)# 357897

# #convert lat/longs
#        shkbio$newlat <- rep(0,length(shkbio$obstrip_id))
#        shkbio$newlon <- rep(0,length(shkbio$obstrip_id))
#        
#         shkbio$newlat <- ifelse(substr(shkbio$lat_long,9,9)=="S",(-1*((as.numeric(substr(shkbio$lat_long,1,2)))+((as.numeric(substr(shkbio$lat_long,3,8)))/60))),((as.numeric(substr(shkbio$lat_long,1,2)))+((as.numeric(substr(shkbio$lat_long,3,8)))/60)))
#               
#       shkbio$newlon <- ifelse(substr(shkbio$lon_long,10,10)=="E",(((as.numeric(substr(shkbio$lon_long,1,3)))+((as.numeric(substr(shkbio$lon_long,4,9)))/60))), (180+(180-(((as.numeric(substr(shkbio$lon_long,1,3)))+((as.numeric(substr(shkbio$lon_long,4,9)))/60))))))
#            dim(shkbio)
#            head(shkbio)
# 
 
           
        
 #        shkbio$lon1 <- round(shkbio$newlon) +0.5
 #         shkbio$lon5 <- floor(shkbio$newlon /5)*5 +2.5 
         
 #        shkbio$lat10 <- 10*floor(shkbio$newlat/10) 
 #         shkbio$lon10 <- 10*floor(shkbio$newlon/10)
  
head(catch)
  #1. Create a new region field in shk and trim non WCPFC Statistical Area data:

#       shkbio$region<- rep(0,length(shkbio$obstrip_id))
#       shkbio$region <- ifelse(shkbio$lat1 >= 20 & shkbio$lat1 <= 50 & shkbio$lon1 >= 120 & shkbio$lon1 < 180, 1, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= 20 & shkbio$lat1 <= 50 & shkbio$lon1 >= 180 & shkbio$lon1 < 210, 2, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < 20 & shkbio$lon1 >= 120 & shkbio$lon1 < 170, 3, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < 20 & shkbio$lon1 >= 170 & shkbio$lon1 < 210, 4, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -10 & shkbio$lat1 < -4 & shkbio$lon1 >= 210 & shkbio$lon1 < 230, 4, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -40 & shkbio$lat1 < -10 & shkbio$lon1 >= 141 & shkbio$lon1 < 170, 5, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -55 & shkbio$lat1 < -40 & shkbio$lon1 >= 141 & shkbio$lon1 < 150, 5, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -60 & shkbio$lat1 < -40 & shkbio$lon1 >= 150 & shkbio$lon1 < 170, 5, shkbio$region)
#       shkbio$region <- ifelse(shkbio$lat1 >= -60 & shkbio$lat1 < -10 & shkbio$lon1 >= 170 & shkbio$lon1 < 230, 6, shkbio$region)
#       shkbio <- shkbio[shkbio$region > 0,]
# 
# points(shkbio$newlon, shkbio$newlat, cex=.4, col=2, pch=16)


#add a field for northern/southern hemisphere
 catch$hemi <- 0
catch$hemi <- ifelse(catch$lat1d > 0,"N","S")

  

#2. Add additional fields and edits
catch$HPBCAT <- ifelse(as.numeric(as.character(catch$hk_bt_flt))<11,"S","D")   #S=Shallow, D=Deep

# if hook_est (the estimated number of effctive hooks ?) is zero or NULL take the number   set
#    shkbio$hook_est <- ifelse( shkbio$hook_est=="NULL", shkbio$hook_set,shkbio$hook_est)
#    shkbio$hook_est <- ifelse( shkbio$hook_est==0, shkbio$hook_set,shkbio$hook_est)
#  dim(shkbio) 
  
# table( shkbio$hook_est=="NULL"  , useNA='always')
#   shkbio$hook_est <- as.numeric(as.character( shkbio$hook_est )) 
 #  shkbio <- shkbio[shkbio$hook_est > 500,] #   small sets are usually exploratory
    
               
  
  
 


#  
# keyspp <- shkbio[shkbio$sp_id %in% c("BSH","SMA","LMA","MAK","OCS","FAL","THR","PTH","BTH","ALV"),]
# table(keyspp$sp_id,keyspp$fate_id %in% c("DFR","DOR","RFR","ROR","RPT"))

  
#R=retained; D=discarded; E=escaped  
#DFR=discarded trunk, fins retained
#DPA=species of special interest discarded alive
#DPD=species of special interest discarded dead
#DPU=species of special interest discarded condition unknown
#DOR=discarded, other reason
#RPT=retained partial
#RFR=retained both fins and trunk

#note "retained = yes" means retained for more than just fins
catch$retained <- ifelse(substr(catch$fate_code,1,1)=="R",1,0)

catch$escaped <- ifelse(catch$fate_code %in% c("ESC"),1,0)

catch$discardednf <- ifelse(substr(catch$fate_code,1,1)=="D" & catch$fate_code!="DFR",1,0)
catch$finned <- ifelse(catch$fate_code %in% c("DFR"),1,0)


#> table(catch$condition_land)

#    A0     A1     A2     A3      D      U 
#141703  73175  15127  14947  47399  69676 

#A0=alive
#A1=alive, healthy  
#A2=Alive, injured or distressed
#A3=Alive, but dying
#D=dead
#U=condition unknown

catch$alivestart <- ifelse(substr(catch$condition_land,1,1)=="A",1,0)
catch$healthystart <- ifelse(catch$condition_land %in% c("A0","A1"),1,0)
catch$deadstart <- ifelse(substr(catch$condition_land,1,1)=="D",1,0)
catch$uidcondstart <- ifelse(substr(catch$condition_land,1,1)=="U",1,0)

catch$aliveend <- ifelse(substr(catch$condition_release,1,1)=="A",1,0)
catch$healthyend <- ifelse(catch$condition_release %in% c("A0","A1"),1,0)
catch$deadend <- ifelse(substr(catch$condition_release,1,1)=="D",1,0)
catch$uidcondend <- ifelse(substr(catch$condition_release,1,1)=="U",1,0)

     head(catch)
     dim(catch) #  357897     39



 
save(catch, file="C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata" )  


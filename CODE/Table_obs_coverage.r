


# make tables - %obs coverage
# Joel Rice
# july 2015


options(stringsAsFactors=FALSE)
require(dplyr)
require(magrittr)
require(xtable)

skjdir <- "C:/Projects/SHK-indicators-2015/" 
dat.dir <- "C:/Projects/DATA_2015/LL/"
#dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
setwd(skjdir)

# new data on 6 July 2015
# note that NZ (recent years) has to get added (rbind) as does the AUS data, updated with location
# also the NZ data has non-unique id's (obs and l_set...........)  w.r.t the llset data

#----------------------------------------------------------------------------------------------
# set by set data
#---------------------------------------------------------------------------------------------
llset      <- read.csv(  paste(dat.dir, "ll_shark_SET_non_HWOB.csv", sep='') ,  header=T,  stringsAsFactors =F); nrow(llset) #43554
llseth     <- read.table(paste(dat.dir,"ll_shark_SET_HWOB.csv" , sep=''), sep="\t", header=T,  stringsAsFactors =F); nrow(llseth) # 810619
llset_NZ   <- read.csv(paste(dat.dir,  "ll_shark_SET_NZOB_2012_2014.csv",sep=""),header=T,  stringsAsFactors =F)  ; nrow(llset_NZ )# 855
# bc this is seperately generated we need specific l_set_id and obstripid numbers
#match(llset_NZ$l_set_id , llset$l_set_id)
llset_NZ$obstrip_id <- llset_NZ$obstrip_id +1e6
llset_NZ$l_set_id <- llset_NZ$l_set_id +1e6

#
llset_AU   <- read.table(paste(dat.dir,"/l_shark_set_AUOB_2010_2013.txt", sep=''), sep=",",header=FALSE, stringsAsFactors =F);   nrow(llset_AU )# 1128
dim(llset_AU); head(llset_AU)
#tdate <-format( as.Date(llset_AU$set_start_date, "%d/%m/%Y")  , "%Y%m%d")
#llset_AU$set_date <- tdate
names(llset_AU) <- names(llset)
#old.AU.catch.names <- c("trip_id","l_set_id","cat_date","cat_time","sp_code","sp_categor","hk_bt_flt","hook_no","condition_",
#                        "condition2","fate_code","len","len_code","sex_code")
#head(llset_AU)
#cbind( names(llset), names(llseth), names(llset_NZ), names(llset_AU))
# llset_AU is named just a bit different ; trip_id, not obstrip_id

# old.AU.set.names <- c("trip_id","program_code","flag","fishery","vessel_id","l_set_id","set_date","set_time","set_end_time",
# "haul_start","haul_star2","lat1d","lon1d","eez_code","tar_sp_cod","target_tun","target_swo",
# "target_shk","hk_bt_flt","hook_set","hook_est","lightstick","bask_set","bask_obser","nbshark_li",
# "bait1_sp_c","bait2_sp_c","bait3_sp_c","bait4_sp_c","bait5_sp_c","wire_trace","hook_type","sharktarge",
# "sharkbait","moonfrac","sst")

llseth  %<>% filter(!duplicated(l_set_id)) ; nrow(llseth) # 56929

rm(sets, catch)
sets    <- rbind(llset, llseth, llset_NZ, llset_AU) # get some warnings
sets <- sets[!is.na(sets$l_set_id) ,] 
head(sets);nrow(sets) # 102466 records now
# bunch of duplicates in here, from the NCOB and others?
#sets[sets$l_set_id %in%  c(114473 ,  114474 ,  114476 ,  114478 ,  114479),]  
sets  %<>% filter(!duplicated(l_set_id)) 
#str(sets )
head(sets); dim(sets) # ] 102311     36 on  11 JULY2015
head(sets)
sets$yy <- as.numeric(substring(sets$set_start_date,1,4))
sets<- sets[sets$yy %in% 1995:2014,]; nrow(sets) #95985
sets_all <- sets; rm(sets)
 #save(sets_all, file=paste0(dat.dir, "ll_obs_no_trimming_1995_2014.rdata") )

# sets<-sets_all

x <- c("set_start_time","set_end_time","haul_start_time",
       "lat1d","lon1d","hk_bt_flt","hook_set","hook_est",
       "lightsticks","bask_set","bask_observed","nbshark_lines")
sets_all[,x] %<>% sapply(as.numeric) 
#range(sets_all$lon1d, na.rm=T); table(sets_all$lon1d>0)
sets_all$lon1d %<>% "+"(ifelse(sets_all$lon1d<0, 360, 0))

# make regions according to the analysis
sets_all$region<- rep(0,length(sets_all$obstrip_id))
sets_all$region <- ifelse(sets_all$lat1d >= 20 & sets_all$lat1d <= 50 & sets_all$lon1d >= 120 & sets_all$lon1d < 180, 1, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= 20 & sets_all$lat1d <= 50 & sets_all$lon1d >= 180 & sets_all$lon1d < 210, 2, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -10 & sets_all$lat1d < 20 & sets_all$lon1d >= 120 & sets_all$lon1d < 170, 3, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -10 & sets_all$lat1d < 20 & sets_all$lon1d >= 170 & sets_all$lon1d < 210, 4, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -10 & sets_all$lat1d < -4 & sets_all$lon1d >= 210 & sets_all$lon1d < 230, 4, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -40 & sets_all$lat1d < -10 & sets_all$lon1d >= 141 & sets_all$lon1d < 170, 5, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -55 & sets_all$lat1d < -40 & sets_all$lon1d >= 141 & sets_all$lon1d < 150, 5, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -60 & sets_all$lat1d < -40 & sets_all$lon1d >= 150 & sets_all$lon1d < 170, 5, sets_all$region)
sets_all$region <- ifelse(sets_all$lat1d >= -60 & sets_all$lat1d < -10 & sets_all$lon1d >= 170 & sets_all$lon1d < 230, 6, sets_all$region)


nrow(sets_all) #95985
 sets_all <- sets_all[sets_all$region > 0,]
 sets_all <- sets_all[!is.na(sets_all$region),]
nrow(sets_all)# 80000

obs_cover <- tapply(sets_all$hook_est, list(sets_all$yy, sets_all$region), sum,  na.rm=T)


#################################################################################################################
# table Millions of hooks fished in longline fishery by region

aggr <- read.table( "C:/Projects/SHK-indicators-2015_backup/DATA/AGG_EFF_5X5_1995_2014.TXT", sep=',',header=T, stringsAsFactors =F)
head(aggr); str(aggr)
aggr$region<- 0
aggr$region <- ifelse(aggr$latd >= 20 & aggr$latd <= 50 & aggr$lond >= 120 & aggr$lond < 180, 1, aggr$region)
aggr$region <- ifelse(aggr$latd >= 20 & aggr$latd <= 50 & aggr$lond >= 180 & aggr$lond < 210, 2, aggr$region)
aggr$region <- ifelse(aggr$latd >= -10 & aggr$latd < 20 & aggr$lond >= 120 & aggr$lond < 170, 3, aggr$region)
aggr$region <- ifelse(aggr$latd >= -10 & aggr$latd < 20 & aggr$lond >= 170 & aggr$lond < 210, 4, aggr$region)
aggr$region <- ifelse(aggr$latd >= -10 & aggr$latd < -4 & aggr$lond >= 210 & aggr$lond < 230, 4, aggr$region)
aggr$region <- ifelse(aggr$latd >= -40 & aggr$latd < -10 & aggr$lond >= 141 & aggr$lond < 170, 5, aggr$region)
aggr$region <- ifelse(aggr$latd >= -55 & aggr$latd < -40 & aggr$lond >= 141 & aggr$lond < 150, 5, aggr$region)
aggr$region <- ifelse(aggr$latd >= -60 & aggr$latd < -40 & aggr$lond >= 150 & aggr$lond < 170, 5, aggr$region)
aggr$region <- ifelse(aggr$latd >= -60 & aggr$latd < -10 & aggr$lond >= 170 & aggr$lond < 230, 6, aggr$region)


nrow(aggr) #95985
aggr <- aggr[aggr$region > 0,]
aggr <- aggr[!is.na(aggr$region),]
nrow(aggr)# 80000

agg_cover<- tapply(aggr$hhooks, list(aggr$yy, aggr$region), sum,  na.rm=T)
#
agg_cover<- agg_cover*100 # true nums now


pnct_cover <- obs_cover/agg_cover
pnct_cover <- round(100*pnct_cover, 2)
colnames(pnct_cover) <- paste0("%Obs_Reg", 1:6)
#
aggr_mil <- round(agg_cover/1e6,1)
colnames(aggr_mil) <- paste0("Hks_Reg", 1:6)



#  (Table XX Logsheet coverage by region \% that ID sharks to species)
#------------------------------------------------------------------

load("C:/Projects/DATA_2015/logbook/LL_oper_processed_10July2015.rdata") # loads shklog
head(shklog)
temp<-    with(shklog, table(yy, region, totalshk>0  ));
log_pcnt_report <- temp[,,2] / (  temp[,,1] +temp[,,2])
colnames(log_pcnt_report) <- paste0("Log%Report_Reg", 1:6)

 log_pos <- temp[2]/nrow(shklog)
# log_cover<- tapply(shklog$hook, list(shklog$yy, shklog$region), sum,  na.rm=T)
 # calc percent by region
   log_pcnt <-  log_cover/agg_cover
   log_pcnt <-  round(100*log_pcnt,1 ) 
   colnames(log_pcnt) <- paste0("Log%Cover_Reg", 1:6)
   log_pcnt # this is the percent of the total the got reported


  temp2<-   with(shklog, table(totalshk>0)); log_pos <- temp2[2]/nrow(shklog)
  print(paste(round(100*log_pos), "% of operational sets recorded sharks"))
   "42 % of operational sets recorded sharks"
#-----------make tables

obstable <- xtable(pnct_cover, caption=c( "Percent of effort observed in the longline fishery by region." ))
#digits(obstable) <-0 
write.table(print(obstable) , file='clipboard', row.names =FALSE)

aggtable <- xtable(aggr_mil, caption=c( "Millions of hooks fished in longline fishery by region." ))
#digits(aggr_mil) <-0 
write.table(print(aggtable) , file='clipboard', row.names =FALSE)

logtable <- xtable(log_pcnt_report, caption=c( "Percent of Logsheets reporting sharks to species, longline fishery by region." ))
#digits(logtable) <-0 
write.table(print(logtable) , file='clipboard', row.names =FALSE)



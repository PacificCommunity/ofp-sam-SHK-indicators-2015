## skj_cpue-2015_data-prep.r
## Reading in already extracted data from observer database
## Formatting for analysis SKJ in S Pacific
## -------------------------------------------------------
## Author: J Rice (based on Code from LTB)
## Written on: 30:06:2015
## based largely on the code skj_cpue-2015_data-prep_3JULY2015.r

options(stringsAsFactors=FALSE)
require(dplyr)
require(magrittr)

skjdir <- "C:/Projects/SHK-indicators-2015/"
#dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
setwd(skjdir)

# new data on 3 July 2015
# note that NZ (recent years) has to get added (rbind) as does the AUS data, updated with location
# also the NZ data has non-unique id's (obs and l_set...........)  w.r.t the llset data
#
dat.dir <- "C:/Projects/DATA_2015/LL/"
#----------------------------------------------------------------------------------------------
# set by set data
#---------------------------------------------------------------------------------------------
llset      <- read.csv(paste(dat.dir, "ll_shark_SET_non_HWOB.csv", sep='') ,  header=T,  stringsAsFactors =F); nrow(llset) #43554
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
old.AU.catch.names <- c("trip_id","l_set_id","cat_date","cat_time","sp_code","sp_categor","hk_bt_flt","hook_no","condition_",
"condition2","fate_code","len","len_code","sex_code")
head(llset_AU)
cbind( names(llset), names(llseth), names(llset_NZ), names(llset_AU))
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
sets[sets$l_set_id %in%  c(114473 ,  114474 ,  114476 ,  114478 ,  114479),]
sets  %<>% filter(!duplicated(l_set_id))
#

#str(sets )
head(sets); dim(sets) # ] 102311     36 on  11 JULY2015
rownames(sets) <- as.character(sets$l_set_id) # adding l_set_id as table index
tail(sets)


#
#
#----------------------------------------------------------------------------------------------
# catch specific data
#----------------------------------------------------------------------------------------------

llcatch      <- read.csv(paste(dat.dir,  "ll_shark_catch_non_HWOB.csv",sep=""),header=T,  stringsAsFactors =F); dim(llcatch) # 464762     13
llcatch_HW   <- read.csv(paste(dat.dir,  "ll_shark_catch_HWOB.csv",sep=""),    header=T,  stringsAsFactors =F); dim(llcatch_HW) # 416138     13
llcatch_NZ   <- read.csv(paste(dat.dir,  "ll_shark_catch_NZOB_2012_2014.csv",sep=""),header=T,  stringsAsFactors =F); dim(llcatch_NZ) # 36690    13
llcatch_NZ$obstrip_id <- llcatch_NZ$obstrip_id +1e6
llcatch_NZ$l_set_id   <- llcatch_NZ$l_set_id +1e6

llcatch_AU   <- read.csv(paste(dat.dir,  "l_shark_catch_AUOB_2010_2013.txt",sep=""),  header=F,  stringsAsFactors =F); dim(llcatch_AU)  # 5362   13
      # head(llcatch_AU); dim(llcatch_AU)
       # llset_AU is named just a bit different ; trip_id, not obstrip_id
      names(llcatch_AU ) <- names( llcatch  )

catch <- rbind(  llcatch,  llcatch_HW,  llcatch_NZ,  llcatch_AU  )

head(catch); dim(catch) #  dim of 922952     13    on 13 JULY 2015


#
# okay so this is 'final data'
#

#

##########################################################################
#################### Effort/set information ##############################
##########################################################################
## Data format
# Things that should be numeric
#  careful, this fucked up the lat and lon bc no as.character....
# x <- c("set_start_time","set_end_time","haul_start_time",
#        "lat1d","lon1d","hk_bt_flt","hook_set","hook_est",
#        "lightsticks","bask_set","bask_observed","nbshark_lines")
# sets[,x] %<>% sapply(as.numeric)

sets$set_start_time <- as.numeric(as.character( sets$set_start_time ))
sets$set_end_time <- as.numeric(as.character( sets$set_end_time ))
sets$haul_start_time <- as.numeric(as.character( sets$haul_start_time ))
sets$lat1d <- as.numeric(as.character( sets$lat1d ))
sets$lon1d <- as.numeric(as.character( sets$lon1d ))
#
sets$hk_bt_flt <- as.numeric(as.character( sets$hk_bt_flt ))
sets$hook_set <- as.numeric(as.character( sets$hook_set ))
sets$hook_est <- as.numeric(as.character( sets$hook_est ))
sets$lightsticks <- as.numeric(as.character( sets$lightsticks ))
#
sets$bask_set <- as.numeric(as.character( sets$bask_set ))
sets$bask_observed <- as.numeric(as.character( sets$bask_observed ))
sets$nbshark_lines <- as.numeric(as.character( sets$nbshark_lines ))



# remove duplicates
print(nrow(sets))
sets  %<>% filter(!duplicated(l_set_id)) ;
print(nrow(sets)) #   102311



### Initial data cleaning

#
## Define fisheries:
# Add AS to M2
# Remove VU fishing in VU from M2
# Only CN and FM flagged vessels in fishery 1
# x <- sets[sets$fishery=='M1_FM',]
sets$fishery %<>% as.character
sets$fishery[sets$eez_code=="AS"] <- "M2_FV"    # then exclude Vanuatu ...
sets$fishery[sets$eez_code=="VU" & sets$flag != "FJ"] <- "MD_NA"    # then exclude Vanuatu ...
sets$fishery[sets$fishery=="M1_FM" & !(sets$flag %in% c('FM','CN'))] <- "MD_NA"    # Only CN and FM flagged vessels in fishery 1

# only data sets we are working with (chosen for Bruno and Carl because of fishery features)
# setdat <- sets[sets$fishery %in% c('M1_FM','M2_FV','M7_HD'),]

# extract year and month
sets$yy <- as.numeric(substring(sets$set_start_date,1,4))
sets$mm <- as.numeric(substring(sets$set_start_date,5,6))
    #table(sets$yy, sets$program_code, useNA='ifany')


# a simple relative soak time index (adjusting for overnight as needed)
sets$soak <-  with(sets, haul_start_time-set_start_time +
                         ifelse(haul_start_time > set_start_time, 0, 24))


# switch NA fields to 'no' for lightsticks and shark lines
sets$lightsticks[is.na(sets$lightsticks)] <- 0
sets$nbshark_lines[is.na(sets$nbshark_lines)] <- 0

head(sets); dim(sets)
# Was shark bait used at all on the set no-0 or yes-1
# Set to true if any of the bait variables match a value in shkbait
# (May 2015: less than 0.5% = 1)
shkbait <- unlist(read.csv(paste0(dat.dir,"sharkbait.csv"))) # code for bait used for shark
baitcols <- sprintf("bait%s_sp_code",1:5) # columns with bait info
sets$sharkbait <- as.numeric(apply(apply(sets[,baitcols],2, "%in%", shkbait),1,any))

# filtering out missing or inconsistent data
# get rid of data with NA's in critical fields  - hk_bt_flt and hook_est   not using this is.na(sets$hook_set) |
 sets <- sets[!(is.na(sets$hk_bt_flt) | is.na(sets$hook_est) |   is.na(sets$lon1d)),]
#sets <- sets[sets$sharktarget=="N",] # no shark targeting
dim(sets) #90954    39

# get rid of records where hook_set!=hook_est  not doitn this bc hook est is sometimes th only one filled in or different if some of the line gets tangled
#sets <- sets[sets$hook_set==sets$hook_est,]
dim(sets)   #  84424    39
# less than 40 hbf and at least five and at least 1000 hooks set
#sets <- sets[sets$hook_set >= 1000 & sets$hk_bt_flt <= 40 & sets$hk_bt_flt >= 5,]
dim(sets) # 66012
# switch negative values for longitude data
sets$lon1d %<>% "+"(ifelse(sets$lon1d<0, 360, 0))
dim(sets) # dim(sets)
tail(sets)

sets$lat5 <-  floor(sets$lat1d  /5)*5 +2.5
sets$lon5 <- floor(sets$lon1d /5)*5 +2.5
  table(sets$lat5, sets$lon5)   #
#
sets$cell <- as.character(paste(round(sets$lat5),round(sets$lon5),sep=""))     #note that 2.5 rounds to 2 and 7.5 rounds to 8
sets$cell<- ifelse(nchar(sets$cell)==5 & substr(sets$cell,1,2)=="-2",paste("-02",substr(sets$cell,3,5),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==4 & substr(sets$cell,1,1)=="2",paste("02",substr(sets$cell,2,4),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==5 & substr(sets$cell,1,2)=="-8",paste("-08",substr(sets$cell,3,5),sep=""),sets$cell)
sets$cell<- ifelse(nchar(sets$cell)==4 & substr(sets$cell,1,1)=="8",paste("08",substr(sets$cell,2,4),sep=""),sets$cell)
sets$cell <- as.factor(sets$cell)
table(sets$cell)
head(sets)
dim(sets)


###################################################################################  NOW GROOM THE CATCH DATA
# Things that should be numeric
#x <- c("catch_time","hk_bt_flt","hook_no")
#catch[,x] %<>% sapply(as.numeric)

   catch$catch_time <- as.numeric(as.character(catch$catch_time   ))
   catch$hk_bt_flt <- as.numeric(as.character( catch$hk_bt_flt   ))
   catch$hook_no <- as.numeric(as.character( catch$hook_no   ))


# individual species to include
sp <- c('FAL','OCS','BSH') # silky, oceanic whitetip , blue
THR <- c('THR','BTH','PTH','ALV')
MAK <- c('MAK','SMA','LMA')
HHD <- c('SPN','SPZ','SPL','SPK','EUB')

# make my own categories using sp_category
# catch$sp_category[catch$sp_code %in% sp] <- catch$sp_code[catch$sp_code %in% sp]
# catch$sp_category[catch$sp_code %in% THR] <- "THR"
# catch$sp_category[catch$sp_code %in% MAK] <- "MAK"
# catch$sp_category[catch$sp_code %in% HHD] <- "HHD"

catch$sp_category <- as.character(catch$sp_category )
catch$sp_code <- as.character(catch$sp_code)
catch$sp_category  <- ifelse(catch$sp_code %in% sp, catch$sp_code, catch$sp_category)
catch$sp_category  <- ifelse(catch$sp_code %in% MAK, "MAK", catch$sp_category)
catch$sp_category  <- ifelse(catch$sp_code %in% THR, "THR", catch$sp_category)
catch$sp_category  <- ifelse(catch$sp_code %in% HHD, "HHD", catch$sp_category)
catch$sp_category  <- ifelse(catch$sp_code %in% "SKJ", "SKJ", catch$sp_category)
catch$sp_category  <- ifelse(catch$sp_code %in% "POR", "POR", catch$sp_category)
table(catch$sp_category)
str(catch)


#table(catch[catch$sp_code %in% HHD, "sp_code"]) # spn is most common so use that for the conversion factors?

# now aggregate A categories
catch$condition_use <- catch$condition_land
catch$condition_use[catch$condition_use %in% c('A0','A1','A2','A3')] <- 'A'

# Now get rid of shark data we don't need!
   #catchdat <- catch[catch$sp_category!="SHK",]
a <- dim(catch)[1]
# Only use sets still remaining in the set data
catch  <- filter(catch, l_set_id %in% sets$l_set_id) #
     a - dim(catch)[1] # num lost


# Let's sort out hook position in the basket - if greater than hpb then set to NA
catch$hook_no[catch$hook_no==99] <- 0
catch$hook_no[catch$hook_no > catch$hk_bt_flt] <- NA # check meaning hook_no = 0

# standardise against middle of the basket
catch$hook_pos <- with(catch, ifelse(hook_no <= (hk_bt_flt/2),
                            hook_no, hk_bt_flt-hook_no+1))
head(catch)
#########################################################
main.sharks <- c("BSH","FAL","HHD","MAK","OCS","POR","THR","SHK","SKJ")
# blue shark and mako are split in south and north stocks, porbeagles only found in south
sets[,main.sharks] <- 0

# use tapply to get l_set_id as index
start.timer()
lx <- lapply(main.sharks, function(ssp) {
                 ssp.ind <- with(filter(catch, sp_category==ssp), tapply(sp_category, l_set_id, table));
                 message(sprintf("Adding catch to 'sets' for %s (%s individuals)", ssp, sum(ssp.ind)));
                 sets[names(ssp.ind),ssp] <<- ssp.ind})
stop.timer()

tail(sets);dim(sets)
sets <- sets[!is.na(sets$l_set_id),]


# create CPUE

scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE", "HHDCPUE", "PORCPUE")
sets[,scpue] <- 0; head(sets[,scpue])
sets[,scpue] <- sets[,spec] /(sets[,"hook_est"] /1000)




# old, not quite right, way of adding the catch to sets
#       # FAL
#       #
#       tx <- with(catch[catch$sp_category=="FAL",], tapply(sp_category, l_set_id, table))
#       txx<- as.matrix(tx);  sum(txx)
#       FAL <- as.data.frame( cbind( l_set_id=as.numeric(rownames(txx)), FAL=txx), nrow=length(txx), byrow=F , dimnames=list(NULL, c("l_set_id", "FAL")))
#       colnames(FAL) <- c("l_set_id", "FAL")
#       #  FAL[1:20,]
#       sets$FAL<-0
#       #
#       pntr <- match( FAL$l_set_id, sets$l_set_id) # sum(is.na(pntr))
#       sets$FAL[pntr] <- FAL$FAL[pntr]
#       head(sets)
#       rm(tx, txx, FAL)
#--------------------------------------------------------------------------------------
table(sets$lat1d)

# define regions for analysis:
sets$region<- rep(0,length(sets$obstrip_id))
sets$region <- ifelse(sets$lat1 >= 20 & sets$lat1 <= 50 & sets$lon1 >= 120 & sets$lon1 < 180, 1, sets$region)
sets$region <- ifelse(sets$lat1 >= 20 & sets$lat1 <= 50 & sets$lon1 >= 180 & sets$lon1 < 210, 2, sets$region)
sets$region <- ifelse(sets$lat1 >= -10 & sets$lat1 < 20 & sets$lon1 >= 120 & sets$lon1 < 170, 3, sets$region)
sets$region <- ifelse(sets$lat1 >= -10 & sets$lat1 < 20 & sets$lon1 >= 170 & sets$lon1 < 210, 4, sets$region)
sets$region <- ifelse(sets$lat1 >= -10 & sets$lat1 < -4 & sets$lon1 >= 210 & sets$lon1 < 230, 4, sets$region)
sets$region <- ifelse(sets$lat1 >= -40 & sets$lat1 < -10 & sets$lon1 >= 141 & sets$lon1 < 170, 5, sets$region)
sets$region <- ifelse(sets$lat1 >= -55 & sets$lat1 < -40 & sets$lon1 >= 141 & sets$lon1 < 150, 5, sets$region)
sets$region <- ifelse(sets$lat1 >= -60 & sets$lat1 < -40 & sets$lon1 >= 150 & sets$lon1 < 170, 5, sets$region)
sets$region <- ifelse(sets$lat1 >= -60 & sets$lat1 < -10 & sets$lon1 >= 170 & sets$lon1 < 230, 6, sets$region)
sets <- sets[sets$region > 0,]
sets <- sets[!is.na(sets$region),]
dim(sets[sets$region!=0,])
tail(sets)
nrow(sets)


a <- dim(catch)[1]
# Only use sets still remaining in the set data
sets<- sets[sets$yy %in% 1995:2014,]

catch  <- filter(catch, l_set_id %in% sets$l_set_id) #
dim(catch)



########################################################


# Saving cleaned versions of catch and sets for analysis
  save(sets, catch,  file=paste0(dat.dir, "lldata_11JULY2015.rdata"))

########################################################
#table(sets$FAL>0)
#plot(sets$lon1d, sets$lat1d, col=mygrey, pch=16)


# scrap below.

##################################################
# JUST SELECT THE VARIABLES THAT WE NEED TO GIVE TO Carl
# [1] "obstrip_id"      "program_code"    "flag"            "fishery"         "vessel_id"       "l_set_id"        "set_start_date"  "set_start_time"  "set_end_time"    "haul_start_date" "haul_start_time" "lat1d"
#[13] "lon1d"           "eez_code"        "tar_sp_code"     "target_tun_yn"   "target_swo_yn"   "target_shk_yn"   "hk_bt_flt"       "hook_set"        "hook_est"        "lightsticks"     "bask_set"        "bask_observed"
#[25] "nbshark_lines"   "bait1_sp_code"   "bait2_sp_code"   "bait3_sp_code"   "bait4_sp_code"   "bait5_sp_code"   "wire_trace"      "hook_type"       "sharktarget"     "sharkbait"       "moonfrac"        "sst"
#[37] "soak"


setvar <- c("fishery","l_set_id","obstrip_id","vessel_id","flag",
                          "lat1d","lon1d","eez_code","set_start_date",
                          "yy","mm","set_start_time","soak","hk_bt_flt","hook_set",
                          "bait1_sp_code","wire_trace","hook_type",
                          "sharkbait","nbshark_lines")

set_dmp <- setdat[,setvar]



              #> names(catchdat)
              # [1] "obstrip_id"        "l_set_id"          "catch_time"        "sp_code"           "sp_category"       "hk_bt_flt"         "hook_no"           "condition_land"    "condition_release" "fate_code"         "condition_use"
              #[12] "hook_pos"
              #>
              catvar <- c("l_set_id","obstrip_id","sp_category",
                          "hk_bt_flt","hook_no","hook_pos",
                          "condition_land","condition_use")
              #catch_dmp <- catchdat[,catvar]



              ######################## set_dmp.Rdata
              set.variables <- c(
              "fishery",          #Three fisheries defined to be analayzed separately
              "l_set_id",         # Unique identify to the longline set
              "obstrip_id",       # Unique identifier to the fishing trip
              "vessel_id",        # unique identifier to the vessel
              "flag",             # flag of the vessel
              "lat1d",            # latitude start of set at 1 degree res in decimal degrees
              "lon1d",            # longitude start of set at 1 degree res in decimal degrees
              "eez_code",         # approximate EEZ for start of set
              "set_start_date",   # long character with set start data yyyymmdd
              "yy",               # year of set
              "mm",               # month of set
              "set_start_time",   # decimal hour start of set
              "soak",             # approximate soak time in decimal hours - time between start of set and start of haul
              "tar_sp_code",      # target species reported for set - unsure if actually useful
              "hk_bt_flt",        # hooks between floats - the greater the number the deeper the hooks can go
              "hook_set",         # number of hooks used on the set
              "bait1_sp_code",    # main bait used - not sure if useful - to many categories and a bit of a mess
              "wire_trace",       # wire trace used n/y 0/1    - key variable
              "hook_type",        # hook type circle or J shaped - key variable - would love an interaction with wire_trace, but unlikely contrast
              "sharkbait",         # was any of the bait used at all considered bait used to target sharks (rather than tuna)
              "nbshark_lines")    # did they have lines off the floats specifically trying to catch sharks - hook_pos=0 in the catch_dmp file



              ######################## catch_dmp.Rdata
              catch.variables <- c(
              "l_set_id",         # Unique identify to the longline set
              "obstrip_id",       # Unique identifier to the fishing trip
              "sp_category",      # These are the species of species groups to analyze separately
              "hk_bt_flt"         # hooks between floats - the greater the number the deeper the hooks can go
              ,"hook_no",         # the number of the hook between the floats
              "hook_pos",         # the position of the hook compared to its nearest float
              "condition_land",   # the detailed info on condition at the side of the boat - don't use
              "condition_use")    # sumamrised down to alive, dead, or unknown






## skj_cpue-2015_data-prep.r
## Reading in already extracted data from observer database
## Formatting for analysis SKJ in S Pacific
## -------------------------------------------------------
## Author: J Rice (based on Code from LTB)
## Written on: 30:06:2015

options(stringsAsFactors=FALSE)
require(dplyr)
require(magrittr)

skjdir <- "C:/Projects/SHK-indicators-2015/"
source("get-sunrise-sunset.r")
#dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
setwd(skjdir)

# new data on 3 July 2015
# note that NZ (recent years) has to get added (rbind) as does the AUS data, but that has no location data....
# also the NZ data has non-unique id's (obs and l_set...........)  w.r.t the llset data

#### Summary of variables ####
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

catch.variables <- c(
    "l_set_id",         # Unique identify to the longline set
    "obstrip_id",       # Unique identifier to the fishing trip
    "sp_category",      # These are the species of species groups to analyze separately
    "hk_bt_flt",         # hooks between floats - the greater the number the deeper the hooks can go
    "hook_no",         # the number of the hook between the floats
    "hook_pos",         # the position of the hook compared to its nearest float
    "condition_land",   # the detailed info on condition at the side of the boat - don't use
    "condition_use")    # sumamrised down to alive, dead, or unknown

#----------------------------------------------------------------------------------------------
# Importing set data
#---------------------------------------------------------------------------------------------
message("Loading SPC observer program data...")
llset      <- read.csv(paste(skjdir,  "DATA/ll_shark_SET_non_HWOB.csv",sep=""),header=T,  stringsAsFactors =F)
message(sprintf("%s sets", nrow(llset)))

message("Loading Hawaii...")
llseth     <- read.table(paste0(skjdir,"DATA/ll_shark_SET_HWOB_13_July_2015.csv"), sep=",", header=T,  stringsAsFactors =F)
# Hawaii data has weird issues with duplicated sets having changing haul_start_date. Remove
# that column from data
# llseth <- llseth[,!(names(llseth)=="haul_start_date")] %>% unique
message(sprintf("%s sets", nrow(llseth)))

message("Loading New Zealand...")
llset_NZ   <- read.csv(paste(skjdir,  "DATA/ll_shark_SET_NZOB_2012_2014.csv",sep=""),header=T,  stringsAsFactors =F)
message(sprintf("%s sets", nrow(llset_NZ)))
# bc NZ is separately managed, we need specific l_set_id and obstrip_id numbers to ensure no overlap with SPC's
llset_NZ$obstrip_id <- llset_NZ$obstrip_id +1e6  # adding 1 million
llset_NZ$l_set_id <- llset_NZ$l_set_id +1e6


message("Loading Australia...")
llset_AU   <- read.table(paste0(skjdir,"DATA/l_shark_set_AUOB_2010_2013.txt"),
                         sep=",", header=FALSE, stringsAsFactors=F)
names(llset_AU) <- names(llset)
conv.AU.time <- function(x) {
    hr <- as.numeric(substring(x,1,2))
    mn <- as.numeric(substring(x,3,4))/60
    hr+mn
}
llset_AU$set_start_time %<>% conv.AU.time

message(sprintf("%s sets", nrow(llset_AU)))

# compare set names:
# cbind( names(llset), names(llseth), names(llset_NZ), names(llset_AU))
# llset_AU is named just a bit different ; trip_id, not obstrip_id
sets    <- rbind(llset, llseth, llset_NZ, llset_AU) # get some warnings
message(sprintf("Collating all sets dataframes, nrow = %s", nrow(sets)))
sets %<>% filter(!duplicated(l_set_id ))
sets %<>% filter(!is.na(l_set_id))
message(sprintf("Removed duplicated l_set_id, nrow = %s", nrow(sets)))

message("Setting l_set_id as table key")
rownames(sets) <- as.character(sets$l_set_id) # adding l_set_id as table index
# setting ID fied for US data given partial data coverage
sets$us.pg <- ifelse(sets$program_code %in% c("HWOB","ASOB"),"US","OTH")
#
#----------------------------------------------------------------------------------------------
# Importing catch data: one record per individual caught
#----------------------------------------------------------------------------------------------

llcatch      <- read.csv(paste(skjdir,  "DATA/ll_shark_catch_non_HWOB.csv",sep=""),header=T,  stringsAsFactors =F)
llcatch_HW   <- read.csv(paste(skjdir,  "DATA/ll_shark_catch_HWOB_13_july_2015.csv",sep=""),    header=T,  stringsAsFactors =F)
llcatch_NZ   <- read.csv(paste(skjdir,  "DATA/ll_shark_catch_NZOB_2012_2014.csv",sep=""),header=T,  stringsAsFactors =F)
llcatch_NZ$obstrip_id <- llcatch_NZ$obstrip_id +1e6
llcatch_NZ$l_set_id   <- llcatch_NZ$l_set_id +1e6

llcatch_AU   <- read.csv(paste(skjdir,  "DATA/l_shark_catch_AUOB_2010_2013.txt",sep=""),  header=T,  stringsAsFactors =F)
message("Loaded four data files for catch.")
      #
      llcatch_AU <- llcatch_AU[ ,-3]
      names(llset_AU) <- names(llset)
      # llset_AU is named just a bit different ; trip_id, not obstrip_id
      names(llcatch_AU ) <- names( llcatch  )
catch <- rbind(  llcatch,  llcatch_HW,  llcatch_NZ,  llcatch_AU  )

##########################################################################
#################### Effort/set information ##############################
##########################################################################
## Data format
# Things that should be numeric
x <- c("set_start_time","set_end_time","haul_start_time",
       "lat1d","lon1d","hk_bt_flt","hook_set","hook_est",
       "lightsticks","bask_set","bask_observed","nbshark_lines")
sets[,x] %<>% sapply(as.numeric)

### Initial data cleaning
## Define fisheries:
# Add AS to M2
# Remove VU fishing in VU from M2
# Only CN and FM flagged vessels in fishery 1
# x <- sets[sets$fishery=='M1_FM',]
sets$fishery %<>% as.character
sets$fishery[sets$eez_code=="AS"] <- "M2_FV"    # then exclude Vanuatu ...
sets$fishery[sets$eez_code=="VU" & sets$flag != "FJ"] <- "MD_NA"    # then exclude Vanuatu ...
sets$fishery[sets$fishery=="M1_FM" & !(sets$flag %in% c('FM','CN'))] <- "MD_NA"    # Only CN and FM flagged vessels in fishery 1

# extract year and month
sets$yy <- as.numeric(substring(sets$set_start_date,1,4))
sets$mm <- as.numeric(substring(sets$set_start_date,5,6))
sets$date <- as.Date(strptime(sets$set_start_date, "%Y%m%d"))
sets <- sets[,names(sets)!="set_start_date"] # remove set_start_date column

# a simple relative soak time index (adjusting for overnight as needed)
sets$soak <-  with(sets, haul_start_time-set_start_time +
                         ifelse(haul_start_time > set_start_time, 0, 24))

# switch NA fields to 'no' for lightsticks and shark lines
sets$lightsticks[is.na(sets$lightsticks)] <- 0
sets$nbshark_lines[is.na(sets$nbshark_lines)] <- 0

# Was shark bait used at all on the set no-0 or yes-1
# Set to true if any of the bait variables match a value in shkbait
# (May 2015: less than 0.5% = 1)
shkbait <- unlist(read.csv(paste0(skjdir,"/DATA/sharkbait.csv"))) # code for bait used for shark
baitcols <- sprintf("bait%s_sp_code",1:5) # columns with bait info
sets$sharkbait <- as.numeric(apply(apply(sets[,baitcols],2, "%in%", shkbait),1,any))

# filtering out missing or inconsistent data
# get rid of data with NA's in critical fields  - hk_bt_flt and hook_est
sets <- with(sets,sets[!(is.na(hk_bt_flt) | is.na(hook_set) | is.na(hook_est) | is.na(lon1d) | is.na(set_start_time)),])
message(sprintf("Removing NA values in HBF, hook_set, hook_est, lon1d... %s sets left", nrow(sets)))
sets <- sets[sets$sharktarget=="N",] # no shark targeting
message(sprintf("Removing sets declaring sharks as target... %s sets left", nrow(sets)))
# get rid of records where hook_set!=hook_est
sets <- sets[sets$hook_est<=sets$hook_est,]
message(sprintf("Removing sets where hook_set<=hook_est... %s sets left", nrow(sets)))

# less than 40 hbf and at least five and at least 1000 hooks set
sets <- sets[sets$hook_set >= 1000,]
message(sprintf("Only keeping sets with hook_set>1000... %s sets left", nrow(sets)))
sets <- sets[sets$hk_bt_flt <= 40 & sets$hk_bt_flt >= 5,]
message(sprintf("Only keeping sets with HBF between 5 and 40... %s sets left", nrow(sets)))
# switch negative values for longitude data
sets$lat1d <- floor(sets$lat1d)+0.5
sets$lon1d <- floor(sets$lon1d)+0.5
sets$lon1d %<>% "+"(ifelse(sets$lon1d<0, 360, 0))
sets$lat5 <-  5*floor(sets$lat1d/5) +2.5
sets$lon5 <- 5*floor(sets$lon1d/5) +2.5
sets$cell <- paste(sets$lon5, sets$lat5, sep="_")

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

nr <- nrow(sets)
sets <- sets[sets$region > 0,]
message(sprintf("Removed %s set records outside of regions, %s rows left", nr-nrow(sets),nrow(sets)))

# add extra fields for analyses downstream
sets$loghook <- log(sets$hook_est)
sets$HPBCAT <- "S"
sets$HPBCAT[sets$hk_bt_flt >10] <- "D"
sets$HPBCAT2 <- "S"
sets$HPBCAT2[sets$hk_bt_flt %between% c(10.1, 15)] <- "I" # id intermediate set depth based on patterns seen in data exploration
sets$HPBCAT2[sets$hk_bt_flt >15] <- "D"

#########################################################
message("\nFiltering analysis for years from 1995 to 2014")
sets <- sets[sets$yy %in% 1995:2014,]

###################################################################################
###################################################################################
###################################################################################
# CLEANING CATCH DATA
# Only keeping catch records in 'sets'
catch %<>% filter(l_set_id %in% sets$l_set_id)

# Things that should be numeric
catch.num <- c("catch_time","hk_bt_flt","hook_no")
catch[,catch.num] %<>% sapply(as.numeric)

# individual species to include
sp <- c('FAL','OCS','BSH','POR') # silky, oceanic whitetip, blue
THR <- c('THR','BTH','PTH','ALV') # threshers
MAK <- c('MAK','SMA','LMA') # makos
HHD <- c('SPN','SPZ','SPL','SPK','EUB') # hammerheads
shkcat.index <- c(sp, rep("THR",length(THR)), rep("MAK",length(MAK)), rep("HHD",length(HHD)),"SHK","SKJ")
names(shkcat.index) <- c(sp,THR,MAK,HHD,"SHK","SKJ") # SHK is others, SKJ is... skipjack (kept for other analysis)

# assign to categories as defined above
catch$sp_category <- shkcat.index[match(catch$sp_code, names(shkcat.index))] # faster
message(sprintf("Assigning these species to sp_category 'SHK':\n %s",
                paste(unique(catch[(is.na(catch$sp_category)),"sp_code"]), collapse=", ")))

# Format condition_use categories A
catch$condition_use <- catch$condition_land
catch$condition_use[catch$condition_use %in% c('A0','A1','A2','A3')] <- 'A'
# A is: XXXX, D is discarded , U is unknown

# Only keep sets still remaining in the set data
nr <- dim(catch)[1]
catch  <- filter(catch, l_set_id %in% sets$l_set_id)
message(sprintf("Filtered catch to only retaine l_set_id in 'sets', removed %s rows", nr-nrow(catch)))

# Let's sort out hook position in the basket - if greater than hpb then set to NA
catch$hook_no[catch$hook_no==99] <- 0
catch$hook_no[catch$hook_no > catch$hk_bt_flt] <- NA # check meaning hook_no = 0

# standardise against middle of the basket
catch$hook_pos <- with(catch, ifelse(hook_no <= (hk_bt_flt/2),
                            hook_no, hk_bt_flt-hook_no+1))


#########################################################
## Adding SHK species specific catch from 'catch' to 'sets'
main.sharks <- c("BSH","FAL","HHD","MAK","OCS","POR","THR","SHK","SKJ") # trouvez l'erreur
# blue shark and mako are split in south and north stocks, porbeagles only found in south
sets[,main.sharks] <- 0
print(nrow(sets))
# use tapply to get l_set_id as index
start.timer()
lx <- lapply(main.sharks, function(ssp) {
                 ssp.ind <- with(filter(catch, sp_category==ssp), tapply(sp_category, l_set_id, table));
                 message(sprintf("Adding catch to 'sets' for %s (%s individuals)", ssp, sum(ssp.ind)));
                 sets[names(ssp.ind),ssp] <<- ssp.ind})
stop.timer()
print(nrow(sets))

# add time/sunrise-sunset information
sets.ss <- get.ss.info(sets)
sets %<>% inner_join(sets.ss)
rownames(sets) <- sets$l_set_id

########################################################
# Saving cleaned versions of catch and sets for analysis
message(sprintf("Saving cleaned datasets for catch (%s rows) and sets (%s rows)",
                nrow(catch), nrow(sets)))
save(sets, catch,  file=paste0(skjdir, "DATA/SHK-obsv-LL_catch-sets.RData"))

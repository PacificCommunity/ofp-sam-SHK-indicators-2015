#
#
#  Trying to resolve issues with the catch vs shk bio
#
#
#
#
source("C:/Projects/SHK-indicators-2015/CODE/1_ind_analysis_preamble.r")
#
#object name is shkbio; note that the length processing is done here, not in the data processing file
# load(file="C:/Projects/SHK-indicators-2015_backup/DATA/ll_obs_bio_280615_processed_allsharks.rdata" )
# load( file=paste0(dat.dir, "lldata_11JULY2015.rdata"))


dat.dir <- "C:/Projects/DATA_2015/"

load(  file="C:/Projects/DATA_2015/LL/ll_obs_CATCH_11JULY_processed.rdata" )  
load(  file=paste0(dat.dir, "lldata_16JULY2015.rdata"))

#load("C:/Projects/DATA_2015/LL/lldata_11JULY2015.rdata") # loads

head(catch)
with(catch[catch$sp_category=="BSH" & catch$sex_code%in%c("F","M"), ], table(yy,  region, sex_code) )


head( catch[catch$sp_category=="BSH" & catch$sex_code%in%c("F","M") & catch$region==2, ])

with(shkbio[shkbio$sp_id=="BSH" & shkbio$sex_id%in%c("F","M"), ], table(yy,  region, sex_id) )
#0---------------------------------------------------------------------
# what appears to be missing from the 'catch' data is
temp <-   shkbio[shkbio$sp_id=="BSH" & shkbio$sex_id %in% c("F","M") & shkbio$region==2,    ]
ldhw <- unique(temp$obstrip_id); length(oldhw)

llseth[llseth$obstrip_id %in% oldhw,]

head(sets[sets$obstrip_id %in% oldhw,])
testw <- shkbio[shkbio$obstrip_id%in%oldhw,]
dim(testw)
write.csv( shkbio[shkbio$obstrip_id%in%oldhw,],   file = "C:/Projects/DATA_2015/LL/sample_missing_LL.csv", row.names =FALSE)
 #-------------------------------------------------so missing some


head(catch)
head(sets)
399377
head(temp)
table(temp$yy, temp$region)


with(catch[catch$sp_category=="BSH"  & catch$program_code=="HWOB", ], table(yy,region, sex_code) )

with(shkbio[shkbio$sp_id=="BSH" & shkbio$sex_id%in%c( "M") & shkbio$region==2, ], table(yy,  sex_id) )

with(shkbio[shkbio$sp_id=="BSH" & shkbio$sex_id%in%c( "M") & shkbio$region==2, ], table(yy,  sex_id) )

llcatch_HW[llcatch_HW$obstrip_id==oldhw, ]
head(catch)
with(catch[catch$sp_code=="BSH"& catch])


#  
length(shkbio$obstrip_id )
length(catch$obstrip_id )
# on a hunch, grab all the HWOB data for sharks prior to 2003
miss <- shkbio[shkbio$obs_prg_id == "HWOB" & shkbio$sp_cat_id=='SHK' & shkbio$yy<2003, ]
nrow(miss)
head(catch); head(miss)

miss$condition_use <- miss$cond_id
miss$condition_use[miss$condition_use %in% c('A0','A1','A2','A3')] <- 'A'
str(miss)
miss$hook_no<- as.numeric(as.character(miss$hook_no))
miss$hk_bt_flt<- as.numeric(as.character(miss$hk_bt_flt))

miss$hook_pos <- with(miss, ifelse(hook_no <= (hk_bt_flt/2),
                                     hook_no, hk_bt_flt-hook_no+1))

mycolumn <-  c(  "obstrip_id", "l_shaul_id" , "ctime","sp_id"  , "group", "hk_bt_flt", "hook_no",  "cond_id",  "cond_2_id","fate_id", "len", "len_id", "sex_id" ,  "condition_use",  "hook_pos", "yy", "mm",   "lat1", "lon1", "flag_id" , "region", "ez_id","TL_flag1" , "TL_flag", "convFL", "hemi", "HPBCAT", "retained", "escaped", "discardednf", "finned", "alivestart", "healthystart", "deadstart", "uidcondstart", "aliveend", "healthyend", "deadend", "uidcondend", "obs_prg_id")

cbind(mycolumn, names(catch) )
# match(mycolumn, names(miss))

miss2 <- miss [ , mycolumn]
names(miss2) <- names(catch)

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

miss2$sp_category <- as.character(miss2$sp_category )
miss2$sp_code <- as.character(miss2$sp_code)
miss2$sp_category  <- ifelse(miss2$sp_code %in% sp, miss2$sp_code, miss2$sp_category)
miss2$sp_category  <- ifelse(miss2$sp_code %in% MAK, "MAK", miss2$sp_category)
miss2$sp_category  <- ifelse(miss2$sp_code %in% THR, "THR", miss2$sp_category)
miss2$sp_category  <- ifelse(miss2$sp_code %in% HHD, "HHD", miss2$sp_category)
miss2$sp_category  <- ifelse(miss2$sp_code %in% "SKJ", "SKJ", miss2$sp_category)
miss2$sp_category  <- ifelse(miss2$sp_code %in% "POR", "POR", miss2$sp_category)
table(miss2$sp_category)


catch2  <- rbind(catch, miss2)

save(catch2,  file="C:/Projects/DATA_2015/LL/reconciled_catch.rdata" ) 
nrow(catch); nrow(catch2)
with(catch2[catch2$sp_category=="BSH"  & catch2$program_code=="HWOB", ], table(yy,region, sex_code) )

with(miss2[miss2$sp_category=="BSH"  & miss2$program_code=="HWOB", ], table(yy,region, sex_code) )

table(miss$group, miss$region, miss#)


  
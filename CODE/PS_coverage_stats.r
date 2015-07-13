


# summary data from PS operational data
#  and maybe some plots
#
results <- c()

psop <- read.csv("C:/Projects/DATA_2015/logbook/ps_op_data_july_2015.txt", header=F, stringsAsFactors =F)
psop_fields <-  read.csv("C:/Projects/DATA_2015/logbook/2 - ps_operational_fields.txt", header=F, stringsAsFactors =F)
psop_fields[13] <- "shk_c"
colnames(psop) <- psop_fields
str(psop); dim(psop)
psop<- psop[psop$yy %in% 1995:2014, ]
psop  %<>% filter(!duplicated(set_id)) ; nrow(psop) # 
100* sum(psop$shk_n>0)/nrow(psop) #  0.0122848
100* sum(psop$shk_ret_or_disc>0)/nrow(psop) #  2.507099     ****should be the 'any interaction stat***** 
100* sum(psop$shk_c >0)/nrow(psop) #  0.01585596
100* sum(psop$shk_disc >0)/nrow(psop) # 0.1214196
100* sum(psop$shk_ret >0)/nrow(psop) # 2.389108
#_-----------------------------
results <- c(results, paste0("Percent of Logbook that records any shark ineraction ", 100* sum(psop$shk_ret_or_disc>0)/nrow(psop)))
#_-----------------------------
psag <- read.table(  "C:/Projects/SHK-indicators-2015_backup/DATA/PS_AGG5X5_NSETS_CNTRY_SETTYPE.TXT", header=TRUE, sep=',', stringsAsFactors=FALSE)
head(psag)
psag <- psag[psag$yy %in% 1995:2014,]


dput(unique(psag$flag))
#drop the IP PH VT (and japan coastal?)
keepflag <- c("AU", "FM",   "JP", "KI", "KR", "NZ", "PG",   "SB",   "TW", "US", "VU", "FR", "ES", "MH",   "CN", "SV", "EC", "TV")
psag <- psag[psag$flag %in% keepflag,]
  nrow(psop)  /    sum(psag$sets)  # 87 is the logbook coverage for the time period
#_-----------------------------
results <- c(results, paste0("percent coverage of logsheets vs total sets ", 100* nrow(psop)  /    sum(psag$sets) )  )
#_-----------------------------

 
round( table(psop$yy) / round(tapply(psag$sets, psag$yy, sum)),2) #hm,  
#1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#0.74 0.86 0.82 0.84 0.85 0.80 0.90 0.90 0.86 1.05 0.86 0.88 0.88 0.97 0.94 0.93 0.81 0.87 0.82 0.79 


# check ps obs coverage by area, 

# first get rid of coastal japan
 length( )
psag <- psag[-which(psag$flag =="JP" & psag$latd>17.5 ),]; nrow(psag)

# and that southern stuff
 psag <- psag[ -which(psag$latd <= -22.5),]; nrow(psag)




# load observer data
psobs <- read.csv("C:/Projects/DATA_2015/PS/ps_obs_set_shk.csv", header=TRUE )
psobs <-psobs[psobs$yy %in% 1995:2014,]
round( table(psobs$yy) / round(tapply(psag$sets, psag$yy, sum)),2)
 

#1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#0.03 0.06 0.06 0.07 0.06 0.05 0.07 0.09 0.10 0.15 0.14 0.16 0.14 0.12 0.18 0.42 0.46 0.45 0.56 0.33 

round( table(psobs$yy) / round(tapply(psag$sets, psag$yy, sum)),2)






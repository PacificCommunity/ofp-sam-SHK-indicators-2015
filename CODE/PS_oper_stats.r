


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
  nrow(psop)  /    sum(psag$sets)  # 0.3937713 is the logbook coverage for the time period
#_-----------------------------
results <- c(results, paste0("percent coverage of logsheets vs total sets ", 100* nrow(psop)  /    sum(psag$sets) )  )
#_-----------------------------

 
round( table(psop$yy) / round(tapply(psag$sets, psag$yy, sum)),2) #hm, bout 32-50 in the last few years

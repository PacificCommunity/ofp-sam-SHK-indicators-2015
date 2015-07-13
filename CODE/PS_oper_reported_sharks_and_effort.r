



# Okay, look at the #   caught and reported by countries, by year on each region
# make 4 plots, 2 each for assoc and unassoc , total 
#
#
library(r4ss)


psop <- read.csv("C:/Projects/DATA_2015/logbook/ps_op_data_july_2015.txt", header=F, stringsAsFactors =F)
psop_fields <-  read.csv("C:/Projects/DATA_2015/logbook/2 - ps_operational_fields.txt", header=F, stringsAsFactors =F)
psop_fields[13] <- "shk_c"
colnames(psop) <- psop_fields
str(psop); dim(psop)
psop<- psop[psop$yy %in% 1995:2014, ]
psop  %<>% filter(!duplicated(set_id)) ; nrow(psop) # 
nrow(psop)  #700052
head(psop)
# process the psop data a bit
psop$region<- 0
psop$region <- ifelse(psop$lat1 >= 20 & psop$lat1 <= 50 & psop$lon1 >= 120 & psop$lon1 < 180, 1, psop$region)
psop$region <- ifelse(psop$lat1 >= 20 & psop$lat1 <= 50 & psop$lon1 >= 180 & psop$lon1 < 210, 2, psop$region)
psop$region <- ifelse(psop$lat1 >= -10 & psop$lat1 < 20 & psop$lon1 >= 120 & psop$lon1 < 170, 3, psop$region)
psop$region <- ifelse(psop$lat1 >= -10 & psop$lat1 < 20 & psop$lon1 >= 170 & psop$lon1 < 210, 4, psop$region)
psop$region <- ifelse(psop$lat1 >= -10 & psop$lat1 < -4 & psop$lon1 >= 210 & psop$lon1 < 230, 4, psop$region)
psop$region <- ifelse(psop$lat1 >= -40 & psop$lat1 < -10 & psop$lon1 >= 141 & psop$lon1 < 170, 5, psop$region)
psop$region <- ifelse(psop$lat1 >= -55 & psop$lat1 < -40 & psop$lon1 >= 141 & psop$lon1 < 150, 5, psop$region)
psop$region <- ifelse(psop$lat1 >= -60 & psop$lat1 < -40 & psop$lon1 >= 150 & psop$lon1 < 170, 5, psop$region)
psop$region <- ifelse(psop$lat1 >= -60 & psop$lat1 < -10 & psop$lon1 >= 170 & psop$lon1 < 230, 6, psop$region)
psop <- psop[psop$region > 0,]
nrow(psop)   # 697550
table(psop$sch_id)
#create a school ID field
psop$asso <- ifelse(psop$sch_id %in% c(1:2), "U",ifelse(psop$sch_id %in% c(3:7),"A","X"))

#

table(psop$shk_ret_or_disc)

#########################################################################
#load(file="C:/Projects/SHK-indicators-2015/DATA/agg_eff_by_flag.rdata")

# aggr$totalshark <- rowSums(aggr[,6:11])
# with(aggr, tapply(totalshark, list(yy, region), sum))

top4 <-  names(sort(with(psop, tapply(shk_ret_or_disc, list(flag_id), sum)) ,decreasing=TRUE)[1:4])          #  get the overall top 4 repoters of sharks
# so top 4 should match ll_agg_oper_effortbyflag.r

psop_t <- psop[psop$asso=="U",]

top_report <- with(psop_t[psop_t$flag_id %in% c(top4),], tapply(  shk_ret_or_disc, list(  yy, region),sum) )   # t for the top 4 summed in each region
all_report <- with(psop_t, tapply(shk_ret_or_disc, list(yy,region), sum))      # total shark interaction reported by region

 dim(all_report); dim(top_report) # 
 all_report<- all_report[,as.character(3:6)]
top_report<- top_report[,as.character(3:6)]

otherreport <-  all_report-top_report                #total shark encounters (by other nations)  by region 
#now change top_report to country specific
top_report <-with(psop[psop_t$flag_id %in% c(top4),], tapply(  shk_ret_or_disc, list( flag_id, yy, region),sum) ) 
top_report<- top_report[,,as.character(3:6)]
#now top report is dis aggregated by flag
rm(Dat)
Dat<-list()   #make final data list    will have totals in 1000's of hooks
for(i in 1:dim(top_report)[3]){ Dat[[i]]<-  rbind(  top_report[top4,,i], OT=t(otherreport[,i])  ) 
                                Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                rownames(Dat[[i]])<-c(top4,"OT") 
                                  
                                }  #   
#visual check
 Dat[[1]]
 # number of sets with shark interactions
# 


library(r4ss)



j<-1
# opflag <- sort(unique(psop$flag))
# opcol<- cbind( opflag , rainbow(2*length(opflag))[seq(1,2*length(opflag),2)])
#  
 flagcol <-  c( rich.colors.short(4, alpha = 1), 'white' )

scalecex<-0.75 # the character expansion for the text on the scale
legcex<-1.25  # size of the legend
 
# how big does the scale need to be?
#  have to do it manually
 
#
acode<- c("unassociated","associated")
png(file=  paste0(shkdir,"GRAPHICS/ps_oper_shks_rep_cntry_",acode[j],".png" ) )  
#
par(mar=c( 2.55,3.55, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#
#loop over the areas 
for (i in c(1:6)) {   #loop over areas
  if(i %in% 1:2){ 
    plot.new()
    }else{
  barplot(Dat[[i-2]] ,ylab="",xlab="",names.arg=colnames(Dat[[i-2]]),col= flagcol,  las=1)
  mtext(side=3,paste("Region ", as.character(i)),line=1, cex=0.8)
   }
  if(i%in%3:4){mtext(side=2,outer=F,paste0("Number of sets with sharks recorded in logbooks ", acode[j]),line=3,cex=0.75, las=0)}
}
#
par(mar = par("mar")/2)
plot.new()
legend("center",legend=rownames(Dat[[i-2]]) ,fill=flagcol ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=NA)    #bty="n" (no box);
#
dev.off()

#--------------------------------------------------------------------------------------------------
#associated
j<-2
psop_t <- psop[psop$asso=="A",]
nrow(psop_t)
top_report <- with(psop_t[psop_t$flag_id %in% c(top4),], tapply(  shk_ret_or_disc, list(  yy, region),sum) )   # t for the top 4 summed in each region
all_report <- with(psop_t, tapply(shk_ret_or_disc, list(yy,region), sum))      # total shark interaction reported by region

dim(all_report); dim(top_report) # 
all_report<- all_report[,as.character(3:6)]
top_report<- top_report[,as.character(3:6)]

otherreport <-  all_report-top_report                #total shark encounters (by other nations)  by region 
#now change top_report to country specific
top_report <-with(psop[psop_t$flag_id %in% c(top4),], tapply(  shk_ret_or_disc, list( flag_id, yy, region),sum) ) 
top_report<- top_report[,,as.character(3:6)]
#now top report is dis aggregated by flag
rm(Dat)
Dat<-list()   #make final data list    will have totals in 1000's of hooks
for(i in 1:dim(top_report)[3]){ Dat[[i]]<-  rbind(  top_report[top4,,i], OT=t(otherreport[,i])  ) 
                                Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                rownames(Dat[[i]])<-c(top4,"OT") 
                                } 
 #  
 #

png(file=  paste0(shkdir,"GRAPHICS/ps_oper_shks_rep_cntry_",acode[j],".png" ) )  

par(mar=c( 2.55,3.55, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))

#loop over the areas 
for (i in c(1:6)) {   #loop over areas
  if(i %in% 1:2){ 
    plot.new()
  }else{
    barplot(Dat[[i-2]] ,ylab="",xlab="",names.arg=colnames(Dat[[i-2]]),col= flagcol,  las=1)
    mtext(side=3,paste("Region ", as.character(i)),line=1, cex=0.8)
  }
  if(i%in%3:4){mtext(side=2,outer=F,paste0("Number of sets with sharks recorded in logbooks ", acode[j]),line=3,cex=0.75, las=0)}
}
# plot legend
par(mar = par("mar")/2)
plot.new()
legend("center",legend=rownames(Dat[[i-2]]) ,fill=flagcol ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=NA)    #bty="n" (no box);
dev.off()
#
#-------------------------------------------------
#------------------------------------------------
# effort
psag <- read.table(  "C:/Projects/SHK-indicators-2015_backup/DATA/PS_AGG5X5_NSETS_CNTRY_SETTYPE.TXT", header=TRUE, sep=',', stringsAsFactors=FALSE)
psag <- psag[psag$yy %in% 1995:2014,]
#dput(unique(psag$flag))
#drop the IP   VT (and japan coastal?)  and PH?
keepflag <- c("AU", "FM",   "JP", "KI", "KR", "NZ", "PG",  "PH", "SB",   "TW", "US", "VU", "FR", "ES", "MH",   "CN", "SV", "EC", "TV")
psag <- psag[psag$flag %in% keepflag,]
# check ps obs coverage by area, 

# first get rid of coastal japan
length( )
psag <- psag[-which(psag$flag =="JP" & psag$latd>17.5 ),]; nrow(psag)
# and that southern stuff
psag <- psag[ -which(psag$latd <= -22.5),]; nrow(psag)

psag$region<- 0
psag$region <- ifelse(psag$latd >= 20 & psag$latd <= 50 & psag$lond >= 120 & psag$lond < 180, 1, psag$region)
psag$region <- ifelse(psag$latd >= 20 & psag$latd <= 50 & psag$lond >= 180 & psag$lond < 210, 2, psag$region)
psag$region <- ifelse(psag$latd >= -10 & psag$latd < 20 & psag$lond >= 120 & psag$lond < 170, 3, psag$region)
psag$region <- ifelse(psag$latd >= -10 & psag$latd < 20 & psag$lond >= 170 & psag$lond < 210, 4, psag$region)
psag$region <- ifelse(psag$latd >= -10 & psag$latd < -4 & psag$lond >= 210 & psag$lond < 230, 4, psag$region)
psag$region <- ifelse(psag$latd >= -40 & psag$latd < -10 & psag$lond >= 141 & psag$lond < 170, 5, psag$region)
psag$region <- ifelse(psag$latd >= -55 & psag$latd < -40 & psag$lond >= 141 & psag$lond < 150, 5, psag$region)
psag$region <- ifelse(psag$latd >= -60 & psag$latd < -40 & psag$lond >= 150 & psag$lond < 170, 5, psag$region)
psag$region <- ifelse(psag$latd >= -60 & psag$latd < -10 & psag$lond >= 170 & psag$lond < 230, 6, psag$region)
psag <- psag[psag$region > 0,]
nrow(psag)   #  12341
#

#head(psag)
settype <-c( "ASS", "UNA")
acode<- c("Associated", "Unassociated")

top4 <-  names(sort(with(psag, tapply(sets, list(flag ), sum)) ,decreasing=TRUE)[1:4])
top4
for( k in 1:2){
  #make dummy data
  psag_t <- psag[psag$school==settype[k], ]
# top 4 fishing nations without ID, PH VT JP  coastal
nrow(psag_t)
top_report <- with(psag_t[psag_t$flag %in% c(top4),], tapply(  sets, list(  yy, region),sum) )   # t for the top 4 summed in each region
all_report <- with(psag_t, tapply(sets, list(yy,region), sum))      # total shark interaction reported by region

dim(all_report); dim(top_report) # 
all_report<- all_report[,as.character(3:6)]
top_report<- top_report[,as.character(3:6)]

otherreport <-  all_report-top_report                #total shark encounters (by other nations)  by region 
#now change top_report to country specific
top_report <-with(psag_t[psag_t$flag %in% c(top4),], tapply(  sets, list( flag, yy, region),sum) ) 
top_report<- top_report[,,as.character(3:6)]
#now top report is dis aggregated by flag
rm(Dat)
Dat<-list()   #make final data list    will have totals in 1000's of hooks
for(i in 1:dim(top_report)[3]){ Dat[[i]]<-  rbind(  top_report[top4,,i], OT=t(otherreport[,i])  ) 
                                Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                rownames(Dat[[i]])<-c(top4,"OT")  }
# Dat[[i]] 
#  make image
png(file=  paste0(shkdir,"GRAPHICS/ps_total_eff",acode[k],".png" ) )  

par(mar=c( 2.55,3.55, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,1,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))

#loop over the areas 
for (i in c(1:6)) {  #loop over areas
  if(i %in% 1:2){ 
    plot.new()
  }else{
    barplot(Dat[[i-2]]/1000 ,ylab="",xlab="",names.arg=colnames(Dat[[i-2]]),col= flagcol,  las=1)
    mtext(side=3,paste("Region ", as.character(i)),line=1, cex=0.8)
  }
  if(i%in%3:4){mtext(side=2,outer=F,paste0("Total Number of ", acode[k], " Sets (1000s)"),line=3,cex=0.75, las=0)}
}
# plot legend
par(mar = par("mar")/2)
plot.new()
legend("center",legend=rownames(Dat[[i-2]]) ,fill=flagcol ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=NA)    #bty="n" (no box);

dev.off()

  
}
 







# Okay, look at the #   caught and reported by countries, by year on each region
#
#
#



#########################################################################
#load(file="C:/Projects/SHK-indicators-2015/DATA/agg_eff_by_flag.rdata")
load("C:/wcpfc/shark indicators/shk-indicators-2015/DATA/agg_eff_by_flag.rdata")

aggr$totalshark <- rowSums(aggr[,6:11])
with(aggr, tapply(totalshark, list(yy, region), sum))

top4 <-  names(sort(with(aggr, tapply(hhooks, list(flag), sum)) ,decreasing=TRUE)[1:4])          #  get the overall top 4 FISHERS
# so top 4 should match ll_agg_oper_effortbyflag.r


top_report <- with(aggr[aggr$flag %in% c(top4),], tapply(  totalshark, list(  yy, region),sum) )   # hooks for the top 4 summed in each region
all_report <- with(aggr, tapply(totalshark, list(yy,region), sum))      # total sharks (key species) reported by region

dim(all_report); dim(top_report) # 


otherreport <-  all_report-top_report                #total sharks(by other nations)  by region here top_report is summed over the high fishing. 

top_report <-with(aggr[aggr$flag %in% c(top4),], tapply(  totalshark, list( flag, yy, region),sum) ) 
#now top report is dis aggregated by flag
rm(Dat)
Dat<-list()   #make final data list    will have totals in 1000's of hooks
for(i in 1:dim(top_report)[3]){ Dat[[i]]<-  rbind(  top_report[top4,,i], OT=t(otherreport[,i])  ) 
                                Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                rownames(Dat[[i]])<-c(top4,"OT") 
                                  Dat[[i]]<- Dat[[i]]/1000 
                                }  #  sharks are in 1000's
#visual check
 Dat[[1]]

# 

scalecex<-0.75 # the character expansion for the text on the scale
legcex<-1.25  # size of the legend
mycol <-c( rainbow(5) )
# how big does the scale need to be?
#  have to do it manually
ymax <-   c(2,.5,8,.5,.5,.5)*1000
#
shkdir_rds <- "C:/wcpfc/shark indicators/shk-indicators-2015/"
png(file=paste(shkdir_rds,"GRAPHICS/FIG_xx_LLreported_catch_FLAG_RDS.png",sep='') )  
#
par(mar=c( 2.55,3.55, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,3,1,1)) #  
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
#loop over the areas 
for (i in c(1:6)) {   #loop over areas
  barplot(Dat[[i]] ,ylab="",xlab="",names.arg=colnames(Dat[[i]]),col=mycol,ylim=c(0, ymax[i]), las=1)
  mtext(side=3,paste("Region ", as.character(i)),line=1, cex=0.8)
  
  if(i%in%3){mtext(side=2,"Total Sharks Reported (1000s)",line=4,cex=1.0, las=0)}
}

top4
#
par(mar = par("mar")/2)

plot.new()
legend("center",legend=rownames(Dat[[i]]) ,fill=mycol ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=NA)    #bty="n" (no box);


dev.off()

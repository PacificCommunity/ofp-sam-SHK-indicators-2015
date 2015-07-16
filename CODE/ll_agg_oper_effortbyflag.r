

# longline effort from aggregate log sheets by region & country
#
#
#25/6/2015


  aggr <- read.table("C:/Projects/SHK-indicators-2015/DATA/AGGEFF5X5_CNTRY_SHK.TXT", header=TRUE, sep=',' )
  aggr <- read.table("C:/wcpfc/shark indicators/shk-indicators-2015/DATA/AGGEFF5X5_CNTRY_SHK.TXT", header=TRUE, sep=',')
  head(aggr)
  dim(aggr)
  
  colnames(aggr)[3:4] <- c("lon5", "lat5" ) 
  range(aggr$yy)
  # data looks good, so add the region to it
  aggr$region <- 0  ; head(aggr)
  
  # process region
  aggr$region <- ifelse(aggr$lat5 >= 20 & aggr$lat5 <= 50 & aggr$lon5 >= 120 & aggr$lon5 < 180, 1, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= 20 & aggr$lat5 <= 50 & aggr$lon5 >= 180 & aggr$lon5 < 210, 2, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -10 & aggr$lat5 < 20 & aggr$lon5 >= 120 & aggr$lon5 < 170, 3, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -10 & aggr$lat5 < 20 & aggr$lon5 >= 170 & aggr$lon5 < 210, 4, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -10 & aggr$lat5 < -4 & aggr$lon5 >= 210 & aggr$lon5 < 230, 4, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -40 & aggr$lat5 < -10 & aggr$lon5 >= 141 & aggr$lon5 < 170, 5, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -55 & aggr$lat5 < -40 & aggr$lon5 >= 141 & aggr$lon5 < 150, 5, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -60 & aggr$lat5 < -40 & aggr$lon5 >= 150 & aggr$lon5 < 170, 5, aggr$region)
  aggr$region <- ifelse(aggr$lat5 >= -60 & aggr$lat5 < -10 & aggr$lon5 >= 170 & aggr$lon5 < 230, 6, aggr$region)
  aggr <- aggr[aggr$region > 0,]
  aggr <- aggr[aggr$yy %in% 1995:2014, ]
  #   head(aggr)  # looks good
  save(aggr, file="C:/Projects/SHK-indicators-2015/DATA/agg_eff_by_flag.rdata")
 
  top4 <-  names(sort(with(aggr, tapply(hhooks, list(flag), sum)) ,decreasing=TRUE)[1:4])          #  get the overall top 4 fishers  (ignoring region  for the moment)
 
  placehook <-with(aggr[aggr$flag %in% c(top4),], tapply(  hhooks, list(  yy, region),sum) )   # hooks for the top 4 summed in each region
  allhook<-with(aggr, tapply(hhooks, list(yy,region), sum))      #total hooks by region
  otherhook<-  allhook-placehook                #total other hooks by region
  
  placehook <-with(aggr[aggr$flag %in% c(top4),], tapply(  hhooks, list( flag, yy, region),sum) ) 
  
  Dat<-list()   #make final data list    will have totals in 1000's of hooks
  for(i in 1:dim(placehook) [3]){ Dat[[i]]<-  rbind(placehook[top4,,i],OT=t(otherhook[,i])) 
                                  Dat[[i]]<- ifelse(is.na(Dat[[i]]),0,Dat[[i]]) 
                                  rownames(Dat[[i]])<-c(top4,"OT") 
                                  Dat[[i]]<-round(Dat[[i]]/10000,2) }  # effort in Millions of hooks
 
   Dat[[1]]
  scalecex<-0.75 # the character expansion for the text on the scale
  legcex<-1.25  # size of the legend
  mycol <-c( rainbow(5) )
  # how big does the scale need to be?
  ymax <-  max(  sapply(Dat, colSums))
  #
  shkdir_rds <- "C:/wcpfc/shark indicators/shk-indicators-2015/"
  png(file=paste(shkdir_rds,"GRAPHICS/FIG_xx_LLeff_FLAG_RDS.png",sep='') )  
  
  par(mar=c( 2.55,2.05, 2.05, 1.05), mgp=c(3, 1, 0), las=0, oma=c(1,3,1,1)) #  
  layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
  #loop over the areas 
  for (i in c(1:6)) {   #loop over areas
    barplot(Dat[[i]] ,ylab="",xlab="",names.arg=colnames(Dat[[i]]),col=mycol,ylim=c(0,1.25*ymax), las=1) #, main=paste('Region',i))
    mtext(side=3,paste("Region ", as.character(i)),line=0, cex=0.8)
     
    if(i%in%3){mtext(side=2,"Total Hooks Fished (Millon Hooks)",line=3,cex=1, las=0)}
  }
  
 
  #
  par(mar = par("mar")/2)
  
  plot.new()
  legend("center",legend=rownames(Dat[[i]]) ,fill=mycol ,cex=legcex,horiz=TRUE,bty="n",  title="Country Code", xpd=NA)    #bty="n" (no box);
 
  
  dev.off()

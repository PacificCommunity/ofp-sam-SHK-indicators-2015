## shark-indicator-2015_data-prep.r
## Reading in already extracted data from observer database and massaged a bit.
## 
## makes simple plots and descriptions of data
##
## -------------------------------------------------------
## Author: Joel Rice (joelrice@uw.edu)
## Written on: June 5, 2015  (updated a bit since)
 
options(stringsAsFactors=FALSE)
require(dplyr)
require(magrittr)
library(maps)
library(mapproj)
library(mapdata)
oldpar <- par()

#####
#rm(shkdir)
shkdir <- "C:/Projects/SHK-indicators-2015/"
dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
 
# make colors for the main species - use previous
hues=c("royalblue","gray","red","mediumspringgreen","sienna") ; mycol<- hues
#
s.yr <- 1995
e.yr <- 2014
#
# make names and other init dec
spec<- c("BSH", "MAK", "OCS","FAL", "THR"); nspec<- length(spec)
scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE")
#
nreg <- 6
#
eez <- read.table(file=paste(shkdir,"DATA/EZNEW2.txt", sep=""), sep="",header=F)
#
# load the combined (SPC and HW) and cleaned observer data
load( file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" )   #loads shk_all
head(shk_all)
#ENSURE TIME FRAME IS CORRECT.
shk_all <- shk_all[shk_all$yy > 1994, ]
shk_all <- shk_all[shk_all$yy < 2015, ]
shk_all <- shk_all[shk_all$region %in% 1:6,]
#
#
#load( file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_sets_9June15.rdata") # loads sets
# sets was from the extract by DMP, not quite what I needed.
#     head(sets)
#     
#     
#     # with(sets, table(program_code, yy))
#     sets$yy <- substr(sets$set_start_date, 1,4)
#     table(sets$yy)
#     sets$mm <- substr(sets$set_start_date, 5, 6)
#     table( sets$mm)
#     sets$dd <- substr(sets$set_start_date, 7, 8)
#     table( sets$dd)
#     
#     which(is.null(sets$set_start_date ) ) [1:10]
#     
#     sets$yy [1:30]
#     class(sets$yy)
#     sets$yy <- as.numeric (sets$yy)
#     sets$mm <- as.numeric (sets$mm)
#     sets$dd <- as.numeric (sets$dd)
#     sets <- sets[sets$yy>1994,]; sets <- sets[sets$yy <2015,]
#     table(sets$yy)
#     
#     sets$nlon51d <- ifelse(sets$lon51d<0, sets$lon51d+360, sets$lon51d)
#     sets$totshk <-  rowSums(cbind(sets$BSH, sets$FAL,  sets$OCS, sets$MAK, sets$THR, sets$HHD))
#     
 
#######################################################3
#
#  Plot 1
#

png(file=paste(shkdir,"GRAPHICS/FIG_1_MAP.png",sep='')) 

par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0,0))
plot(1,1,type="n",ylab="",xlab="",xlim=c(110,240),ylim=c(-60,50),col="cadetblue",cex=0.1)

lines(eez[,1], eez[,2], col=1) # draw these boundaries over the set locations (obscure misplaced sets!)
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), fill=T, add=T, yaxt="n", xaxt="n", col="black")

#Source the region lines
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")
# Make Region Names.
text( c(160,200,160,200,160,200 ), y=c(25,25,0,0,-30,-30) , labels=1:6, cex=4, pos=3)
dev.off()

#############################################################################################
#
#
#PLOT EFFORT & assessment time frame
#
# data pulled From CES is in file H:/SC8_shark assessments/Assessment Reports_in prep/Presentation/Presentation Graphics.xlsx


# in hundreds of hooks
#         
#         lleff<-    c(40467, 70928, 1130213.56, 1238970.2, 1386500.51, 1496159.6, 
#                      1480472.29, 1713366.07, 2028913.63, 2182597.15, 2424868.68, 2717273.06, 
#                      2566026.8, 2721289.88, 2100045.93, 2578297.74, 3013931.17, 3095365.1, 
#                      2994050.67, 2939024.69, 2917448.56, 3247930.49, 3396341.25, 3582904.93, 
#                      4163907.58, 3394195.4, 3678504.67, 3805787.79, 3418713.79, 4351669.11, 
#                      4665496.34, 5207033.41, 4605505.57, 3559966.57, 4008367.2, 4593498.34, 
#                      3608439.07, 4255433.76, 5019066.49, 4418851.39, 4535607.04, 4414065.35, 
#                      4329996.33, 4501381.96, 5031248.48, 5056960.59, 4443234.64, 4578145.55, 
#                      4944986.58, 5999221.68, 5951123.11, 7867086.19, 8276766.86, 8131424.78, 
#                      8303280.14, 7418145.3, 7224460.73, 7906085.67, 8021637.92, 9983103.3, 
#                      9492896.44)
#         
#         
#         pseff<-c(1732.298507, 2158.967779, 2012.521222, 2831.558736, 3102.658228, 
#                  2026.385593, 3335.693921, 3346.432616, 2301.246883, 2475.710469, 
#                  2772.324436, 2411.045219, 4383.672278, 4496.208964, 8800.217279, 
#                  12335.21406, 21274.44195, 29187.73579, 23904.91434, 23415.00302, 
#                  27353.07447, 27054.83017, 29041.82578, 32836.65068, 42112.92032, 
#                  44448.62006, 46016.59683, 42600.98479, 42625.7432, 43674.54643, 
#                  48407.24116, 43366.78154, 45283.63826, 44570.17252, 45467.47294, 
#                  48718.07616, 54872.95285, 59620.73523, 61593.78834, 57703.98666, 
#                  60515.94319, 65383.55683, 62530.10164, 63853.74173)    
#         
#         
#         
#         KEEPplot<-1
#         # plot the effort 
#         windows(11,6)  
#         par(mfrow=c(1,2) )
#         ldat <- lleff/10000
#         plot(1950:2010, ldat, col='dodgerblue2',  type='l', ylab="Millions of Hooks", xlab='Year', ylim=c(0,max(ldat)), lwd=2)    
#         dlen<-length(ldat)
#         mod<-length( 1995:2009)
#         shd<-  ldat[-c(1:(dlen-mod-1), dlen)]
#         polygon(x=c(1995:2009, rev(1995:2009))     , y= c(shd, rep(0, length(1995:2009))), col='lightsteelblue', border = NA)
#         lines(1950:2010, ldat, col='dodgerblue4',lwd=2)
#         title("Longline Effort & Assessment Period")
#         abline(h=0, col=grey(0.8))
#         ###      
#         pyrs<-c(1967:2010)
#         pdat<-pseff/1000
#         plot(pyrs, pdat, col='dodgerblue4', type='l', ylab="Effort Days (1000s)", xlab='Year', ylim=c(0,max(pdat)), lwd=2)    
#         dlen<-length(pseff)
#         mod<-length( 1995:2009)
#         shd<-  pdat[-c(1:(dlen-mod-1), dlen)]
#         polygon(x=c(1995:2009, rev(1995:2009))      , y= c(shd, rep(0, length(1995:2009))), col='lightsteelblue', border = NA )
#         lines(pyrs, pdat,  col='dodgerblue4',lwd=2)
#         title("Purse Seine Effort & Assessment Period")
#         abline(h=0, col=grey(0.8))
#         
# if(KEEPplot==1){savePlot(paste("H:/SC8_shark assessments/Assessment Reports_in prep/Graphics/Effort_&Assessment_period", sep=''), type="png")   }





################################
#
#  plot 2
#
#
#


png(file=paste(shkdir,"GRAPHICS/FIG_2_MAP_sets.png",sep='')) 
#
par(mfrow=c(1,1),mar=c(2.5,2,2,1),omi=c(0.5,0.5,0,0))
plot(sets$nlon51d,sets$lat51d,type="p",ylab="",xlab="",xlim=c(110,240),ylim=c(-60,50),col="grey",cex=0.1)

plot(sets$nlon51d[sets$totshak>0],sets$lat51d[sets$totshak>0],type="p",ylab="",xlab="",xlim=c(110,240),ylim=c(-60,50),col="cadetblue",cex=0.1)

#now load the dataframe with the LL logsheet data
load(file=paste(shkdir, "DATA/Shark_Operational_processed.rdata", sep='') ) # loads shklllog
head(shkLLlog)
 points(shkLLlog$newlon5[shkLLlog$totshk>0],shkLLlog$newlat5[shkLLlog$totshk>0],type="p",ylab="",xlab="", col="orange",cex=0.1)   
 
lines(eez[,1], eez[,2], col=1) # draw these boundaries over the set locations (obscure misplaced sets!)
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",
                               "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), fill=T, add=T, yaxt="n", xaxt="n", col="black")
 
#horizontal lines
lines(c(120,210),c(50,50),col="black",lwd=2)
lines(c(120,210),c(20,20),col="black",lwd=2)
lines(c(210,230),c(-4,-4),col="black",lwd=2)
lines(c(120,230),c(-10,-10),col="black",lwd=2)
#lines(c(141,170),c(-40,-40),col="red",lwd=3)
lines(c(141,150),c(-55,-55),col="black",lwd=2)
lines(c(150,230),c(-60,-60),col="black",lwd=2)
#vertical lines
lines(c(120,120),c(50,-10),col="black",lwd=2)
lines(c(141,141),c(-10,-55),col="black",lwd=2)
lines(c(150,150),c(-55,-60),col="black",lwd=2)
lines(c(180,180),c(50,20),col="black",lwd=2)
lines(c(170,170),c(20,-60),col="black",lwd=2)
lines(c(210,210),c(50,-4),col="black",lwd=2)
lines(c(230,230),c(-4,-60),col="black",lwd=2)


legend("bottomleft",legend=c("Sets","Sets with Sharks Recorded","Observed Sets"),pch=c(20,20,20),cex=0.54,text.col="white",pt.cex=c(1,1,1),bg="dimgrey",col=c("cadetblue", "thistle", "lemonchiffon"),title="lon5GLINE FISHERY 1995-2010 OPERATIONAL DATA")  
#mtext(side=3,outer=T,"lon5gline Logsheet Data",line=1,cex=1.5)
#savePlot(file=paste(shkdir,"GRAPHICS/FIG_2_LLSetLocationsCombined",sep=''), type="png")
dev.off()




cntrylst<- c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia",  "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia", "Canada", "Peru", "Ecuador", "Chile", "USSR","Mexico", "Argentina", "Guatemala", "Honduras",  "El Salvador" , "Bolivia", "Colombia", "Brazil", "Venezuela", "Cuba", "Haiti", "Nicaragua", "Panama", "Costa.Rica","Belize", "Hawaii:Hawaii")    

# make a standard multiplier to scale the hooks & CPUE observed
hk.mult<-5500
mygrey<-rgb(red=220,green=220, blue=220, alpha=75, maxColorValue=255)  
#cpue
cpue.mult<- 25  # max is 224       98% are less than 
myblue<-rgb(red=0,green=205, blue=255, alpha=175, maxColorValue=255)
mygold<-rgb(red=255,green=215, blue=0, alpha=105, maxColorValue=255)
#title text
# t_text<-c("1995-1999", "2000-2004", "2005-2009")



# % ------------------------------------------------------------------------------------------------
#
#  Plot observed effort and catch of sharks
#
#
#
#

par(mai=c(1.02, 0.82, 0.42, 0.42) )  #
par(mfrow=c(2,1))


# make the plot
plot(1,1,ylab="",xlab="",xlim=c(120,220),ylim=c(-40,40),col="white")

#lines(eez[,1], eez[,2], col=4) # draw these boundaries over the set locations (obscure misplaced sets!)
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region =c(cntrylst), fill=T, add=T, yaxt="n", xaxt="n", col=grey(0.001))
# plot effort
with(shk, points(  lon1, lat1, pch=21, col=1, bg=mygrey, cex=5* hook_est/hk.mult, ylab="Longitude", xlab="Lattitude")  )
# plot observed catch
with(shk[shk$silky>0,], points(  lon1, lat1, pch=21, col=1 ,bg=myblue , cex=1.5, new=F)    )
#  Add title 
title("Observed Shark Catch and Effort \n Longline") 
# EEZ lines
#lines(eez[,1], eez[,2], col=1)

 

##################### PURSE SEINE
#   
# plot the observed effort and catch. 

load( file= "C:/Projects/SHK-indicators-2015/DATA/PSObs16Jun2015.RData"  ) # loads PSObsShk  # 
 
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_eMAP_sets.png",sep='')) 
# 
plot(1,1, ylab="Longitude", xlab="Lattitude"  ,xlim=c(110,240),ylim=c(-60,50),col="white"  )
 
#
#lines(eez[,1], eez[,2], col=4) #  
map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
map('world2Hires',  region =c(cntrylst), fill=T, add=T, yaxt="n", xaxt="n", col=grey(0.001))
# plot effort
#with(PSObsShk, points(    lon1,   lat1 ,pch=21, col=1, bg=mygrey, cex=2, ylab="Longitude", xlab="Lattitude")  )
 with(PSObsShk, points(    newlon,   newlat ,pch=21, col=1, bg=mygrey, cex=2))
# plot observed catch
with(PSObsShk[PSObsShk$totalshk>0,], points( newlon,  newlat , pch=21, col=1 ,bg=myblue , cex=1.5, new=F)    )
#  Add title 
title("Observed Shark Catch and Effort \nPurse Seine") 
# region lines
source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")
legend('topright',  legend=c("Observed Set", "Obsereved Shark Catch"), pch=21,col=1,  pt.bg=c(mygrey, myblue) , pt.cex=1.5)


dev.off()



################################
#
#  plot 3 A 
#   from "Logbook Sharks by Flag.r"
#
#   logsheet data is way out of date now....skipping this one.


png(file=paste(shkdir,"GRAPHICS/FIG_3_MAP_sets.png",sep='')) 


#COLORS as of 22 September
hues <- c("gray","cornsilk","turquoise","gold","tomato","greenyellow","sienna","thistle","blue","forestgreen","orange","khaki","palevioletred","cadetblue","pink","darkviolet","lightskyblue")
huenames <- c("Australia","China","Fiji","Indonesia","Japan","Korea","French Polynesia","Chinese Taipei","United States","Vanuatu","Solomon Is.","Papua New Guinea","New Zealand","Philippines","Marshall Is.","FSM","Other")
huecodes <- c("AU","CN","FJ","ID","JP","KR","PF","TW","US","VU","SB","PG","NZ","PH","MH","FM","ZZ")

 
names(shkLLlog)[names(shkLLlog)=="tripflag_id"] <- "flag"
names(shkLLlog)[names(shkLLlog)=="totshk"] <- "tot_shk_no"

# table(shkLLlog$flag, useNA="ifany")


place <-tapply(shkLLlog$tot_shk_no,list(shkLLlog$yy,shkLLlog$flag),sum)
totals <-colSums(place,na.rm="T")   
topten <-names(sort(totals,decreasing=TRUE)[1:10])     #these 3 steps get the overall top ten recorders of sharks (ignoring region for the moment)
topten[11]<-"ZZ"


#now split by area
reg1<-shkLLlog[shkLLlog$region==1,]
reg2<-shkLLlog[shkLLlog$region==2,]
reg3<-shkLLlog[shkLLlog$region==3,]
reg4<-shkLLlog[shkLLlog$region==4,]
reg5<-shkLLlog[shkLLlog$region==5,]
reg6<-shkLLlog[shkLLlog$region==6,]

# 
 
#MAKE PLOT MATRIX WITH A FIXED ARRAY OF YEARS AND FLAGS

plotready<-list()                      #the only matrix that is output repeatedly is this one, so it needs to be a list
regiontot<-array(0,c(11,20))            #11 flags, for 1995 to 2010  
colnames(regiontot)<-c(1995:2014)
rownames(regiontot)<- topten
for (i in c(1:6)) {
  a <-  get(paste("reg",i,sep=""))     #each time through make "a" the name of the source file
  tab<-tapply(a$tot_shk_no,list(a$flag,a$yy),sum,na.rm="T")             #gives a table of flag (row) x year(column) of # of sets for the years with data
  TTT<-tab[match(rownames(regiontot),rownames(tab)),match(colnames(regiontot),colnames(tab))] #writes tab into the standard array format for top ten years
  tabsum<-colSums(tab,na.rm=T)        #sum of sets in each year with data
  TTTsum<-colSums(TTT,na.rm=T)        #sum of sets in top ten flags in each year in standardized array
  MG<-TTTsum[match(names(tabsum),names(TTTsum))]  #removes the years for which there are no data
  ZZ<-tabsum-MG                           #subtracts total for top ten flags from overall total leaving "other"
  TTT[11,]<-ZZ[match(colnames(regiontot),names(ZZ))]   #writes "other" results into the 11th line of the top ten array
  rownames(TTT)[11]<-"ZZ"                 #names the "other" row "ZZ"
  plotready[[i]]<-TTT
  plotready[[i]]<-ifelse(is.na(plotready[[i]]),0,plotready[[i]])    #replace the NAs with zeros otherwise the barplot won't plot
  plotready[[i]]<-plotready[[i]]/1000       #make units of thousand sharks
}

#PLOTTING
options(scipen=10)   #suppresses scientific notation on the y axis
countrycolor<-hues[match(topten,huecodes)]
countries<-huenames[match(topten,huecodes)] 
countries_adj <- substr(paste(countries,"                          ",sep=""),1,max(nchar(countries))+1)    #this makes all the legend text entries the same length


par(oma=c(0.5,2,0.5,0.5))
layout(matrix(c(7,7,1,2,3,4,5,6),4,2,byrow=TRUE),widths=c(2,2), heights=c(2,4,4,4))
#layout.show(dc)
par(mar=c(2.5,4,1.5,1),ask=T)

for (i in c(1:6)) {   #loop over areas
  barplot(plotready[[i]],ylab="",xlab="",names.arg=colnames(plotready[[i]]),col=countrycolor,ylim=c(0,1200))#,legend=rownames(plotready[[i]])
  mtext(side=3,paste("Region ", as.character(i)),line=0.3)
}

mtext(side=2,outer=T,"Total Sharks Recorded in Logbooks (thousands)",line=-1.5,cex=1.5)

plot(1, ann = FALSE, axes = FALSE, type = "n")  # "draws" a blank space in box 7 and then the legend will plot over this blank
par(mar=c(0,0.5,0.5,0))

legend(0.58,1.1,legend=countries_adj[1:4],fill=countrycolor[1:4],cex=1.2,horiz=TRUE,bty="n",text.width=1.25*max(strwidth(countries_adj)))
legend(0.58,0.8,legend=countries_adj[5:8],fill=countrycolor[5:8],cex=1.2,horiz=TRUE,bty="n",text.width=1.25*max(strwidth(countries_adj)))
legend(0.58,0.5,legend=countries_adj[9:11],fill=countrycolor[9:11],cex=1.2,horiz=TRUE,bty="n",text.width=1.25*max(strwidth(countries_adj)))

#savePlot("P:\\WCPFC Shark\\Graphical Output\\LogbookSharks",type="png")    
 dev.off()




############################3
#
#  FIG XX number of hooks set by region- annually  1995-2014
#
#

load(file=paste(shkdir,"DATA/AggregateEffort.rdata", sep='') ) # loads aggr
head(aggr)
aggr$region

#don't have a separate data processing file because this is probably the only time I'll use this database
aggr$region<- rep(0,length(aggr$yy))
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
aggr <- aggr[aggr$yy > 1994, ]
agg2 <- with(aggr, tapply(sum_hhooks, list(yy, region), sum))
head(agg2)
agg2 <- agg2/10000 # effor in million hooks now

# 


png(file=paste(shkdir,"GRAPHICS/FIG_xx_agg_eff.png",sep='')) 

matplot( as.numeric(rownames(agg2)), agg2, type='l', lty=1, lwd=2, col=rainbow(7)[1:6]  , ylab="Effort(Million Hooks)", xlab='Year', las=1 )
legend('topleft', legend= paste("Region", 1:6), lty=1, lwd=2, col=rainbow(7)[1:6], bty='n'    )
dev.off()



############################3
#
#  FIG XX number of hooks OBSERVED by region- annually  1995-2014
#
#

#CALC EFFORT
obs_eff<- with(shk_all, tapply(hook_est, list(yy, region), sum))
obs_eff<- obs_eff/1e6
# 
# MAKE FILE
png(file=paste(shkdir,"GRAPHICS/FIG_xx_OBS_eff.png",sep='')) 

matplot( as.numeric(rownames(obs_eff)), obs_eff, type='l', lty=1, lwd=2, col=rainbow(7)[1:6]  , ylab="Observed Effort (Million Hooks)", xlab='Year', las=1 )
legend('topleft', legend= paste("Region", 1:6), lty=1, lwd=2, col=rainbow(7)[1:6], bty='n'    )
dev.off()


#######################
#
# number of cells observed in each region
#

with(shk_all, tapply(as.numeric(cell), list(yy,region), unique))
with(shk_all, aggregate(as.numeric(cell), list(yy,region), unique))


c_info <- with(shk_all, aggregate( cell , list(yy,region), unique))



dim(c_info)
head(c_info)
summary(c_info)


class(shk_all$cell)

############################
#
#  FIG XX 
# Starting on the distribution indicator analyeses.
#
The high-CPUE indicator was the proportion of half-degree rectangles having unstandardised
CPUE greater than a specified threshold in the commercial TLCER data. It was calculated as the
number of high-CPUE rectangles divided by the total number of rectangles with reported effort.
This indicator acts as a measure of the spatial extent of high abundance areas. [Observer data
were too sparse and limited in their spatial distribution to be useful for this purpose.] CPUE was
calculated as the total number of sharks caught per rectangle divided by the total number of hooks
set in the rectangle (in thousands) in each fishing year. Following preliminary tests using a range
of potential thresholds, indicator thresholds were arbitrarily set at 25 sharks per 1000 hooks for
blue shark, and one shark per 1000 hooks for porbeagle and mako sharks.



A proportion-zeroes indicator was calculated as the number of half-degree rectangles having zero
reported catches in a fishing year divided by the total number of rectangles with reported effort in
that year.

For both of the above indicators, only rectangles having more than 5000 hooks of fishing effort in a
given fishing year were included in the analyses so that extreme catch rates from a small number of
sets did not bias the result. A limit of 5000 hooks ensures that each included rectangle has at least
three domestic sets or two foreign charter vessel sets1.

with(shk_all, table(BLUECPUE>0, yy))

#############################################
# SPECIES SPECIFIC 
#  annual %positive by region
#


#
for(j in 1:nspec){
  
temp <-  table( shk_all[,scpue[j]] >0, shk_all$yy,shk_all$region)

pcnt_pos <- matrix(NA, nrow=6, ncol=length(1995:2014) , dimnames=list(1:6, 1995:2014))
for ( i in 1:6){
  pcnt_pos[i,]<- round( temp[1,,i]/colSums(temp[,,i]) ,3)
  
   }

png(file=paste(shkdir,"GRAPHICS/FIG_xx_pcntpos_reg_", spec[j], ".png",sep='')) 
matplot(colnames(pcnt_pos), t(pcnt_pos)*100, type='b', lwd=2, pch=rownames(pcnt_pos), col=mycol[j], lty=1, ylim=c(0,100), xlab='Year', ylab="Percent of Positive sets", las=1, )
dev.off()
}


##########################
# plots nominal mean CPUE by region for each species
#
#
str(shk_all$BLUECPUE)
nreg <- 6

#
for(j in 1:nspec){
  
  temp <-   tapply( shk_all[,scpue[j]] >0, list( shk_all$yy,shk_all$region), mean )
 

  png(file=paste(shkdir,"GRAPHICS/FIG_xx_nomCPUE_reg_", spec[j], ".png",sep='')) 
  par(mfrow=c(3,2))
  for(i in 1:nreg){
  plot(rownames(temp), temp[,i], type='b', lwd=2, pch=16, col=mycol[j], lty=1, ylim=c(0,1.5*(max(temp[,i], na.rm=T))), xlab='Year', ylab=" ", main=paste("Region", i) , las=1 )
  if(i %in% c(3:4)) mtext("Mean Nominal CPUE (/1000 hooks)" , side=2, line=3, outer=F )
  
  }
    dev.off()
}
dev.off()


###############################
#
#
#  diagnostic histograms for size of the catch
#
#
dev.off()

for(j in 1:nspec){
png(file=paste(shkdir,"GRAPHICS/FIG_xx_HIST_CPUE_", spec[j], ".png",sep='')) 

par(mfrow=c(3,2))
for(i in 1:nreg){
  
   tdat <- shk_all[ shk_all[,scpue[j]]>0 &  shk_all$region==i,scpue[j]] 
 #
  if( length(tdat)==0 )   plot( 1,1, type='n', ylim=c(0,1), xlim=c(0,10), ylab='Density', xlab='CPUE',  col=mycol[j], main=paste("Region", i) , las=1)
  if( length(tdat)>0 )  hist( tdat    , xlab='CPUE', freq=F, col=mycol[j], nclass=50, main=paste("Region", i) , las=1  )
  
  
  rm(tdat)
 
}
 mtext(paste(spec[j]) , 3,outer=T, line=-2)

 dev.off()
}


###########################
# SPECIES DIST Section
#  high cpue 
#

rm(tdat)
#tdat <- tapply(shk_all$BLUECPUE, list(shk_all$cell, shk_all$yy), mean)




tdat <- tapply( shk_all[  shk_all$region==i,scpue[j] ] , list(shk_all[  shk_all$region==i,"cell"], shk_all[shk_all$region==i,'yy']), mean) 
#
dim(tdat)  
head(tdat)  
# for BSH it was 1/1000hks
#mycol<- mycol[c(4,1,2,3,5)]

# this is the stat that needs to get worked out....by region
length(which(tdat[,10] > 1)) / length(which(!is.na(tdat[,10] ) )   )

#make storage
spec_thres <- c( 1, 1,1,1,1) # could make this an array so that the different regions have different thresholds.
hicpue <- array(data=NA, dim=c(length(1995:2014), nreg, nspec), dimnames=list(1995:2014, 1:6, spec))               #make stoage
# hicpue[1:5,1:3,]

#start calcs and plot
for(j in 1:nspec){

  png(file=paste(shkdir,"GRAPHICS/FIG_xx_HIGH_CPUE_", spec[j], ".png",sep='')) 
  par(mfrow=c(3,2))
  for(i in 1:nreg){
   tdat <- tapply( shk_all[  shk_all$region==i,scpue[j] ] , list(shk_all[  shk_all$region==i,"cell"], shk_all[shk_all$region==i,'yy']), mean) 
   
    tvec <- c()
    for( k in 1:dim(tdat)[2] ) tvec <- c(tvec,length(which(tdat[,k] > spec_thres[j])) / length(which(!is.na(tdat[,k] ) )   ))
     #
     hicpue[1:length(tvec), i,j] <- tvec # store
     # ~~~~ Plot 
     plot(1995:(1995+length(tvec)-1),   hicpue[1:length(tvec), i,j],  type='o', lwd=2, pch=16, col=mycol[j], lty=1, xlim=c(1995,2014), ylim=c(0,1),
          xlab="Year", ylab="Proportion HiGH CPUE", las=1, main=paste("Region", i))
  } #over each region
  mtext(paste(spec[j]) , 3,outer=T, line=-2)
 
  dev.off()
   
} # over each species
  



#########################3
#  Plot the  Proportion of sets by month , annually
#
#  OBSERVER DATA
#

# init dec.
bcol<- rainbow(25)[seq(2,24,2)]
mnths <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
# make calc
tobs <- table(shk_all$mm, shk_all$yy, shk_all$region)
#plot file
png(file=paste(shkdir,"GRAPHICS/FIG_xx_obsBY_mm.png",sep='')) 
par(mfrow=c(3,2))
for(i in 1:nreg){
barplot(prop.table(tobs[,,i], 2), col=bcol,  legend.text = mnths,  args.legend = list(x =10, y=-.8, bty='n',cex=0.7, ncol=6,xjust=.5, yjust=0), main=paste("Region", i) )

}
dev.off()
 

#########################3
#  Plot the  Proportion of sets by month , annually
#  in each region
#  LOGSHEET DATA
############

#now load the dataframe with the LL logsheet data
load(file=paste(shkdir, "DATA/Shark_Operational_processed.rdata", sep='') ) # loads shklllog
head(shkLLlog); dim(shkLLlog)

# init dec.
bcol<- rainbow(25)[seq(2,24,2)]
mnths <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
# make calc
tlog <- table(shkLLlog$mm, shkLLlog$yy, shkLLlog$region)
#plot file
png(file=paste(shkdir,"GRAPHICS/FIG_xx_LOGSHEET_mm.png",sep='')) 
par(mfrow=c(3,2))
for(i in 1:nreg){
bp<-  barplot(prop.table(tlog[,,i], 2), col=bcol,  legend.text = mnths,  args.legend = list(x =10, y=-.6, bty='n',cex=0.7, ncol=6,xjust=.5, yjust=0), main=paste("Region", i) )
   text( x=bp, y=rep(.9, dim(tlog)[2] ) ,labels=round(colSums(tlog[,,i]) /1000, 0), cex=0.7 , pos=3 )
  
}
mtext('LOGSHEET- NOT UPDATED', side=3, line=-2, cex=2, outer=T)

dev.off()

#########################3
#  Plot the  cummulative difference in the proportion of sets by month , annually
#  in each region
#    abs( prop ObsData - prop LOGSHEET DATA)
#
############
png(file=paste(shkdir,"GRAPHICS/FIG_xx_obsDIFFlog_mm.png",sep='')) 
par(mfrow=c(3,2))
for(i in 1:nreg){

 tdif  <- colSums(   abs(prop.table(tobs[,1:17,i] ,2) -   prop.table(tlog[,,i], 2)))
 tdif<- ifelse(is.na(tdif), NA, tdif)
 
 plot(names(tdif), tdif, type='o', lwd=2, pch=16, col=3, lty=1, xlim=c(1995,2014), ylim=c(0,2),
 xlab="Year", ylab="", las=1, main=paste("Region", i))
  
 if(i %in% c(3:4)) mtext("Cumulative Difference In Data Coverage" , side=2, line=3, outer=F )
 }
mtext(' NOT UPDATED', side=3, line=-2, cex=2, outer=T)
dev.off() 
 


#########################3
#  Plot the the cummulative CPUE (annually) for sharks of interest
#
#  ws total shark
#

#init calcs : sharks per 1000 hooks by region & yr
teff <-   tapply(shk_all$hook_est,  list(shk_all$region, shk_all$yy), sum)
#
tshark <- tapply(shk_all$othershk, list(shk_all$region, shk_all$yy), sum) /(teff/1000)
tbsh <- tapply(shk_all$blue, list(shk_all$region, shk_all$yy), sum) /(teff/1000)
tmak <- tapply(shk_all$mako, list(shk_all$region, shk_all$yy), sum) /(teff/1000)
tocs <- tapply(shk_all$ocs,  list(shk_all$region, shk_all$yy), sum) /(teff/1000)
tfal <- tapply(shk_all$silky,list(shk_all$region, shk_all$yy), sum) /(teff/1000)
tthr <- tapply(shk_all$thresher,  list(shk_all$region, shk_all$yy), sum) /(teff/1000)



#start calcs and plot

    png(file=paste(shkdir,"GRAPHICS/FIG_xx_shksP1000Hooks.png",sep='')) 
    par(mfrow=c(3,2))


for(i in 1:nreg){

  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  
  
  barplot(tmat, col=c(mycol, "brown"),  legend.text = c(spec,'OtherShark'),  args.legend = list("topright", bty='n',cex=0.7, ncol=3), main=paste("Region", i) )
  if(i %in% c(3:4)) mtext("Sharks per 1000 hooks)" , side=2, line=3, outer=F )
  }
dev.off()
#
#------------------------ Shallow and Deep Sets.
#
s_llobs <- shk_all[shk_all$HPBCAT=="S",]
#init calcs : sharks per 1000 hooks by region & yr  for SHALLOW SETS
teff <-   tapply(s_llobs$hook_est,  list(s_llobs$region, s_llobs$yy), sum)
#

tbsh <- tapply(s_llobs$blue, list(s_llobs$region, s_llobs$yy), sum) 
tmak <- tapply(s_llobs$mako, list(s_llobs$region, s_llobs$yy), sum)  
tocs <- tapply(s_llobs$ocs,  list(s_llobs$region, s_llobs$yy), sum)  
tfal <- tapply(s_llobs$silky,list(s_llobs$region, s_llobs$yy), sum)  
tthr <- tapply(s_llobs$thresher,  list(s_llobs$region, s_llobs$yy), sum)  
tshark <- tapply(s_llobs$othershk, list(s_llobs$region, s_llobs$yy), sum)  



png(file=paste(shkdir,"GRAPHICS/FIG_xx_shksP1000Hooks_shallow.png",sep='')) 
par(mfrow=c(3,2))
for(i in 1:nreg){
  
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  
  
  barplot(tmat, col=c(mycol, "white"), ylim=c(0,15000), names.arg=colnames(tmat),  main=paste("Region", i) )
  
  if(i %in% c(1) ) legend('topright',  legend  = c(spec,'OtherShark'),    bty='n',cex=0.8, horiz=F,ncol=2, fill =c(mycol,'white' ),xpd=T )
  if(i %in% c(3:4)) mtext("Sharks Observed" , side=2, line=3, outer=F )
}

dev.off()
#---------------------------Deep Sets
d_llobs <- shk_all[shk_all$HPBCAT=="D",]
#init calcs : sharks per 1000 hooks by region & yr  for SHALLOW SETS
teff <-   tapply(d_llobs$hook_est,  list(d_llobs$region, d_llobs$yy), sum)
#

tbsh <- tapply(d_llobs$blue, list(d_llobs$region, d_llobs$yy), sum) 
tmak <- tapply(d_llobs$mako, list(d_llobs$region, d_llobs$yy), sum)  
tocs <- tapply(d_llobs$ocs,  list(d_llobs$region, d_llobs$yy), sum)  
tfal <- tapply(d_llobs$silky,list(d_llobs$region, d_llobs$yy), sum)  
tthr <- tapply(d_llobs$thresher,  list(d_llobs$region, d_llobs$yy), sum)  
tshark <- tapply(d_llobs$othershk, list(d_llobs$region, d_llobs$yy), sum)  



png(file=paste(shkdir,"GRAPHICS/FIG_xx_shksP1000Hooks_deep.png",sep='')) 
par(mfrow=c(3,2))
for(i in 1:nreg){
  
  if(i!=1) { j<- i-1; tmat <- rbind( tbsh[j,], tmak[j,], tocs[j,], tfal[j,], tthr[j,], tshark[j,]) }
  
  if(i==1){barplot(matrix(NA,nreg, ncol(tmat),  dimnames=dimnames(tmat)), col=c(mycol, "white"), ylim=c(0,15000), names.arg=colnames(tmat),  main=paste("Region", i) )
  }
  if(i!=1) barplot(tmat, col=c(mycol, "white"), ylim=c(0,15000), names.arg=colnames(tmat),  main=paste("Region", i) )
  
  if(i %in% c(1) ) legend('topright',  legend  = c(spec,'OtherShark'),    bty='n',cex=0.8, horiz=F,ncol=2, fill =c(mycol,'white' ),xpd=T )
  if(i %in% c(3:4)) mtext("Sharks Observed" , side=2, line=3, outer=F )
}

dev.off()


#############################  #############################################################
#
# PURSE SEINE CATCH COMPOSITON 

# TOTAL first then  by set type.

PSObsShk<- PSObsShk[PSObsShk$yy > 1994,]
PSObsShk<- PSObsShk[PSObsShk$yy < 2015,]
#init calcs : sharks per 1000 hooks by region & yr
 
tbsh <- tapply(PSObsShk$blue, list(PSObsShk$region, PSObsShk$yy), sum)  
       tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(PSObsShk$mako, list(PSObsShk$region, PSObsShk$yy), sum)  
    tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(PSObsShk$ocs,  list(PSObsShk$region, PSObsShk$yy), sum)  
  tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(PSObsShk$silky,list(PSObsShk$region, PSObsShk$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(PSObsShk$thresher,  list(PSObsShk$region, PSObsShk$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(PSObsShk$othershk, list(PSObsShk$region, PSObsShk$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )
 


#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_set.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
   tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
    barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,25000),  main=paste("Region", i) )
   if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,  legend=c(spec,'OtherShark'), fill=c(mycol, "white")) 
  if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}

dev.off()
#-------------------------------------------------------------------------

#  NOW For ASSOCIATED AND UNASSOCIATED
tps <- PSObsShk[PSObsShk$asso =="A",]
dim(tps)

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)  
tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)  
tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)  
tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )



#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_ASSO.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,20000),  main=paste("Region", i) )
  if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,    legend=c(spec,'OtherShark'),   fill=c(mycol, "white")) 
  if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}
dev.off()

#____________________________________________
#_______ Un-Associated                      #   
#____________________________________________

#  NOW For ASSOCIATED AND UNASSOCIATED
tps <- PSObsShk[PSObsShk$asso =="U",]
dim(tps)

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)  
tbsh <- rbind( rep(NA, ncol(tbsh)  ), rep(NA, ncol(tbsh)  ), tbsh[-1,] )
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)  
tmak <- rbind( rep(NA, ncol(tmak)  ), rep(NA, ncol(tmak)  ), tmak[-1,] )
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)  
tocs <- rbind( rep(NA, ncol(tocs)  ), rep(NA, ncol(tocs)  ), tocs[-1,] )

tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)  
tfal <- rbind( rep(NA, ncol(tfal)  ), rep(NA, ncol(tfal)  ), tfal[-1,] )
#
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)  
tthr <- rbind( rep(NA, ncol(tthr)  ), rep(NA, ncol(tthr)  ), tthr[-1,] )


tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
tshark <- rbind( rep(NA, ncol(tshark)  ), rep(NA, ncol(tshark)  ), tshark[-1,] )



#-------------Start Plot
png(file=paste(shkdir,"GRAPHICS/FIG_xx_PS_shks_UNAS.png",sep='')) 
#
par(mfrow=c(3,2))
#i<-1
for(i in 1:nreg){
  tmat <- rbind( tbsh[i,], tmak[i,], tocs[i,], tfal[i,], tthr[i,], tshark[i,])
  barplot(tmat, col=c(mycol, "white"),  ,ylim=c(0,15000),  main=paste("Region", i) )
  if(i %in% c(1))   legend("center", ncol=2,bty='n',pt.cex=1.5,   legend=c(spec,'OtherShark'), col=1, fill=c(mycol, "white")) 
  if(i %in% c(3:4)) mtext("Number Recorded By Purse Seine Observers" , side=2, line=3, outer=F )
  
}
dev.off()





#
#
#





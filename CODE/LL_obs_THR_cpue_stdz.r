





####################################################################################################################  
###     
###		Content: General framework to implement the Zero Inflated Negative Binomial (ZINB )   and NB 
###		Purpose: R code used to compute relative abundance indices for  sharks
###	 # preliminary analysis of the BSH CPUE following Zurr - Zero inflated models and generalized linear mixed modesl with R 2012		 		
###
###		Author: Joel Rice 
###					
###		Notes: for shark indicator paper, needs to be split north and south.###			 
###
###		Index:
###
###			SECTION 1: Import data
###
###			SECTION 2: GLM CODE (NB and ZINB?)
###
###			SECTION 3: diagnostics and prediction
###
###							
####################################################################################################################

# 
# rm(list=ls(all=TRUE))
# gc()


##################################################################################
###		SECTION 1: Import data
##################################################################################

# Libraries needed for functions
library(MASS)
library(lattice) # needed in the deltalogboot
library(pscl)  
require(geepack)
require(MuMIn)
require(mgcv)
require(car) 
require(boot)
library(cairo)



#####
#rm(shkdir)
shkdir <- "C:/Projects/SHK-indicators-2015/"
#dir.create(shkdir, showWarnings = TRUE, recursive = TRUE)
mygrey<-rgb(red=220,green=220, blue=220, alpha=75, maxColorValue=255)  
cpue.mult<- 25  # max is 224       98% are less than 
myblue<-rgb(red=0,green=205, blue=255, alpha=175, maxColorValue=255)
mygold<-rgb(red=255,green=215, blue=0, alpha=105, maxColorValue=255)



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
load( file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" )   #loads shk_all (no SST data)

# this should have been done in the script that processed the data from a csv file.

shk_all <- shk_all[shk_all$yy %in% 1995:2014,]
shk_all <- shk_all[shk_all$region !=0, ]

head(shk_all)
#save(shk_all, file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" ) 

range(shk_all$yy)
table(shk_all$region)


with(shk_all, plot(newlon, newlat, pch=21, bg=mygrey, col=mygrey, main='Observed Thresher'))

table(shk_all$thresher)
tdat <- shk_all[shk_all$thresher %in% ] 
points( shk_all[shk_all$thresher>0,"newlon"] ,   shk_all[shk_all$thresher>0,"newlat"] , pch=21, col='black', bg=mygold, cex=1.5)

table(shk_all$thresher>0, shk_all$yy) # higher percentage of 


species <- "thresher"
sp_code <- "THR" 

plot()

DataFile <- shk_all[ , c( 'thresher', "THRCPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]
#----------------------------------------------------------------------------
# one thresher stock
#
#----------------------------------------------------------------------------
# Sort factor levels
for (i in 5:ncol(DataFile))
{
  DataFile[[i]] <- factor(DataFile[[i]], levels=(names(sort(table(DataFile[[i]]), decreasing=T))))
} 
#
# begin canditdate models 


###
TH1 <- glm.nb(  thresher ~ yy  , data=DataFile, offset(log(hook_est))  ); AIC(TH1)  #   927136
TH2 <- glm.nb(  thresher ~ yy + mm , data=DataFile, offset(log(hook_est)) ); AIC(TH2)  #  921058.7   
TH3 <- glm.nb(  thresher ~ yy + mm +flag_id, data=DataFile, offset(log(hook_est)) ); AIC(TH3)  #   895422.7
TH4 <- glm.nb(  thresher ~ yy + mm +flag_id+ hk_bt_flt , data=DataFile, offset(log(hook_est)) ); AIC(TH4)  #   878468.5
TH5 <- glm.nb(  thresher ~ yy + mm +flag_id+ hk_bt_flt +cell, data=DataFile, offset( log(hook_est)) ); AIC(TH5)  #   805842.2 
# takes a long time.
TH6 <- glm.nb(  thresher ~ yy + mm +flag_id+ hk_bt_flt +cell + lat5, data=DataFile, offset(log(hook_est) ) ); AIC(TH6)  #  Warning message: glm.fit: fitted rates numerically 0 occurred            805842.2
TH7 <- glm.nb(  thresher ~ yy + mm +flag_id+ hk_bt_flt +cell + TIMECAT, data=DataFile, offset(log(hook_est) ) ); AIC(TH7)  #   805688.4   

summary(TH5)
summary(TH7)

head(DataFile)
summary(DataFile)
#newdata

THd7<- expand.grid( yy  = as.factor(1995:2014), 
                   mm  = as.factor(3),
                   flag_id = as.factor("US"),
                   hk_bt_flt = as.factor("30"),
                   cell = as.factor("-42168"),
                   TIMECAT = as.factor(2),
                   hook_est = 2000)

# for  model 5
THd5<- expand.grid( yy  = as.factor(1995:2014), 
                   mm  = as.factor(3),
                   flag_id = as.factor("US"),
                   hk_bt_flt = as.factor("30"),
                   cell = as.factor("-42168"),
                   hook_est = 2000)

# for model 4
THd4 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hk_bt_flt = as.factor("30"),
                    hook_est = 2000)
# for model 3
THd3 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hook_est = 2000)
# for model 2
THd2 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    hook_est = 2000)
# for model 1
THd1 <- expand.grid( yy  = as.factor(1995:2014), 
                    hook_est = 2000)

#
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
#---------------------------------------------------------
#
#-----------------------------------prediction step 
# Predict asd plot the 'final model'  asd otehrs
#
#---------------------------------------------------------

THd7 <- cbind(THd7,   predict(TH7, newdata=THd7, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )


THd5<- cbind(THd5,   predict(TH5, newdata=THd5, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )

# model 4
THd4 <- cbind(THd4, predict(TH4, newdata=THd4, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
THd3 <- cbind(THd3, predict(TH3, newdata=THd3, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
THd2 <- cbind(THd2, predict(TH2, newdata=THd2, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
THd1 <- cbind(THd1, predict(TH1, newdata=THd1, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
#----------------------------

png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_THRESHER_NB_cpue.png",sep='') )  

plot(yrs, THd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,0.75), las=1)
lines( yrs, THd7$fit+1.96*THd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, THd7$fit-1.96*THd7$se.fit, type='o', col=grey(0.5), lty=3 )
dev.off()
#
#
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_THRESHER_NB_cpue_wnominal.png",sep='') )  

plot(yrs, THd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,0.75), las=1)
lines( yrs, THd7$fit+1.96*THd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, THd7$fit-1.96*THd7$se.fit, type='o', col=grey(0.5), lty=3 )
nom <- tapply(DataFile$thresher, DataFile$yy, mean)
lines(yrs, nom, col=2, lty=2, lwd=2, pch=16, type='o')
dev.off()
#----------------------------------------------------------------------------------------

# Make the step plot
yrs<- s.yr:e.yr
stepcol  <- rainbow(5)
# make the plot
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_THRESHER_NB_step.png",sep='') )  

plot(yrs, THd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,1.5), las=1)
#
lines(yrs, THd5$fit, col=stepcol[1], lty=2, lwd=1, type='o')
lines(yrs, THd4$fit, col=stepcol[2], lty=2, lwd=1, type='o')
lines(yrs, THd3$fit, col=stepcol[3], lty=2, lwd=1, type='o')
lines(yrs, THd2$fit, col=stepcol[4], lty=2, lwd=1, type='o')
lines(yrs, THd1$fit, col=stepcol[5], lty=2, lwd=1, type='o')

legend('topright', legend=c("yy",
                            "yy + mm", 
                            "yy + mm + flag_id", 
                            "yy + mm + flag_id + hk_bt_flt",
                            "yy + mm + flag_id + hk_bt_flt + cell",
                            "yy + mm + flag_id + hk_bt_flt + cell+TIMECAT"), 
       col=c(rev(stepcol),1), pch=c(1,1,1,1,1,16), bty='n', lty=c(2,2,2,2,2,1),lwd=2,  cex=.75)

dev.off()

###############






#######################################################################################################
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#######################################################################################################  
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_THRESHER_NB_diag.png",sep='') )  
par(mfrow=c(2,2))
plot(TH7)
dev.off() 
# note see the bottom of http://www.ats.ucla.edu/stat/r/dae/nbreg.htm for checking residuals
################################################################
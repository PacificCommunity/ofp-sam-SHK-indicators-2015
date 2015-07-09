







####################################################################################################################  
###     
###    Content: General framework to implement the Zero Inflated Negative Binomial (ZINB )   and NB 
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

# quick look at the catch
with(shk_all, plot(newlon, newlat, pch=21, bg=mygrey, col=mygrey, main='Observed \nOCeanic WHi'))
points( shk_all[shk_all$ocs>0,"newlon"] ,   shk_all[shk_all$ocs>0,"newlat"] , pch=21, col='black', bg=mygold, cex=1.5)

abline(h=c(-15,15))


table(shk_all$silky>0, shk_all$yy) # higher percentage of 


species <- "Oceanic Whitetip"
sp_code <- "OCS" 



DataFile <- shk_all[ , c( 'OCS', "OCSCPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]
dim(DataFile)
#----------------------------------------------------------------------------
# assume one silky stock core is between 20 and -20
#
#----------------------------------------------------------------------------
#
DataFile <- DataFile[abs(DataFile$newlat)<=20,]
dim(DataFile)
#
# Sort factor levels
for (i in 5:ncol(DataFile))
{
  DataFile[[i]] <- factor(DataFile[[i]], levels=(names(sort(table(DataFile[[i]]), decreasing=T))))
} 
#
# begin canditdate models 


###
OCS1 <- glm.nb(  OCS ~ yy  , data=DataFile, offset(log(hook_est))  ); AIC( OCS1 )  #     487992.5 
OCS2 <- glm.nb(  OCS ~ yy + mm , data=DataFile, offset(log(hook_est)) ); AIC( OCS2 )  #   486415.8
OCS3 <- glm.nb(  OCS ~ yy + mm +flag_id, data=DataFile, offset(log(hook_est)) ); AIC( OCS3 )  #   475236.3
OCS4 <- glm.nb(  OCS ~ yy + mm +flag_id+ hk_bt_flt , data=DataFile, offset(log(hook_est)) );  AIC( OCS4 )  #   467467.3
OCS5 <- glm.nb(  OCS ~ yy + mm +flag_id+ hk_bt_flt +cell, data=DataFile, offset( log(hook_est)) ); AIC( OCS5 )  #  438754.9
OCS7 <- glm.nb(  OCS ~ yy + mm +flag_id+ hk_bt_flt +cell + TIMECAT, data=DataFile, offset(log(hook_est) ) ); AIC( OCS7 )  #  438597.6

head(DataFile)
summary(DataFile)
#newdata

OCSd7<- expand.grid( yy  = as.factor(1995:2014), 
                     mm  = as.factor(3),
                     flag_id = as.factor("US"),
                     hk_bt_flt = as.factor("30"),
                     cell = as.factor("18198"),
                     TIMECAT = as.factor(2),
                     hook_est = 2000)

# for  model 5
OCSd5<- expand.grid( yy  = as.factor(1995:2014), 
                     mm  = as.factor(3),
                     flag_id = as.factor("US"),
                     hk_bt_flt = as.factor("30"),
                     cell = as.factor("18198"),
                     hook_est = 2000)

# for model 4
OCSd4 <- expand.grid( yy  = as.factor(1995:2014), 
                      mm  = as.factor(3),
                      flag_id = as.factor("US"),
                      hk_bt_flt = as.factor("30"),
                      hook_est = 2000)
# for model 3
OCSd3 <- expand.grid( yy  = as.factor(1995:2014), 
                      mm  = as.factor(3),
                      flag_id = as.factor("US"),
                      hook_est = 2000)
# for model 2
OCSd2 <- expand.grid( yy  = as.factor(1995:2014), 
                      mm  = as.factor(3),
                      hook_est = 2000)
# for model 1
OCSd1 <- expand.grid( yy  = as.factor(1995:2014), 
                      hook_est = 2000)

#
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
#---------------------------------------------------------
#
#-----------------------------------prediction step 
# Predict asd plot the 'final model'  asd otehrs
#
#---------------------------------------------------------

OCSd7 <- cbind(OCSd7,   predict(OCS7, newdata=OCSd7, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )


OCSd5<- cbind(OCSd5,   predict(OCS5, newdata=OCSd5, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )

# model 4
OCSd4 <- cbind(OCSd4, predict( OCS4, newdata = OCSd4, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
OCSd3 <- cbind( OCSd3, predict(OCS3, newdata = OCSd3, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
OCSd2 <- cbind(OCSd2, predict(OCS2, newdata=OCSd2, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
OCSd1 <- cbind(OCSd1, predict(OCS1, newdata=OCSd1, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
#----------------------------

png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_OCS_NB_cpue.png",sep='') )  

plot(yrs, OCSd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,0.5), las=1)
lines( yrs, OCSd7$fit+1.96*OCSd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, OCSd7$fit-1.96*OCSd7$se.fit, type='o', col=grey(0.5), lty=3 )
dev.off()
#
#
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_OCS_NB_cpue_wnominal.png",sep='') )  

plot(yrs, OCSd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,5), las=1)
lines( yrs, OCSd7$fit+1.96*OCSd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, OCSd7$fit-1.96*OCSd7$se.fit, type='o', col=grey(0.5), lty=3 )
nom <- tapply(DataFile$OCS, DataFile$yy, mean)
lines(yrs, nom, col=2, lty=2, lwd=2, pch=16, type='o')
dev.off()
#----------------------------------------------------------------------------------------

# Make the step plot
yrs<- s.yr:e.yr
stepcol  <- rainbow(5)
# make the plot
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_OCS_NB_step.png",sep='') )  

plot(yrs, OCSd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,3.5), las=1)
#
lines(yrs, OCSd5$fit, col=stepcol[1], lty=2, lwd=1, type='o')
lines(yrs, OCSd4$fit, col=stepcol[2], lty=2, lwd=1, type='o')
lines(yrs, OCSd3$fit, col=stepcol[3], lty=2, lwd=1, type='o')
lines(yrs, OCSd2$fit, col=stepcol[4], lty=2, lwd=1, type='o')
lines(yrs, OCSd1$fit, col=stepcol[5], lty=2, lwd=1, type='o')

legend('topright', legend=c("yy",
                            "yy + mm", 
                            "yy + mm + flag_id", 
                            "yy + mm + flag_id + hk_bt_flt",
                            "yy + mm + flag_id + hk_bt_flt + cell",
                            "yy + mm + flag_id + hk_bt_flt + cell+TIMECAT"), 
       col=c(rev(stepcol),1), pch=c(1,1,1,1,1,16), bty='n', lty=c(2,2,2,2,2,1),lwd=2,  cex=.75)

dev.off()


#######################################################################################################
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#######################################################################################################  
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_OCS_NB_diag.png",sep='') )  
par(mfrow=c(2,2))
plot(OCS7)
dev.off() 
# note see the bottom of http://www.ats.ucla.edu/stat/r/dae/nbreg.htm for checking residuals
################################################################


save.image ( file ='C:/Projects/SHK-indicators-2015/GRAPHICS/CPUE_std/CPUEwork_240515.rdata' )

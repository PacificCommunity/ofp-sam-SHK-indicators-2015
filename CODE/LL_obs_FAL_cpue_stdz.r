







####################################################################################################################  
###     
###  	Content: General framework to implement the Zero Inflated Negative Binomial (ZINB )   and NB 
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
with(shk_all, plot(newlon, newlat, pch=21, bg=mygrey, col=mygrey, main='Observed Silky'))
points( shk_all[shk_all$silky>0,"newlon"] ,   shk_all[shk_all$silky>0,"newlat"] , pch=21, col='black', bg=mygold, cex=1.5)

abline(h=c(-15,15))


table(shk_all$silky>0, shk_all$yy) # higher percentage of 


species <- "silky"
sp_code <- "FAL" 

 

DataFile <- shk_all[ , c( 'silky', "SILKYCPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]
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
FAL1 <- glm.nb(  silky ~ yy  , data=DataFile, offset(log(hook_est))  ); AIC( FAL1 )  #    805688.4
FAL2 <- glm.nb(  silky ~ yy + mm , data=DataFile, offset(log(hook_est)) ); AIC( FAL2 )  # 719599.4 
FAL3 <- glm.nb(  silky ~ yy + mm +flag_id, data=DataFile, offset(log(hook_est)) ); AIC( FAL3 )  #   716034.9 
FAL4 <- glm.nb(  silky ~ yy + mm +flag_id+ hk_bt_flt , data=DataFile, offset(log(hook_est)) );  AIC( FAL4 )  #  615602.8 alternation limit reached but  FAL4$converged = true
FAL5 <- glm.nb(  silky ~ yy + mm +flag_id+ hk_bt_flt +cell, data=DataFile, offset( log(hook_est)) ); AIC( FAL5 )  #    555742.9
#FAL6 <- glm.nb(  silky ~ yy + mm +flag_id+ hk_bt_flt +cell + lat5, data=DataFile, offset(log(hook_est) ) ); AIC( FAL6 )  # glm.fit: algorithm did not converge  
FAL7 <- glm.nb(  silky ~ yy + mm +flag_id+ hk_bt_flt +cell + TIMECAT, data=DataFile, offset(log(hook_est) ) ); AIC( FAL7 )  # 554314.4

head(DataFile)
summary(DataFile)
#newdata

FALd7<- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hk_bt_flt = as.factor("30"),
                    cell = as.factor("18198"),
                    TIMECAT = as.factor(2),
                    hook_est = 2000)

# for  model 5
FALd5<- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hk_bt_flt = as.factor("30"),
                    cell = as.factor("18198"),
                    hook_est = 2000)

# for model 4
 FALd4 <- expand.grid( yy  = as.factor(1995:2014), 
                       mm  = as.factor(3),
                       flag_id = as.factor("US"),
                       hk_bt_flt = as.factor("30"),
                       hook_est = 2000)
# for model 3
FALd3 <- expand.grid( yy  = as.factor(1995:2014), 
                     mm  = as.factor(3),
                     flag_id = as.factor("US"),
                     hook_est = 2000)
# for model 2
FALd2 <- expand.grid( yy  = as.factor(1995:2014), 
                     mm  = as.factor(3),
                     hook_est = 2000)
# for model 1
FALd1 <- expand.grid( yy  = as.factor(1995:2014), 
                     hook_est = 2000)

#
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
#---------------------------------------------------------
#
#-----------------------------------prediction step 
# Predict asd plot the 'final model'  asd otehrs
#
#---------------------------------------------------------

FALd7 <- cbind(FALd7,   predict(FAL7, newdata=FALd7, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )


 FALd5<- cbind(FALd5,   predict(FAL5, newdata=FALd5, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )

# model 4
FALd4 <- cbind(FALd4, predict( FAL4, newdata = FALd4, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
FALd3 <- cbind( FALd3, predict(FAL3, newdata = FALd3, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
FALd2 <- cbind(FALd2, predict(FAL2, newdata=FALd2, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
FALd1 <- cbind(FALd1, predict(FAL1, newdata=FALd1, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
#----------------------------

png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_FAL_NB_cpue.png",sep='') )  

plot(yrs, FALd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,0.15), las=1)
lines( yrs, FALd7$fit+1.96*FALd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, FALd7$fit-1.96*FALd7$se.fit, type='o', col=grey(0.5), lty=3 )
dev.off()
#
#
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_FAL_NB_cpue_wnominal.png",sep='') )  

plot(yrs, FALd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,5), las=1)
lines( yrs, FALd7$fit+1.96*FALd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, FALd7$fit-1.96*FALd7$se.fit, type='o', col=grey(0.5), lty=3 )
nom <- tapply(DataFile$silky, DataFile$yy, mean)
lines(yrs, nom, col=2, lty=2, lwd=2, pch=16, type='o')
dev.off()
#----------------------------------------------------------------------------------------

# Make the step plot
yrs<- s.yr:e.yr
stepcol  <- rainbow(5)
# make the plot
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_FAL_NB_step.png",sep='') )  

plot(yrs, FALd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,5), las=1)
#
lines(yrs, FALd5$fit, col=stepcol[1], lty=2, lwd=1, type='o')
lines(yrs, FALd4$fit, col=stepcol[2], lty=2, lwd=1, type='o')
lines(yrs, FALd3$fit, col=stepcol[3], lty=2, lwd=1, type='o')
lines(yrs, FALd2$fit, col=stepcol[4], lty=2, lwd=1, type='o')
lines(yrs, FALd1$fit, col=stepcol[5], lty=2, lwd=1, type='o')

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
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_SILKY_NB_diag.png",sep='') )  
par(mfrow=c(2,2))
plot(FAL7)
dev.off() 
# note see the bottom of http://www.ats.ucla.edu/stat/r/dae/nbreg.htm for checking residuals
################################################################
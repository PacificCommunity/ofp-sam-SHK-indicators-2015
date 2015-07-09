



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


species <- "mako"
sp_code <- "MAK" 


DataFile <- shk_all[ , c( 'mako', "MAKOCPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]
#----------------------------------------------------------------------------
#
# start with the southern hemisphere 
#
#----------------------------------------------------------------------------
DataFile <-     DataFile[    DataFile$newlat<0,]
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Sort factor levels
for (i in 5:ncol(DataFile))
{
  DataFile[[i]] <- factor(DataFile[[i]], levels=(names(sort(table(DataFile[[i]]), decreasing=T))))
} 
#
# begin canditdate models 


###
S1 <- glm.nb(  mako ~ yy  , data=DataFile, offset(log(hook_est))  ); AIC(N1)  # 336032.8 
S2 <- glm.nb(  mako ~ yy + mm , data=DataFile, offset(log(hook_est)) ); AIC(N2)  #   331303.3
S3 <- glm.nb(  mako ~ yy + mm +flag_id, data=DataFile, offset(log(hook_est)) ); AIC(N3)  # 318440
S4 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt , data=DataFile, offset(log(hook_est)) ); AIC(N4)  #  314143.2
S5 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell, data=DataFile, offset( log(hook_est)) ); AIC(N5)  #   298468.6
# S6 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell + lat5, data=DataFile, offset(log(hook_est) ) ); AIC(N6)  #  298468.6
S7 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell + TIMECAT, data=DataFile, offset(log(hook_est) ) ); AIC(N7)  # 298321.4 * 



head(DataFile)
summary(DataFile)
#newdata

sd7<- expasd.grid( yy  = as.factor(1995:2014), 
                  mm  = as.factor(3),
                  flag_id = as.factor("US"),
                  hk_bt_flt = as.factor("30"),
                  cell = as.factor("-42168"),
                  TIMECAT = as.factor(2),
                  hook_est = 2000)

# for  model 5
sd5<- expasd.grid( yy  = as.factor(1995:2014), 
                        mm  = as.factor(3),
                        flag_id = as.factor("US"),
                        hk_bt_flt = as.factor("30"),
                        cell = as.factor("-42168"),
                        hook_est = 2000)

# for model 4
sd4 <- expasd.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hk_bt_flt = as.factor("30"),
                    hook_est = 2000)
# for model 3
sd3 <- expasd.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hook_est = 2000)
# for model 2
sd2 <- expasd.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    hook_est = 2000)
# for model 1
sd1 <- expasd.grid( yy  = as.factor(1995:2014), 
                    hook_est = 2000)

#
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
#---------------------------------------------------------
#
#-----------------------------------prediction step 
# Predict asd plot the 'final model'  asd otehrs
#
#---------------------------------------------------------

sd7 <- cbisd(sd7,   predict(S7, newdata=sd7, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )


sd5<- cbisd(sd5,   predict(S5, newdata=sd5, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )

# model 4
sd4 <- cbisd(sd4, predict(S4, newdata=sd4, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
sd3 <- cbisd(sd3, predict(S3, newdata=sd3, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
sd2 <- cbisd(sd2, predict(S2, newdata=sd2, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
sd1 <- cbisd(sd1, predict(S1, newdata=sd1, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
#----------------------------

png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAKO_south_NB_cpue.png",sep='') )  

plot(yrs, sd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Stasdardized CPUE', ylim=c(0,0.5), las=1)
  lines( yrs, sd7$fit+1.96*sd7$se.fit, type='o', col=grey(0.5), lty=3 )
  lines( yrs, sd7$fit-1.96*sd7$se.fit, type='o', col=grey(0.5), lty=3 )
dev.off()
#
#
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAKO_south_NB_cpue_wnominal.png",sep='') )  

plot(yrs, sd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Stasdardized CPUE', ylim=c(0,0.5), las=1)
lines( yrs, sd7$fit+1.96*sd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, sd7$fit-1.96*sd7$se.fit, type='o', col=grey(0.5), lty=3 )
nom <- tapply(DataFile$mako, DataFile$yy, mean)
lines(yrs, nom, col=4, lty=2, lwd=2, pch=16, type='o')
dev.off()
#----------------------------------------------------------------------------------------

# Make the step plot
yrs<- s.yr:e.yr
stepcol  <- rainbow(5)
# make the plot
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAKO_south_NB_step.png",sep='') )  

plot(yrs, sd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Stasdardized CPUE', ylim=c(0,1), las=1)
#
lines(yrs, sd5$fit, col=stepcol[1], lty=3, lwd=2, type='o')
lines(yrs, sd4$fit, col=stepcol[2], lty=3, lwd=2, type='o')
lines(yrs, sd3$fit, col=stepcol[3], lty=3, lwd=2, type='o')
lines(yrs, sd2$fit, col=stepcol[4], lty=3, lwd=2, type='o')
lines(yrs, sd1$fit, col=stepcol[5], lty=3, lwd=2, type='o')
#

legesd('topright', legesd=c("yy",
                            "yy + mm ", 
                            "yy + mm + flag_id", 
                            "yy + mm + flag_id + hk_bt_flt",
                            "yy + mm + flag_id + hk_bt_flt + cell",
                            "yy + mm + flag_id + hk_bt_flt + cell+TIMECAT"), 
       col=c(rev(stepcol),1), pch=c(1,1,1,1,1,16), bty='n', lty=c(3,3,3,3,3,1),lwd=2,  cex=.75)

dev.off()

###############

 




#######################################################################################################
#      Diagnostics                                                                                    #
#      Diagnostics            # Southern   hemisphere                                                  #
#      Diagnostics                                                                                    #
#######################################################################################################  
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAK_south_NB_diag.png",sep='') )  
par(mfrow=c(2,2))
plot(N7)
dev.off() 
# note see the bottom of http://www.ats.ucla.edu/stat/r/dae/nbreg.htm for checking residuals
################################################################
############################################################################################# 
#                                                                                           #
#                                                                                           #
#                                                                                           #
####################################################################################################### 
#######################################################################################################
#       Start the                                                                                     #
#      Northern   hemisphere                                                                          #
#                                                                                                     #
#######################################################################################################  

 
#
DataFile <- shk_all[ , c( 'mako', "MAKOCPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]
#----------------------------------------------------------------------------
#
# Northern hemisphere 
#
#----------------------------------------------------------------------------
DataFile <-     DataFile[    DataFile$newlat>0,]
#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
# Sort factor levels
for (i in 5:ncol(DataFile))
{
  DataFile[[i]] <- factor(DataFile[[i]], levels=(names(sort(table(DataFile[[i]]), decreasing=T))))
} 
#
# begin canditdate models 

# northern stock
###
Nrth1 <- glm.nb(  mako ~ yy  , data=DataFile, offset(log(hook_est))  ); AIC(Nrth1)  #  517195.2
Nrth2 <- glm.nb(  mako ~ yy + mm , data=DataFile, offset(log(hook_est)) ); AIC(Nrth2)  #  505426.4  
Nrth3 <- glm.nb(  mako ~ yy + mm +flag_id, data=DataFile, offset(log(hook_est)) ); AIC(Nrth3)  #  504916.6
Nrth4 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt , data=DataFile, offset(log(hook_est)) ); AIC(Nrth4)  #  498317.9 
Nrth5 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell, data=DataFile, offset( log(hook_est)) ); AIC(Nrth5)  #   483274.1
#Nrth6 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell + lat5, data=DataFile, offset(log(hook_est) ) ); AIC(Nrth6)  # 483274.1  
Nrth7 <- glm.nb(  mako ~ yy + mm +flag_id+ hk_bt_flt +cell + TIMECAT, data=DataFile, offset(log(hook_est) ) ); AIC(Nrth7)  #  482789.2



head(DataFile)
summary(DataFile)
#newdata

Nrthd7<- expand.grid( yy  = as.factor(1995:2014), 
                   mm  = as.factor(3),
                   flag_id = as.factor("US"),
                   hk_bt_flt = as.factor("30"),
                   cell = as.factor("18198"),
                   TIMECAT = as.factor(2),
                   hook_est = 2000)

# for  model 5
Nrthd5<- expand.grid( yy  = as.factor(1995:2014), 
                   mm  = as.factor(3),
                   flag_id = as.factor("US"),
                   hk_bt_flt = as.factor("30"),
                   cell = as.factor("18198"),
                   hook_est = 2000)

# for model 4
Nrthd4 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hk_bt_flt = as.factor("30"),
                    hook_est = 2000)
# for model 3
Nrthd3 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    flag_id = as.factor("US"),
                    hook_est = 2000)
# for model 2
Nrthd2 <- expand.grid( yy  = as.factor(1995:2014), 
                    mm  = as.factor(3),
                    hook_est = 2000)
# for model 1
Nrthd1 <- expand.grid( yy  = as.factor(1995:2014), 
                    hook_est = 2000)

#
# http://www.ats.ucla.edu/stat/r/dae/nbreg.htm
#---------------------------------------------------------
#
#-----------------------------------prediction step 
# Predict and plot the 'final model'  and others
#
summary(DataFile)
#---------------------------------------------------------

Nrthd7 <- cbind(Nrthd7,   predict(Nrth7, newdata=Nrthd7, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )


Nrthd5<- cbind(Nrthd5,   predict(Nrth5, newdata=Nrthd5, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )

# model 4
Nrthd4 <- cbind(Nrthd4, predict(Nrth4, newdata=Nrthd4, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
Nrthd3 <- cbind(Nrthd3, predict(Nrth3, newdata=Nrthd3, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
Nrthd2 <- cbind(Nrthd2, predict(Nrth2, newdata=Nrthd2, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
Nrthd1 <- cbind(Nrthd1, predict(Nrth1, newdata=Nrthd1, type='response',  se.fit = TRUE, MC = 2500, conf = .95)  )
#
#
#----------------------------

png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAKO_north_NB_cpue.png",sep='') )  

plot(yrs,  Nrthd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,0.5), las=1)
lines( yrs, Nrthd7$fit+1.96*nd7$se.fit, type='o', col=grey(0.5), lty=3 )
lines( yrs, Nrthd7$fit-1.96*nd7$se.fit, type='o', col=grey(0.5), lty=3 )
dev.off()

#----------------------------------------------------------------------------------------

# Make the step plot
yrs<- s.yr:e.yr
stepcol  <- rainbow(5)
# make the plot
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAKO_north_NB_step.png",sep='') )  

plot(yrs, Nrthd7$fit, type='o', col=1, pch=16, lwd=2, xlab='Year', ylab='Standardized CPUE', ylim=c(0,1), las=1)
#
lines(yrs, Nrthd5$fit, col=stepcol[1], lty=3, lwd=2, type='o')
lines(yrs, Nrthd4$fit, col=stepcol[2], lty=3, lwd=2, type='o')
lines(yrs, Nrthd3$fit, col=stepcol[3], lty=3, lwd=2, type='o')
lines(yrs, Nrthd2$fit, col=stepcol[4], lty=3, lwd=2, type='o')
lines(yrs, Nrthd1$fit, col=stepcol[5], lty=3, lwd=2, type='o')
#

legend('topright', legend=c("yy",
                            "yy + mm ", 
                            "yy + mm + flag_id", 
                            "yy + mm + flag_id + hk_bt_flt",
                            "yy + mm + flag_id + hk_bt_flt + cell",
                            "yy + mm + flag_id + hk_bt_flt + cell+TIMECAT"), 
       col=c(rev(stepcol),1), pch=c(1,1,1,1,1,16), bty='n', lty=c(3,3,3,3,3,1),lwd=2,  cex=.75)


dev.off()

###############





#######################################################################################################
#      Diagnostics                                                                                    #
#      Diagnostics            # Northern  hemisphere                                                  #
#      Diagnostics                                                                                    #
#######################################################################################################  
png(file=paste(shkdir,"GRAPHICS/CPUE_std/LLcpue_MAK_north_NB_diag.png",sep='') )  
par(mfrow=c(2,2))
plot(Nrth7)
dev.off() 
# note see the bottom of http://www.ats.ucla.edu/stat/r/dae/nbreg.htm for checking residuals
################################################################




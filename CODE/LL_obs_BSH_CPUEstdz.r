
####################################################################################################################  
### 		
###		Content: General framework to implement the Zero Inflated Negative Binomial (ZINB  and ZIP
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
###			SECTION 2: ZINB CODE
###
###			SECTION 3: 
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
load( file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" )   #loads shk_all (no SST data)
head(shk_all)
 
species <- "blue"
sp_code <- "BSH" 


DataFile <- shk_all[ , c( 'blue', "BLUECPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]

# start with the south
# 
DataFile <-     DataFile[    DataFile$newlat<0,]

# Sort factor levels
for (i in 5:ncol(DataFile))
{
  DataFile[[i]] <- factor(DataFile[[i]], levels=(names(sort(table(DataFile[[i]]), decreasing=T))))
} 
#
# begin canditdate models    NEGATIVE BINOMIAL
#
  N1 <- zeroinfl(blue ~ yy + TIMECAT + vesselname | yy + hk_bt_flt,dist ="negbin",  offset=log(hook_est), data=DataFile); AIC(N1)  #singular  
   
#  N1 <- zeroinfl(blue~yy+mm+SST  |yy+cell ,  dist ="negbin", offset=log(hook_est), data=DataFile); AIC(N1)      #           Error in solve.default(as.matrix(fit$hessian)) :    
#   N1 <- zeroinfl(blue~yy+TIMECAT+SST+target+obsv_id|yy+hk_bt_flt+target+TIMECAT,  dist ="negbin", offset=log(hook_est), data=DataFile); AIC(N1)      #    Error in solve.default(as.matrix(fit$hessian)) :    
#    N1 <- zeroinfl(blue ~ yy + cell | yy + mm,dist ="negbin",  offset=log(hook_est), data=DataFile); AIC(N1)      #   Error in solve.default(as.matrix 

# N10c <- zeroinfl(blue ~ yy + mm +  SST + flag_id + hk_bt_flt + cell | yy + mm +lat5,dist ="negbin",  offset=log(hook_est),link="logit",  data 
N10 <- zeroinfl(blue ~ yy + mm + flag_id + hk_bt_flt + cell | yy + mm +lat5,dist ="negbin",  offset=log(hook_est), data=DataFile); AIC(N10)   97844.56           
#  N11 <- zeroinfl(blue ~ yy + mm +  SST + flag_id + hk_bt_flt + cell + offset(log(hook_est))| yy + mm +lat5 + wire_trace2,dist ="negbin",  link="logit",  data=DataFile); AIC(N11)   # 75619.7
# N11b <- zeroinfl(blue ~ yy + mm +  SST + flag_id + hk_bt_flt + cell + offset(log(hook_est))| yy + mm +lat5 + wire_trace,dist ="negbin",  link="logit",  data=DataFile); AIC(N11b)   #  Error in optiomnon-finite value supplied by optim
# N12 <- zeroinfl(blue ~ yy + mm +  SST + flag_id + hk_bt_flt + cell + wire_trace2+offset(log(hook_est))| yy + mm +lat5 + wire_trace2,dist ="negbin",  link="logit",  data=DataFile); AIC(N12)   #  
NB_best <- N10
summary(NB_best)
modDF <-   NB_best$n  - NB_best$df.residual

N <- nrow(DataFile)
E4 <- resid(NB_best, type= "pearson")
Dispersion <- sum(E4^2) / (N- modDF)  
Dispersion     # slightly  over dispersed with parameter of   1.403395

#
#  the zinb models are not all working and taking long time to run.
#




#######################################################################################################
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#      Diagnostics                                                                                    #
#######################################################################################################  

NB_best <- N10
windows(record=T)
par(mfrow=c(3,2))
plot(resid(NB_best,type="pearson") ~ sort(as.numeric(as.character(DataFile$yy)) ), ylab='Year', xlab='Pearson Residual' )
plot(resid(NB_best,type="pearson") ~ sort(as.numeric(as.character(DataFile$mm)) ), ylab='Year', xlab='Pearson Residual' )

  
plot(resid(NB_best,type="pearson") ~ DataFile$flag_id)                                                                                                                  

plot(resid(NB_best,type="pearson") ~ DataFile$hk_bt_flt)
plot(resid(NB_best,type="pearson") ~ DataFile$cell)

plot(resid(NB_best,type="pearson") ~ fitted(NB_best))


title("Zero Inflated Negative Poisson")

# savePlot(file=paste(GraphicsFolder,  "/ZIP_diag_", sp_code, sep=''),type="png")  

################################################################
 

NewData <- expand.grid( yy  = as.factor(1995:2014), 
                        mm  = as.factor(6),
                        flag_id = as.factor("AU"),
                        hk_bt_flt = as.factor("10"),
                        cell = as.factor("-02142"),
                        lat5 = as.factor("-7.5"),
                        wire_trace2=as.factor('Y'),
                        hook_est = 2000)
head(NewData) ; dim(NewData)                      
NewData$Ncount <- predict(NB_best, newdata=NewData, type = "count")                         
NewData$Nzero <- predict(NB_best, newdata=NewData, type = "zero") 
NewData$Nresp <- predict(NB_best, newdata=NewData, type = "response") 

png(file=paste(shkdir,"GRAPHICS/FIG_xx_S_BSH_CPUE_zinb_comp.png",sep='')) 
par(mfrow=c(2,2))
plot(1995:2014,  NewData$Ncount , type='b',xlab='year', col=4,ylab="ZINB Predicted count", ylim=c(0,2)) 
plot(1995:2014,  NewData$Nzero , type='b',xlab='year', col=4,ylab="ZINB Predicted Probabilities", ylim=c(0,1.2))                             
plot(1995:2014,  NewData$Nresp , type='b',xlab='year', col=4,ylab="ZINB Predicted Response", lwd=2, ylim=c(0,1))                             
title("BSH ZINB Components - S. Pacific  ", outer=T, line=-2)          

#savePlot(paste(shkdir,"GRAPHICS/", sp_code, "ZINB_comp_output_S",sep='') , type="png") 
dev.off()
#---------------------------------
png(file=paste(shkdir,"GRAPHICS/FIG_xx_S_BSH_CPUE_zinb_res.png",sep=''))
par(mfrow=c(1,1))    
plot(1995:2014,  NewData$Nresp , type='b',xlab='year', col=4,ylab="Standardized CPUE", lwd=2, ylim=c(0,1))    
title("BSH ZINB Response S. Pacific ", outer=T, line=-2)          
dev.off()
#savePlot(paste(GraphicsFolder,"/", sp_code, "ZINB_output_soHemiOnly",sep='') , type="png") 





#save(BSH_zinb_out, N10, DataFile, pred.results,  file="H:/BLUE_SHARK_ALL/Data/BSH_zinb_LL_SoPac_NoJPNoNZ.rdata") 

###########################################################################################################################################
###########################################################################################################################################
###########################################################################################################################################





################################################################################
#    Prediction
#    Prediction
#    Prediction
#    Prediction
################################################################################        

# 
# nms<-names( M_best$coefficients[[1]]  )
#   valls<-sort( M_best$coefficients[[1]] [grep( "TIMECAT",nms)])
#   valls[length(valls)/2]   
#   
# #   valls<-sort( M_best$coefficients[[1]] [grep( "vesselname",nms)])
#   valls[length(valls)/2]     
#   #
#   nms<-names( M_best$coefficients[[2]]  )
#   valls<-sort( M_best$coefficients[[2]] [grep( "hk_bt_flt",nms)])
#   valls[length(valls)/2]      
#   ################################################################################      
#   


#set up 


#predict main effects  
yhat <-  predict(N10, newdata=NewData, type = "response" ,  se.fit=T,MC=2500)
yrs<-  as.numeric(as.character(NewData$yy)  )
#first look at the data
plot(x= yrs,    y=yhat  ,xlab="Year",ylab="Predicted Response",type="b", col=4, pch=19 )  


# get CI's for coeffecients 
MODci<-confint( N10)
#MODci    check
dim(MODci)

# store coeff
nms1<-names(  NB_best$coefficients[[1]] )
# len1<- length(M_best$coefficients[[1]])
NB_best$coefficients[[1]][2:20] <- MODci[2:20,1] # just the year effects
names(  NB_best$coefficients[[1]] )<- nms1

#     nms2<-names(  M_best$coefficients[[2]] )
#      M_best$coefficients[[2]][c(2):c(15)] <-  MODci[c(len1+2):c(len1+15),1]
#   names(  M_best$coefficients[[2]] )<-  nms2
#
ylow<-predict( NB_best,newdata=NewData, se.fit=T,MC=2500)   #this is the default    
yhat #check
ylow

plot(x= yrs,    y=yhat  ,xlab="Year",ylab="Predicted Response",type="b", col=4, pch=19 )  
lines(x= yrs,    y=ylow  ,xlab="Year",ylab="Predicted Counts",type="b", col=1, pch=19 )   

NB_best <- N10
nms<-names(NB_best$coefficients[[1]] )
len1<- length( NB_best$coefficients[[1]])
NB_best$coefficients[[1]][2:20] <- MODci[2:20,2]
names(  NB_best$coefficients[[1]] )<-  nms1
#M_best$coefficients[2]
#   nms<-names(  NB_best$coefficients[[2]] )
#   NB_best$coefficients[[2]] <-  MODci[c(len1+1):dim(MODci)[1],2]
#   names(  M_best$coefficients[[2]] )<-  nms2
#
yhigh <-predict( NB_best,newdata=NewData, se.fit=T,MC=2500)   #this is the default 
lines(x= yrs,    y=yhigh  , col=1, pch=19 )   
yhigh    



#----------------------------------plot
png(file=paste(shkdir,"GRAPHICS/ll_cpue_BSHzinb.png",sep='')) 
par(mfrow=c(1,1))
plot(x= yrs,    y=yhat  ,xlab="Year",type="b", ylim=c(0,1),col=4, pch=19, ylab='Standardized CPUE', main="BSH, Longline Bycatch, ZINB ")  
lines(x= yrs,    y=ylow   ,type="b", col=grey(0.5), pch=19, lty=3 )  
lines(x= yrs,    y=yhigh ,type="b", col=grey(0.5), pch=19, lty=3 )   
legend('topleft', legend=c("ZINB CPUE", "95% CI" ), col=c(4,grey(0.5) ),    lty=c(1,3 ), lwd=c(2,1 ))
title('South Pacific', line=0.1)
dev.off()
#  savePlot(paste(GraphicsFolder,"/", sp_code, "ZINB_1_NoJP_NZ_soHemiOnly",sep='') , type="png") 


#-----------------------same plot with the nominal, so normalized
png(file=paste(shkdir,"GRAPHICS/ll_cpue_BSHzinb_nominal.png",sep='')) 
par(mfrow=c(1,1))# win the nominal CPUE

mnom<-tapply(DataFile[,'BLUECPUE'], INDEX=list(as.numeric(as.character(DataFile[,'yy']))), FUN=mean)
#mnom <- mnom[-c(17:18)]
plot(x= yrs,    y=yhat/max(yhat) ,xlab="Year",type="b", ylim=c(0,2),col=4, pch=19, ylab='Standardized CPUE (/ max.)', main="BSH, Longline Bycatch, ZINB ")  
lines(x= yrs,    y=ylow/max(yhat)   ,type="b", col=grey(0.5), pch=19, lty=3 )  
lines(x= yrs,    y=yhigh/max(yhat) ,type="b", col=grey(0.5), pch=19, lty=3 )   

lines(x= yrs,    y=mnom/max(mnom)   ,type="b", col=2, pch=19, lty=1 )   
legend('topleft', legend=c("Std. CPUE", "95% CI", "Nominal"), col=c(4,grey(0.5),2 ),    lty=c(1,3 ,1), lwd=c(2,1,2 ))
 
#savePlot(paste(GraphicsFolder,"/", sp_code, "",sep='') , type="png") 


dev.off()

BSH_zinb_out <- cbind( yy=1995:2014, yhat, ylow, yhigh, nominal_mean=mnom)




#save(BSH_zinb_out, N10, DataFile, pred.results,  file="H:/BLUE_SHARK_ALL/Data/BSH_zinb_LL_SoPac_NoJPNoNZ.rdata") 




################################################################################################
# DLN models
#
################################################################################################
head(shk_all)
table(shk_all$tar_sp_id)
DataFile <- shk_all[ , c( 'blue', "BLUECPUE","hook_est","newlat", "yy","mm","flag_id", "HPBCAT","TIMECAT","lat5","lon5","hk_bt_flt","vesselname","cell" ) ]

# start with the south
# 
DataFile <-     DataFile[    DataFile$newlat<0,]



DataFile$presabs <- ifelse(DataFile[,"blue"] > 0,1,0)       #vector of zeros and ones     length is equal to shk (n=45,952)
posdata <- DataFile[DataFile$blue > 0,]   



head(DataFile)
# begin canditdate models    Binomial
bsh_fmla <-as.formula( presabs ~ yy + mm  )
bsh_fmla2 <-as.formula( presabs ~ yy + mm + cell      )
bsh_fmla3 <-as.formula( presabs ~ yy + mm + cell + flag_id )
bsh_fmla4 <-as.formula( presabs ~ yy + mm + cell + flag_id + HPBCAT )
bsh_fmla5 <-as.formula( presabs ~ yy + mm + cell + flag_id + HPBCAT +TIMECAT )
bsh_fmla6 <-as.formula( presabs ~ yy + mm + cell + flag_id + HPBCAT + TIMECAT +vesselname )



#--~~~~  Binomial ~~~------#            
Bin1_01 <- glm(bsh_fmla,  family = "binomial",  na.action = "na.omit", data=DataFile);  AIC(Bin1_01) #     32615.7
Bin1_02 <- glm(bsh_fmla2, family = "binomial",  na.action = "na.omit", data=DataFile); AIC(Bin1_02)  #     26521.7 
Bin1_03 <- glm(bsh_fmla3, family = "binomial",  na.action = "na.omit", data=DataFile); AIC(Bin1_03)  #     26260.09
Bin1_04 <- glm(bsh_fmla4, family = "binomial",  na.action = "na.omit", data=DataFile); AIC(Bin1_04)  #     26247.69 
Bin1_05 <- glm(bsh_fmla5, family = "binomial",  na.action = "na.omit", data=DataFile); AIC(Bin1_05)  #     26206.34   **
Bin1_06 <- glm(bsh_fmla6, family = "binomial",  na.action = "na.omit", data=DataFile); AIC(Bin1_06)  #     #did not converge

#~~~~~~~~~Lognormal~~~~~~~~~#
PosMod <-  glm(BLUCPUE ~  as.factor(yy) +  as.factor(hk_bt_flt)  + as.factor(vesselname) + as.factor(TIMECAT) + as.factor(SHKLINE) , data=PosDat,family=gaussian(link="log")) ; AIC(PosMod)     # 


bsh_fmla <-as.formula( BLUECPUE ~ yy + mm  )
bsh_fmla2 <-as.formula( BLUECPUE ~ yy + mm + cell      )
bsh_fmla3 <-as.formula( BLUECPUE ~ yy + mm + cell + flag_id )
bsh_fmla4 <-as.formula( BLUECPUE ~ yy + mm + cell + flag_id + HPBCAT )
bsh_fmla5 <-as.formula( BLUECPUE ~ yy + mm + cell + flag_id + HPBCAT +TIMECAT )
bsh_fmla6 <-as.formula( BLUECPUE ~ yy + mm + cell + flag_id + HPBCAT + TIMECAT +vesselname )


log_01 <- glm(bsh_fmla,  family = gaussian(link="log"),  na.action = "na.omit", data=posdata);  AIC(log_01) #    105085.8
log_02 <- glm(bsh_fmla2, family = gaussian(link="log"),  na.action = "na.omit", data=posdata); AIC(log_02)  #    100820.1 
log_03 <- glm(bsh_fmla3, family = gaussian(link="log"),  na.action = "na.omit", data=posdata); AIC(log_03)  #    99614.06 
log_04 <- glm(bsh_fmla4, family = gaussian(link="log"),  na.action = "na.omit", data=posdata); AIC(log_04)  #    99597.33 
log_05 <- glm(bsh_fmla5, family = gaussian(link="log"),  na.action = "na.omit", data=posdata); AIC(log_05)  #    99301.36  **
log_06 <- glm(bsh_fmla6, family = gaussian(link="log"),  na.action = "na.omit", data=posdata); AIC(log_06)  #    99527.74
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# diagnostics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
png(file=paste(shkdir,"GRAPHICS/LLcpue_BSH_South_logDIAG.png",sep='') )  
par(mfrow=c(2,2))
plot(log_05)
dev.off()


png(file=paste(shkdir,"GRAPHICS/LLcpue_BSH_South_BIN_DIAG.png",sep='') )  
par(mfrow=c(2,2))
plot(Bin1_05)
dev.off()
# some high leverage points 1190, 19795  in addition to the inidcated values; 8457, 50788, 21875

head(DataFile)
 
ppos<-  round( with(DataFile, table(blue>0,as.numeric(yy) )[2,]/ colSums( with(DataFile, table(blue>0,as.numeric(yy)) )  ), 3)

  sort(ppos)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# prediction step
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# both models  are yy + mm + cell + flag_id + HPBCAT +TIMECAT  




PredData.bi <- expand.grid(yy=as.factor(1995:2014), 
                           mm=as.factor(6),  
                           cell=as.factor("-18178"), 
                           flag_id = as.factor("TW"),
                           HPBCAT=as.factor("S") , 
                           TIMECAT= as.factor(4)    )



PredData.lg <- expand.grid(yy=as.factor(1995:2014), 
                           mm=as.factor(6),  
                           cell=as.factor("-18178"), 
                           flag_id = as.factor("TW"),
                           HPBCAT=as.factor("S") , 
                           TIMECAT= as.factor(4)    )
#
pred.bi  <- predict(Bin1_05,  newdata = PredData.bi, se.fit = TRUE, type = "terms") 
pred.lg  <- predict(log_05, newdata = PredData.lg, se.fit = TRUE, type = "terms") 




yhat<-(exp(pred.bi$fit[,1])/(1+exp(pred.bi$fit[,1]))) * exp(pred.lg$fit[,1])

ci.bi <- data.frame(pred.bi$fit[,1] + matrix(pred.bi$se.fit[,1], ncol =  1) %*% c(-1.96, 1.96)) 
ci.lg <- data.frame(pred.lg$fit[,1] + matrix(pred.lg$se.fit[,1], ncol =  1) %*% c(-1.96, 1.96))   



cis<- (exp(ci.bi)/(1+exp(ci.bi)))* exp(ci.lg    )

yhigh<-cis[,2]
ylow<-cis[,1]
yrs<-c(1995:2014)  
par(mfrow=c(1,1))
plot(x= yrs,    y=yhat  ,xlab="Year",type="b", ylim=c(0,1.25*max(yhat)),col=4, pch=19, ylab='Standardized CPUE', main=paste(sp_code,"Longline DLN") ) 
lines(x= yrs,    y=cis[,1]   ,type="b", col=grey(0.5), pch=19, lty=3 )  
lines(x= yrs,    y=cis[,2] ,type="b", col=grey(0.5), pch=19, lty=3 )   
legend('topleft', legend=c("DLN CPUE", "95% CI" ), col=c(4,grey(0.5) ),    lty=c(1,3 ), lwd=c(2,1 ))
#
mnom<-tapply(DataFile[,'BLUECPUE'], INDEX=list(as.numeric(as.character(DataFile[,'yy']))), FUN=mean)

png(file=paste(shkdir,"GRAPHICS/FIG_xx_LLcpue_BSH_South.png",sep='') )  
plot(x= yrs,    y=yhat/max(yhat) ,xlab="Year",type="b", ylim=c(0,1.5),col=4, pch=19, ylab='Standardized CPUE (/ max.)', main=paste(sp_code,"Longline DLN") ) 
lines(x= yrs,    y=ylow/max(yhat)   ,type="b", col=grey(0.5), pch=19, lty=3 )  
lines(x= yrs,    y=yhigh/max(yhat) ,type="b", col=grey(0.5), pch=19, lty=3 )   

lines(x= yrs,    y=mnom /max(mnom )   ,type="b", col=2, pch=19, lty=1 )   
legend('topleft', legend=c("Std. CPUE", "95% CI", "Nominal"), col=c(4,grey(0.5),2 ),    lty=c(1,3 ,1), lwd=c(2,1,2 ))
dev.off()
 

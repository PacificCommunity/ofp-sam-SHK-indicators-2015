## shk-CPUE-standrz-by-species.r
## Standardizing shark CPUE from observer longline
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: June 30, 2015
## Time-stamp: <2015-07-01 17:07:45 lauratb>
shkdir <- "C:/Projects/SHK-indicators-2015/"
setwd(shkdir)

# Packages:
require(dplyr)
require(magrittr)
require(pscl) # for function zeroinfl
# define colors for the main species - use previous
hues <- mycol <- c(BSH="royalblue",MAK="gray",OCS="red",
                   FAL="mediumspringgreen",THR="sienna")

s.yr <- 1995 # start/end year
e.yr <- 2014
#
# make names and other init dec
spec <- c("BSH", "MAK", "OCS","FAL", "THR"); nspec <- length(spec)
scpue <- c("BLUECPUE", "MAKOCPUE", "OCSCPUE", "SILKYCPUE", "THRCPUE")
#
nreg <- 6 # number of regions
#
#eez <- read.table(file=paste0(shkdir,"DATA/EZNEW2.txt"), sep="",header=F)
#
if(!exists("shk_all")) {
    load("DATA/ll_obs_set_with_HW_11JUNE2015.rdata")   # shk_all (no SST data)
    shk_all %<>% filter(yy %between% c(s.yr, e.yr), region!=0)
    shk_all %<>% filter(flag_id != "NZ")
    mako.dat <- shk_all[,c("yy","mm","mako","hook","lond","latd","lon1","lat1","flag_id","hk_bt_flt")]
    mako.dat$pres <- ifelse(mako.dat$mako >0, 1, 0)

}

# HBF category: <=10 is surface S, otherwise deep D

expl.vars <- c("hook_est","newlat","yy","mm","flag_id","HPBCAT","TIMECAT",
               "lat5","lon5","hk_bt_flt","vesselname","cell")

model1 <- "resp ~ as.factor(yy) + offset(log(hook_est))"
model2 <- "resp ~ as.factor(yy) + as.factor(mm) + cs(hk_bt_flt) + offset(log(hook_est))"
model3 <- "resp ~ as.factor(yy) + as.factor(mm) + as.factor(flag_id)"
model4 <- "resp ~ as.factor(yy) + as.factor(mm) + as.factor(flag_id) + hk_bt_flt"
model4.spline <- "resp ~ as.factor(yy) + as.factor(mm) + as.factor(flag_id) + s(hk_bt_flt)"

message("Remove AUS and NZ program ids")

run.cpue.nb <- function(wsp="mako.south", wmodel=model1, dat2use=shk_all) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1 <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1 >= 0)
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point


    frml <- as.formula(gsub("resp",wsp,wmodel))
    #    mod <- glm.nb(frml, data=dat2use, offset(log(hook_est)))
    mod <- gam(frml, data=dat2use, offset(log(hook_est)))
    mod.aic <- AIC(mod)
    colv <- ifelse(dat2use$mako==0,"red","black")
    qresids <- qres.nbinom(mod)
    par(mfrow=c(1,2))
    qqnorm(qresids, las=1, col=colv); abline(0,1); hist(qresids, las=1)
    return(list(mod=mod, aic=mod.aic, qres=qresids, dat=dat2use))
}

run.cpue.nb.gamlss <- function(wsp="mako.south", wmodel=model1, dat2use=shk_all) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1 <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1 >= 0)
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point

    dat2use <- dat2use[,c("yy","mm","mako","program_code","flag_id","newlat","newlon","hk_bt_flt","timeofday","hook_est")]
    dat2use <- na.omit(dat2use)
    frml <- as.formula(gsub("resp",wsp,wmodel))
    #mod <- gamlss(frml, sigma.formula=~1, family=NBI(), data=dat2use, n.cyc=100)
    mod <- gamlss(frml, mu.formula=~1, nu.formula=~as.factor(yy), family=ZANBI(), data=dat2use, n.cyc=100)#, offset(log(dat2use$hook_est)))
    mod.aic <- AIC(mod)

 #   par(mfrow=c(1,2))
  #  qqnorm(qresids, las=1, col=colv); abline(0,1); hist(qresids, las=1)
    return(list(mod=mod, aic=mod.aic, dat=dat2use))#, qres=qresids, dat=dat2use))
}


run.cpue.tweedie <- function(wsp="mako.south", wmodel=model1, dat2use=shk_all) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1 <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1 >= 0)
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point


    frml <- as.formula(gsub("resp",wsp,wmodel))
    start.timer()
    mod0 <- gam(frml, data=dat2use, offset(log(hook_est)), family=tw())#using tw() estimates p -- scale parameter for tweedie
    mod.now <<- mod0
    tweedie.p <- as.numeric(gsub(".*=(.*).","\\1", mod0$family$family))
     print(tweedie.p)
    mod <- gam(frml, data=dat2use, offset(log(hook_est)), family=Tweedie(p=tweedie.p))

    stop.timer()
    mod.aic <- AIC(mod)
    qresids <- qres.tweedie(mod)
    par(mfrow=c(1,2))
    qqnorm(qresids, las=1); abline(0,1); hist(qresids, las=1)
    return(list(mod=mod, aic=mod.aic, qres=qresids))
}

run.cpue.zeroinfl <- function(wsp="mako.south", wmodel=model1, dat2use=shk_all) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1 <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1 >= 0)
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point

    frml <- as.formula(gsub("resp",wsp,wmodel))

    start.timer()
    mod <- zeroinfl(frml, dist="negbin", data=dat2use)#using tw() estimates p -- scale parameter for tweedie
    stop.timer()

    mod.aic <- AIC(mod)
    #qresids <- qres.tweedie(mod)
    #par(mfrow=c(1,2))
#    qqnorm(qresids, las=1); abline(0,1); hist(qresids, las=1)
    return(list(mod=mod, aic=mod.aic)) #, qres=qresids))
}

qnorm.by.fact <- function(wfact, df, wmod) {

    qres <- qqnorm(residuals(wmod), plot=FALSE)
    plot(1:10, type="n", xlim=c(-5,5), ylim=c(-5,5), ann=FALSE, las=1)
    abline(0,1)
    df$x <- qres$x
    df$y <- qres$y
    df$qqdev <- df$y-df$x
    df$fact <- df[,wfact]
    df %<>% arrange(x)
    df.spl <- split(df, df$fact)
    pchv <- rep(1:3,length.out=length(df.spl))
    colv <- col2transp(cpue.colpal(length(df.spl)))
    dmm <- sapply(1:length(df.spl), function(i) points(df.spl[[i]][,c("x","y")], col=colv[i], pch=1))
    legend("topleft", legend=names(df.spl), col=colv, pch=pchv, bty="n", ncol=2)
    invisible(df)
}

## shk-CPUE-standrz-by-species.r
## Standardizing shark CPUE from observer longline
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: June 30, 2015
## Time-stamp: <2015-07-02 19:47:29 lauratb>
shkdir <- "C:/Projects/SHK-indicators-2015/"
setwd(shkdir)
cpue.colpal <- function(n=12) {
    fb35 <- "#AC2020" #intermediate firebrick 3-4
    # other option going from neutral to red
    # colorRampPalette(c("wheat3","wheat2","orange1","indianred1","firebrick2",fb35))
    colorRampPalette(c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2",fb35))(n)
}

# Packages:

require(dplyr)
require(magrittr)
require(pscl) # for function zeroinfl
require(gamlss) # gams that fit scale parameters too
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
    shk_all$loghook <- log(shk_all$hook_est)
    mako.dat <- shk_all[,c("yy","mm","mako","hook","lond","latd","lon1","lat1","program_code","flag_id","hk_bt_flt")]
    mako.dat$pres <- ifelse(mako.dat$mako >0, 1, 0)

}

# HBF category: <=10 is surface S, otherwise deep D
expl.vars <- c("hook_est","newlat","yy","mm","flag_id","HPBCAT","TIMECAT",
               "lat5","lon5","hk_bt_flt","vesselname","cell")
expl.vars <- c("yy","mm","program_code","flag_id","newlat","newlon",
               "lon5","lat5","vesselname","cell","HPBCAT","hk_bt_flt","TIMECAT","timeofday","target","sharkbait","loghook","hook_est")

model1 <- "resp ~ as.factor(yy) + as.factor(mm) + offset(log(hook_est))"
model2 <- "resp ~ as.factor(yy) + as.factor(mm) + as.factor(HPBCAT) + offset(loghook)"
model3 <- "resp ~ as.factor(yy) + as.factor(mm) + as.factor(program_code) + as.factor(HPBCAT) + offset(loghook)"
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

run.cpue.nb.gamlss <- function(wsp="mako.north", wmodel=model1, dat2use=shk_all) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1 <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1 >= 0)
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point

    dat2use <- dat2use[,c(wsp, expl.vars)]
    dat2use$resp <- dat2use[,wsp]
    dat2use <- na.omit(dat2use)
    maxv <- quantile(dat2use$resp[dat2use$resp>0], 0.99)
    dat2use %<>% filter(resp <= maxv, program_code %nin% c("PGOB", "FMOB", "WSOB")) # removing PG helps
    frml <- as.formula(gsub("resp",wsp,wmodel))
    mod <- gamlss(frml, sigma.formula=~as.factor(yy) + as.factor(HPBCAT) + as.factor(program_code),# + as.factor(HPBCAT),
                  family=NBI(), data=dat2use, n.cyc=100)
    #mod <- gamlss(frml, nu.formula=~as.factor(yy), family=ZANBI(), data=dat2use, n.cyc=500)
    mod.aic <- AIC(mod)
    plot(mod)
    qres <- qqnorm(residuals(mod), plot=FALSE)
    dat2use$xqr <- qres$x
    dat2use$yqr <- qres$y
    dat2use$qrdiff <- qres$y - qres$x

    newdf <- data.frame(yy=1995:2014, mm="1", program_code="PFOB", HPBCAT="S", loghook=7.5)
    newdf$pred <- predict(mod, newdata=newdf, data=dat2use)
    return(list(mod=mod, aic=mod.aic, pred.df=newdf, dat=dat2use))
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

hist.by.fact <- function(wfact, df) {

    df$resp <- df$mako
    zero.time <- tapply(df$resp==0, df[,wfact], mean)
    breakv <- seq(0, max(df$resp))
    ymax <- quantile(df$resp, 0.999)
    make.histo <- function(wf) {

        dnow <- df %>% filter_(paste(wfact,"==",wf))
        propzero <- mean(dnow$resp==0)
        dnow %<>% filter(resp>0)
        hval <- hist(dnow$resp, breaks=breakv, plot=FALSE)
        xv <- hval$breaks
        yv <- hval$counts/sum(hval$counts)
        plot(xv[-1], yv, xlim=c(0,ymax), ann=FALSE, ylim=c(0,1), type="n", las=1)
        polygon(x=c(0,xv[-length(xv)]),y=c(0,yv), border=NA, col="grey")
        legend("topright", legend=round(propzero,2), bty="n")
        mtext(wf)
    }

    par(mai=rep(0.25,4))
    lay.mat <- rbind(1, matrix(2:21,nrow=4,byrow=TRUE))
    layout(lay.mat, height=c(2,1,1,1,1))
    plot(as.numeric(names(zero.time)), zero.time ,pch=19)
    levs <- unique(df[,wfact])
    dmm <- sapply(levs, make.histo)


}

fact.histo <- function(varx="hk_bt_flt", fact="program_code", dat=mako.dat, resp="mako") {

        f2k <- names(sort(table(dat[,fact]), TRUE))[1:12]
        xvals <- sort(unique(as.numeric(dat[,varx])))
        xcolv <- cpue.colpal(length(xvals))
        names(xcolv) <- xvals
        breakv <- 0:ceiling(quantile(dat[,resp], 0.999))



        make.set <- function(val) {
            val <- paste0("'",val,"'")
            dnow <- dat %>% filter_(paste(fact,"==",val))
            plot(dnow[,varx], dnow[,resp], ylim=range(breakv),xlim=range(xvals),pch=19,las=1,
                 col=xcolv[as.character(dnow[,varx])])
        mtext(paste0(gsub("'","",val)," (",nrow(dnow)," sets)"),adj=0)
        count.tbl <- table(dnow[,resp])
        pmai <- par("mai")
        par("mai"=c(pmai[1],0,pmai[3],0.5))
        barplot(count.tbl, border=NA, horiz=TRUE, names=NA, ylim=range(breakv), width=0.75, space=c(-0.325,rep(1/3,length(count.tbl)-1)))
        par("mai"=pmai)
    }

    par(mfrow=c(1,1),mai=c(0.25,0.25,0.5,0.05), omi=c(0.65,0.65,0.05,0.05), family="HersheySans")
    layout(matrix(1:24,nrow=4,byrow=TRUE),width=rep(2:1,4))
        dmm <- sapply(f2k, make.set)
        mtext("Catch (#indivs) per set", side=2, outer=TRUE, line=2, cex=1.2)
        mtext(varx, side=1, outer=TRUE, line=2, cex=1.2)
}

multi.sp <- function() {
    unique(unlist(lapply(s1, function(x) names(sort(colSums(x[,spcol]),TRUE))[1:5])))
    pairs(shk_all[,unique(unlist(lapply(s1, function(x) names(sort(colSums(x[,spcol]),TRUE))[1:5])))])
}

# good model for south mako re: residuals
# negative binomial
# mu.formula
# mako ~ as.factor(yy) + as.factor(mm) + as.factor(program_code) +  as.factor(HPBCAT) + offset(loghook)
# sigma.formula
# sigma~as.factor(yy) + as.factor(HPBCAT) + as.factor(program_code)

# PG removed, < 100 sets removed

## shk-CPUE-standrz-by-species.r
## Standardizing shark CPUE from observer longline
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: June 30, 2015
## Time-stamp: <2015-07-21 18:49:47 lauratb>
require(colorspace)
shkdir <- "C:/Projects/SHK-indicators-2015/"
if(!exists("getFactors")) source("C:/Projects/ALB-CPUE-2015/GLM-CPUE-utils.r")
shk.vect <- c("mako.south","blue.south","blue.north","silky","ocs","thresher","POR")

logit <- function(x) log(x/(1-x))
inv.logit <- function(x) 1/(1+exp(-x))
invrt.links <- c(log="exp", logit="inv.logit")
#   sp_category sp_code
#         BSH     BSH # blue shark
#         FAL     FAL # silky shark
#         HHD     SPN # hammerheads
#         HHD     SPL
#         HHD     SPZ
#         HHD     SPK
#         MAK     SMA # makos
#         MAK     LMA
#         MAK     MAK
#         OCS     OCS # oceanic whitetip
#         POR     POR # porbeagle
#         THR     BTH # threshers
#         THR     PTH
#         THR     ALV
#         THR     THR

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
message("Remove AUS and NZ program ids")

run.cpue.nb.gamlss <- function(wsp="MAK.south",
                               wmodel=model1, sigma.mod="1", dat2use=sets,
                               add.offset=TRUE, do.pred=FALSE, mod.trace=TRUE,
                               add.confint=FALSE, sst.filt=TRUE,
                               pg.min=100, yy.min=50, year.range=1995:2014) {

    if(grepl("south",wsp)) dat2use %<>% filter(lat1d <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1d >= 0)
    wsp0 <- wsp # keep orig in case includes north/south
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point

    if(wmodel=="null") { null.model <- TRUE; wmodel <- "1"; sigma.mod <- "1" }

    fname <- gsub("as.factor\\(|\\)","",
                  sprintf("%s_MU~%s_SIGMA~%s%s", wsp0, wmodel, sigma.mod, ifelse(add.confint,"_CI","")))
    fname <- gsub("1","intrcpt",fname)

    if(add.offset) {
        wmodel <- paste(wmodel, "+ offset(loghook)") }

    dat2use <- dat2use[,c(wsp, expl.vars)]
    nr <- nrow(dat2use)
    if(sst.filt) dat2use %<>% filter(cell %in% cells.by.sharks[[wsp]])

    dat2use$resp <- dat2use[,wsp]
    dat2use <- na.omit(dat2use)
    maxv <- quantile(dat2use$resp[dat2use$resp>0], 0.975)
    pc.lowN <- names(which(table(dat2use$program_code)<pg.min))

    dat2use %<>% filter(resp <= maxv, sharktarget=="N",
                        program_code %nin% c("HWOB", "ASOB", "PGOB", pc.lowN),
                        yy %in% year.range) # removing PG helps

    yy.lowN <- names(which(table(dat2use$yy)<yy.min))
    yr0 <- names(which(tapply(dat2use$resp>0, dat2use$yy, mean)==0))
    if(length(yr0)>0) yy.lowN <- c(yy.lowN, yr0)

    if(length(yy.lowN)>0) warning(sprintf("Removed years with low N: %s", paste(yy.lowN,collapse=" ")))
    dat2use %<>% filter(yy %nin% as.numeric(yy.lowN))
    # prepare formula:
    frml <- as.formula(paste(wsp, "~", wmodel))
    sigma.mod <- as.formula(paste("~",sigma.mod))# %<>% as.formula

    # run model:
    mod <- gamlss(frml, sigma.formula=sigma.mod,
                  family=NBI(), data=dat2use, n.cyc=100, trace=mod.trace)
    dat2use$mu.pred <- do.call(invrt.links[mod$mu.link], list(predict(mod)))
    dat2use$sigma.pred <- do.call(invrt.links[mod$sigma.link], list(predict(mod, "sigma")))
    mod.aic <- AIC(mod)

    ww <- 9.5; hh <- 8
    check.dev.size(ww, hh)
    par(bg="white",family="sans",mai=c(0.5,0.65,0.35,0.1),omi=c(0.1,0.25,1.25,0.25), las=1)
    plot(mod, parameters=par(mfrow=c(2,2), bg="transparent", col="darkgreen"),
         summaries=FALSE)
    px <- grconvertX(0.05, from="ndc")
    py <- grconvertY(0.975, from="ndc")
    mtext(as.character(mod$mu.formula)[2], outer=TRUE, adj=0,line=4, font=2, cex=2, col="royalblue4")
    mtext(paste("mu:", as.character(mod$mu.formula)[3]), outer=TRUE, adj=0,line=2.5, font=2,)
    mtext(paste("sigma:", as.character(mod$sigma.formula)[2]), outer=TRUE, adj=0, line=1, font=2)
    mtext(paste("AIC:", round(mod.aic,1)), outer=TRUE, adj=1, line=1, font=2, cex=1.5, col="tomato")
    dev.copy(CairoPNG, file=paste0("Diagnostics/",fname,"_resids.png"), width=ww, height=hh, units="in", res=100)
    dev.off()

    save(mod, file=paste0("CPUE-models/",fname,".RData"))
    start.timer()
    qres <- qqnorm(residuals(mod), plot=FALSE)
    stop.timer()
    dat2use$xqr <- qres$x
    dat2use$yqr <- qres$y
    dat2use$qrdiff <- qres$y - qres$x

    if(do.pred) {
    #newdf <- data.frame(yy=1995:2014, mm="1", program_code="PFOB", HPBCAT="S", loghook=7.5)
        #newdf$pred <- predict(mod, newdata=newdf, data=dat2use)

    obsdf <- predict(mod, se.fit=TRUE)
    dat2use$fit <- obsdf$fit
    dat2use$se.fit <- obsdf$se.fit
}

    # extract coefficients
    smr.obj <- coef(mod)
    if(grepl("yy", wmodel)) {
        smr.obj <- smr.obj[grep("yy", names(smr.obj))] # grab year effects + intercept
        yy <- gsub(".*yy.(.*)","\\1",(names(smr.obj)))
        smr.obj <- data.frame(yy=yy, mu=smr.obj)
    }else if(exists("null.model")) { # if null, get intercept for mu and sigma
        smr.obj <- c(coef(mod, "mu"), coef(mod, "sigma"))
        names(smr.obj) <- c("mu.intrcpt", "sigma.intrcept")
    } else {smr.obj <- NA } # else don't extract coefficients

    r.obj <- list(wsp=wsp0, mod=mod, aic=mod.aic, dat=dat2use, mu.est=smr.obj, confint=NA)
    if(add.confint) r.obj$confint <- confint(mod)
    if(!exists("null.model")) try(make.stz.plot(r.obj, fname),TRUE)
    return(r.obj)
}

make.stz.plot <- function(wmod, fname) {

    dp <- wmod$mu.est
    resp <- as.character(wmod$mod$mu.formula)[2]
    cpue <- tapply(wmod$dat[,resp], wmod$dat$yy, sum)/tapply(wmod$dat$hook/1000, wmod$dat$yy, sum)
    catch.vals <- tapply(wmod$dat$resp, wmod$dat$yy, sum)
    catch.cex <- (1 + catch.vals/max(catch.vals))
    catch.breaks <- seq(min(catch.vals)-1,max(catch.vals)+1,length.out=20)
    catch.col <- rev(heat_hcl(19))
    catch.cutv <- catch.col[cut(catch.vals, catch.breaks, labels=FALSE)]
    cpue <- cpue/mean(cpue)
    cpue <- cpue - mean(cpue)

    if(!is.na(wmod$confint[1])) {
        conf.mat <- wmod$confint[-1,]
        conf.mat <- conf.mat[grep("as.factor\\(yy",rownames(conf.mat)),]
        yrconf <- as.numeric(gsub(".*(.{4})$","\\1",rownames(conf.mat)))
    }

    ww <- 8; hh <- 8
    check.dev.size(ww, hh)
    yl <- range(c(dp$mu, cpue[-1])) # need to shift for confint... c(dp$low.se, dp$high.se, cpue))
    par(family="HersheySans", mai=c(0.75,0.95,0.85,0.2))
    plot(dp$yy, dp$mu, type="n", ylim=yl, las=1, xlab="", ylab="Standardized CPUE")

    if(!is.na(wmod$confint[1])) {
        polygon(c(yrconf,rev(yrconf)), c(conf.mat[,1],rev(conf.mat[,2])),
                border=NA, col=col2transp("grey92"))
    }
    #    abline(h=0, col="grey30")

    #points(as.numeric(names(cpue))[-1], cpue[-1], xpd=NA, pch=19, col=catch.cutv)
    #polygon(c(dp$yy,rev(dp$yy)), c(dp$low.se,rev(dp$high.se)), border=NA, col=col2transp("grey92"))
    #lines(c(dp$yy,rev(dp$yy)), c(dp$low.se,rev(dp$high.se)), col="grey")


    lines(dp$yy, dp$mu, col="royalblue3", type="b",  pch=19, lwd=2)
    lines(as.numeric(names(cpue))[-1], cpue[-1], xpd=NA, pch=19, col="tomato", type="b", lwd=2)

    mtext(wmod$wsp, adj=0, cex=1.5, line=0.5)
    legend.ltb.2("topright", legend=c("Nominal CPUE", "Standardized CPUE", "95% CI"),
                 col=c("tomato","royalblue3","grey90"),
                 pt.cex=c(1.5,1.5,2), lty=c(1,1,NA),lwd=2, pch=c(19,19,15), bty="n")
    if(!missing(fname)) {
    dev.copy(CairoPNG, file=paste0("Diagnostics/",fname,"_CPUE-stdz.png"), width=ww, height=hh, units="in", res=100)
    dev.off()
}

    invisible(data.frame(yy=dp$yy, mu=dp$mu))
}

rsim.fittedpars <- function(mod.obj,nsim=50){

    by.prog <- function(wpo) {
        dnow <- filter(mod.obj$dat, program_code == wpo)
        y.obs <- table(dnow$resp)
        yrange <- range(as.numeric(names(y.obs)))
        yrange[2] <- 2*yrange[2]
        bwidth <- 0.5
        barplot(y.obs, xlim=yrange-bwidth, space=c(-bwidth,rep(1,length(y.obs)-1)), width=bwidth, border=NA)
        replicate(nsim, get.sim(dnow))
        mtext(wpo, adj=0)
    }

    get.sim <- function(dat) {
        rd1 <- rNBI(nrow(dat), dat$mu.pred, dat$sigma.pred)
        rd.tbl <- table(rd1)
        lines(as.numeric(names(rd.tbl)), rd.tbl, type="l", col="royalblue", xpd=TRUE)
    }

    po2use <- names(head(sort(-table(mod.obj$dat$program_code))))
    par(mfrow=c(3,2))
    dmm <- sapply(po2use, by.prog)

}

# extract standardized:
# 1. get categorical factors for model
# 2. find combination in observed data that have factors
get.stz.index <- function(wmod) {

    fct <- unique(c(getFactors(wmod$mod$mu.formula)))#,
    #                    getFactors(wmod$sigma.formula)))
    fct <- fct %val.nin% c("yy","loghook")

    fct.levs <- sapply(fct, function(i) names(which.max(table(wmod$dat[,i]))))
#fct.levs <- "NZOB"
    dfpred <- expand.grid(yy=s.yr:e.yr, loghook=mean(wmod$dat$loghook))
    dfpred[,fct] <- rep(fct.levs, each=nrow(dfpred))
    dfpred$pred <- predict(wmod$mod, newdata=dfpred, data=wmod$dat)
    points(dfpred[,c("yy","pred")], ylim=c(-2,2), col="red")
}


# identify order of inclusion of variables in model,
# compare AIC change when using one variable only
make.one.var.table <- function(wsp="MAK.south", wfacts=model.vars) {

    null.mod <- run.cpue.nb.gamlss(wsp, wmod="null", do.pred=FALSE, mod.trace=FALSE)$aic
    mod.alls <<- lapply(wfacts, function(mdl) try(run.cpue.nb.gamlss(wsp=wsp, wmod=mdl, mod.trace=FALSE)))
    aic.alls <- sapply(mod.alls, function(x) try(as.numeric(x$aic)))
    names(mod.alls) <- gsub("as.factor\\((.*)\\)","\\1",wfacts)
    names(aic.alls) <- names(mod.alls)
    aic.alls.sigma <- sapply(wfacts, function(mdl) try(run.cpue.nb.gamlss(wsp=wsp, wmod="1", sigma=mdl, mod.trace=FALSE)$aic))
    mu.errors <- any(sapply(aic.alls, class)=="character")
    sigma.errors <- any(sapply(aic.alls.sigma, class)=="character")
    if(mu.errors) {
        aic.alls %<>% as.numeric
        message(sprintf("Model error with: %s", names(aic.alls)[is.na(aic.alls)])) }
    if(sigma.errors) {
        print(aic.alls.sigma)
        aic.alls.sigma %<>% as.numeric
        message(sprintf("Model error with: %s", names(aic.alls.sigma)[is.na(aic.alls.sigma)])) }
    df <- data.frame(Variable=names(aic.alls), AIC.diff=null.mod-aic.alls, AIC.diff.sigma=null.mod-aic.alls.sigma) %>% arrange(-AIC.diff)

    tbl.obj <- list(title=paste0(gsub(".","",wsp),":aic1"), ref=paste0(wsp,":aic1"), tbl=df)
    save(tbl.obj, file=sprintf("Tables-in-report/AIC1var_%s.RData", wsp))
    shk.yymod <- mod.alls[["yy"]]
    save(tbl.obj, file=sprintf("Tables-in-report/AIC1var_%s.RData", wsp))
    save(shk.yymod, file=sprintf("NB-model-year-only_%s.RData", wsp))
    invisible(tbl.obj)
}


qnorm.by.fact <- function(wfact, df, wmod) {

    qres <- qqnorm(residuals(wmod), plot=FALSE)
    mval <- max(abs(range(unlist(qres))))
    plot(1:10, type="n", xlim=c(-5,5), ylim=c(-mval,mval), ann=FALSE, las=1)
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
    legend.ltb.2("topleft", legend=names(df.spl), col=colv, pch=pchv, bty="n", ncol=2)
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

#########################################################
#########################################################
#########################################################
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

smr.bp.glm.vars <- function(wsp="MAK.south", dat2use=sets, sst.filt=TRUE,
                            pg.min=100, year.range=1995:2014, yy.min=50, add.fig=FALSE){

    message(wsp)

    mod2p <- gsub(".*\\((.*)\\)","\\1", model.vars[-1])
    mod2p <- mod2p[mod2p!="HPBCAT"]
    message(sprintf("Started with %s rows", nrow(dat2use)))
    nr <- nrow(dat2use)
    dat.filt <- nr
    if(grepl("south|POR",wsp)) dat2use %<>% filter(lat1d <= 0)
    if(grepl("north",wsp)) dat2use %<>% filter(lat1d >= 0)
    message(sprintf("Accounting for hemisphere, removed %s rows, %s rows left", nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    wsp0 <- wsp # keep orig in case includes north/south
    wsp <- gsub("(.*)\\..*","\\1",wsp) # only keep what's before the point
    dat2use <- dat2use[,c(wsp, expl.vars)]

    nr <- nrow(dat2use)

    # filter cells based on sst
    if(sst.filt) dat2use %<>% filter(cell %in% cells.by.sharks[[wsp]])
    message(sprintf("Accounting for SST, removed %s rows, %s rows left", nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    dat2use$resp <- dat2use[,wsp]
    dat2use <- na.omit(dat2use)
    message(sprintf("Accounting for missing values, removed %s rows, %s rows left", nr-nrow(dat2use), nrow(dat2use)))
    #dat.filt %<>% "c"(nr-nrow(dat2use));
    nr <- nrow(dat2use)
    maxv <- quantile(dat2use$resp[dat2use$resp>0], 0.975)
    pc.lowN <- names(which(table(dat2use$program_code)<pg.min))

    dat2use %<>% filter(resp <= maxv)
    message(sprintf("Accounting for max quantile, removed %s rows, %s rows left",
                    nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    dat2use %<>% filter(sharktarget=="N")
    message(sprintf("Accounting for shark targets, removed %s rows, %s rows left",
                    nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)

    dat2use %<>% filter(program_code %nin% c("HWOB","ASOB"))
    message(sprintf("Accounting for HWOB/ASOB, removed %s rows, %s rows left",
                    nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    dat2use %<>% filter(program_code %nin% "PGOB")
  message(sprintf("Accounting for PGOB, removed %s rows, %s rows left",
                    nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    dat2use %<>% filter(program_code %nin% pc.lowN)
    message(sprintf("Accounting for sampling by observer code, removed %s rows, %s rows left",
                    nr-nrow(dat2use), nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)

    dat2use %<>% filter(yy %in% year.range) # removing PG helps
    message(sprintf("Accounting for rows outside of year.range, removed %s rows, %s rows left", nr-nrow(dat2use),nrow(dat2use)))
    dat.filt %<>% "c"(nr-nrow(dat2use)); nr <- nrow(dat2use)
    dat.filt %<>% "c"(nr)

    names(dat.filt) <- c("ini.rows", "hemi", "SST", "max.quant",
                         "shark.target", "HWOB-ASOB", "PGOB", "OB-sampling",
                         "year.range","row.left")

    make.bp <- function(wvar) {

        tbl1 <- table(dat2use[,wvar], dat2use$yy)
        id.rn <- nrow(tbl1)
        tbl1.yr <- colSums(tbl1)
        tbl2 <- t(t(tbl1)/tbl1.yr)

        barplot(100*tbl2, border="grey30", las=1)#, col=cpue.colpal(id.rn))
        abline(h=0)
        mtext(wvar, cex=1.25, line=4, side=2, las=0)
    }

    if(add.fig) {
    ww <- 9.5; hh <- 10.2
    check.dev.size(ww, hh)
    par(family="HersheySans", omi=c(0.2,0.4,0.4,0.1),mai=c(0.2, 0.5, 0.2, 0.2),
        mfrow=c(1+length(mod2p),1))
    tbl.yr <- table(dat2use$yy)
    colyr <- rep("grey",length(tbl.yr))
    colyr[tbl.yr<yy.min] <- "tomato"
    bp <- barplot(tbl.yr, col=colyr, border=NA, las=1, ylab="")
    mtext("yy", cex=1.25, line=4, side=2, las=0)
    text(bp, tbl.yr, tbl.yr,col=colyr,pos=3,cex=0.75,xpd=NA)
    mtext(wsp0, outer=TRUE, cex=1.2)
    dmm <- sapply(mod2p, make.bp)
    dev.copy(CairoPNG, file=sprintf("SHK-GLM-vars-bp-smry_%s.png", wsp0),
             width=ww, height=hh, units="in", res=100)
    dev.off()
    invisible(dat2use)
}
    print(dat.filt)
    return(paste0(paste(c(wsp0,dat.filt[-1]), collapse="&"),"\\"))
}

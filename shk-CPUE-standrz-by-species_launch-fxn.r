## shk-CPUE-standrz-by-species_launch-fxn.r
## Launch all steps of CPUE standardization based
## on functions defined in shk-CPUE-standrz-by-species.r
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: July  3, 2015
## Time-stamp: <2015-07-07 08:12:37 lauratb>
require(Cairo)
# HBF category: <=10 is surface S, otherwise deep D
expl.vars <- c("yy","mm","program_code","flag_id","newlat","newlon",
               "lon5","lat5","vesselname","cell","HPBCAT","HPBCAT2",
               "hk_bt_flt","TIMECAT","timeofday",
               "target","sharkbait","loghook","hook_est")

model.vars <- c("as.factor(yy)","as.factor(mm)","as.factor(program_code)","as.factor(flag_id)",
                "as.factor(HPBCAT)","as.factor(HPBCAT2)","as.factor(sharkbait)")
var.types <- rep("categorical",length(model.vars))

model0 <- "as.factor(yy)"
model1 <- "as.factor(yy)+as.factor(program_code)"
model1.int <- "as.factor(yy)*as.factor(program_code)"
model2 <- "as.factor(yy) + as.factor(program_code) + as.factor(mm)"
model3 <- "as.factor(yy) + as.factor(program_code) + as.factor(mm) + as.factor(HPBCAT2)"
all.models <- c(model0, model1, model1.int, model2, model3)

launch.shk.cpue <- function(wsp="mako.south", dat=shk_all, onevar.redo=FALSE,
                            Oallow.yr.po.intrxn=FALSE) {

    # get importance of variables on their own
    # (saves r object to be made into latex table)
    if(onevar.redo) {
        tbl.obj <- make.one.var.table(wsp=wsp)
    }
    load(sprintf("Tables-in-report/AIC1var_%s.RData", wsp)) #tbl.obj
    load(sprintf("NB-model-year-only_%s.RData", wsp)) #shk.yymod -- year-only model

    # extract variables to add (already ordered by importance),
    # removing the ones that we handle manually
    # also keeping program_code over flag_id
    var2add <- tbl.obj$tbl$Variable %val.nin% c("yy","program_code","flag_id")
    print(var2add)
    var2add <- var2add[-(grep("HPBCAT", var2add)[2])] # keep best HPBCAT predictor
    print(var2add)
    yypc <- run.cpue.nb.gamlss(wsp=wsp, wmodel=model1, dat2use=dat)

    if(allow.yr.po.intrxn) {
        message("Checking for year-program code interactions")

    yypcintrxn <- run.cpue.nb.gamlss(wsp=wsp, wmodel=model1.int, dat2use=dat)
    if((yypc$aic-yypcintrxn$aic)<10) { print("Using year + program_code")
                              base.frml <- model1 }else{
                              print("Using year-program_code interaction")
                              base.frml <- model1.int
                          }
    } else {
        message("Not allowing interaction between year + program_code")
             base.frml <- model1
             yypcintrxn <- list(aic=NA)
         }

    # create remainder of model formulas with additive terms
    extra.frml <- sapply(1:length(var2add), function(vrb) paste(base.frml,"+",
                                                                paste(paste0("as.factor(",var2add[1:vrb],")"),collapse=" + ")))
    all.mod.runs <- lapply(extra.frml, function(mod)
        run.cpue.nb.gamlss(wsp=wsp, wmodel=mod, dat2use=dat, do.pred=TRUE))
    names(all.mod.runs) <- extra.frml
    aic.vect <- try(c(yronly=shk.yymod$aic, yrprog=yypc$aic, yrprogint=yypcintrxn$aic,
                      sapply(all.mod.runs,"[[","aic")))
    aic.tbl <- try(data.frame(model=c("as.factor(yy)", model1,model1.int,extra.frml), aic.vect))
    sigma.base <- aic.tbl[which.min(aic.vect),1] # get base formula for sigma

    #add.sigma <- sapply(model.vars, function(mv) try(run.cpue.nb.gamlss(wsp, wmod=sigma.base, sigma.mod=mv)))
    cpue.shk <- list(var1=tbl.obj, yy=shk.yymod, yypc=yypc, yypcixn=yypcintrxn,
                     extra.mods=all.mod.runs, aic.vect=aic.vect, aic=aic.tbl)#, sigma.mod=add.sigma))

    save(cpue.shk, file=sprintf("CPUE-mod-sel_%s_%s.RData", wsp,
                   ifelse(allow.yr.po.intrxn,"no-year-x-prog-allowed","year-x-prog-check")))
    return(cpue.shk)
    }

shk.step.plot <- function(mod.all=smako.mod) {

    yr.only <- mod.all$yy$mu.est[-1,c("yy","Mean")]
    yr.prog <- mod.all$yypc$mu.est[-1,c("yy","Mean")]
    omods <- lapply(mod.all$extra.mods, function(x) x$mu.est[-1,c("yy","Mean")])
    ao.mods <- c("as.factor(yy)"=list(yr.only),
                 "as.factor(yy)+as.factor(program_code)"=list(yr.prog), omods)
    ao.mods.ctr <- lapply(ao.mods, function(ao) data.frame(yy=ao$yy, cmean=ao$Mean/mean(ao$Mean))) #centering
    aic.diff <- c(0, diff(filter(mod.all$aic, model %in% names(ao.mods))$aic))
    aic.diff[1] <- -99999 #setting to low val for color coding on plot

    par(mfrow=c(length(ao.mods), 1), omi=c(0.5, 0.5, 0.2, 0.7),
        mai=rep(0.2, 4), family="HersheySans")
    colvmat <- matrix("grey", nrow=length(ao.mods), ncol=length(ao.mods))
    diag(colvmat) <- "black"
    lao <- length(ao.mods)
    diag(colvmat[-lao,-1]) <- "indianred3"
    colvmat[lower.tri(colvmat)] <- NA
    yl <- range(sapply(ao.mods.ctr,"[[", "cmean"))

    pfunk <- function(wmod.id) {
        wmod <- names(ao.mods)[wmod.id]
        dat <- ao.mods[[wmod.id]]
        colv <- colvmat[,wmod.id]
        type.pl <- rep("l", length(ao.mods))
        type.pl[wmod.id] <- "b"
        lwd.pl <- rep(1, length(ao.mods))
        lwd.pl[wmod.id] <- 2

        plot(dat$yy, dat$Mean, type="n", las=1, ann=FALSE, xaxt="n", ylim=yl)
        abline(h=1,col="grey",lty=2)
        dmm <- sapply(1:length(ao.mods.ctr),
                      function(i) lines(ao.mods.ctr[[i]], col=colv[i], pch=19, lwd=lwd.pl[i], type=type.pl[i]))

         if(wmod==last(names(ao.mods))) axis(1)

        aic.now <- round(filter(mod.all$aic, model ==wmod)$aic,1)
        try(mtext(paste("AIC:", aic.now), adj=1, col=ifelse(aic.diff[wmod.id]< -10,"indianred1","black"), font=2))
        wmod <- gsub("as.factor\\(|\\)| ","",wmod)
        mtext(wmod, adj=0, font=2)
    }

    dmm <- sapply(1:length(ao.mods), pfunk)


}


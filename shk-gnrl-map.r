## shk-gnrl-map.r
## lalalalala
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: July  3, 2015
## Time-stamp: <2015-07-14 10:29:36 lauratb>

require(RColorBrewer)
## loading map and legend functions ##
gw <- getwd()
try(setwd("C:/Projects/Oprtnl-data"))
source("add-continents-sc.r") # load function for quick continent outline
source("add-continents-mapdata.r")
source("legend-funk-gnrl.r")
source("colourbar-legend.r")
setwd(gw)

# cells to include in the map
map.lat.vals <- seq(min(shk_all$lat1), max(shk_all$lat1))
map.lon.vals <- seq(min(shk_all$lon1), max(shk_all$lon1))

draw.ellipse <- function(centr, xlong, ylong, ..., res=0.01) {

    xvals <- seq(xlong-centr, xlong+centr, by=res)
    yvals <- sqrt((ylong^2)*(1-(xvals^2)/(xlong^2)))

    lines(xvals, yvals, ...)
}
point.map <- function(wsp="mako", pvar="totcatch", df=shk_all, res=1, add.obs.lab=TRUE, coords=c("newlon","newlat")) {

    df$resp <- df[,wsp]
    df$y5 <- 5*floor(df$yy/5)
    y5labs <- c("1995"="1995-1999","2000"="2000-2004",
                "2005"="2005-2009","2010"="2010-2014")
    df$x <- df[,coords[1]]
    df$y <- df[,coords[2]]
    df$x.cell <- res*floor(df$x/res)
    df$y.cell <- res*floor(df$y/res)

    mapdf <- df %>% group_by(y5, x.cell, y.cell) %>% summarize(totcatch=sum(resp),
                                                           pos1=any(resp>0),
                                                           proppos=mean(resp>0)) %>% data.frame
    breakv <- get.breaks.abso(mapdf[,pvar])-0.01
    cutv <- cut(mapdf[,pvar], breaks=breakv, lab=FALSE)
    colv <- c("grey",rev(heat_hcl(length(breakv)-1)))
    mapdf$colv <- colv[cutv]
    lonlim <- range(mapdf$x.cell)
    latlim <- range(mapdf$y.cell)

    make.sub <- function(wvar) {
        dnow <- mapdf %>% filter(y5 == wvar)
        obsdf <- df %>% filter(y5==wvar) %>% group_by(program_code) %>%
            summarize(xc=median(x), yc=median(y), nc=n())
#                                                         xrad.min=quantile(x, tol),
#                                                         xrad.max=quantile(x, 1-tol),
#                                                         yrad.min=quantile(y, tol),
#                                                         yrad.max=quantile(y, 1-tol))

        plot(dnow$x.cell, dnow$y.cell, col=dnow$colv, las=1, pch=19, cex=0.25,
             ann=FALSE, asp=1, xlim=lonlim, ylim=latlim, axes=FALSE)
        abline(h=seq(-60,60,by=20), col="grey")
        abline(v=seq(100,3200,by=20), col="grey")
        add.continents()
        box()

        mtext(y5labs[as.character(wvar)],col="royalblue4",adj=0,cex=1.2)
        if(add.obs.lab) text(obsdf$xc, obsdf$yc, obsdf$program_code, col="royalblue3", cex=logb(obsdf$nc,100))
    }

    ww <- 9.5; hh <- 8.5
    check.dev.size(9.5, 8.5)
    par(mfrow=c(2,2), mai=c(0.35,0.35,0.1,0.1), omi=rep(0.25,4), family="HersheySans")
    dmm <- sapply(seq(1995,2010,by=5), make.sub)
    dev.copy(CairoPNG, file=sprintf("WRITEUP/obs-data-shk-catch-program_%s.png", wsp),
             width=ww, height=hh, units="in", res=100)
    dev.off()
}

point.map.yr <- function(wsp="OCS", pvar="totcatch", df=sets,
                         yr2keep=1995:2014, res=1, coords=c("lon1d","lat1d")) {

    df$resp <- df[,wsp]
    df %<>% filter(yy %in% yr2keep)
#    y5labs <- c("1995"="1995-1999","2000"="2000-2004",
#                "2005"="2005-2009","2010"="2010-2014")
    df$x <- df[,coords[1]]
    df$y <- df[,coords[2]]
    df$x.cell <- res*floor(df$x/res)
    df$y.cell <- res*floor(df$y/res)

    mapdf <- df %>% group_by(yy, x.cell, y.cell) %>% summarize(totcatch=sum(resp),
                                                           pos1=any(resp>0),
                                                           proppos=mean(resp>0)) %>% data.frame
    breakv <- get.breaks.abso(mapdf[,pvar])-0.01
    cutv <- cut(mapdf[,pvar], breaks=breakv, lab=FALSE)
    colv <- c("grey",rev(heat_hcl(length(breakv)-1)))
    mapdf$colv <- colv[cutv]
    lonlim <- range(mapdf$x.cell); print(lonlim)
    latlim <- range(mapdf$y.cell)

    make.sub <- function(wvar) {
        dnow <- mapdf %>% filter(yy == wvar)
        obsdf <- df %>% filter(yy==wvar) %>% group_by(program_code) %>%
            summarize(xc=median(x), yc=median(y), nc=n())
#                                                         xrad.min=quantile(x, tol),
#                                                         xrad.max=quantile(x, 1-tol),
#                                                         yrad.min=quantile(y, tol),
#                                                         yrad.max=quantile(y, 1-tol))

        plot(dnow$x.cell, dnow$y.cell, col=dnow$colv, las=1, pch=19, cex=0.25,
             ann=FALSE, asp=1, xlim=lonlim, ylim=latlim, axes=FALSE)
        abline(h=seq(-60,60,by=20), col="grey")
        abline(v=seq(100,3200,by=20), col="grey")
        add.continents()
        box()

        mtext(wvar,col="royalblue4",adj=0,cex=1.2)
        #text(obsdf$xc, obsdf$yc, obsdf$program_code, col="royalblue3", cex=logb(obsdf$nc,100))
    }

#    ww <- 9.5; hh <- 8.5
 #   check.dev.size(ww, hh)
    par(mfrow=c(5,4), mai=c(0.15,0.15,0.1,0.1), omi=c(0.1,0.2,0.5,0.2), family="HersheySans")
    dmm <- sapply(yr2keep, make.sub)
    mtext(wsp, line=2, outer=TRUE, cex=1.5)
#    dev.copy(CairoPNG, file=sprintf("WRITEUP/obs-data-shk-catch-program_%s.png", wsp),
#             width=ww, height=hh, units="in", res=100)
#    dev.off()
}

######################## ######################## ########################
######################## ######################## ########################
# started to adapt from ALB but then switched to points
grid.map <- function(var="bet_mt", smr.type="cpue", df=vdat, which.item,
                     dat.filt, var2="hook",
                     calc.hotspot=FALSE, quant.hotspot=c(0.5, 0.9),
                     use.breaks,
                     multi.fig=FALSE, ax.x="t", ax.y="t",
                     nice.map=FALSE, save=FALSE,
                     add.5=TRUE,add.1=TRUE,add.region=F,
                     add.legend=TRUE, leg.inset=c(-0.07,0),
                     figdat="", afacts=c("lond","latd")) {

    var2 <- grepv(var2, names(df))[1] # assume 2nd variable is hook

    # adding lon/lat fields if needed, using first column match
    if(!"lond" %in% names(df)) df$lond <- df[,grep("lon",names(df))[1]]
    if(!"latd" %in% names(df)) df$latd <- df[,grep("lat",names(df))[1]]
    if("list" %in% class(df)) df <- df[[which.item]]
    if(!missing(dat.filt)) df %<>% filter_(dat.filt) # filtering rows if specified

    df$lond <- df[,afacts[1]]
    df$latd <- df[,afacts[2]]
    if(!calc.hotspot) {
        d1 <- get.spatial.obj(df=df, smr.type=smr.type, var1=var, var2=var2, afacts=afacts)
   }else{
       # if plotting quantile closures, further process map object
       message("not fixed to more general version yet")
      d1 <- assign.quantiles(df1=df, smr.type=smr.type,
                             var=var, year=year, wquant=c(0.5,0.9))
  }

  d1no0 <- d1[d1!=0] # obj without empty cells for further calcs

  if(length(d1no0)>0) { # there is positive v2p

      # set breaks for colours
      breakfunk <- smr.breaks[[smr.type]]
      if(is.null(breakfunk)) breakfunk <- "get.breaks.abso" # by default

      breakv <- do.call(breakfunk, list(d1))

      if(calc.hotspot) breakv <- c(0, quant.hotspot, 1)
      if(!missing(use.breaks)) breakv <- use.breaks
    if(smr.type == "mean-res") {
      # set background to negative values
      d2 <- tapply(d2$v2p, as.list(d2[,c("lond","latd")]), is.na)
      d1[d2==1] <- -1000
    }

  }else{ # else, no data, plot empty map
    breakv <- c(-999,0,1)
  }

  bckg.col <- col2transp("azure2",0.25) # background colour
    pal.zero <- smr.colpal[[smr.type]]
    if(is.null(pal.zero)) {
        pal.zero <- function(n) heat_hcl(n, h = c(90, 0), c. = c(30, 100), l = c(90, 50))
    }


  if(calc.hotspot) pal.zero <- closure.pal
  colv <- c(col2transp(bckg.col,0.95),pal.zero(length(breakv)-2))
  colv <- pal.zero(length(breakv)-1)
  lco <- lapply(dimnames(d1), as.numeric)

  if(!multi.fig) {
    ww <- 12.125; hh <- 7.25
    check.dev.size(12.125, 7.250)
    par(family="HersheySans", mai=c(1,1,1,2))
}

    d1[is.na(d1)] <- -999

  image(lco$lond, lco$latd, d1, las=1, asp=1,
        breaks=c(-1000,breakv), col=c(bckg.col,colv), xlab="", ylab="",
        xlim=range(map.lon.vals), ylim=range(map.lat.vals),
        xaxt=ax.x, yaxt=ax.y)

  if(nice.map) {

    if(add.1) add.1.grid()
    if(add.5) add.5.grid()
    if(add.region) region.grid(border="azure3", lwd=1)
    add.continents.poly(col=col2transp("cornsilk3"),border="grey")
  } else {
    if(add.1) add.1.grid()
    if(add.5) add.5.grid()
    #    region.grid(border="azure4",lwd=1.5)
    add.continents(col="grey", lwd=1.5)

  }
    box()
    if(!missing(dat.filt)) mtext(dat.filt, adj=0, cex=1.2, line=0.5)

    break.mod <- breakv[-length(breakv)] # remove max value so that legend looks ok
    pu <- par("usr")

    if(!calc.hotspot) {

        if(add.legend) {
        lz <- legend.ltb(legd=break.mod, colv=colv, inset=leg.inset,
                         max.val=last(breakv), leg.title=var)
    }

        }else{
    lx <- grconvertX(1.02, from="npc")
    ly <- grconvertY(0.93, from="npc")

  lz <- legend.ltb(lx, ly, legd=100*c(0,break.mod), colv=colv, max.val=100*last(breakv),
                   maxval.approx=FALSE, leg.title="Quantiles")
}


   if(save) {
       dev.copy(png, file = sprintf("Pcf-map_%s%s_%s_%s%s.png",
                         figdat, ifelse(calc.hotspot,"-hotspots",""),
                         smr.type, var,
                         ifelse(missing(dat.filt),"", paste("_",dat.filt,sep=""))),
             res = 100,  units="in", width=ww, height=hh)
    dev.off()
  }

  invisible(list(datmat=d1, breaks=breakv, col=colv[-1]))
}
######################## ######################## ########################
######################## ######################## ########################
get.spatial.obj <- function(df=dat, smr.type="catch",
                            var1, var2, afacts=c("lond","latd")) {

    res <- min(diff(sort(unique(df$lond)))) # extract resolution (assuming already rounded up)
    df$var1 <- df[[var1]] # define variable to plot v2p
    df$var2 <- df[[var2]] # add 2nd variable (NULL if doesn't exist)

    funk2call <- smr.funk[[smr.type]]
    if(is.null(funk2call)) funk2call <- smr.type # use function as is if no match

    # summarize by cell, add hooks (this column ignored unless cpue calc)
    df3 <- df %>% group_by(lond, latd) %>%
        summarize(v2p=do.call(funk2call, list(var1, var2)))

    # merge with all-pacific cell grid to get even image map
    all.lat <- seq(min(map.lat.vals), max(map.lat.vals), by=res)
    all.lon <- seq(min(map.lon.vals), max(map.lon.vals), by=res)
    all.coords <- expand.grid(lond=all.lon, latd=all.lat)

    d2 <- left_join(all.coords, df3) # slow... could index by row

    # switch to matrix
    d1 <- tapply(d2$v2p, as.list(d2[,c("lond","latd")]), sum)
}


######################################################################################
count <- function(x, y=NULL) length(unique(x))
prop.pos <- function(x, y=NULL) mean(x>0, na.rm=TRUE) # proportion of zeroes in factor
mean.cpue <- function(no, hook) {
    sh <- sum(hook, na.rm=TRUE)
    if(sh == 0) sh <- NA
    sum(no, na.rm=TRUE)/sh # aggregated CPUE
}

# write mean that can take two variables but discard second one
# so that function is more general
mean2 <- function(x, y=NULL) mean(x, na.rm=TRUE)
sum2 <- function(x, y=NULL) sum(x, na.rm=TRUE)

mean.res <- function(x, y=NULL) mean(x, na.rm=TRUE) # mean residuals

cpue.colpal <- function(n=12) {
    fb35 <- "#AC2020" #intermediate firebrick 3-4
    # other option going from neutral to red
    # colorRampPalette(c("wheat3","wheat2","orange1","indianred1","firebrick2",fb35))
    colorRampPalette(c("royalblue3","deepskyblue1","gold","orange1","indianred1","firebrick2",fb35))(n)
}

smr.colpal <- list("zero-prop"=colorRampPalette(rev(brewer.pal(9,"Blues"))),
                   "year-count"=colorRampPalette(brewer.pal(9,"OrRd")),
                   "ag-cpue"=cpue.colpal(), #colorRampPalette(brewer.pal(9,"OrRd")))
                   "tot-effort"=cpue.colpal(),
                   "tot-catch"=cpue.colpal(),
                   "mean-res"=colorRampPalette(c("royalblue4","deepskyblue","grey","indianred1","tomato3")),
                   "mean"=cpue.colpal())
smr.funk <- list("zero-prop"="prop.pos",
                 "year-count"="count",
                 "ag-cpue"="mean.cpue",
                 "mean-res"="mean.res",
                 "cpue"="mean.cpue",
                 "mean"="mean2",
                 "tot-catch"="sum2",
                 "tot-effort"="sum2")
smr.title <- list("ag-cpue"=quote(sprintf("Aggregated CPUE, %s", ttlvar)),
                  "tot-catch"=quote(sprintf("Total catch, %s", ttlvar)),
                  "tot-effort"=quote(sprintf("Total longline effort, %s", ttlvar)))
smr.legttl <- list("ag-cpue"=quote(sprintf("%s: %s CPUE (#indivs/100 hooks)",ttlvar, toupper(substr(var,1,3)))),
                   "tot-effort"=quote(sprintf("%s: Total effort (100 hooks)", ttlvar)),
                   "tot-catch"=quote(sprintf("%s: Total catch (# individuals)", ttlvar)))
smr.breaks <- list("zero-prop"="get.breaks.abso",
                   "year-count"="get.breaks.year",
                   "ag-cpue"="get.breaks.abso",
                   "mean-res"="get.breaks.res",
                   "tot-catch"="get.breaks.abso",
                   "tot-effort"="get.breaks.abso")
########################################################

get.breaks.quantile <- function(x) {
    c(-999,0,quantile(x[x>0], seq(0,1,by=0.05), na.rm=TRUE))
}

get.breaks.abso <- function(x, nbr=21) {

    scl.bounds <- quantile(x, c(0.05,0.95), na.rm=TRUE)
    breakv <- pretty(seq(scl.bounds[1], scl.bounds[2], length.out=nbr/2), n=nbr)

    # attempt to make intervals larger if pretty breakv is too long
    if(length(breakv)>24) breakv <- pretty(seq(scl.bounds[1], scl.bounds[2],
                                               length.out=0.75*nbr/2), n=0.75*nbr)

    # make sure the edges of the data are included in the scale
    mx <- max(x, na.rm=TRUE)
    mn <- min(x, na.rm=TRUE)
    if(mx > last(breakv)) breakv <- c(breakv, round(mx+0.1,1))
    if(mn < breakv[1]) {
        sig <- 10^floor(log10(abs(mn)))
        breakv <- c(sig*floor(mn/sig), breakv)
    }

    breakv
}

get.breaks.res <- function(x, nbr=20) {

    scl.lim <- quantile(x[x!=0], c(0.05,0.95), na.rm=TRUE)
    breakv <- pretty(seq(scl.lim[1], scl.lim[2], length.out=nbr/2), n=nbr)

    if(length(breakv)>24) breakv <- pretty(seq(scl.lim[1], scl.lim[2],
                                               length.out=0.75*nbr/2), n=0.75*nbr)
    intn <- diff(breakv[1:2])
    edg <- max(c(abs(breakv[1]),breakv[length(breakv)]))
    breakv <- round(seq(-edg, edg, by=intn),1)
    mx <- max(x[x>0], na.rm=TRUE)
    min.x <- min(x[x<0])
    if(mx > last(breakv)) breakv <- c(breakv, ceiling(mx))
    if(min.x < breakv[1]) breakv <- c(floor(min.x), breakv)

    c(-999, breakv)
}

get.breaks.year <- function(x) seq(0,65,by=2)

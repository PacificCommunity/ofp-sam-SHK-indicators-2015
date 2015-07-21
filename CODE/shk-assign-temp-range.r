aa <- load("DATA/GODAS-extract_temp005, temp055, temp125, temp225_2015-07-14.RData")
sets.sst <- right_join(res.aggr, sets) %>% mutate(sst1=floor(temp005))

get.sp.range <- function(wsp) {

    # prop of positive sets by 1-degree band
    ppos <- tapply(sets.sst[,wsp]>0, sets.sst$sst1, mean)
    tempv <- as.numeric(names(ppos))

    # identify minimum/maximum temp where positive catches occur
    min.tmp <- last(which(cumsum(ppos)==0))+1
    if(is.na(min.tmp)) min.tmp <- 1
    max.tmp <- first(which(rev(cumsum(rev(ppos)))==0))-1
    if(is.na(max.tmp)) max.tmp <- length(tempv)
    c(tempv[min.tmp], tempv[max.tmp])
}

cell.sst.bounds <- function(wsp="MAK", sst.dat=sets.sst, min.quant=0, max.quant=1, scenario="med.in.range") {

    shk.range <- get.sp.range(wsp)
    message(sprintf("Using range of %s to %s for %s", shk.range[1], shk.range[2], wsp))
    subd <- sst.dat %>% group_by(cell, lon5, lat5) %>% summarize(med.sst=median(temp005, na.rm=TRUE),
                                                                 low.sst=quantile(temp005,min.quant,na.rm=TRUE),
                                                                 high.sst=quantile(temp005,max.quant,na.rm=TRUE)) %>%
        mutate(in.sst.range=(low.sst>=shk.range[1]) & (high.sst<=shk.range[2]),
               out.range=!((low.sst>=shk.range[2])|(high.sst<=shk.range[1])),
               med.in.range=med.sst %between% shk.range) %>% data.frame

    prop.cells.kept <- colMeans(subd[,c("in.sst.range","med.in.range","out.range")],na.rm=TRUE)
    subd <- subd[,c("cell", "lon5", "lat5", scenario)] %>% data.frame

    plot(subd$lon5, subd$lat5, asp=1, pch=19, col="grey")
    subd <- subd[subd[,scenario],]
    points(subd$lon5, subd$lat5, asp=1, pch=19, col="tomato")
    add.continents()
    mtext(wsp)

    c2k <- subd$cell
    attr(c2k,"scenario") <- scenario
    attr(c2k,"qt.range") <- c(min.quant,max.quant)
    return(c2k)
}

cells.by.sharks <- lapply(main.sharks, cell.sst.bounds, scen="in.sst.range")
names(cells.by.sharks) <- main.sharks

## figs-2-latex.r
## Extracts figures from a given directory and outputs into
## a latex file ready for inclusion in a main latex document
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: May 14, 2015
## Time-stamp: <2015-06-26 09:21:29 lauratb>
if(!exists("gitdir.alb2015")) message("Add 'gitdir.alb2015' and 'figdir.repo' to your .Rprofile")
figdir.repo <- "C:/Projects/ALB-CPUE-2015/Figs"
figdir.texfile <- "report-figs.tex"
tbldir.texfile <- "report-tables.tex"
figs2latex <- function(texfile=figdir.texfile,
                          indexfile="report-figs_index.r",
                          figdir=figdir.repo, texdir=gitdir.alb2015) {

    source(paste0(texdir, indexfile)) # get latest figure settings
    fname <- paste0(texdir, texfile)
    allfigs <- sort(paste0("Figs/",list.files(path=figdir, pattern="pdf|png", recurs=TRUE))) # orders first by file name

    # Extract figures that are defined in index file
    figtype <- gsub(".*/([A-Za-z0-9\\-]*)_.*$","\\1",allfigs)
    allfigs <- allfigs[figtype %in% names(fig.index)]
    figtype <- figtype[figtype %in% names(fig.index)]

    # Define order of figure appearance:
    # assume first 20 characters of figure name are unique
    # and refer to a common block of figures
    fig.pos <- unlist(sapply(fig.index,"[[", "pos")) # first order on position, as defined in index
    figreg <- gsub(".*_R[eg]*([0-9]+)_.*","\\1",allfigs) # then order on region
    figreg[nchar(figreg)>2] <- 0 # if no region match, set to 0
    # (only for diagnoses plots, put round 1 first, i.e. no GLMdata-clst in filename)
    ordr2 <- grepl("GLMdata-clst", allfigs)
    # get second underscore tag for figure order *within* regions
    figref2 <- gsub(".*/[A-Za-z0-9\\-]*_([A-Za-z]*)_.*$","\\1",allfigs)
    ordr3 <- paste(figreg, figref2)

    # ordering first by position, then region, then -clst second (for round 2), then second underscore
    plot.ordr <- order(fig.pos[figtype], ordr2, as.numeric(figreg), ordr3)
    # ... re-ordering file names, figure regions, and figure types accordingly
    allfigs <- allfigs[plot.ordr] # file names
    figreg <- figreg[plot.ordr] # regions
    figtype <- figtype[plot.ordr] # figure types
    diagno.plot <- grepl("^Inf|^Res", figtype)
    first.diagno <- which(diagno.plot)[1]
    ordr2 <- ordr2[plot.ordr] # to get round label after

    # identify single figures to keep the reference label number-free
    print(figtype[!duplicated(figtype)])
    singlefig <- !(figtype %in% figtype[duplicated(figtype)])

    # setting locations for \clearpage:
    # add one before repeats of the same region within figure type
    regrep <- cumsum(sapply(seq(figreg)[-1],function(i) figreg[i]!=figreg[i-1]))
    figclearpage <- c(FALSE, as.logical(regrep %in% regrep[duplicated(regrep)]) * !duplicated(regrep) )
    figclearpage[figreg==0] <- FALSE # avoid clearpage if there are no regions (e.g. maps)
    figclearpage[!duplicated(figtype)] <- TRUE# but still add one before each new figure type

    # template for latex functions to call
    fig.template <- "\\addcenterfig[%s]{%s}{%s}\n"
    fig.template.LS <- "\\addcenterfigLS[%s]{%s}{%s}\n"

    addfig2tex <- function(fig.num) {
        settng <- fig.index[[figtype[fig.num]]] #settn
        fig.tpl <- ifelse(settng$landscape, fig.template.LS, fig.template)
        rnum <- figreg[fig.num]

        if(rnum==0) rnum <- fig.num

        # add explanation for diagnostics before first diagnosis plot
        if(fig.num == first.diagno) cat(diagno.legend, file=fname, append=TRUE)

        # get round label if diagnosis
        if(diagno.plot[fig.num]) {
            stepval <- ifelse(ordr2[fig.num], "2a", "1")
        }
        # extract caption and evaluate if there is info from filename to be inserted
        captnow <- settng$caption
        if(class(captnow)=="call") captnow <- eval(captnow)

        figlab <- paste0(settng$figref,":",rnum)
        if(singlefig[fig.num]) figlab <- settng$figref
        if(figclearpage[fig.num]) {
            cat("\\clearpage\n", file=fname, append=TRUE)}
        z1 <- try(cat(sprintf(fig.tpl, captnow, figlab,
                              allfigs[fig.num]), file=fname, append=TRUE),silent=TRUE)
        if(class(z1)=="try-error") {
            message(sprintf("No index information for %s", allfigs[fig.num]))}
    }


    #########################################################################
    #########################################################################

    pcnow <- system("hostname",TRUE)
    preamble <- paste0("%%%%%%% Figures from ",figdir,"\n%%%%%%% From ",pcnow,"\n\n")
    cat(preamble, file=fname)
    dmm <- sapply(seq(allfigs), addfig2tex)

    invisible(data.frame(file=allfigs, reg=figreg, fig.ordr=plot.ordr, clearpage=figclearpage, regrep=c(FALSE,regrep)))
}

make.latex.table <- function(wtbl=region.settings, tblname, ref,
                             texdir=gitdir.alb2015, fname=tbldir.texfile,
                             switch.truefalse=TRUE) {

    message("Make this work like for figures but with .RData instead")
    col.logic <- which(sapply(wtbl, is.logical))
    if(switch.truefalse & length(col.logic)>0) {

        wtbl[,col.logic] <- c("No","Yes")[1+unlist(wtbl[,col.logic])]
    }

    require(xtable)
    fname <- paste0(texdir, fname)
    cat(sprintf("\n\n\n\\newcommand{\\%s}{\n", tblname), file=fname)
    print(xtable(wtbl, label=ref), include.rownames=FALSE, file=fname, append=TRUE)
    cat("\n}", file=fname, append=TRUE)
    }

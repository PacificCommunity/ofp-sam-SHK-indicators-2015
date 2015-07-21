require(mapdata)

# support functions
# make a vector of color transparent according to 'tlev'
col2transp <- function (col, tlev = 0.5) {
    sa <- lapply(col, function(cc) col2rgb(cc)/255)
    s2 <- sapply(sa, function(s1) rgb(s1[1], s1[2], s1[3], alpha = tlev))
}

# add square grid with resolution 'res', ... arguments passed on to abline
add.grid <- function(res=5, col=col2transp("azure3",0.35), lwd=0.1, ...) {
    pu <- par("usr")
    lonlim <- res*c(floor(pu[1]/res), ceiling(pu[2]/res))
    latlim <- res*c(floor(pu[3]/res), ceiling(pu[4]/res))
    abline(v=seq(lonlim[1], lonlim[2], by=res), col=col, lwd=lwd)
    abline(h=seq(latlim[1], latlim[2], by=res), col=col, lwd=lwd)
}

# main function, new.map=TRUE produces new plot (using lonlim and latlim)
# defaults to FALSE, adding to existing plot
# ... arguments get passed on to 'map', except for
# col and border which are specified formally
add.continents.poly <- function(..., col=col2transp("cornsilk3"), border="grey",
                                lonlim=c(100,320), latlim=c(-50,70), new.map=FALSE,
                                add.grid) {

    # Define plot window if drawing new.map

    if(new.map) {
        par(mfrow=c(1,1), mai=c(0.75,0.75,0.5,0.5), omi=rep(0,4),
            family="HersheySans")
        plot(1,type="n",xaxs="i",yaxs="i",xlim=lonlim, ylim=latlim,
             ann=FALSE, axes=TRUE, las=1, asp=1)
        box(col="dark grey")
    }

    # Add world map

    # EEZ land masses to keep for map (focus on Pacific)
    eez2keep <- c("American Samoa", "Antarctica","Australia","Bhutan",
                  "Canada", "China", "Cook Islands","Cuba",
                  "Fiji", "French Polynesia", "Guam", "Haiti","Hawaii", "Indonesia","Japan",
                  "Kiribati", "Korea","Malaysia","Marshall Islands", "Mexico", "Micronesia",
                  "Mongolia", "Myanmar","Nauru", "New Caledonia", "New Zealand", "Niue",
                  "Northern Mariana Islands", "Palau", "Papua New Guinea", "Philippines",
                  "Samoa", "Solomon Islands", "Tokelau", "Tonga", "Tuvalu", "USA", "USSR",
                  "Vanuatu", "Panama","Chile","Argentina", "Belize","Nicaragua","Ecuador",
                  "Honduras","Costa Rica", "French Guiana", "Suriname",
                  "Colombia", "Uruguay", "Brazil", "Peru", "Guatemala", "Guyana",
                  "Venezuela","Bolivia","Paraguay","Dominica")

    # to view names of polygons:
    #sa <- map('world2Hires', namesonly=T, ylim=c(-50,20), xlim=c(260,300), plot=F)

    data(world2HiresMapEnv) # loads dataset with landlines
    spc.region <- map('world2Hires', namesonly=T, ylim=c(-50,50), plot=F)

    # Keep countries specified in eez2keep
    geo <- lapply(eez2keep, function(ee) spc.region[grep(ee,spc.region)])

    # Add grid before continents if specified
    if(!missing(add.grid)) add.grid(add.grid)

    # ... add land for SPC countries (defined in object "geo")
    map('world2Hires', regions=unlist(geo), add=T, wrap=TRUE,
        resolution=0.1, fill=TRUE, col=col, border=border,...)

 }

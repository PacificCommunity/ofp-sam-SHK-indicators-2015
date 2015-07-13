
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# ------------------------------------------------------------------
# Import world EEZ shapefiles (downloaded at www.marineregions.org/)
message("Need to add other EEZs e.g. Indonesia which gets cut")
# too increase resolution of continent lines, see map function in add-continents-mapdata.r
require(maptools)
require(PBSmapping)
get.eez.outl <- function() {
  redo.eez <- FALSE
  if(redo.eez) {
    eez.sh <- readShapePoly("World_EEZ_v8_20140228/World_EEZ_v8_2014_HR.shp",
                            proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    eez.sh2 <- recenter(eez.sh)
    eez.ps2 <- SpatialPolygons2PolySet(eez.sh)
  }

  country.names <- read.xlsx("NTFSR_DataSummaries_SupportingData.xlsx",1)
  country.names$name_label[country.names$name_label == "Wallis & Futuna"] <- "Wallis and Futuna"
  n2keep <- unique(unlist(lapply(country.names$name_label, function(ss) grepv(ss, eez.sh$Country))))
  eez.ps <- lapply(n2keep, function(ee) eez.sh[eez.sh$Country==ee,]@polygons[[1]]@Polygons[[1]]@coords)
  eez.ps2 <- lapply(n2keep, function(ee) eez.sh[eez.sh$Country==ee,]@polygons[[1]]@Polygons[[2]]@coords)
  names(eez.ps) <- n2keep
  c2shift <- names(which(sapply(eez.ps, function(x) x[1,1])<0))
  scofunk <- function(x) {x[x<0] <- x[x<0]+360; x}
  eez.ps[c2shift] <- lapply(eez.ps[c2shift], function(ee) {cbind(scofunk(ee[,1]),ee[,2])})

  plot(1:10,type="n",xlim=c(120,240),ylim=c(-40,40),asp=1)
  dmm <- sapply(eez.ps2, function(x) lines(x[,1],x[,2],col="green"))
  lines(eez.ps[["French Polynesia"]],col="blue")
  lines(eez.ps2,col="green")
  #options(map.xlim=par("usr")[1:2], map.ylim=par("usr")[3:4])

  start.timer();
  t1 <- SpatialPolygons2PolySet(eez.sh[eez.sh$Country %in% n2keep,])
  stop.timer()

  eez.wmb <- readShapePoly("World_EEZ_v8_20140228/World_Maritime_Boundaries_v8.shp",delete_null_obj=TRUE)
  eez.wmb.sp <- SpatialPolygons2PolySet(eez.wmb)

  t1 <- joinPolys(hopspl[[1]],hopspl[[153]])
}

get.EEZ.outline <- function(wflag="Fiji") {

  # extract general country data from S4 object:
  dnow <- eez.sh@data[grepl(wflag,eez.sh@data$Country),]
  objid <- which(eez.sh@data$Country == wflag)
  poly.list <- eez.sh@polygons[[objid]] #

  # identify sub-polygons that from the EEZ outline
  # based on the area field
  # if all the EEZ is on one side of the date line, only pick the largest
  order(sapply(eez.sh@polygons[[187]]@Polygons,function(x) x@area),decreasing=TRUE)
  # if the EEZ is split, pick the two largest chunks

}

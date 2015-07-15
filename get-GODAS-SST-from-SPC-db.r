# adapted from lauratboyer's gist based on shark data coverage + indicator
# analysis (WP SC 2015)
# database location
require(RODBC)
db1 <- "driver=SQL Server;server=tetautai;database=OCEAN_DBS;UID=ofpuser;PWD=ofp123;"
channel<-odbcDriverConnect(db1)

getGODAS <- function(tb="GODAS.temperature",
                     variable=c("temp005","temp055","temp125","temp225"),
                     year=1995:2013,month=1:12,
                     rlat=c(-50,40),rlon=c(130,230),...) {

 if(any(c(length(rlat),length(rlon))!=2)) {
 stop("Provide range for lat/lon coordinates as so: c(minlat,maxlat)") }
 if(!exists("channel")) stop("Define object channel (connection to database)")

 variable0 <- variable
 variable <- paste(unique(variable),collapse=", ")
 year <- paste(unique(year),collapse=", ")
 month <- paste(unique(month),collapse=", ")

 query <- sprintf("select yy, mm, lat, lon, %s from %s where yy in (%s)
 and mm in (%s)
 and lon between %s and %s and lat between %s and %s",
 variable,tb,year,month,
 rlon[1]-1,rlon[2]+1,rlat[1]-1,rlat[2]+1)

 # run database extract
 start.timer()
 res <- sqlQuery(channel, query, ...)
 stop.timer()

 # aggregate at 1 degree resolution month x lon x lat
 res$lat1d <- floor(res$lat)+0.5
 res$lon1d <- floor(res$lon)+0.5
 start.timer()
 res.aggr <- aggregate(res[,variable0], as.list(res[,c("mm","lon1d","lat1d")]), mean)
 stop.timer()

 attr(res.aggr,"Notes") <- "Average over years 1995-2014, lat and lon centered at mid one-degree cell, e.g. 6.5 contains lats from 6 to 7"
 save(res.aggr, file=sprintf("DATA/GODAS-extract_%s_%s.RData",
               paste(variable0, collapse="-"), Sys.Date()))
 invisible(res.aggr)
}
#hop <- sqlQuery(channel, "select yy from GODAS.temperature")

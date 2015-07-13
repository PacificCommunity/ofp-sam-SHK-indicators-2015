## get-sunrise-sunset.r
## Sunrise/sunset calculation by lon-lat
## Equations from http://www.esrl.noaa.gov/gmd/grad/solcalc/calcdetails.html
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (lauratb@spc.int)
## Written on: July  7, 2015
## Time-stamp: <2015-07-13 18:31:18 lauratb>
options(digits=10)
lat.in <- 0
lon.in <- 170
excel.origin <- "1899-12-30"
in.date <- 40350 # "21/06/2010"
time.zone <-  1

get.timezone <- function() {


    time1 <- as.POSIXct("2015-01-01 12:00:00")
    latv <- seq(-40, 40,by=10)
    lonv <- seq(100,180,by=10)

    send.api <- function(lat, lon) {

        apiurl <- sprintf("https://maps.googleapis.com/maps/api/timezone/xml?location=%s,%s&timestamp=%d&sensor=false",
                          lat, lon, 10)
        print(apiurl)
        tz <- xmlParse(readLines(apiurl))
#        tz <- as.numeric(xmlParse(readLines(apiurl))[["string(//raw_offset)"]])
    }

   print(send.api(latv[1], lonv[1]))

}
deg2rad <- function(x) x * pi/180
rad2deg <- function(x) 180 * x/pi

# lon is -180 to 180
# i.e. longitudes greater than 180 need to be converted
get.ss <- function(lat.in=40, lon.in=-105, in.date="2000-01-01") {

    if(lon.in > 180) lon.in <- lon.in-360
    time.zone <- lon.in/15 # approx... could match to EEZ but MEH.
    time.past.local.midnight <- 0.1/24 # in 10th of an hour # E2
    in.date <- as.numeric(as.Date(in.date)-as.Date(0, origin=excel.origin))

    # F2
    julian.day <- in.date + 2415018.5 + time.past.local.midnight-time.zone/24
    # G2
    julian.century <- (julian.day-2451545)/36525
    # I2
    geom.mean.long <- (280.46646+julian.century*(36000.76983 + julian.century*0.0003032))%%360
    # J2
    geom.mean.anom.sun <- 357.52911+julian.century*(35999.05029 - 0.0001537*julian.century)
    # K2
    ecc.earth.orb <- 0.016708634-julian.century*(0.000042037+0.0000001267*julian.century)
    # L2
    sun.ctr <- sin(deg2rad(geom.mean.anom.sun))*(1.914602-julian.century*(0.004817+0.000014*julian.century))+
        sin(deg2rad(2*geom.mean.anom.sun))*(0.019993-0.000101*julian.century)+sin(deg2rad(3*geom.mean.anom.sun))*0.000289
    # M2
    sun.true.long <- geom.mean.long + sun.ctr
    # P2
    sun.app.long <- sun.true.long-0.00569-0.00478*sin(deg2rad(125.04-1934.136*julian.century))
    # Q2
    mean.obliq.eclipt <- 23+(26+((21.448-julian.century*(46.815+julian.century*(0.00059-julian.century*0.001813))))/60)/60 # Q2
    # R2
    obliq.corr <- mean.obliq.eclipt+0.00256*cos(deg2rad(125.04-1934.136*julian.century))
    # U2
    var.y <- (tan(deg2rad(obliq.corr/2)))^2
    # T2
    sun.decl <- rad2deg(asin(sin(deg2rad(obliq.corr))*sin(deg2rad(sun.app.long))))
    # W2
    HA.sunrise <- rad2deg(acos(cos(deg2rad(90.833))/(cos(deg2rad(lat.in))*cos(deg2rad(sun.decl)))-
                                   tan(deg2rad(lat.in))*tan(deg2rad(sun.decl))))
    # V2
eq.time <- 4*rad2deg(var.y*sin(2*deg2rad(geom.mean.long))-
                          2*ecc.earth.orb*sin(deg2rad(geom.mean.anom.sun))+
                              4*ecc.earth.orb*var.y*sin(deg2rad(geom.mean.anom.sun))*cos(2*deg2rad(geom.mean.long))-
                                  0.5*var.y^2*sin(4*deg2rad(geom.mean.long))-
                                      1.25*ecc.earth.orb^2*sin(2*deg2rad(geom.mean.anom.sun)))
# X2
solar.noon <- (720-4*lon.in - eq.time + time.zone*60)/1440

# Y2
sunrise.time <- 24*(solar.noon - HA.sunrise*4/1440)
sunset.time <- 24*(solar.noon + HA.sunrise*4/1440)

return(list(sunrise.dec=sunrise.time, sunset.dec=sunset.time))
}

#time2dec <- function(x) {

#    hr.

###}

get.ss.info <- function(wdat=head(sets)) {

    start.timer()
    ss.set <- lapply(1:nrow(wdat), function(i)
        get.ss(wdat[i, "lat1d"], wdat[i,"lon1d"], wdat[i,"date"]))
    stop.timer()
    sunrise <- sapply(ss.set,"[[","sunrise.dec")
    sunset <- sapply(ss.set,"[[","sunset.dec")

    set.in.day <- sapply(1:nrow(wdat), function(i) wdat$set_start_time[i] %between% ss.set[[i]])
    set.in.day <- ifelse(set.in.day,"day","night")
    dist.day <- sapply(1:nrow(wdat), function(i) min(abs(wdat$set_start_time[i] - unlist(ss.set[[i]]))))

    ss.obj <- data.frame(l_set_id=wdat$l_set_id, set.start=wdat$set_start_time, sunrise=sunrise, sunset=sunset, set.day=set.in.day, dist.day=dist.day)
    ss.obj$daypos <- ifelse(ss.obj$set.start<12, "am", "pm")
    ss.obj$daycat <- paste(ss.obj$set.day, ss.obj$daypos, sep="-")

    ss.obj$sunrise.dist1 <- abs(ss.obj$sunrise - ss.obj$set.start)
    ss.obj$sunrise.dist1[ss.obj$sunrise.dist1>12] %<>% "-"(24)%>%abs

    ss.obj

}

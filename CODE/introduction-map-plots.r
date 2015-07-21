library(maps)
library(mapproj)
library(mapdata)
source("CODE/add-continents-mapdata.r")
source("CODE/1_ind_analysis_preamble.r")
if(!exists("eez.contour")) aa <- load("DATA/EEZ-contours-0-360.RData")

make.map1 <- function() {
#png(file=paste(shkdir,"GRAPHICS/FIG_1_MAP.png",sep=''))
    #png(file="C:/wcpfc/shark indicators/shk-indicators-2015/GRAPHICS/FIG_1_MAP_RDS.png")
    ww <- 8; hh <- 7
check.dev.size(ww, hh)
par(family="HersheySans", mfrow=c(1,1), mai=c(0.5,0.5,0.2,0.2),omi=rep(0,4))
    plot(1,1,type="n",ylab="",xlab="",xlim=c(110,240),ylim=c(-60,50),col="cadetblue",cex=0.1,las=1, asp=1)
        abline(h=0,col="grey",lty=2)
    draw.eez(col="peachpuff")

    colp <- colorRampPalette(c("azure2","azure3"))(3)[1]
add.continents.poly(col=colp)
box()
#lines(eez[,1], eez[,2], col=1) # draw these boundaries over the set locations (obscure misplaced sets!)
#map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
#map('world2Hires',  region = c("USA","Hawaii","Mexico","Japan","China","South Korea","North Korea","Philippines","Vietnam","Laos","Taiwan","Fiji", "Vanuatu", "Malaysia","Burma","Thailand","Cambodia", "Australia", "New Zealand", "Indonesia", "New Caledonia", "Papua New Guinea", "Solomon Islands","Mongolia"), fill=T, add=T, yaxt="n", xaxt="n", col="black")

#Source the region lines
    #source("C:/Projects/SHK-indicators-2015/CODE/region_lines.r")
    draw.regions(lwd=2,col="royalblue3")
# Make Region Names.
text( c(152,195,147,190,157,200 ), y=2+c(27,27,-2,-2,-41,-41) , labels=1:6, cex=3.5, pos=3, font=2, col="royalblue4")
dev.copy(CairoPNG, file=paste0(shkdir,"GRAPHICS/Defined/FIG_01_MAP.png"),
         width=ww, height=hh, units="in", res=100)
dev.off()

}

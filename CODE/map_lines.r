
#map and lines for plots
    #    lines(eez[,1], eez[,2], col=1) # draw these boundaries over the set locations (obscure misplaced sets!)
    map('world2Hires',  yaxt="n", xaxt="n", add=T, resolution=1)
    map('world2Hires',  region =c(cntrylst,nams[grep("Hawaii", nams)]), fill=T, add=T, yaxt="n", xaxt="n", col=grey(0.75))
  
   #horizontal lines
   lines(c(120,210),c(50,50),col="black",lwd=2)
   lines(c(120,210),c(20,20),col="black",lwd=2)
   lines(c(210,230),c(-4,-4),col="black",lwd=2)
   lines(c(120,230),c(-10,-10),col="black",lwd=2)
   #lines(c(141,170),c(-40,-40),col="red",lwd=3)
   lines(c(141,150),c(-55,-55),col="black",lwd=2)
   lines(c(150,230),c(-60,-60),col="black",lwd=2)
   #vertical lines
   lines(c(120,120),c(50,-10),col="black",lwd=2)
   lines(c(141,141),c(-10,-55),col="black",lwd=2)
   lines(c(150,150),c(-55,-60),col="black",lwd=2)
   lines(c(180,180),c(50,20),col="black",lwd=2)
   lines(c(170,170),c(20,-60),col="black",lwd=2)
   lines(c(210,210),c(50,-4),col="black",lwd=2)
   lines(c(230,230),c(-4,-60),col="black",lwd=2)
   #labels for area numbers
    ctxt<-3
   text(125,35,"1",pos=3,cex=ctxt,col="black")
   text(125,7,"3",pos=3,cex=ctxt,col="black")
   text(145,-35,"5",pos=3,cex=ctxt,col="black")
   text(205,35,"2",pos=3,cex=ctxt,col="black")
   text(205,7,"4",pos=3,cex=ctxt,col="black")
   text(225,-35,"6",pos=3,cex=ctxt,col="black") 
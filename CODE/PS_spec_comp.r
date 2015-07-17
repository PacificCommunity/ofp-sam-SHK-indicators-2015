
all.sharks<-  c("ALS", "ALV", "AML", "BLR", "BRO", "BSH", "BSK", "BTH", "CCA",
"CCB", "CCE", "CCG", "CCL", "CCP", "CCU", "CNX", "CPS", "CVX",
"CYO", "CYP", "CYU", "CYW", "CZI", "DCA", "DGS", "DGX", "DOP",
"DUS", "EUB", "FAL", "GAG", "GTF", "GUQ", "HDQ", "HXT", "ISB",
"LMA", "LMD", "MAK", "NTC", "OCS", "ODH", "OSF", "POR", "PSK",
"PTH", "RSK", "SCK", "SHK", "SHL", "SKH", "SKX", "SMA", "SPK",
"SPL", "SPN", "SPZ", "SSQ", "SYR", "THR", "TIG", "TRB", "WSH",
            "YSA", "YSM")
all.sharks <- all.sharks[!(all.sharks %in% main.sharks)] # remove focus species
if(!exists("PS.long")) {
PSdat <- PSObsShk %>% filter(yy %in% 1995:2014, region %in% 1:6)
PSdat$MAK <- PSdat$SMA + PSdat$LMA + PSdat$MAK
PSdat$THR <- PSdat$BTH + PSdat$ALV + PSdat$PTH + PSdat$THR
PSdat$HHD <- PSdat$SPN + PSdat$SPL + PSdat$SPZ + PSdat$SPK # hammerheads
PSdat$SHK <- rowSums(PSdat[,all.sharks])
f2k <- c("yy","flag_id","region",main.sharks[1:8],"asso")
PSdat <- PSdat[,f2k]
PS.long <- data.frame(yy=PSdat$yy,
                      flag=PSdat$flag_id,
                      region=PSdat$region,
                      sp=rep(main.sharks[1:8],each=nrow(PSdat)),
                      count=unlist(PSdat[,main.sharks[1:8]]),
                      asso=PSdat$asso,
                      us.pg="OTH")
PS.long$regf <- factor(PS.long$region, levels=1:6)
PS.long$yyf <- factor(PS.long$yy, levels=1995:2014)

ps.all <- PS.long %>% group_by(yyf, us.pg, regf, sp) %>% summarize(count=sum(count))
ps.asso <- filter(PS.long, asso=="A") %>% group_by(yyf, us.pg, regf, sp) %>% summarize(count=sum(count))
ps.unasso <- filter(PS.long, asso=="U") %>% group_by(yyf, us.pg, regf, sp) %>% summarize(count=sum(count))
}



run.rest <- FALSE
if(run.rest){

head(PSObsShk)
range(PSObsShk$yy)


#goal is to show the % of shark species vs the # of tuna
#shark catch vs tuna catch, but it is in number...so does that make sense?
# had to represent the tuna catch as 1/10,000 of the total number.
# Maybe better to just show sharks?


par(xpd=NA)
par(las=1,   omi=c(1,1,0.2,0.1) )

PSObsShk<- PSObsShk[PSObsShk$yy %in% 1995:2014,]
tps <- PSObsShk[PSObsShk$region %in% 3:6, ]
tps$tuna<- rowSums(tps[,c("SKJ", "YFT", "BET")])

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)
tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)
tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
ttuna <- tapply(tps$tuna, list(tps$region, tps$yy), sum)
ttuna<-ttuna/1e4



#plot file
png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_PS_comp_reg.png",sep=''))
#
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
par(mar=c(3, 2, 2, 1) + 0.1)
par(las=1,   oma=c(2,2,3.5,1) )
plot.new()    # "draws" a blank space in reg 1
plot.new()    # "draws" a blank space in reg 2
for(i in 3:nreg){
  j <- i-2 # the matrices are just 4 columns long
  tmat <- rbind( tbsh[j,], tmak[j,], tocs[j,], tfal[j,], tthr[j,], ttuna[j,])
  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"black") , main=paste("Region", i), las=1 )
  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
}
plot.new()
#par(mar=c(0.5,0.5,0.5,0.5)  )
legend("center",legend=c(spec, "Tuna") ,fill=c(mycol, "black") , cex=1.5,horiz=TRUE,bty="n",  title="Species",xpd=NA)

dev.off()
 par("mai")
#----------------------------------------------------------for UNAssociated sets
tps <- PSObsShk[PSObsShk$region %in% 3:6 & PSObsShk$asso=="U", ]
tps$tuna<- rowSums(tps[,c("SKJ", "YFT", "BET")])

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)
tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)
tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
ttuna <- tapply(tps$tuna, list(tps$region, tps$yy), sum)
ttuna<-ttuna/1e4



#plot file
png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_PS_comp_reg_UNAS.png",sep=''))
#
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))

par(mar=c(3, 2, 2, 1) + 0.1)
par(las=1,   oma=c(2,2,3.5,1) )

plot.new()    # "draws" a blank space in reg 1
plot.new()    # "draws" a blank space in reg 2
for(i in 3:nreg){
  j <- i-2 # the matrices are just 4 columns long
  tmat <- rbind( tbsh[j,], tmak[j,], tocs[j,], tfal[j,], tthr[j,], ttuna[j,])
  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"black") , main=paste("Region", i) )
  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
}
plot.new()

legend("center",legend=c(spec, "Tuna") ,fill=c(mycol, "black") , cex=1.5,horiz=TRUE,bty="n",  title="Species",xpd=NA)

dev.off()

#
# plot 3 of this script
#----------------------------------------------------------forAssociated sets
tps <- PSObsShk[PSObsShk$region %in% 3:6 & PSObsShk$asso=="A", ]
tps$tuna<- rowSums(tps[,c("SKJ", "YFT", "BET")])

tbsh <- tapply(tps$blue, list(tps$region, tps$yy), sum)
tmak <- tapply(tps$mako, list(tps$region, tps$yy), sum)
tocs <- tapply(tps$ocs,  list(tps$region, tps$yy), sum)
tfal <- tapply(tps$silky,list(tps$region, tps$yy), sum)
tthr <- tapply(tps$thresher,  list(tps$region, tps$yy), sum)
tshark <- tapply(tps$othershk, list(tps$region, tps$yy), sum)
ttuna <- tapply(tps$tuna, list(tps$region, tps$yy), sum)
ttuna<-ttuna/1e4



#plot file
png(file=paste(shkdir,"GRAPHICS/catchcomp_xx_PS_comp_reg_ASSOC.png",sep=''))
#
layout( matrix(c(1,2,3,4,5,6,7,7),4,2,byrow=TRUE), widths=c(4,4), heights=c( 2,2,2,1))
par(mar=c(3, 2, 2, 1) + 0.1)
par(las=1,   oma=c(2,2,3.5,1) )

plot.new()    # "draws" a blank space in reg 1
plot.new()    # "draws" a blank space in reg 2
for(i in 3:nreg){
  j <- i-2 # the matrices are just 4 columns long
  tmat <- rbind( tbsh[j,], tmak[j,], tocs[j,], tfal[j,], tthr[j,], ttuna[j,])
  bp<-  barplot(prop.table(tmat,2), col=c(mycol,"black") , main=paste("Region", i) )
  if(i%in% 3:4) { mtext(side=2,outer=F,"Proportion of Catch Observed",line=2.5,cex=0.75, las=0 )  }
}
plot.new()

legend("center",legend=c(spec, "Tuna") ,fill=c(mycol, "black") , cex=1.5,horiz=TRUE,bty="n",  title="Species",xpd=NA)

dev.off()
}

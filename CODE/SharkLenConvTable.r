

#makes a table using  xtable 

require(xtable)

fields <- c( "Species", "Length at Maturity", "Reference(s)", "Conversion Factor(s)", "Reference(s)" )

           
        spec <-c( "Blue", "Mako (shortfin)", "Oceanic whitetip", "Silky", "Thresher (bigeye thresher", "Hammerhead (scalloped hammerhead)", "Porbeagle")
 
        r1 <- c("Blue","Males: 168 FL (200 TL) \n Females: 168 FL (200 TL)", "Nakano and Stevens (2008)", "FL=0.8313(TL)+1.39", "Skomal and Natanson (2003)")
        r2 <-c("Mako (shortfin mako)", 
        "Males: 180 FL \n Females: 275 FL",
        "Francis and Duffy (2005)",
        "FL=0.911(TL)+0.821",
        "Francis and Duffy (2005)")
        r3<- c("Oceanic whitetip",
        "Males: 138 FL (168 TL) \n Females: 144 FL (175 TL)",
        "Seki et al. (1998)",
        "FL =0.822(TL)+0",
        "Seki et al. (1998)")
        r4 <- c("Silky", 
        "Males: 175 FL (212 TL)\n Females: 173 FL (210 TL)",
        "Joung et al. (2008)",
        "FL = 0.8388(TL)-2.651",
        "Kohler, Casey and Turner (1996)")
        r5<- c("Thresher (bigeye thresher)", 
        "Males: 168 FL (270 TL)\n Females: 203 FL (332 TL)",
        "Smith et al. (2008)",
        "FL = 0.5598(TL) + 17.666",
        "Kohler, Casey and Turner (1996)")
        r6 <- c("Hammerhead (scalloped hammerhead)", "Males: 153 ( 198 TL)\n Females:  163 FL (210 TL)","Chen et al. 1990", "FL = 0.7756(TL) -0.3132" , "Kohler et al. 1996")
        r7 <- c("Porbeagle", "Males: 145 FL \n Females: 175 FL",
        "Francis and Duffy (2005)",
        "FL = 0.893(TL) -6.943", 
        "Francis and Duffy (2005)")


       sharktab <-  rbind(r1,r2,r3, r4, r5, r6,r7)
       colnames(sharktab) <- fields
        
        
sh_cap <- "Sources of information used in defining length at maturity and converting between total length (TL) and fork length (FL) measurement standards. TL measurements which fell outside the range of data used to construct the FL-TL conversion equations were excluded from the analysis."

        
sharktab1 <- xtable(sharktab, caption=c( sh_cap ))
        #digits(obstable) <-0 
        write.table(print(sharktab1, include.rownames=FALSE) , file='clipboard'  )

?xtable

        
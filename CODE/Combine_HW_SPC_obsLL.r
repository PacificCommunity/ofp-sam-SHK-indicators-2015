

load("E:/NOUMEA/C_drive_JoelR/Data/hw_data_ll_obs_set_070513.rdata")
head(hw_data)
dim(hw_data)

table(hw_data$program_code)

colnames(hw_data)
colnames(shk)

hw_data2 <- hw_data[,  - which(names(hw_data) %in% c("wire_trace", "SST")) ]

match(names(shk), names(hw_data2) )
dim(shk) ;
dim(hw_data2)

shk has flag and 
hw_data2 has bsh region

table(shk$flag)
shk3 <- shk[,-which(colnames(shk)=='flag')]
hw_data3 <- hw_data2[,-which(colnames(hw_data2)=="bsh_region")]
match(names(shk3), names(hw_data3) )

# clean up a bit
shk <- shk3; rm(shk3)
hw_data <- hw_data3; rm(hw_data)
#the SPC DB had some thatwwere in the other file 
hwspc <- shk[shk$program_code=='HWOB',]
hwspc$un_id <-  paste(hwspc$setdate ,hwspc$settime , sep='&')   ; head(hwspc)
hw_data$un_id <-  paste(hw_data$setdate ,hw_data$settime , sep='&')   ; head(hw_data)

table( match(hwspc$un_id ,  hw_data$un_id ) >0, useNA='always')

length( match(hwspc$un_id , hw_data$un_id ))
pntr <-  match(hw_data$un_id ,hwspc$un_id ) ; head(pntr)

cbind(hwspc[pntr[1:10],c(1:4,6,9)], hw_data[1:10,c(1:4,6,9)]) 
dim( hwspc)  ; dim(hw_data)  


 



cbind(hwspc[ 1:10 ,c(1:4,6,9, 141:145)], hw_data[pntr2[1:10],c(1:4,6,9,  141:145)]) 
pntr2 <-  match(hwspc$un_id,hw_data$un_id  ) ; head(pntr2)
pntr3 <- pntr2[is.na(pntr2)=="FALSE"]
length(pntr3)
hw_data3 <- hw_data[-pntr3,]
dim(hw_data3) ; 
cbind(colnames(shk), colnames(hw_data3))
shk_all <- rbind(shk, hw_data3[,-160])  
dim(shk_all)


shk_all <- shk_all[order(shk_all$yy, shk_all$mm),]
save( shk_all,  file="C:/Projects/SHK-indicators-2015/DATA/ll_obs_set_with_HW_11JUNE2015.rdata" )   



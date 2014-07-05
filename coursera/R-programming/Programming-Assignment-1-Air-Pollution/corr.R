corr <- function(directory, threshold = 0) {
   compres <- complete(directory)
   compres <- compres[compres[,"nobs"] > threshold,]
   len <- nrow(compres)
   result <- numeric(len)
   if(len>0){
   for(i in 1:len){
     
     fileid <- as.character(compres[i,"id"])
     numofchar <- nchar(fileid)
     if(numofchar < 2 ){
       fileid <- paste("00",fileid,sep="")
     }
     else if(numofchar < 3){
       fileid <- paste("0",fileid,sep="")
     }
     filename <- paste(fileid, ".csv", sep = "")
     filepath <- paste(directory, filename, sep = "\\" )
     pdata <- read.csv(filepath, header = TRUE)
     pdata <- pdata[complete.cases(pdata),];
     result[i] = round(cor(pdata[,"sulfate"],pdata[,"nitrate"]),digits=4)
   }
   }
  result
}
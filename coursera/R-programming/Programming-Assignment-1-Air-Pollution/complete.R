complete <- function(directory, id = 1:332) {
  len <- length(id)
  nobs <- integer(len)
  for(i in 1:len){
    
    fileid <- as.character(id[i])
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
    nobs[i] <- sum(complete.cases(pdata))
  
  }
  data.frame(id,nobs)
}
  
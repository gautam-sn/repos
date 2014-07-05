pollutantmean <- function(directory, pollutant, id = 1:332){
  
  count = 0
  
  for(idno in id){
    fileid <- as.character(idno)
    numofchar <- nchar(fileid)
    
    if(numofchar < 2 ){
      fileid <- paste("00",fileid,sep="")
    }
    
    else if(numofchar < 3){
      fileid <- paste("0",fileid,sep="")
    }
    
    filename <- paste(fileid, ".csv", sep = "")
    filepath <- paste(directory, filename, sep = "\\" )
    
    if(count == 0){
      count = 1
      pdata <- read.csv(filepath, header = TRUE)  
    }
    
    else{
      pdata <- rbind(pdata, read.csv(filepath, header=TRUE))
    }
  }
  round(mean(pdata[,pollutant] ,na.rm = TRUE),3)
}

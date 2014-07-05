rankall <- function(outcome, num = "best") {
   
  findrank <- function(x,column_name,rank){
      
     filterdata <- order(x[,column_name], x[,"Hospital.Name"], na.last=NA)
     if(rank == "best"){
       return(x[filterdata[1],"Hospital.Name"])
     }
     if(rank == "worst"){
       return(x[filterdata[length(filterdata)],"Hospital.Name"])
     }
    x[filterdata[rank],"Hospital.Name"]
  }
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" ){
    stop("invalid outcome")
  }
  
  value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if(outcome == "heart attack"){
    value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome == "heart failure"){
    value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  data[,value_column] <- suppressWarnings(as.numeric(data[,value_column]))
  
  ## For each state, find the hospital of the given rank
  res<-do.call(rbind.data.frame,by(data,data[,"State"],findrank,column_name=value_column,rank=num,simplify = FALSE))
  res<-data.frame(res,rownames(res))
  colnames(res) <- c("hospital","state")
  res
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!is.element(state,data[,"State"])){
    stop("invalid state")
  }
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" ){
    stop("invalid outcome")
  }
  ## Return hospital name in that state with lowest 30-day death
  value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if(outcome == "heart attack"){
    value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  }
  else if(outcome == "heart failure"){
    value_column <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  
  filterdata <- data[data[,"State"] == state, c("Hospital.Name",value_column)]
  
  filterdata[,value_column] <- suppressWarnings(as.numeric(filterdata[,value_column]))
  
  filterdata <- filterdata[order(filterdata[,value_column],filterdata[,"Hospital.Name"],na.last = NA),"Hospital.Name"]

  ## Return hospital name in that state with the given rank
  if(num == "best"){
    return(filterdata[1])
  }
  if(num == "worst"){
    return(filterdata[length(filterdata)])
  }
  filterdata[num]
}
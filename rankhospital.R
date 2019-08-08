rankhospital <- function(state, outcome, num = "best") {
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out[,11] <- as.numeric(out[,11])
  out[,17] <- as.numeric(out[,17])
  out[,23] <- as.numeric(out[,23])
  result <- vector()
  if (outcome=="heart attack"){
    result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name))
    result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,result$Hospital.Name,na.last=NA),]}
  else if (outcome =="heart failure"){
    result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name))
    result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,result$Hospital.Name,na.last=NA),]}
  else if (outcome=="pneumonia"){
    result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name))
    result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,result$Hospital.Name,na.last=NA),]}
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (num=="best"){
    return(result[1,"Hospital.Name"])}
  else if (num=="worst"){
    return(result[nrow(result),"Hospital.Name"])
  }
  else
    return(result[num,"Hospital.Name"])
}
result <- subset(out,State=="TX" ,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name))
result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)]   
result
?order

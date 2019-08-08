best <- function(state, outcome) {
  if (outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia" )
    return(warning(paste("Error in best(\"",state,',',outcome,"\") : invalid outcome")))
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if (!state %in% out$State){
    return(warning(paste("Error in best(\"",state,',',outcome,"\") : invalid state")))
  }
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
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  result[1,"Hospital.Name"]
}
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
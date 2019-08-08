rankall <- function(outcome, num = "best") {
  out <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  out[,11] <- as.numeric(out[,11])
  out[,17] <- as.numeric(out[,17])
  out[,23] <- as.numeric(out[,23])
  result <- vector()
  mx <- matrix(ncol=2)
  if (num=="best"){
    num <- 1 }
  for (state in unique(out$State)){
    if (outcome=="heart attack"){
      result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name,State))
      result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,result$Hospital.Name,na.last=NA),]}
    else if (outcome =="heart failure"){
      result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name,State))
      result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,result$Hospital.Name,na.last=NA),]}
    else if (outcome=="pneumonia"){
      result <- subset(out,State==state,select=c(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name,State))
      result<- result[order(result$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,result$Hospital.Name,na.last=NA),]}
    if (num=="worst"){
      mx <- rbind(mx,c(result[nrow(result),"Hospital.Name"],state))}
    else{
    mx <- rbind(mx,c(result[num,"Hospital.Name"],state))}}
  colnames(mx) <- c("hospital","state")
  data.frame(mx)
}
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
m
cbind(1:7, diag(3)) # vector is subset -> warning
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


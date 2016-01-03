## best.R

best <- function(state, outcome) {
  
  ## Read data from .csv
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## checks if the outcome input is in the list of valid outcomes
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  
  ## checks if the state input is in the list of valid states
  validState = unique(data[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,validOutcome)]
  
  ## Return hospital name in that state with lowest 30dDR
  data.state <- data[data$State==state,]
  idx <- which.min(as.double(data.state[,colName]))
  data.state[idx,"Hospital.Name"]
}
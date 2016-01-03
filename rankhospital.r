##rankhospital.R

rankhospital <- function(state,outcome,num){
  
  ##read data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ##check for validity of states and outcomes
  states <- data[ , 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  if ((state %in% states) == FALSE) {
    stop(print("invalid state"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("invalid outcome"))
  }
  
  
  ##get state-specific data
  state_data <- subset(data, State == state)
  
  ##get correct outcome column
  if (outcome == "heart attack") {
    outcome_column <- 11
  }
  else if (outcome == "heart failure") {
    outcome_column <- 17
  }
  else {
    outcome_column <- 23
  }
  
  
  ##check to make sure num is inbounds/valid
  if (is.numeric(num) == TRUE) {
    if (length(data[,2]) < num) {
      return(NA)
    }
  }
  
  ##purge the NA's
  state_data[, outcome_column] <- as.numeric(state_data[,outcome_column])
  bad <- is.na(state_data[, outcome_column])
  desired_data <- state_data[!bad, ]
  
  ##order the data frame by outcome values
  outcome_column_name <- names(desired_data)[outcome_column]
  hospital_column_name <- names(desired_data)[2]
  index <- with(desired_data, order(desired_data[outcome_column_name], desired_data[hospital_column_name]))
  ordered_desired_data <- desired_data[index, ]
  
  
  #check if num is best or worse and interpret as necessary
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ordered_desired_data[, outcome_column])
    }
  }
  
  #return the hospital name with the outcome ranking of num
  ordered_desired_data[num, 2]

}
## returns a character vector with the name of the hospital that has the best 
## (i.e. lowest) 30-day  mortality  for  the  specified  outcome in that state.

best <- function(state, outcome) {
    # Check for invalid outcome input type
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    # Get index for our given outcome string.
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    
    # Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomeData[, index] <- suppressWarnings(as.numeric(outcomeData[,index])) 
    outcomeData <- na.omit(outcomeData) # get rid of NA values
    
    ## Check that state and outcome are valid
    states <- table(outcomeData$State)
    if (!state %in% names(states)) { 
        stop("invalid state")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    slice <- subset(outcomeData, State==state) # slice data by state
    slice <- slice[order(slice[,index], na.last=TRUE),2] # sort by hospital name
    slice <- na.omit(slice)
    bestHospital <- slice[1]
    bestHospital
}
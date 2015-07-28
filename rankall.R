rankall <- function(outcome, num="best") {
    # Check for invalid outcome input type
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    outcome.names <- c("heart attack", "heart failure", "pneumonia")
    
    # Get index for our given outcome string.
    index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    
    # Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    
    # rename outcome columns for easy reference
    names(outcomeData)[c(11,17,23)] <- outcome.names
    
    # take just the columns we need, convert outcome column to numeric
    outcomeData <- outcomeData[,c("State","Hospital.Name",outcome)]
    outcomeData[,outcome] <- suppressWarnings(as.numeric(outcomeData[,outcome]))
    outcomeData <- outcomeData[!is.na(outcomeData[outcome]),] # get rid of NA values
    
    # sort data by state name, then outcome, then hospital name
    outcomeData <- outcomeData[order(outcomeData$State, outcomeData[outcome], outcomeData$Hospital.Name),]
    
    # aggregate by state, choosing the row that corresponds to the rank num
    ranksByState <- aggregate(outcomeData, by=list(outcomeData$State), function(x) {
        if(num == "best"){  # do some input cleaning for num
            num <- 1
        }
        else if(num == "worst"){
            num <- length(x)
        }
        else if(!is.numeric(num)){
            stop("invalid num")
        }
        
        x[num]
    })
    
    # get just the columns we need and rename them
    retRanks <- ranksByState[,c(3,1)]
    names(retRanks) <- c("hospital","state")
    retRanks
}
complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # set working directory
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    # get the length of id vector
    idLen <- length(id)
    completeData <- rep(0, idLen)
    # find all files in the specdata folder
    allFiles <- as.character( list.files(directory) )
    filePaths <- paste(directory, allFiles, sep="")
    j <- 1 
    for (i in id) {
        currentFile <- read.csv(filePaths[i], header=T, sep=",")
        completeData[j] <- sum(complete.cases(currentFile))
        j <- j + 1
    }
    result <- data.frame(id = id, nobs = completeData)
    return(result)
}
pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
  
    # set working directory
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    # vector for pollutant data
    meanVec <- c()
    # find all files in the specdata folder
    allFiles <- as.character( list.files(directory) )
    filePaths <- paste(directory, allFiles, sep="")
    for(i in id) {
        currentFile <- read.csv(filePaths[i], header=T, sep=",")
        head(currentFile)
        pollutant
        naRemoved <- currentFile[!is.na(currentFile[, pollutant]), pollutant]
        meanVec <- c(meanVec, naRemoved)
    }
    result <- mean(meanVec) # DO NOT ROUND THE RESULT
    return(result) 
}
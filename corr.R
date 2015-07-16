corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    # set working directory
    if(grep("specdata", directory) == 1) {
        directory <- ("./specdata/")
    }
    # get the complete table
    completeTable <- complete("specdata", 1:332)
    nobs <- completeTable$nobs
    # find the valid ids that are > threshhold
    ids <- completeTable$id[nobs > threshold]
    # get the length of ids vector
    idLen <- length(ids)
    corrVec <- rep(0, idLen)
    # find all files in the specdata folder
    allFiles <- as.character( list.files(directory) )
    filePaths <- paste(directory, allFiles, sep="")
    j <- 1
    for(i in ids) {
        currentFile <- read.csv(filePaths[i], header=T, sep=",")
        corrVec[j] <- cor(currentFile$sulfate, currentFile$nitrate, use="complete.obs")
        j <- j + 1
    }
    result <- corrVec
    return(result)   
}
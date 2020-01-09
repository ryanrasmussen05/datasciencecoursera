pollutantmean <- function(directory, pollutant, id = 1:332) {
  allMetrics <- numeric()
  for(i in id) {
    filePath <- paste(directory, '/', formatC(i, width=3, flag='0'), '.csv', sep='')
    data <- read.csv(filePath)
    metrics <- data[[pollutant]]
    filteredMetrics <- metrics[!is.na(metrics)]
    allMetrics <- c(allMetrics, filteredMetrics)
  }
  
  mean(allMetrics)
}


complete <- function(directory, id = 1:332) {
  nobs <- numeric()
  
  for(i in id) {
    filePath <- paste(directory, '/', formatC(i, width=3, flag='0'), '.csv', sep='')
    data <- read.csv(filePath)
    fullObservations = data[!is.na(data$sulfate) & !is.na(data$nitrate),]
    nobs <- c(nobs, nrow(fullObservations))
  }
  
  data.frame(id=id, nobs=nobs)
}


corr <- function(directory, threshold = 0) {
  nobsData <- complete(directory)
  validCases = nobsData[nobsData$nobs > threshold,]
  
  correlations <- numeric()
  
  for(i in validCases$id) {
    filePath <- paste(directory, '/', formatC(i, width=3, flag='0'), '.csv', sep='')
    data <- read.csv(filePath)
    fullObservations = data[!is.na(data$sulfate) & !is.na(data$nitrate),]
    
    correlations <- c(correlations, cor(fullObservations$sulfate, fullObservations$nitrate))
  }
  
  correlations
}
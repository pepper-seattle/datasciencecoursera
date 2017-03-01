corr <- function(directory, threshold = 0) {
  fileList <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  crNum <- numeric()
  nobsData <- complete("specdata/")
  nobsData <- nobsData[nobsData$nobs > threshold, ]
  
  for(cid in nobsData$id) {
    monitorDf <- getmonitor(cid, directory)
    crNum <- c(crNum, cor(monitorDf$sulfate, monitorDf$nitrate, use = "pairwise.complete.obs"))
  }
  return(crNum)
}

complete <- function(directory, id = 1:332) {
  nobsNum <- numeric()
  
  for(i in id) {
    cDf <- getmonitor(i, directory)
    nobsNum <- c(nobsNum, nrow(na.omit(cDf)))
  }
  data.frame(id = id, nobs = nobsNum)
}

getmonitor <- function(id, directory, summarize = FALSE) {
  fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv",
                   sep = "")
  rawDf <- read.csv(fileStr)
  
  if(summarize) {
    print(summary(rawDf))
  }
  return(rawDf)
}
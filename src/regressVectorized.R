library(reshape)
library(plyr)

readData <- function(aSymbol, aInterval){
  theData <- read.csv(paste(getwd(), "\\output\\clockIntervals\\", aSymbol, "\\",
                            aInterval, ".csv", sep =""),
                      header = FALSE, stringsAsFactors = FALSE)
  return(theData)
}
decodeExchange <- function(aCode){
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "DATE", "X_1", "O")
  exchange = c("NYSE MKT","NASDAQ BX","NSX", "FINRA", "ISE", "BATS EDGA","BATS EDGX","CHX",
               "NYSE","NASDAQ T","NYSE Arca","Consolidated Tape System","NASDAQ TQ", "NASDAQ Q", 
               "CBSX", "NASDAQ PSX", "BATS BYX", "BATS BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  theExchange <- mapvalues(aCode, from = code, to = exchange)
  return(theExchange)
}
formatData <- function(myShares, aInterval){
  myShares[is.na(myShares)] = 0
  colnames(myShares) <- c("DATETIME", "EXCHANGE", "SHARE")
  myShares$DATETIME <- as.POSIXct(myShares$DATETIME)
  myShares$EXCHANGE <- decodeExchange(myShares$EXCHANGE)
  return(myShares)
}
NAtoZero
fillInBlanks <- function(myShares, aInterval){
  myStartDateTime <- min(myShares$DATETIME)
  myEndDateTime <- max(myShares$DATETIME)
  myReformattedInterval <- tolower(gsub("([0-9])([A-z]*)", "\\1 \\2s", aInterval))
  myDateTimeSeq <- seq(from=myStartDateTime, to=myEndDateTime, by=myReformattedInterval)
  myIsPast <- as.numeric(substr(myDateTimeSeq, 12,13)) >= 10
  myIsBefore <- as.numeric(substr(myDateTimeSeq, 12,13)) < 15
  myDateTimeSeq <- myDateTimeSeq[myIsPast & myIsBefore]
  
  
  myExchanges <- unique(myShares$EXCHANGE)
  
  myPairings <- expand.grid(myDateTimeSeq, myExchanges)
  colnames(myPairings) <- c("DATETIME", "EXCHANGE")
  myPairings$FILLEDSHARE <- rep(0, dim(myPairings)[1])
  
  
  myFilled<-join(myPairings, myShares, by=c("DATETIME", "EXCHANGE"), type="left")
  myFilled[is.na(myFilled)] <- 0
  myFilled <- myFilled[,-c(3,4)]
  
  return(myFilled)
  ####################
}


setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/Code/TAQ")
mySymbol="AMD"
myInterval = "1Min"

myShares <- readData(mySymbol, myInterval)
myShares <- formatData(myShares)
myFilledShares <- fillInBlanks(myShares, myInterval)







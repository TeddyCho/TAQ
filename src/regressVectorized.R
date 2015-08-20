library(reshape)

readData <- function(aSymbol){
  theData <- read.csv(paste(getwd(), "\\output\\clockIntervals\\", aSymbol, "\\",
                            "1Min.csv", sep =""),
                      header = FALSE, stringsAsFactors = FALSE)
  return(theData)
}
decodeExchange <- function(aCode){
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "DATE", "X_1", "O")
  exchange = c("NYSE MKT","NASDAQ BX","NSX", "FINRA", "ISE", "BATS EDGA","BATS EDGX","CHX",
               "NYSE","NASDAQ T","NYSE Arca","Consolidated Tape System","NASDAQ TQ", "NASDAQ Q", 
               "CBSX", "NASDAQ PSX", "BATS BYX", "BATS BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  myColMap = data.frame(code, exchange)
  theExchange = myColMap$exchange[which(myColMap$code==aCode)]
  return(theExchange)
}
formatData <- function(myShares){
  myShares[is.na(myShares)] = 0
  colnames(myShares) <- c("DATE", "EXCHANGE", "SHARE")
  return(myTable)
}

setwd("C:/Users/tcho/Dropbox/Project - Platform Competition/Code/TAQ")
mySymbol="AMD"

myShares <- readData(mySymbol)
formatData(myShares)
getTAQBreakdown <- function(aFilePath, aSymbolExchange){
  aFileName<- paste(getwd(), aFilePath, sep ="")
  myBreakdown <- read.csv(aFileName, header = TRUE)
  
  myBreakdown = myBreakdown[myBreakdown$Exchange != "FINRA", ]
  
  myBreakdown$Date = as.Date(sapply(myBreakdown$Date, toString),"%Y%m%d")
  myBreakdown$DaysSince = myBreakdown$Date - as.Date("2014-01-01")
  
  myBreakdown$ListedExchange <- sapply(myBreakdown$Symbol,
                                       function(x) inferListedExchangeFromSymbol(x, aSymbolExchange))
  myBreakdown
}

inferListedExchangeFromSymbol <- function(aSymbol, aSymbolExchange) {
  theExchange = "UNKNOWN"
  theExchange = aSymbolExchange$exchange[aSymbolExchange$symbol == aSymbol]
}
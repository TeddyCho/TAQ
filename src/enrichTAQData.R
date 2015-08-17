getTAQBreakdown <- function(aFilePath, aSymbolExchange){
  myBreakdown <- read.csv(aFilePath, header = TRUE)
  
  myBreakdown = myBreakdown[myBreakdown$Exchange != "FINRA", ]
  
  myBreakdown$Date = as.Date(sapply(myBreakdown$Date, toString),"%Y%m%d")
  myBreakdown$DaysSince = myBreakdown$Date - as.Date("2014-01-01")
  
  myBreakdown$ListedExchange <- sapply(myBreakdown$Symbol,
                                       function(x) inferListedExchangeFromSymbol(as.character(x), aSymbolExchange))
  myBreakdown
}

inferListedExchangeFromSymbol <- function(aSymbol, aSymbolExchange) {
  theExchange = "UNKNOWN"
  theExchange = aSymbolExchange$listedExchange[which(aSymbolExchange$symbol == aSymbol)]
  return(theExchange)
}
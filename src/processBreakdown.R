library(rCharts)
library(ggplot2)
library(knitr)
library(base64enc)
source("src/enrichTAQData.r")
source("src/createTAQCharts.r")

if(TRUE){
  myHomeExchanges = c("NASDAQ", "NYSE", "Arca", "NYSEMKT")
  myTODs = c("Open", "Regular", "Close", "After-Hours")
  
  myNASDAQSymbols <- c("GOOG", "TSLA", "GT", "STLD", "WFM", "NDAQ")
  myNYSESymbols <-  c("AMD", "CVS", "GE", "WSM", "LTM", "LUV", "BHP", "BRKA", "T", "FE", "BAC", "XOM")
  myArcaSymbols <- c("TBF", "SLV")
  myNYSEMKTSymbols <- c("ONP", "LIQT", "CCF", "ONVO")
  symbol = c(myNASDAQSymbols, myArcaSymbols, myNYSESymbols, myNYSEMKTSymbols)
  exchange = c(rep("NASDAQ", length(myNASDAQSymbols)), rep("Arca", length(myArcaSymbols)),
    rep("NYSE", length(myNYSESymbols)), rep("NYSEMKT", length(myNYSEMKTSymbols)))
  mySymbolExchange = data.frame(symbol, exchange)
  myExchanges = unique(mySymbolExchange$exchange)
}
myBreakdown = getTAQBreakdown("\\data\\breakdown5aa0ce4b018e0067.csv", mySymbolExchange)

myResults <- createAggregateMultiBar(myBreakdown, "1000000000", "billions")
myResults$plot$save('AllStocks.html', standalone = TRUE)
myResults$collapsed$Proportion = round(100*myResults$collapsed$Volume / sum(myResults$collapsed$Volume), 4)  

for(i in 1:length(myTODs)) {
  myTOD = myTODs[i]
  myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                      c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                      myTOD)
  myPerExchangeForTOD$save(paste('perExchangeFor', myTOD, '.html', sep=""), standalone = TRUE)
  for(j in 1:length(myExchanges)) {
    myExchange = myExchanges[j]
    mySymbolsForExchange = mySymbolExchange$symbol[mySymbolExchange$exchange == myExchange]
    myPerSymbolForTOD <- createBySymbolForTOD(myBreakdown, mySymbolsForExchange, myTOD)
    myPerSymbolForTOD$save(paste('', myExchange, 'SymbolsFor', myTOD, '.html', sep=""), standalone = TRUE)
  }
}
myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                    c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                    c("Open", "Close"))
myPerExchangeForTOD$save(paste('perExchangeForOpenClose.html', sep=""), standalone = TRUE)
for(i in 1:length(myHomeExchanges)) {
  myHomeExchange = myHomeExchanges[i]
  myHomeSymbols = mySymbolExchange$symbol[mySymbolExchange$exchange == myHomeExchange]
  myPerTODForSymbols <- createByTODForSymbol(myBreakdown, myHomeSymbols, myTODs)
  myPerTODForSymbols$save(paste('perTODFor', myHomeExchange, 'Symbols.html', sep=""), standalone = TRUE) 
}

# Trades by Exchange over 2014
graphExchangeShareByTOD(myBreakdown, "Open")
graphExchangeShareByTOD(myBreakdown, "Close")
graphExchangeShareByTOD(myBreakdown, "Regular")
graphExchangeShareByTOD(myBreakdown, "After-Hours")

myDateTODExchange <- aggregate(Volume ~ Date + TOD + Exchange, data=myBreakdown, function(x) sum(as.numeric(x)))
myDateTOD <- aggregate(Volume ~ Date + TOD, data=myBreakdown, function(x) sum(as.numeric(x)))
myDateTODExchange$Proportion <- mapply(function(x,y,z)
  x / myDateTOD$Volume[myDateTOD$Date == y & myDateTOD$TOD == z],
                                       myDateTODExchange$Volume,
                                       myDateTODExchange$Date,
                                       myDateTODExchange$TOD)
write.csv(myDateTODExchange, file = "taskTwo.csv")

"GOOG TSLA GT STLD WFM NDAQ AMD TBF SLV CVS GE WSM LTM LUV BHP BRKA T FE BAC XOM ONP LIQT CCF ONVO"
opts_chunk$set(comment = NA, results = "asis", comment = NA, tidy = F)
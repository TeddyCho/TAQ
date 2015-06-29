library(rCharts)
library(ggplot2)
library(knitr)
library(xtable)
library(reshape)
library(base64enc)
source("src/enrichTAQData.r")
source("src/createTAQCharts.r")
source("src/writeTablesToHTML.r")


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
myResults$plot$save('\\output\\breakdowncharts\\AllStocks.html', standalone = TRUE)

for(i in 1:length(myTODs)) {
  myTOD = myTODs[i]
  myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                      c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                      myTOD)
  myPerExchangeForTOD$plot$save(paste('\\output\\breakdowncharts\\perExchangeFor', myTOD, '.html', sep=""), standalone = TRUE)
  dataFrameToFormatOutput(myPerExchangeForTOD$propTable)
  for(j in 1:length(myExchanges)) {
    myExchange = myExchanges[j]
    mySymbolsForExchange = mySymbolExchange$symbol[mySymbolExchange$exchange == myExchange]
    myPerSymbolForTOD <- createBySymbolForTOD(myBreakdown, mySymbolsForExchange, myTOD)
    myPerSymbolForTOD$plot$save(paste('', myExchange, 'SymbolsFor', myTOD, '.html', sep=""), standalone = TRUE)
    dataFrameToFormatOutput(myPerSymbolForTOD$propTable)
  }
}
myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                    c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                    c("Open", "Close"))
myPerExchangeForTOD$plot$save(paste('\\output\\breakdowncharts\\perExchangeForOpenClose.html', sep=""), standalone = TRUE)
dataFrameToFormatOutput(myPerExchangeForTOD$propTable)
for(i in 1:length(myHomeExchanges)) {
  myHomeExchange = myHomeExchanges[i]
  myHomeSymbols = mySymbolExchange$symbol[mySymbolExchange$exchange == myHomeExchange]
  myPerTODForSymbols <- createByTODForSymbol(myBreakdown, myHomeSymbols, myTODs)
  myPerTODForSymbols$plot$save(paste('\\output\\breakdowncharts\\perTODFor', myHomeExchange, 'Symbols.html', sep=""), standalone = TRUE) 
}

if(TRUE){
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
  
}

"GOOG TSLA GT STLD WFM NDAQ AMD TBF SLV CVS GE WSM LTM LUV BHP BRKA T FE BAC XOM ONP LIQT CCF ONVO"
opts_chunk$set(comment = NA, results = "asis", comment = NA, tidy = F)
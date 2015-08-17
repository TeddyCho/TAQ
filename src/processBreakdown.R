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
  mySymbolExchange = data.frame(symbol = c(myNASDAQSymbols, myArcaSymbols, myNYSESymbols, myNYSEMKTSymbols),
                                listedExchange = c(rep("NASDAQ", length(myNASDAQSymbols)),
                                                   rep("Arca", length(myArcaSymbols)),
                                                   rep("NYSE", length(myNYSESymbols)),
                                                   rep("NYSEMKT", length(myNYSEMKTSymbols))))
  myExchanges = unique(mySymbolExchange$exchange)
}

mySymbols = c("AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY")
myExchangesMaster = c("NSX", "CBSX", "NASDAQ PSX", "CHX", "BATS BYX", "BATS EDGA", "NASDAQ BX",
                      "NYSE MKT","NYSE",  "NYSE Arca", "BATS BZX", "BATS EDGX", "NASDAQ")
for(sym in mySymbols){
  myBreakdown = getTAQBreakdown(paste(getwd(), "\\data\\breakdown", sym, ".csv", sep =""),
                                mySymbolExchange)
  
  brkPlot=createExchangeShareLines(myBreakdown, "stackedAreaChart")
  brkPlot$set(title = paste(sym, "'s Exchange Shares over 2014", sep=""))
  brkPlot$save(paste(getwd(),'\\output\\exchangeShares\\stackedArea', sym, '.html', sep=""),
               standalone = TRUE)
  brkPlot=createExchangeShareLines(myBreakdown, "lineChart")
  brkPlot$set(title = paste(sym, "'s Exchange Shares over 2014", sep=""))
  brkPlot$save(paste(getwd(),'\\output\\exchangeShares\\line', sym, '.html', sep=""),
               standalone = TRUE)
}
for(sym in mySymbols){
  if(sym == mySymbols[1]){
    myBreakdown = getTAQBreakdown(paste(getwd(), "\\data\\breakdown", sym, ".csv", sep =""), mySymbolExchange)
  }else{
    myBreakdown = rbind(myBreakdown, getTAQBreakdown(paste(getwd(), "\\data\\breakdown", sym, ".csv", sep =""),
                                        mySymbolExchange))
    print(sym)
  }}

myExchanges = unique(myBreakdown$Exchange)
for(exch in myExchanges){
  myFilteredBreakdown = myBreakdown[which(myBreakdown$TOD == "Regular" & 
                                            myBreakdown$Exchange == exch),]
  myFilteredBreakdown = myFilteredBreakdown[order(myFilteredBreakdown$Date),]
  myFilteredBreakdown$Proportion = round(myFilteredBreakdown$Proportion,4)
  myMaxProportion = max(myFilteredBreakdown$Proportion)
  
  breakdownPlot <- nPlot(Proportion ~ Date, data = myFilteredBreakdown,
                         group = "Symbol", type = "lineChart")
  
  breakdownPlot$yAxis(axisLabel = paste("Proportion"), 
                      showMaxMin = FALSE, width = 40)
  breakdownPlot$chart(forceY = c(0, 1.1*myMaxProportion))
  breakdownPlot$xAxis(axisLabel = paste("Date"), tickFormat =   "#!
                      function(d) {return d3.time.format('%m-%d-%y')(new Date(d*1000*3600*24));}
                      !#",
                      rotateLabels = -45,axisLabel = paste("Date"), 
                      showMaxMin = FALSE, width = 40)
  
  breakdownPlot$set(width = 1200, height = 800)
  
  #brkPlot=createExchangeShareLines(myBreakdown, "lineChart")
  breakdownPlot$set(title = paste(exch, "'s Shares over 2014", sep=""))
  breakdownPlot$save(paste(getwd(),'\\output\\exchangeShares\\line', exch, '.html', sep=""),
               standalone = TRUE)
  
}



myResults <- createAggregateMultiBar(myBreakdown, "1000000000", "billions")
myResults$plot$save(paste(getwd(),'\\output\\breakdowncharts\\AllStocks.html', sep=""), standalone = TRUE)

for(i in 1:length(myTODs)) {
  myTOD = myTODs[i]
  myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                      c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                      myTOD)
  myPerExchangeForTOD$plot$save(paste(getwd(), '\\output\\breakdowncharts\\perExchangeFor', myTOD, '.html', sep=""), standalone = TRUE)
  dataFrameToFormatOutput(myPerExchangeForTOD$propTable)
  for(j in 1:length(myExchanges)) {
    myExchange = myExchanges[j]
    mySymbolsForExchange = mySymbolExchange$symbol[mySymbolExchange$exchange == myExchange]
    myPerSymbolForTOD <- createBySymbolForTOD(myBreakdown, mySymbolsForExchange, myTOD)
    myPerSymbolForTOD$plot$save(paste(getwd(), '\\output\\breakdowncharts\\', myExchange, 'SymbolsFor', myTOD, '.html', sep=""), standalone = TRUE)
    dataFrameToFormatOutput(myPerSymbolForTOD$propTable)
  }
}
myPerExchangeForTOD <- createByListedExchangeForTOD(myBreakdown,
                                                    c("NASDAQ", "NYSE", "NYSEMKT", "Arca"),
                                                    c("Open", "Close"))
myPerExchangeForTOD$plot$save(paste(getwd(), '\\output\\breakdowncharts\\perExchangeForOpenClose.html', sep=""), standalone = TRUE)
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
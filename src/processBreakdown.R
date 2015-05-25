library(rCharts)
library(ggplot2)
library(knitr)
library(base64enc)

if(TRUE) {
  aFileName<- paste(getwd(), "\\data\\breakdown5aa0ce4b018e0067.csv", sep ="")
  myBreakdown <- read.csv(aFileName, header = TRUE)
  myBreakdown$Date = as.Date(sapply(myBreakdown$Date, toString),"%Y%m%d")
  myBreakdown$DaysSince = myBreakdown$Date - as.Date("2014-01-01")
  
  myNASDAQSymbols <- c("GOOG", "TSLA", "GT", "STLD", "WFM", "NDAQ")
  myNYSESymbols <-  c("AMD", "CVS", "GE", "WSM", "LTM", "LUV", "BHP", "BRKA", "T", "FE", "BAC", "XOM")
  myArcaSymbols <- c("TBF", "SLV")
  myNYSEMKTSymbols <- c("ONP", "LIQT", "CCF", "ONVO")
  mySymbols = c(myNASDAQSymbols, myArcaSymbols, myNYSESymbols, myNYSEMKTSymbols)
  
  inferListedExchangeFromSymbol <- function(aSymbol) {
    if(aSymbol %in% myNASDAQSymbols){
      return("NASDAQ")
    } else if(aSymbol %in% myNYSESymbols){
      return("NYSE")
    } else if(aSymbol %in% myArcaSymbols){
      return("Arca")
    } else if(aSymbol %in% myNYSEMKTSymbols){
      return("NYSEMKT")
    } else {
      return("UNKNOWN")
    }
  }
  myBreakdown$ListedExchange <- sapply(myBreakdown$Symbol, function(x) inferListedExchangeFromSymbol(x))
}

createAggregateMultiBar <- function(aBreakdown, aTickSizeString, aTickSizeWord){
  myVolumesByExchange = aggregate(Volume ~ Exchange, data = aBreakdown, FUN= function(x) sum(as.numeric(x)))
  myVolumesByExchange$Common = sapply(myVolumesByExchange$Exchange, function(x) "Aggregate")
  n1 <- nPlot(Volume ~ Common, group = "Exchange", data = myVolumesByExchange, type = "multiBarChart")
  n1$yAxis(showMaxMin = FALSE)
  n1$yAxis(tickFormat= paste("#!function(d) {return d/", aTickSizeString, ";}!#", sep=""))
  n1$yAxis( axisLabel = paste("Volume, in", aTickSizeWord), width = 40)
  return(list("collapsed" = myVolumesByExchange, "plot" = n1))
}
graphExchangeShareByTOD <- function(aBreakdown, myTOD){
  myTODBreakdown = aBreakdown[aBreakdown$TOD == myTOD,]
  myTODCollapse = aggregate(Volume ~ Date + Exchange, data=myTODBreakdown, FUN=sum)
  png(paste(myTOD,"2014.png", sep=""), width=1000, height=500, units="px", res=100)
  g <- ggplot(myTODCollapse, aes(x=Date,y=Volume,group=Exchange,fill=Exchange))
  g<-g + geom_area(position = "fill") + ylab("Proportion of Market") + xlab("2014") + ggtitle(paste("Exchange Market Shares for",myTOD, "Trades"))
  g
  dev.off()
}
createBySymbolForTOD <- function(myBreakdown, mySymbols, aTODs){
  myBreakdownSimilar = myBreakdown[(myBreakdown$Symbol %in% mySymbols & myBreakdown$TOD %in% aTODs),]
  mySummary = aggregate(Volume ~ Exchange + Symbol, data = myBreakdownSimilar,
                        FUN= function(x) sum(as.numeric(x)))
  myDoneExchanges = unique(mySummary$Exchange)
  myDoneSymbols = unique(mySummary$Symbol)
  for(myExchange in myDoneExchanges){
    for(mySymbol in myDoneSymbols){
      myIsRowMatch <- mySummary$Symbol == mySymbol & mySummary$Exchange == myExchange
      if(!any(mySummary$Symbol == mySymbol & mySummary$Exchange == myExchange)) {
        mySummary = rbind(mySummary, list(myExchange, mySymbol, 0))
      }
    }
  }
  mySummary$Proportion = mapply(function(x,y) x / sum(mySummary$Volume[mySummary$Symbol == y]),
                                mySummary$Volume, mySummary$Symbol)
  mySummary = mySummary[with(mySummary, order(Symbol, Exchange)), ]
  n1 <- nPlot(Proportion ~ Symbol, group = "Exchange", data = mySummary, type = "multiBarChart")
  n1$yAxis(showMaxMin = FALSE)
  n1$xAxis(rotateLabels = 34)
  n1$chart(reduceXTicks = FALSE)
  n1
}
createByListedExchangeForTOD <- function(myBreakdown, myListedExchanges, aTODs){
  myBreakdownSimilar = myBreakdown[(myBreakdown$ListedExchange %in% myListedExchanges & myBreakdown$TOD %in% aTODs),]
  mySummary = aggregate(Volume ~ Exchange + ListedExchange, data = myBreakdownSimilar,
                        FUN= function(x) sum(as.numeric(x)))
  myDoneExchanges = unique(mySummary$Exchange)
  myDoneListedExchanges = unique(mySummary$ListedExchange)
  for(myExchange in myDoneExchanges){
    for(myListedExchange in myDoneListedExchanges){
      myIsRowMatch <- mySummary$ListedExchange == myListedExchange & mySummary$Exchange == myExchange
      if(!any(mySummary$ListedExchange == myListedExchange & mySummary$Exchange == myExchange)) {
        mySummary = rbind(mySummary, list(myExchange, myListedExchange, 0))
      }
    }
  }
  mySummary$Proportion = mapply(function(x,y) x / sum(mySummary$Volume[mySummary$ListedExchange == y]),
                                mySummary$Volume, mySummary$ListedExchange)
  mySummary = mySummary[with(mySummary, order(ListedExchange, Exchange)), ]
  n1 <- nPlot(Proportion ~ ListedExchange, group = "Exchange", data = mySummary, type = "multiBarChart")
  n1$yAxis(showMaxMin = FALSE)
  n1
}
if(TRUE){
  opts_chunk$set(comment = NA, results = "asis", comment = NA, tidy = F)
  myResults <- createAggregateMultiBar(myBreakdown, "1000000000", "billions")
  myResults$plot$save('AllStocks.html', standalone = TRUE)
  myResults$collapsed$Proportion = round(100*myResults$collapsed$Volume / sum(myResults$collapsed$Volume), 4)
  
  
  p = createMultiBarProportioned(myBreakdown, mySymbols, "Open", c("NASDAQ", "NYSEMKT", "Arca", "NYSE"))
  p  
  
  p <- createBySymbolForTOD(myBreakdown, myNYSESymbols, c("Regular"))
  p$save('nyseReg.html', standalone = TRUE)
  
  p <- createByListedExchangeForTOD(myBreakdown, c("NASDAQ", "NYSEMKT", "Arca", "NYSE"), c("After-Hours"))
  p$save('listedExchangeAfter.html', standalone = TRUE)
  
  capture.output()
  
  p <- createMultiBar(myBreakdown, myNASDAQSymbols,
                      "1000000", "millions")
  p
  p$show('iframesrc', cdn = TRUE)
  p <- createMultiBar(myBreakdown, c("GOOG"),"1000000", "millions")
  p$show('iframesrc', cdn = TRUE)
  p <- createMultiBar(myBreakdown, c("BRKA"),"1000", "thousands")
  p$show('iframesrc', cdn = TRUE)
  
  # Trades by Exchange over 2014
  graphExchangeShareByTOD(myBreakdown, "Open")
  graphExchangeShareByTOD(myBreakdown, "Close")
  graphExchangeShareByTOD(myBreakdown, "Regular")
  graphExchangeShareByTOD(myBreakdown, "After-Hours")
}

myVolumesByExchangeTOD = aggregate(Volume ~ Exchange + TOD, data = myBreakdown, FUN= function(x) sum(as.numeric(x)))
myVolumesByTOD = aggregate(Volume ~ TOD, data = myBreakdown, FUN= function(x) sum(as.numeric(x)))
myPropVolsByExchTOD <- myVolumesByExchangeTOD
myPropVolsByExchTOD$Proportion <- mapply(function(x, y) x / myVolumesByTOD$Volume[myVolumesByTOD$TOD == y], myPropVolsByExchTOD$Volume, myPropVolsByExchTOD$TOD)
n1 <- nPlot(Proportion ~ TOD, group = "Exchange", data = myPropVolsByExchTOD, type = "multiBarChart")
n1
n1$show('iframesrc', cdn = TRUE)


myExchangeProps <- aggregate(Volume ~ Exchange, data=myBreakdown, function(x) sum(as.numeric(x)))
myExchangeProps$Proportion = myExchangeProps$Volume / sum(myExchangeProps$Volume)

myDateTODExchange <- aggregate(Volume ~ Date + TOD + Exchange, data=myBreakdown, function(x) sum(as.numeric(x)))
myDateTOD <- aggregate(Volume ~ Date + TOD, data=myBreakdown, function(x) sum(as.numeric(x)))
myDateTODExchange$Proportion <- mapply(function(x,y,z)
  x / myDateTOD$Volume[myDateTOD$Date == y & myDateTOD$TOD == z],
                                       myDateTODExchange$Volume,
                                       myDateTODExchange$Date,
                                       myDateTODExchange$TOD)
write.csv(myDateTODExchange, file = "taskTwo.csv")


"GOOG TSLA GT STLD WFM NDAQ AMD TBF SLV CVS GE WSM LTM LUV BHP BRKA T FE BAC XOM ONP LIQT CCF ONVO"

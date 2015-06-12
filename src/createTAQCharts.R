createAggregateMultiBar <- function(aBreakdown, aTickSizeString, aTickSizeWord){
  myVolumesByExchange = aggregate(Volume ~ Exchange, data = aBreakdown, FUN= function(x) sum(as.numeric(x)))
  myVolumesByExchange$Common = sapply(myVolumesByExchange$Exchange, function(x) "Aggregate")
  n1 <- nPlot(Volume ~ Common, group = "Exchange", data = myVolumesByExchange, type = "multiBarChart")
  n1$yAxis(showMaxMin = FALSE, axisLabel = paste("Volume, in", aTickSizeWord), width = 40)
  n1$yAxis(tickFormat= paste("#!function(d) {return d/", aTickSizeString, ";}!#", sep=""))
  n1$chart(stacked = TRUE)
  return(list("collapsed" = myVolumesByExchange, "plot" = n1))
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
  n1$chart(reduceXTicks = FALSE, stacked = TRUE)
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
  n1$chart(stacked = TRUE)
  n1
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
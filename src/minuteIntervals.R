library(ggplot2)
convertToProportion <- function(aRow){
  myTotalVolume <- sum(aRow)
  theRow <- aRow / myTotalVolume
  return(theRow)
}

getHistogramOfExchange <- function(aExchange) {
  myExchangeVolumes = mySeries[aExchange][mySeries[aExchange] != 0]
  hist(myExchangeVolumes, breaks=100)
}

plotPropVsVolume <- function(aProps, aExchange) {
  myDF <- data.frame(proportion = aProps[,aExchange],
                     tradeCount = aProps$TradeCount,
                     volume = aProps$Volume)
  myDF <- myDF[myDF$proportion != 0,]
  thePlot <- ggplot(myDF, aes(x=log(volume), y=proportion)) + geom_point(alpha = .2) +
    theme_bw() + ggtitle(aExchange) + xlim(c(0,15)) + ylim(c(0,1)) + 
    labs(x="Trade Group Volume (Log)",
         y=paste(aExchange, "'s Share of Trade Group", sep=""))
  return(thePlot)
}

translateToPropsDF <- function(aSeries){
  myProps <- data.frame(t(apply(aSeries[,-(1:3)], 1, function(x) convertToProportion(x))))
  myProps$TimeInterval = aSeries$TimeInterval
  myColNames <- names(myProps)
  myExchangeColNames <- myColNames[myColNames != "TimeInterval"]
  myExchangeOrder <- order(myExchangeColNames)
  
  myProps <- subset(myProps, select=c(TimeInterval, myExchangeOrder))
  myProps$TradeCount = aSeries$TradeCount
  myProps$Volume = aSeries$Volume
  return(myProps)
}

mySymbol = "BAC"
aFilePath = paste("\\data\\profileSeries", mySymbol, ".csv", sep="")
aFileName<- paste(getwd(), aFilePath, sep ="")
mySeries <- read.csv(aFileName, header = TRUE)
myProps <- translateToPropsDF(mySeries)

i=1
myExchange = myExchangeColNames[i]
myPlot <- plotPropVsVolume(myProps, myExchange)
ggsave(filename = paste(myExchange,mySymbol, "PropVolumePlot.jpg", sep=""), plot=myPlot)
i = i + 1

write.csv(myProps, file = paste(mySymbol, "VolumeShares.csv", sep=""))

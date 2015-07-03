library(corrplot)

rename <- function(x){
  myPath = "/output/anims/"
  if (x < 10) {
    return(name <- paste(myPath, '000',i,'plot.png',sep=''))
  }
  if (x < 100 && i >= 10) {
    return(name <- paste(myPath, '00',i,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste(myPath, '0', i,'plot.png', sep=''))
  }
}
autocorrOfShares <- function(aExchange, aTitle="None"){
  myProps = myFilteredSeries[, aExchange]
  myVolumes = myFilteredSeries[,"totalVolume"]
  #myTimeSeries = myProps * myVolumes
  myTimeSeries = myProps
  #plot(myTimeSeries)
  #hist(myVolumes, breaks=100)
  myIsBigger = myVolumes > 1000
  #plot(myTimeSeries[myIsBigger])
  #acf(myTimeSeries, na.action = na.pass, 30)
  myACF = acf(myTimeSeries[myIsBigger], na.action = na.pass,30)
  png(filename = paste(getwd(), "/output/correlation/acf", mySymbol, myInterval, aExchange, ".png", sep=""))
  plot(myACF,
       main = paste(aExchange, "'s ", mySymbol,
                    " Volume Share Autocorrelation", sep=""),
       ylab = "Correlation", xlab = paste(myInterval, "Second Lags"))
  dev.off()
}
correlationBetweenExchanges <- function(aSeries){
  #aSeries=myFilteredSeries
  #aSeries[is.na(aSeries)] = 0
  aSeries=aSeries[aSeries$totalVolume > 100,]
  M <- cor(aSeries[,myExchangeColumns])
  M[is.na(M)]=0
  col1 <- colorRampPalette(c("#7F0000",
                             "red", "White", "blue",
                             "#00007F"))
  png(filename = paste(getwd(), "/output/correlation/correlationMatrix", mySymbol, myInterval, ".png", sep=""))
  corrplot(M, method = "number", bg = "white", col = col1(100))
  dev.off()
}
filterSeries <- function(aSeries){
  myStartTime <- as.POSIXct("10:00:00", format="%H:%M:%S")
  myEndTime <- as.POSIXct("15:00:00", format="%H:%M:%S")
  myStartTimeOfDay = as.POSIXct(strftime(aSeries$startTime, format="%H:%M:%S"), format="%H:%M:%S")
  myEndTimeOfDay = as.POSIXct(strftime(aSeries$endTime, format="%H:%M:%S"), format="%H:%M:%S")
  myIsNormalHours <- myStartTimeOfDay > myStartTime & myEndTimeOfDay < myEndTime
  myFilteredSeries <- aSeries[myIsNormalHours,]
  
}


mySymbol = "BAC"
myInterval = 1
aFileName<- paste(getwd(), "\\output\\timeIntervals\\exchangePropsOneWeek",myInterval, mySymbol, ".csv", sep ="")
mySeries <- read.csv(aFileName, header = TRUE, stringsAsFactors = FALSE)
myFilteredSeries <- filterSeries(mySeries)

myExchangeColumns <- c("NSX", "CBSX", "NASDAQ.PSX", "CHX",
                       "BATS.BYX", "BATS.EDGA", "NASDAQ.BX",
                       "NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
#myExchangeColumns <- myExchangeColumns[!(myExchangeColumns %in% c("NYSE", "CBSX"))]
#myProps = myFilteredSeries[, myExchanges]
#if(length(myExchanges) > 1){
#  myProps <- rowSums(myProps)
#}

correlationBetweenExchanges(myFilteredSeries)
myTakerMakerExchanges <- c("BATS.BYX", "BATS.EDGA", "NASDAQ.BX")
myMakerTakerExchanges <- c("NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")

for(i in 1:length(myExchangeColumns)){
  autocorrOfShares(myExchangeColumns[i])
}

autocorrOfShares(myTakerMakerExchanges, "Taker/Maker Group")
autocorrOfShares(myMakerTakerExchanges, "Maker/Taker Group")
autocorrOfShares(c("NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ"), "Maker/Taker Group")

myFrameCount = min(dim(myFilteredSeries)[1], 300)
for(i in 1:myFrameCount){#dim(myFilteredSeries)[1]){
  name <- rename(i)
  png(paste(getwd(), name, sep=""))
  myEndTime <- myFilteredSeries[i, 'endTime']
  myExchangeProps= as.numeric(myFilteredSeries[i, myExchangeColumns])
  myExchangeProps[is.na(myExchangeProps)] = 0
  barplot(myExchangeProps, names.arg = myExchangeColumns, las=2, ylim=c(0,1), main = myEndTime)
  dev.off()
  print(i/myFrameCount)
}
myConvertPath = '"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe"'
myPNGPath = paste(getwd(), "/output/anims/*.png", sep="")
setwd(paste(getwd(), "/output/anims/", sep=""))
my_command <- paste(myConvertPath, " *.png -delay 3 -loop 0 animation", mySymbol, myInterval, ".gif", sep="")
system(my_command)
unlink('*.png')
setwd(paste(getwd(), "/../../", sep=""))

"plotPropVsVolume <- function(aProps, aExchange) {
  myDF <- data.frame(proportion = aProps[,aExchange],
                     tradeCount = aProps$TradeCount,
                     volume = aProps$Volume)
  myDF <- myDF[myDF$proportion != 0,]
  thePlot <- ggplot(myDF, aes(x=log(volume), y=proportion)) + geom_point(alpha = .2) +
    theme_bw() + ggtitle(aExchange) + xlim(c(0,15)) + ylim(c(0,1)) + 
    labs(x="Trade Group Volume (Log)",
         y=paste(aExchange, "'s Share of Trade Group", sep=""))
  return(thePlot)
}"
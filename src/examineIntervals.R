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

mySymbol = "BAC"
myInterval = 5
aFileName<- paste(getwd(), "\\output\\timeIntervals\\exchangeProps",myInterval, mySymbol, ".csv", sep ="")
mySeries <- read.csv(aFileName, header = TRUE, stringsAsFactors = FALSE)

myIsNormalHours <- mySeries$startTime > as.POSIXct("2014-03-13 09:00:00") & 
  mySeries$endTime < as.POSIXct("2014-03-13 14:00:00")
myFilteredSeries <- mySeries[myIsNormalHours,]
myExchangeColumns <- c("NASDAQ.BX", "BATS.BYX", "BATS.EDGA",
                       "NSX", "CBSX", "NASDAQ.PSX", "CHX",
                       "BATS.EDGX", "NYSE.Arca", "BATS.BZX", "NYSE", "NASDAQ")
myExchangeColumns <- myExchangeColumns[!(myExchangeColumns %in% c("NYSE", "CBSX"))]
for(i in 1:dim(myFilteredSeries)[1]){
  name <- rename(i)
  png(paste(getwd(), name, sep=""))
  myEndTime <- myFilteredSeries[i, 'endTime']
  myExchangeProps= as.numeric(myFilteredSeries[i, myExchangeColumns])
  myExchangeProps[is.na(myExchangeProps)] = 0
  barplot(myExchangeProps, names.arg = myExchangeColumns, las=2, ylim=c(0,1), main = myEndTime)
  dev.off()
  print(i/dim(myFilteredSeries)[1])
}
myConvertPath = '"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe"'
myPNGPath = paste(getwd(), "/output/anims/*.png", sep="")
setwd(paste(getwd(), "/output/anims/", sep=""))
my_command <- paste(myConvertPath, " *.png -delay 3 -loop 0 animation", mySymbol, myInterval, ".gif", sep="")
system(my_command)
unlink('*.png')
setwd(paste(getwd(), "/../../", sep=""))


library(corrplot)
a=myFilteredSeries
a[is.na(a)] = 0
a=a[a$totalVolume > 30 & a$totalVolume <14000,]
M <- cor(a[,myExchangeColumns])
corrplot(M, method = "number", bg = "white")



myExch = 'NASDAQ'
myTimeSeries = myFilteredSeries[,myExch]
myVolumes = myFilteredSeries[,"totalVolume"]
myTimeSeries = myTimeSeries * myVolumes
plot(myTimeSeries)
hist(myTimeSeries, breaks=100)
myIsZero = myTimeSeries == 0
myIsOne = myTimeSeries == 1
myIsBigger = myFilteredSeries$totalVolume > 5000
plot(myTimeSeries[!myIsZero & !myIsOne & myIsBigger])
acf(myTimeSeries, na.action = na.pass)
acf(myTimeSeries[!myIsZero], na.action = na.pass)
acf(myTimeSeries[myIsBigger], na.action = na.pass)
acf(myTimeSeries[!myIsOne], na.action = na.pass)
acf(myTimeSeries[!myIsZero & !myIsOne], na.action = na.pass)


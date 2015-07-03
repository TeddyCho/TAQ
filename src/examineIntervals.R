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
autocorrOfShares <- function(aTimeSeries, aSymbol, aExchange, aTimeInterval){
  myACF = acf(aTimeSeries, na.action = na.pass,30)
  png(filename = paste(myOutputFolder, "acf", aExchange, ".png", sep=""))
  plot(myACF,
       main = paste(aExchange, "'s ", aSymbol,
                    " Volume Share Autocorrelation", sep=""),
       ylab = "Correlation", xlab = paste(aTimeInterval, "Second Lags"))
  dev.off()
}
correlationBetweenExchanges <- function(aSeries){
  #aSeries=myFilteredSeries
  #aSeries[is.na(aSeries)] = 0
  aSeries=aSeries[aSeries$totalVolume > 100,]
  M <- cor(aSeries[, myExchangeColumns])
  M[is.na(M)]=0
  col1 <- colorRampPalette(c("#7F0000",
                             "red", "White", "blue",
                             "#00007F"))
  png(filename = paste(myOutputFolder, "/correlationMatrix.png", sep=""))
  corrplot(M, method = "number", bg = "white", col = col1(100))
  dev.off()
}
filterSeries <- function(aSeries){
  myStartTime <- as.POSIXct("10:00:00", format="%H:%M:%S")
  myEndTime <- as.POSIXct("15:00:00", format="%H:%M:%S")
  myStartTimeOfDay = as.POSIXct(strftime(aSeries$startTime, format="%H:%M:%S"), format="%H:%M:%S")
  myEndTimeOfDay = as.POSIXct(strftime(aSeries$endTime, format="%H:%M:%S"), format="%H:%M:%S")
  myIsNormalHours <- myStartTimeOfDay > myStartTime & myEndTimeOfDay < myEndTime
  myIsBigger = aSeries[,"totalVolume"] > 1000
  myFilteredSeries <- aSeries[myIsNormalHours & myIsBigger,]
}

mySymbol = "GOOG"
myInterval = 1
myOutputFolder <- paste(getwd(), "/output/correlation/", mySymbol, "/", 
                        myInterval, "Seconds/", sep="")
dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)

mySeries <- read.csv(paste(getwd(), "\\output\\timeIntervals\\exchangePropsOneWeek",
                           myInterval, mySymbol, ".csv", sep =""),
                     header = TRUE, stringsAsFactors = FALSE)
myFilteredSeries <- filterSeries(mySeries)
if(TRUE){
  myExchangeColumns <- c("NSX", "CBSX", "NASDAQ.PSX", "CHX",
                         "BATS.BYX", "BATS.EDGA", "NASDAQ.BX",
                         "NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
  myTakerMakerExchanges <- c("BATS.BYX", "BATS.EDGA", "NASDAQ.BX")
  myMakerTakerExchanges <- c("NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
  myNonNYSEMakerTakerExchanges <- c("NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")                                            
  myNonExchangeColumns <- c("startTime", "endTime", "totalVolume")
}
if(mySymbol == "GOOG"){
  myExchangeColumns <- myExchangeColumns[myExchangeColumns != "NYSE"]
  myMakerTakerExchanges <- myMakerTakerExchanges[myMakerTakerExchanges != "NYSE"]
}
correlationBetweenExchanges(myFilteredSeries)
for(i in 1:length(myExchangeColumns)){
  myExchange <- myExchangeColumns[i]
  autocorrOfShares(mySeries[,myExchange], mySymbol, myExchange, myInterval)
}
autocorrOfShares(rowSums(mySeries[,myTakerMakerExchanges]), mySymbol, "TakerMaker", myInterval)
autocorrOfShares(rowSums(mySeries[,myMakerTakerExchanges]), mySymbol, "MakerTaker", myInterval)
autocorrOfShares(rowSums(mySeries[,myNonNYSEMakerTakerExchanges]),
                 mySymbol, "NonNYSEMakerTaker", myInterval)


#myVolumes = myFilteredSeries[,"totalVolume"]
#myTimeSeries = myProps * myVolumes
#hist(myVolumes, breaks=100)




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
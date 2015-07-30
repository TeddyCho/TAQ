library(corrplot)

rename <- function(x){
  myPath = "/output/anims/"
  if (x < 10) {
    return(name <- paste(myPath, '000',x,'plot.png',sep=''))
  }
  if (x < 100 && x >= 10) {
    return(name <- paste(myPath, '00',x,'plot.png', sep=''))
  }
  if (x >= 100) {
    return(name <- paste(myPath, '0', x,'plot.png', sep=''))
  }
}
autocorrOfShares <- function(aTimeSeries, aSymbol, aExchange, aTimeInterval, aSuffix){
  myACF = acf(aTimeSeries, na.action = na.pass, 30)
  png(filename = paste(myOutputFolder, "acf", aExchange,  ".png", sep=""))
  plot(myACF,
       main = paste(aExchange, "'s ", aSymbol,
                    " Volume Share Autocorrelation", sep=""),
       ylab = "Correlation", xlab = paste(aTimeInterval, aSuffix, "Lags"))
  dev.off()
}
correlationBetweenExchanges <- function(aSeries,aIntervalType){
  #aSeries=myFilteredSeries
  #aSeries[is.na(aSeries)] = 0
  aSeries=aSeries[aSeries$totalVolume > 100,]
  M <- cor(aSeries[, myExchangeColumns], )
  M[is.na(M)]=0
  col1 <- colorRampPalette(c("#7F0000",
                             "red", "White", "blue",
                             "#00007F"))
  png(filename = paste(myOutputFolder, "/correlationMatrix.png", sep=""))
  par(oma=c(0,0,2,0))
  corrplot(M, method = "number", bg = "white", col = col1(100))
  title(outer=TRUE,adj=0,main = paste("  ", mySymbol, myInterval, aIntervalType, " Intervals")) 
  dev.off()
}
filterSeries <- function(aSeries){
  myStartTime <- as.POSIXct("10:00:00", format="%H:%M:%S")
  myEndTime <- as.POSIXct("15:45:00", format="%H:%M:%S")
  myStartTimeOfDay = as.POSIXct(strftime(aSeries$startTime, format="%H:%M:%S"), format="%H:%M:%S")
  myEndTimeOfDay = as.POSIXct(strftime(aSeries$endTime, format="%H:%M:%S"), format="%H:%M:%S")
  myIsNormalHours <- myStartTimeOfDay > myStartTime & myEndTimeOfDay < myEndTime
  myIsBigger = aSeries[,"totalVolume"] > 1000
  myFilteredSeries <- aSeries[myIsNormalHours & myIsBigger,]
}
createShareGIF <- function(myFilteredSeries, myExchangeColumns){
  myFrameCount = min(dim(myFilteredSeries)[1], 200)
  for(i in 1:myFrameCount){#dim(myFilteredSeries)[1]){
    name <- rename(i)
    png(paste(getwd(), name, sep=""))
    myEndTime <- myFilteredSeries[i, 'endTime']
    myExchangeProps= as.numeric(myFilteredSeries[i, myExchangeColumns])
    myExchangeProps[is.na(myExchangeProps)] = 0
    barplot(myExchangeProps, names.arg = myExchangeColumns, las=2, ylim=c(0,1),
            main = myEndTime)
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
}
mySymbol = "GOOG"
setwd(paste(getwd(), "/Github/TAQ/", sep=""))
myTimeIntervals = c(1, 10, 120, 1800, 3600, 10800, 19800)
for(j in 1:length(myTimeIntervals)){
  myInterval = myTimeIntervals[j]
  myOutputFolder <- paste(getwd(), "/output/correlation/", mySymbol, "/", 
                          myInterval, "Business/", sep="")
  dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)
  
  mySeries <- read.csv(paste(getwd(), "\\output\\businessIntervals\\exchangePropsOneWeek",
                             myInterval, mySymbol, ".csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
  myFilteredSeries <- filterSeries(mySeries)
  
  myDay = strptime(c("2014-03-05 10:00:00"),"%Y-%m-%d", tz="")
  myDaySeries = myFilteredSeries[strptime(myFilteredSeries$endTime, "%Y-%m-%d") == myDay,]
  c=as.numeric(strptime(myDaySeries$endTime, "%Y-%m-%d %H:%M:%S") - 
                 strptime(myDaySeries$startTime, "%Y-%m-%d %H:%M:%S"), units="secs")
  hist(c)
  mav <- function(x,n=5){filter(x,rep(1/n,n), sides=2)}
  myMovingWindow = floor(length(c) / 10)
  m=mav(c, myMovingWindow)
  png(filename = paste(getwd(), "/output/businessIntervalLengths/", strftime(myDay),
                       "-", myInterval, "-", myMovingWindow, mySymbol, ".png", sep=""))
  plot(strptime(myDaySeries$startTime, "%Y-%m-%d %H:%M:%S"), m, type = "l",
       xlab = "Time", 
       ylab = paste("Seconds per Interval", sep=""),
       main = paste(myInterval, "-Trade Intervals on ",myDay, "\n",
                    myMovingWindow, " Interval MA", sep=""))
  dev.off()
  
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
  
  createShareGIF(myFilteredSeries, myExchangeColumns)
  
  correlationBetweenExchanges(myFilteredSeries, "Trade")
  for(i in 1:length(myExchangeColumns)){
    myExchange <- myExchangeColumns[i]
    autocorrOfShares(mySeries[,myExchange], mySymbol, myExchange, myInterval, "BusinessTime")
  }
  autocorrOfShares(rowSums(mySeries[,myTakerMakerExchanges]), mySymbol,
                   "TakerMaker", myInterval, "Trade")
  autocorrOfShares(rowSums(mySeries[,myMakerTakerExchanges]), mySymbol,
                   "MakerTaker", myInterval, "Trade")
  autocorrOfShares(rowSums(mySeries[,myNonNYSEMakerTakerExchanges]),
                   mySymbol, "NonNYSEMakerTaker", myInterval, "Trade")

}
#myVolumes = myFilteredSeries[,"totalVolume"]
#myTimeSeries = myProps * myVolumes
#hist(myVolumes, breaks=100)]

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
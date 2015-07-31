library(corrplot)

rename <- function(x){
  myPath = myOutputFolder
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
                    " Volume Share Autocorrelation \n", myDescription, sep=""),
       ylab = "Correlation", xlab = paste(aTimeInterval, aSuffix, "Lags"))
  dev.off()
}
correlationBetweenExchanges <- function(aSeries,aIntervalType, aIsLagged=FALSE){
  #aSeries=myFilteredSeries
  #aSeries[is.na(aSeries)] = 0
  aSeries=aSeries[aSeries$totalVolume > 100,]
  if(aIsLagged){
    aRawProps = aSeries[, myExchangeColumns]
    myUnlaggedProps = aRawProps[-1,]
    myLaggedProps = aRawProps[-dim(aRawProps)[1],]
    M <- cor(myUnlaggedProps, myLaggedProps)
    mySuffix = "Lagged"
  }else{
    M <- cor(aSeries[, myExchangeColumns], )
    mySuffix = "Unlagged"
  }
  M[is.na(M)]=0
  col1 <- colorRampPalette(c("#7F0000",
                             "red", "White", "blue",
                             "#00007F"))
  png(filename = paste(myOutputFolder, "/correlationMatrix", mySuffix, ".png", sep=""))
  par(oma=c(0,0,2,0))
  corrplot(M, method = "number", bg = "white", col = col1(100))
  title(outer=TRUE,adj=0,main = paste("  ", mySymbol, myInterval, aIntervalType, " Intervals:",
                                      mySuffix)) 
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
    png(paste(name, sep=""))
    myEndTime <- myFilteredSeries[i, 'endTime']
    myExchangeProps= as.numeric(myFilteredSeries[i, myExchangeColumns])
    myExchangeProps[is.na(myExchangeProps)] = 0
    barplot(myExchangeProps, names.arg = myExchangeColumns, las=2, ylim=c(-2,10),
            main = myEndTime)
    dev.off()
    print(i/myFrameCount)
  }
  myConvertPath = '"C:\\Program Files\\ImageMagick-6.9.1-Q16\\convert.exe"'
  myPNGPath = paste(myOutputFolder, "*.png", sep="")
  setwd(paste(myOutputFolder, sep=""))
  my_command <- paste(myConvertPath, " *.png -delay 3 -loop 0 animation", mySymbol, myInterval, ".gif", sep="")
  system(my_command)
  unlink('*.png')
  setwd(paste(getwd(), "/../../../../", sep=""))
}
mySymbol = "BAC"
setwd(paste(getwd(), "/Github/TAQ/", sep=""))
myTimeIntervals = c(1, 10, 120, 1800, 3600, 10800, 19800)
myEmptyBehavior = "NaN" #"NaN"
for(j in 1:length(myTimeIntervals)){
  myInterval = myTimeIntervals[j]
  myOutputFolder <- paste(getwd(), "/output/correlation/", mySymbol, "/", 
                          myInterval, "Clock", myEmptyBehavior, "/", sep="")
  dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)
  
  mySeries <- read.csv(paste(getwd(), "\\output\\clockIntervals\\oneWeek", myEmptyBehavior,
                             myInterval, mySymbol, ".csv", sep =""),
                       header = TRUE, stringsAsFactors = FALSE)
  
  myEmptyIntervalCount = sum(mySeries$isEmpty)
  myEmptyIntervalRate = myEmptyIntervalCount / dim(mySeries)
  
  myDescription = paste(round(100*myEmptyIntervalRate,2), "% of intervals were empty. (",
                        myEmptyIntervalCount, " intervals)",
                        sep="")
  
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
  
  createShareGIF(mySeries, myExchangeColumns)
  
  correlationBetweenExchanges(mySeries, "Clock")
  correlationBetweenExchanges(mySeries, "Clock", TRUE)
  for(i in 1:length(myExchangeColumns)){
    myExchange <- myExchangeColumns[i]
    if(any(!is.nan(mySeries[,myExchange]))){
      autocorrOfShares(mySeries[,myExchange], mySymbol, myExchange, myInterval, "ClockTime")
    }
  }
  autocorrOfShares(rowSums(mySeries[,myTakerMakerExchanges]), mySymbol,
                   "TakerMaker", myInterval, "ClockTime")
  autocorrOfShares(rowSums(mySeries[,myMakerTakerExchanges]), mySymbol,
                   "MakerTaker", myInterval, "ClockTime")
  autocorrOfShares(rowSums(mySeries[,myNonNYSEMakerTakerExchanges]),
                   mySymbol, "NonNYSEMakerTaker", myInterval, "ClockTime")

}
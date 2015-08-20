library(corrplot)
library(dyn)

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
autocorrOfShares <- function(aTimeSeries, aSymbol, aExchange, aTimeInterval, aSuffix, myOutputFolder, myDescription){
  myACF = acf(aTimeSeries, na.action = na.pass, 100)
  png(filename = paste(myOutputFolder, "acf", aExchange,  ".png", sep=""))
  plot(myACF,
       main = paste(aExchange, "'s ", aSymbol,
                    " Volume Share Autocorrelation \n", myDescription, sep=""),
       ylab = "Correlation", xlab = paste(aTimeInterval, aSuffix, "Lags"))
  dev.off()
}
correlationBetweenExchanges <- function(aSeries,aIntervalType, myExchangeColumns, myOutputFolder, myInterval,
                                        aIsLagged=FALSE){
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
createShareGIF <- function(myFilteredSeries, myExchangeColumns, myInterval){
  myFrameCount = min(dim(myFilteredSeries)[1], 200)
  for(i in 1:myFrameCount){#dim(myFilteredSeries)[1]){
    name <- rename(i)
    png(paste(getwd(), name, sep=""))
    myEndTime <- myFilteredSeries[i, 'endTime']
    myExchangeProps= as.numeric(myFilteredSeries[i, myExchangeColumns])
    myExchangeProps[is.na(myExchangeProps)] = 0
    barplot(myExchangeProps, names.arg = myExchangeColumns, las=2, ylim=c(-2,20),
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
orderedSubset <- function(aExhaustiveList, aAvailableList){
  myInAvailable = c()
  for(ex in aExhaustiveList){
    if(ex %in% aAvailableList){
      myInAvailable = c(myInAvailable, ex)
    }
  }
  return(myInAvailable)
}
createAllGraphs <- function(mySymbol, myIntervalStyle, myIntervals, myEmptyBehavior){
  for(j in 1:length(myIntervals)){
    myInterval = myIntervals[j]
    myOutputFolder <- paste(getwd(), "/output/correlation/", mySymbol, "/", 
                            myInterval, myIntervalStyle, myEmptyBehavior, "/", sep="")
    dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)
    
    mySeries <- read.csv(paste(getwd(), "\\output\\", myIntervalStyle, "Intervals\\", mySymbol, "\\",
                               myEmptyBehavior, myInterval, ".csv", sep =""),
                         header = TRUE, stringsAsFactors = FALSE)
    if(myEmptyBehavior == "ThrowOut"){
      mySeries = mySeries[mySeries$isEmpty==0,]
    }
    myCounts <- read.csv(paste(getwd(), "\\output\\", myIntervalStyle, "Intervals\\", mySymbol, "\\",
                               myEmptyBehavior, myInterval, "Counts.csv", sep =""), header = TRUE, 
                         stringsAsFactors = FALSE)
    
    
    myEmptyIntervalCount = myCounts$emptyCount
    myEmptyIntervalRate = myEmptyIntervalCount / myCounts$intervalCount
    
    myDescription = paste(round(100*myEmptyIntervalRate,2), "% of intervals were empty. (",
                          myEmptyIntervalCount, " intervals)",
                          sep="")
    
    if(TRUE){
      myColumns = colnames(mySeries)
      myExchangeColumns <- c("NSX", "CBSX", "NASDAQ.PSX", "CHX",
                             "BATS.BYX", "BATS.EDGA", "NASDAQ.BX",
                             "NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
      myTakerMakerExchanges <- c("BATS.BYX", "BATS.EDGA", "NASDAQ.BX")
      myMakerTakerExchanges <- c("NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
      myNonNYSEMakerTakerExchanges <- c("NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")                                            
      myNonExchangeColumns <- c("startTime", "endTime", "totalVolume")
      
      myExchangeColumns=orderedSubset(myExchangeColumns, myColumns)
      myTakerMakerExchanges=orderedSubset(myTakerMakerExchanges, myColumns)
      myMakerTakerExchanges=orderedSubset(myMakerTakerExchanges, myColumns)
      myNonNYSEMakerTakerExchanges=orderedSubset(myNonNYSEMakerTakerExchanges, myColumns)
    }
    
    createShareGIF(mySeries, myExchangeColumns, myInterval)
    
    correlationBetweenExchanges(mySeries, myIntervalStyle, myExchangeColumns, myOutputFolder, myInterval)
    correlationBetweenExchanges(mySeries, myIntervalStyle, myExchangeColumns, myOutputFolder, myInterval, TRUE)
    for(i in 1:length(myExchangeColumns)){
      myExchange <- myExchangeColumns[i]
      if(any(!is.nan(mySeries[,myExchange]))){
        autocorrOfShares(mySeries[,myExchange], mySymbol, myExchange, myInterval, myIntervalStyle, myOutputFolder,
                         myDescription)
      }
    }
    autocorrOfShares(rowSums(mySeries[,myTakerMakerExchanges]), mySymbol,
                     "TakerMaker", myInterval, myIntervalStyle, myOutputFolder, myDescription)
    autocorrOfShares(rowSums(mySeries[,myMakerTakerExchanges]), mySymbol,
                     "MakerTaker", myInterval, myIntervalStyle, myOutputFolder, myDescription)
    autocorrOfShares(rowSums(mySeries[,myNonNYSEMakerTakerExchanges]),
                     mySymbol, "NonNYSEMakerTaker", myInterval, myIntervalStyle, myOutputFolder, myDescription)
    
  }
}

replaceWithBinary <- function(mySeries, myExchangeColumns){
  for(c in myExchangeColumns){
    myIsZeroProportion = mySeries[,c] == -1 & !is.na(mySeries[,c])
    myIsNonZeroProportion = mySeries[,c] != -1 & !is.na(mySeries[,c])
    
    mySeries[myIsZeroProportion,c] = 0
    mySeries[myIsNonZeroProportion,c] = 1
  }
  return(mySeries)
}
runRegressions <- function(mySymbol, myIntervalStyle, myIntervals, myEmptyBehavior){
  myResults = data.frame()
  for(j in 1:length(myIntervals)){
    myInterval = myIntervals[j]
    myOutputFolder <- paste(getwd(), "/output/correlation/", mySymbol, "/", 
                            myInterval, myIntervalStyle, myEmptyBehavior, "/", sep="")
    dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)
    
    mySeries <- read.csv(paste(getwd(), "\\output\\", myIntervalStyle, "Intervals\\", mySymbol, "\\",
                               myEmptyBehavior, myInterval, ".csv", sep =""),
                         header = TRUE, stringsAsFactors = FALSE)
    if(myEmptyBehavior == "ThrowOut"){
      mySeries = mySeries[mySeries$isEmpty==0,]
    }
    if(TRUE){
      myColumns = colnames(mySeries)
      myExchangeColumns <- c("NSX", "CBSX", "NASDAQ.PSX", "CHX",
                             "BATS.BYX", "BATS.EDGA", "NASDAQ.BX",
                             "NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
      myTakerMakerExchanges <- c("BATS.BYX", "BATS.EDGA", "NASDAQ.BX")
      myMakerTakerExchanges <- c("NYSE", "NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")
      myNonNYSEMakerTakerExchanges <- c("NYSE.Arca", "BATS.BZX", "BATS.EDGX", "NASDAQ")                                            
      myNonExchangeColumns <- c("startTime", "endTime", "totalVolume", "isEmpty")
      
      myExchangeColumns=orderedSubset(myExchangeColumns, myColumns)
      myTakerMakerExchanges=orderedSubset(myTakerMakerExchanges, myColumns)
      myMakerTakerExchanges=orderedSubset(myMakerTakerExchanges, myColumns)
      myNonNYSEMakerTakerExchanges=orderedSubset(myNonNYSEMakerTakerExchanges, myColumns)
    }
    
    myBSeries = replaceWithBinary(mySeries, myExchangeColumns)
    myVolumes = myBSeries$totalVolume
    
    for(exc in myExchangeColumns){
      y = myBSeries[,exc]
      y=ts(y)
      
      fit <- dyn$lm(y ~ lag(y,-1) + lag(y,-2) + lag(y,-3) + lag(y,-4) + lag(y,-5) + myVolumes, na.action = na.omit)
      
      myData = data.frame(y, myVolumes)
      fit = dyn$glm(y ~ lag(y,-1) + lag(y,-2) + lag(y,-3) + lag(y,-4) + lag(y,-5) + myVolumes, 
                    family = binomial(link = "probit"), na.action=na.omit)
      myCoeffs<-summary(fit)$coefficients[,1]
      myPs<-summary(fit)$coefficients[,4]
      
      myNewRow <- data.frame(symbol=mySymbol, interval=myInterval,exchange = exc,Intercept=myCoeffs[1],
                             Lag1=myCoeffs[2],Lag2=myCoeffs[3],Lag3=myCoeffs[4],Lag4=myCoeffs[5],
                             Lag5=myCoeffs[6],Volume=myCoeffs[7],
                             P0=myPs[1], P1=myPs[2],P2=myPs[3],P3=myPs[4],P4=myPs[5],P5=myPs[6],PVolume=myPs[7])
      myResults <- rbind(myResults, myNewRow)
    }
  }
  
  myOutputFolder <- paste(getwd(), "/output/regression/", mySymbol, "/", 
                          myIntervalStyle, myEmptyBehavior, "/", sep="")
  dir.create(myOutputFolder, showWarnings=FALSE, recursive=TRUE)
  
  write.csv(myResults, file = paste(myOutputFolder,"regression.csv", sep=""))
  print(paste(myOutputFolder,"regression.csv", sep=""))
  return(myResults)
}

mySymbols = c("AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY")

setwd('C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\TAQ')

for(mySymbol in mySymbols){
  myTimeIntervals = c(1, 30, 60, 300, 600, 3600)
  myBusinessIntervals = c(5,10,20)
  
  myRegressionResults <- runRegressions(mySymbol, "clock" , myTimeIntervals, "NaN")
  #myRegressionResults <- runRegressions(mySymbol, "business" , myBusinessIntervals, "NaN")
  
  #createAllGraphs(mySymbol, "clock", myTimeIntervals, "NaN")
  #createAllGraphs(mySymbol, "clock", myTimeIntervals, "ThrowOut")
  #createAllGraphs(mySymbol, "business", myBusinessIntervals, "NaN")
    
}

myIntervalStyle = "clock"
myIntervals = myTimeIntervals
myEmptyBehavior = "NaN"
library(rCharts)
library(ggplot2)
library(knitr)
library(xtable)
library(reshape)
library(base64enc)

readData <- function(myFileName){
  myDirectory = "C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Paul Notes\\daily marketshare\\"
  myFilePath = paste(myDirectory, myFileName, ".csv", sep="")
  myShares <- read.csv(myFilePath, header = TRUE)
  return(myShares)
}
formatData <- function(myShares){
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "DATE", "X_1", "O")
  exchange = c("NYSE MKT","NASDAQ BX","NSX", "FINRA", "ISE", "BATS EDGA","BATS EDGX","CHX",
               "NYSE","NASDAQ T","NYSE Arca","Consolidated Tape System","NASDAQ TQ", "NASDAQ Q", 
               "CBSX", "NASDAQ PSX", "BATS BYX", "BATS BZX", "DATE", "UNKNOWN_X_1", "UNKNOWN_O")
  myColMap = data.frame(code, exchange)
  
  myIndexOfColName <- match(colnames(myShares), myColMap[,1])
  colnames(myShares) <- myColMap[myIndexOfColName,2]
  myShares[is.na(myShares)] = 0
  
  myTable <- melt(myShares, id=c("DATE"))
  colnames(myTable) <- c("DATE", "EXCHANGE", "SHARE")
  
  myTable$DATE <- as.Date(as.character(myTable$DATE), format("%Y%m%d"))
  myTable <- myTable[myTable$DATE > as.Date("20040101", format("%Y%m%d")) & 
                       myTable$DATE < as.Date("20100101", format("%Y%m%d")),]
  return(myTable)
}
plotData <- function(myTable, myFileName, myChartStyle){
  thePlot <- nPlot(SHARE ~ DATE, data = myTable,
                         group = "EXCHANGE", type = myChartStyle)
  
  thePlot$yAxis(axisLabel = paste("Share"), showMaxMin = FALSE, width = 40)
  thePlot$xAxis(axisLabel = paste("Date"), tickFormat =   "#!
                      function(d) {return d3.time.format('%m-%d-%y')(new Date(d*1000*3600*24));}
                      !#",
                      rotateLabels = -45,axisLabel = paste("Date"), 
                      showMaxMin = FALSE, width = 40)
  thePlot$set(width = 1200, height = 800)
  
  thePlot$set(title = paste("Exchange Shares (1993-2014)", sep=""))
  thePlot$save(paste('C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\TAQ\\', 
                     'output\\exchangeShares\\', myFileName, '.html', sep=""),
               standalone = TRUE)
  return(thePlot)
}


myFileName = 'daily_ms_volume'
myShares <- readData(myFileName)
myTable <- formatData(myShares)
thePlot <- plotData(myTable, myFileName, "lineChart")
print(myFileName)
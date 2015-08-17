require(rCharts)
to_jsdate <- function(date_){
  val = as.POSIXct(date_,origin="1970-01-01")
  as.numeric(val)
}
mySymbols = c("AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY")

for(sym in mySymbols){
  mySeries <- read.csv(paste(getwd(), "\\Github\\TAQ\\output\\2014SymbolShares\\", sym,
                             ".csv", sep =""),
                       header = FALSE, stringsAsFactors = FALSE)
  colnames(mySeries) <- c("DateTime", "Exchange", "Proportion")
  mySeries$DateTime<-as.POSIXct(mySeries$DateTime,format="%Y-%m-%d %H:%M:%S")
  mySeries = transform(mySeries, jsDate = to_jsdate(DateTime))
  mySeries$ExCon = paste(mySeries$Exchange, mySeries$Condition)
  
  n1 <- nPlot(Proportion ~ jsDate, data = mySeries, group = "Exchange", type = "lineChart")
  n1$yAxis(axisLabel = paste("Proportion"), showMaxMin = FALSE, width = 40)
  n1$xAxis(axisLabel = paste("DateTime"),tickFormat =   "#!
                      function(d) {return d3.time.format('%d-%m-%Y')(new Date(d*1000))}
                      !#",
                      rotateLabels = -45, showMaxMin = FALSE, width = 60)
  n1$set(width = 1200, height = 800)
  n1$save(paste(getwd(), "\\Github\\TAQ\\output\\2014SymbolShares\\", sym,
                ".html", sep =""), standalone = TRUE)
n1
}

mySeries[which(mySeries$Exchange=="BATS BYX"),]

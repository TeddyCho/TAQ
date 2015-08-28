library(ggplot2)
myIntervalStyle = "clock"
mySymbol = "AMD"
myEmptyBehavior = "NaN"
myInterval = 60

mySymbols = c("AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY")

mySymbols = c("AMD", "BAC", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY")
mySymbols=c("BRKA", "BRKB")
for(mySymbol in mySymbols){
  myTimeIntervals = c(1, 30, 60, 300, 600, 3600)
  for(myInterval in myTimeIntervals){
    setwd('C:\\Users\\tcho\\Dropbox\\Project - Platform Competition\\Code\\TAQ')
    
    myFolder = paste(getwd(), "/output/regression/", mySymbol, "/",
          myIntervalStyle, myEmptyBehavior, "/",sep="")
    
    mySeries <- read.csv(paste(myFolder,"regression", ".csv", sep=""),
                         header = TRUE, stringsAsFactors = FALSE)
    
    myFilterSeries = mySeries[which(mySeries$interval==myInterval &
                                      !(mySeries$exchange %in% c("CHX", "NSX", "CBSX"))),]
    myNumberOfExchanges = length(myFilterSeries$Intercept)
    
    myCoeffs = rbind(cbind(rep(0, myNumberOfExchanges), myFilterSeries$Intercept, myFilterSeries$exchange),
                     cbind(rep(1, myNumberOfExchanges), myFilterSeries$Lag1, myFilterSeries$exchange),
                     cbind(rep(2, myNumberOfExchanges), myFilterSeries$Lag2, myFilterSeries$exchange),
                     cbind(rep(3, myNumberOfExchanges), myFilterSeries$Lag3, myFilterSeries$exchange),
                     cbind(rep(4, myNumberOfExchanges), myFilterSeries$Lag4, myFilterSeries$exchange),
                     cbind(rep(5, myNumberOfExchanges), myFilterSeries$Lag5, myFilterSeries$exchange),
                     cbind(rep(6, myNumberOfExchanges), myFilterSeries$Volume, myFilterSeries$exchange))
    myDF <- data.frame(myCoeffs)
    myDF <- na.omit(myDF)
    myDF$X2 <- as.numeric(as.character(myDF$X2))
    
    #png(paste(myFolder, myInterval, "coeffs.png", sep=""))
    #plot(myDF, xlab = "Lagged Variable", ylab="Coefficient",
    #     main=paste(mySymbol, " ", myIntervalStyle, " ", myInterval, sep=""),
    #     col=myDF$X3)
    ggplot(data=myDF, aes(x=X1, y=X2, color=X3)) +
      geom_point(shape=1) 
    ggsave(file=paste(myFolder, myInterval, "coeffs.png", sep=""))
    #abline(h=0, col="red")
    #dev.off()
  }
}
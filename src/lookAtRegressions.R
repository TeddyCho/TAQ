myIntervalStyle = "clock"
mySymbol = "BRKA"
myEmptyBehavior = "NaN"
myInterval = 60

myFolder = paste(getwd(), "/output/regression/", mySymbol, "/",
      myIntervalStyle, myEmptyBehavior, "/",sep="")

mySeries <- read.csv(paste(myFolder,"regression", ".csv", sep=""),
                     header = TRUE, stringsAsFactors = FALSE)

myFilterSeries = mySeries[which(mySeries$interval==myInterval &
                                  !(mySeries$exchange %in% c("CHX", "NSX", "CBSX"))),]
myNumberOfExchanges = length(myFilterSeries$Intercept)

myCoeffs = rbind(cbind(rep(0, myNumberOfExchanges), myFilterSeries$Intercept),
                 cbind(rep(1, myNumberOfExchanges), myFilterSeries$Lag1),
                 cbind(rep(2, myNumberOfExchanges), myFilterSeries$Lag2),
                 cbind(rep(3, myNumberOfExchanges), myFilterSeries$Lag3),
                 cbind(rep(4, myNumberOfExchanges), myFilterSeries$Lag4),
                 cbind(rep(5, myNumberOfExchanges), myFilterSeries$Lag5))


png(paste(myFolder, myInterval, "coeffs.png", sep=""))
plot(myCoeffs, xlab = "Lagged Variable", ylab="Coefficient", main=paste(mySymbol, " ", myIntervalStyle, " ",
                                                                        myInterval, sep=""))
dev.off()

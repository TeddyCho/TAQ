library(plyr)
library(ggplot2)
library(quantmod)
setwd('/Users/theocho/Dropbox/project - platform competition/')

collectData <- function(){
  myData <- read.csv('./Data/Daily Marketshare/daily_volume2014.csv')
  myData <- myData[,c('DATE', 'SYMBOL', 'EX', 'volume_d')]
  colnames(myData) <- c('date', 'symbol', 'exchange', 'volume')
  
  myData <- combineTQVolumes(myData)
  
  myCombinations <- expand.grid(date=unique(myData$date), symbol=unique(myData$symbol), exchange=unique(myData$exchange))
  myCombinations$volume <- rep(0, dim(myCombinations)[1])
  
  myFilled<-join(myCombinations, myData, by=c("date", "symbol", "exchange"), type="left")
  
  myFilledInVolume <- myFilled[,4] + myFilled[,5]
  myFilledInVolume[is.na(myFilledInVolume)] <- 0
  
  myFilled <- myFilled[,-c(4,5)]
  myFilled$volume <- myFilledInVolume
  
  return(myFilled)
}
combineTQVolumes <- function(aData){
  myTQData <- aData[aData$exchange %in% c('T', 'Q'),]
  myTQCombinedVolumes <- aggregate(list(volume=myTQData$volume), by=list(symbol=myTQData$symbol, date=myTQData$date), function(x) sum(x))
  myTQCombinedVolumes$exchange = 'T/Q'
  aData <- aData[!(aData$exchange %in% c('T', 'Q')), ]
  aData <- rbind(aData, myTQCombinedVolumes)
  return(aData)
}
decodeExchange <- function(aCode){
  code = c("A", "B", "C", "D", "I", "J", "K", "M", "N", "T", "P",
           "S", "T/Q", "Q", "W", "X", "Y", "Z", "date", "X_1", "O")
  exchange = c("NYSE MKT LLC","NASDAQ OMX BX, Inc.","National Stock Exchange, Inc.", "FINRA", "ISE",
               "EDGA Exchange, Inc.","EDGX Exchange, Inc.","Chicago Stock Exchange, Inc.",
               "New York Stock Exchange LLC","The Nasdaq Stock Market LLC","NYSE Arca, Inc.",
               "Consolidated.Tape.System","The Nasdaq Stock Market LLC", "The Nasdaq Stock Market LLC", 
               "CBOE Stock Exchange LLC", "NASDAQ OMX PHLX LLC", "BATS Y-Exchange, Inc.", 
               "BATS Exchange, Inc.", "date", "UNKNOWN_X_1", "UNKNOWN_O")
  
  theExchange <- exchange[code == aCode]
  return(theExchange)
}

getAverageDailyTradingVolume <- function(aData, aSymbol){
  mySymbolVolumes <- aData$volume[aData$symbol == aSymbol]
  theAverageDailyTradingVolume <- sum(mySymbolVolumes) / length(mySymbolVolumes)
  return(theAverageDailyTradingVolume)
}
filterVolumeData <- function(aData){
  aData <- aData[!(aData$exchange %in% c("D")),]
  
  aData <- aData[!grepl('ZZT$', aData$symbol),]
  
  myDateCounts <- aggregate(aData$date, by = list(symbol = aData$symbol), function(x) length(unique(x)))
  myTotalVolumes <- aggregate(aData$volume, by = list(symbol = aData$symbol), function(x) sum(x))
  mySymbolAggregates <- merge(myTotalVolumes, myDateCounts, by='symbol')
  colnames(mySymbolAggregates) <- c('symbol', 'totalVolume', 'dateCount')
  mySymbolAggregates <- transform(mySymbolAggregates, averageDailyVolume = totalVolume / dateCount)
  mySymbolAggregates <- mySymbolAggregates[mySymbolAggregates$averageDailyVolume > 1e8,]
  
  theFilteredData <- aData[aData$symbol %in% mySymbolAggregates$symbol, ]
  return(theFilteredData)
}
enrichVolumeData <- function(aData){
  aData <- aData[with(aData, order(date, symbol, exchange)), ]
  
  mySymbolDateVolumes <- aggregate(aData$volume, by=list(symbol=aData$symbol, date=aData$date), 
                                   function(x) c(volume = sum(x), exchangeCount = length(x)))
  myDayVolumeColumn <- rep(mySymbolDateVolumes$x[,'volume'], mySymbolDateVolumes$x[,'exchangeCount'])
  
  aData$dayVolume = myDayVolumeColumn
  aData$marketShare <- aData$volume/aData$dayVolume
  
  aData$exchange <- sapply(aData$exchange, function(x) decodeExchange(x))
  row.names(aData) <- NULL
  
  return(aData)
}
summarizeOverallSharePerSymbolExchange <- function(aData){
  mySimpleAverageShares <- aggregate(list(share_equalDayWeighted = aData$marketShare), by=list(symbol=aData$symbol, exchange=aData$exchange), 
                                     function(x) mean(x, na.rm=TRUE))
  mySimpleAverageShares <- mySimpleAverageShares[with(mySimpleAverageShares, order(symbol, exchange)), ]
  
  mySymbolExchangeVolumes <- aggregate(list(volume=aData$volume), by=list(symbol=aData$symbol, exchange=aData$exchange), 
                                       function(x) sum(x, na.rm=TRUE))
  mySymbolExchangeVolumes$share_volumeWeighted <- apply(mySymbolExchangeVolumes, 1, function(row) as.numeric(row['volume']) / 
                                                          sum(mySymbolExchangeVolumes$volume[mySymbolExchangeVolumes$symbol==row['symbol']]))
  mySymbolExchangeVolumes <- mySymbolExchangeVolumes[with(mySymbolExchangeVolumes, order(symbol, exchange)), ]
  
  mySharePerSymbolExchange <- merge(mySymbolExchangeVolumes, mySimpleAverageShares, by=c('symbol', 'exchange'))
  
  row.names(mySharePerSymbolExchange) <- NULL
  return(mySharePerSymbolExchange)
}
getPercentilesOfSharesByDay <- function(aData){
  myPercentiles = c(.01, .05, .25, .5, .75, .95, .99)
  myExchangeCodes = unique(myShareData$exchange)
  myPercentileShares = sapply(myExchangeCodes, 
                              function(x) quantile(myShareData$marketShare[myShareData$exchange==x], myPercentiles,
                                                   na.rm=TRUE))
  colnames(myPercentileShares) <- myExchangeCodes
  return(myPercentileShares)
}
getPercentilesOfSharesBySymbol <- function(aData){
  myPercentiles = c(.01, .05, .25, .5, .75, .95, .99)
  myExchangeCodes = sort(unique(myShareData$exchange))
  myPerSymbolAverageShares <- aggregate(aData$marketShare, by=list(symbol=aData$symbol, exchange=aData$exchange),
                                        function(x) mean(x, na.rm=TRUE))
  
  myPercentileShares = sapply(myExchangeCodes, 
                              function(x) quantile(myPerSymbolAverageShares$x[myPerSymbolAverageShares$exchange==x], myPercentiles,
                                                   na.rm=TRUE))
  colnames(myPercentileShares) <- myExchangeCodes
  return(myPercentileShares)
}
dealWithTsAndQs <- function(aData){
  aData <- aData[aData$exchange %in% c('T', 'Q'),]
  myCombinedTQ <- aggregate(aData$marketShare, by=list(date=aData$date, symbol=aData$symbol), function(x) sum(x))
  myTQSharesBySymbol <- aggregate(myCombinedTQ$x, by=list(symbol=myCombinedTQ$symbol), 
                                  function(x) mean(x, na.rm=TRUE))
  quantile(myTQSharesBySymbol$x, c(.01, .05, .25, .5, .75, .95, .99), na.rm=TRUE)
  return(myTQSharesBySymbol)
}
returnOrderedSymbols <- function(myAverageShares, aExchange, aTopOrBottom){
  myAverageShares <- myAverageShares[myAverageShares$exchange==aExchange,]
  myAverageShares <- myAverageShares[with(myAverageShares, order(-share_volumeWeighted)), ]
  
  if(aTopOrBottom == 'top'){
    mySymbols = head(myAverageShares,10)
  } else if(aTopOrBottom == 'bottom') {
    mySymbols = tail(myAverageShares,10)
  } else{
    stop('Invalid aTopOrBottom value.')
  }
  theString <- paste(as.character(mySymbols$symbol), collapse=", ")
  return(theString)
}
getExchangeShareTimeSeries <- function(myShareData){
  myDailyExchangeVolumes<-aggregate(list(volume=myShareData$volume), by=list(exchange=myShareData$exchange, date=myShareData$date),
                                    function(x) sum(x))
  myDailyTotalVolumes <- aggregate(myShareData$volume, by=list(date=myShareData$date), function(x) sum(x))
  myDayRowCount <- aggregate(myDailyExchangeVolumes$volume,by=list(Date=myDailyExchangeVolumes$date),
                             function(x) length(x))
  myDailyVolumeColumn <- rep(myDailyTotalVolumes$x, myDayRowCount$x)
  myDailyExchangeVolumes$dayVolume <- myDailyVolumeColumn
  
  myDailyExchangeVolumes$marketShare <- myDailyExchangeVolumes$volume / myDailyExchangeVolumes$dayVolume
  
  myDailyExchangeVolumes <- myDailyExchangeVolumes[,c('date', 'exchange', 'volume', 'marketShare')]
  
  return(myDailyExchangeVolumes)
}
makePlots <- function(myDailySharePerSymbolExchange, myOverallSharePerSymbolExchange, myDailyExchangeShares, aSuffix='', aFileSuffix=''){
  ggplot(myOverallSharePerSymbolExchange, aes(x=share_volumeWeighted, fill=exchange)) + 
    scale_x_continuous(limits = c(0, .4)) +
    geom_histogram(aes(y=.005*..density..), alpha=.4, binwidth=.005) +
    ggtitle(paste("Histograms of Exchange Market Shares By Symbol", aSuffix, sep='')) +
    facet_wrap(~exchange, ncol=2, scales="free") + 
    xlab('Market Share') +
    ylab('Proportion') +
    guides(fill=FALSE)
  ggsave(file=paste(myOutputFolder, "ExchangeShareHistogramsBySymbol", aFileSuffix, ".png", sep=""),
         width = 20, height=10,dpi=200)
  
  ggplot(myDailyExchangeShares, aes(x=marketShare, fill=exchange)) + 
    scale_x_continuous(limits = c(0, .4)) +
    geom_histogram(aes(y=.003*..density..), alpha=.4, binwidth=.003) +
    ggtitle(paste("Histograms of Exchange Market Shares By Day", aSuffix, sep='')) +
    facet_wrap(~exchange, ncol=2, scales="free") + 
    xlab('Market Share') +
    ylab('Proportion') +
    guides(fill=FALSE)
  ggsave(file=paste(myOutputFolder, "ExchangeShareHistogramsByDay", aFileSuffix, ".png", sep=""),
         width = 20, height=10,dpi=200)
  
  myDailyExchangeShares$date <- as.Date(myDailyExchangeShares$date, '%Y-%m-%d')
  ggplot(myDailyExchangeShares, aes(x=date, y=marketShare, colour=exchange)) + 
    geom_line() +
    ggtitle(paste("Exchange Shares Over 2014", sep='')) + 
    ylab('Market Share') +
    xlab('Time') +
    geom_text(data = myDailyExchangeShares[myDailyExchangeShares$date == "2014-01-02",], size=3, aes(label = exchange), hjust = 1.1, vjust = 0)+
    scale_x_date(limits = as.Date(c('2013-12-01','2015-01-01')))
  ggsave(file=paste(myOutputFolder, "ExchangeShares2014", aFileSuffix, '.png', sep=""), width = 20, height=10,dpi=200)
}
getExchangeSummaryTable <- function(myExchanges, myShareData, myOverallSharePerSymbolExchange, myDailyExchangeShares){
  
  mySharePercentiles <- getPercentilesOfSharesBySymbol(myShareData)
  
  myTopSymbols <- sapply(myExchanges, function(x) returnOrderedSymbols(myOverallSharePerSymbolExchange, x, 'top'))
  myBottomSymbols <- sapply(myExchanges, function(x) returnOrderedSymbols(myOverallSharePerSymbolExchange, x, 'bottom'))
  
  myAverageExchangeShares_EqualDayWeighted <- sapply(myExchanges,
                                                     function(x) mean(myDailyExchangeShares$marketShare[myDailyExchangeShares$exchange==x]))
  myTotalVolume = sum(myDailyExchangeShares$volume)
  myAverageExchangeShares_VolumeWeighted <- sapply(myExchanges,
                                                   function(x) sum(myDailyExchangeShares$volume[myDailyExchangeShares$exchange==x])/myTotalVolume)
  
  myExchangeTable <- data.frame(Exchange = myExchanges, MeanShare_EqualDayWeighted = myAverageExchangeShares_EqualDayWeighted,
                                MeanShare_VolumeWeighted = myAverageExchangeShares_VolumeWeighted)
  myExchangeTable <- cbind(myExchangeTable, t(mySharePercentiles), myTopSymbols, myBottomSymbols)
  return(myExchangeTable)
}
myOutputFolder = '/Users/theocho/Github/TAQ/output/'

myData <- collectData()
myFilteredData <- filterVolumeData(myData)
myDailySharePerSymbolExchange <- enrichVolumeData(myFilteredData)
myExchanges <- sort(unique(myDailySharePerSymbolExchange$exchange))

myOverallSharePerSymbolExchange <- summarizeOverallSharePerSymbolExchange(myDailySharePerSymbolExchange)

myDailyExchangeShares <- getExchangeShareTimeSeries(myDailySharePerSymbolExchange)

myExchangeTable <- getExchangeSummaryTable(myExchanges, myDailySharePerSymbolExchange, myOverallSharePerSymbolExchange, myDailyExchangeShares)

myDailySharePerSymbolExchange <- myDailySharePerSymbolExchange[,c('date', 'symbol', 'exchange', 'volume', 'marketShare')]
write.table(myDailySharePerSymbolExchange, file = '/Users/theocho/Github/TAQ/output/symbolExchangeSharePerDate.csv', sep=',', row.names=FALSE)
write.table(myOverallSharePerSymbolExchange, file = '/Users/theocho/Github/TAQ/output/symbolExchangeSharePerYear.csv', sep=',', row.names=FALSE)
write.table(myDailyExchangeShares, file = '/Users/theocho/Github/TAQ/output/exchangeSharePerDate.csv', sep=',', row.names=FALSE)
write.table(myExchangeTable, file = '/Users/theocho/Github/TAQ/output/exchangeTable.csv', sep=',', row.names=FALSE)




myDailyExchangeShares[myDailyExchangeShares$marketShare == max(myDailyExchangeShares$marketShare),]
mySymbolsData <- data.frame(Symbol=sort(unique(myFilteredData$symbol)))
myTickerMap <- stockSymbols()
mySymbolsData <- join(mySymbolsData, myTickerMap, by='Symbol', type='left')

myNASymbols<-mySymbolsData$Symbol[is.na(mySymbolsData$Name)]
myNames = c()
for(sym in myNASymbols){
  myNames <- rbind(myNames, paste(getQuote(sym, what=yahooQF("Name"))[,2]))
}
myNASymbolNames<-data.frame(Symbol=myNASymbols, Name=myNames, stringsAsFactors = FALSE)
mySymbolsData$Name=apply(mySymbolsData, 1, function(row) ifelse(is.na(row['Name']), myNASymbolNames$Name[myNASymbolNames$Symbol==row['Symbol']], row['Name']))


write.table(mySymbolsData, file = '/Users/theocho/Github/TAQ/output/symbolsData.csv', sep=',', row.names=FALSE)

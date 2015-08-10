import datetime
import csv
import math
import os
import itertools
import taqbreakdown
import logging
import collections

def getPerDateTradeList(aFileName, aSymbol, aDates, myStartTime, myEndTime):
    theTrades = collections.defaultdict(list)
    myValidDates = list()
    i=1
    with open(aFileName, 'r') as f:
        myReader = csv.DictReader(f)
        for myRow in myReader:
            myTrade = taqbreakdown.Trade(myRow)
            if myTrade.dateTime.date() in aDates:
                if myTrade.symbol == aSymbol and myTrade.dateTime.time() > myStartTime and \
                myTrade.dateTime.time() < myEndTime and myTrade.exchange != "FINRA":
                    theTrades[myTrade.dateTime.date().strftime("%d%m%Y")].append(myTrade)
                if i%10000 == 0:
                    print(myTrade.dateTime)
                if myTrade.dateTime.date() not in myValidDates:
                    myValidDates.append(myTrade.dateTime.date())
            elif myTrade.dateTime.date() > max(aDates):
                return(theTrades)
            i += 1
    return(theTrades)
def summarizeVolumeByExchange(myTrades, aExchanges):
    theExchangeVolumeSummary = dict.fromkeys(aExchanges, 0)
    for myTrade in myTrades:
        theExchangeVolumeSummary[myTrade.exchange] += myTrade.tradeVolume
    return(theExchangeVolumeSummary)
def addInEmptyIntervals(theTradesPerInterval, myStartDateTime, myPopulatedIntervals, aTimeInterval, myFirstInterval, myLastInterval):
    myEmptyIntervals = [i for i in range(myFirstInterval, myLastInterval) if i not in myPopulatedIntervals]
    for interv in myEmptyIntervals:
        myIntervalEntry = {"startTime":myStartDateTime + interv * aTimeInterval, "endTime":myStartDateTime + (interv+1) * aTimeInterval,\
                           "trades": list()}
        theTradesPerInterval.append(myIntervalEntry)
    return(theTradesPerInterval)
def getIntervaledTrades(aTrades, aDateString, myStartTime, myEndTime, aIntervalStyle, aInterval):
    if aIntervalStyle == "clock":
        theIntervaledTrades = getClockIntervaledTrades(aTrades, aDateString, aInterval, myStartTime, myEndTime)
    elif aIntervalStyle == "business":
        theIntervaledTrades = getBusinessIntervaledTrades(aTrades, aDateString, aInterval, myStartTime, myEndTime)
    else:
        raise ValueError("interval style was not 'clock' or 'business'")
    return(theIntervaledTrades)
def getBusinessIntervaledTrades(aTrades, aDateString, aInterval, myStartTime, myEndTime):
    myDate = datetime.datetime.strptime(aDateString, "%d%m%Y")
    myStartDateTime = datetime.datetime.combine(myDate, myStartTime)
    myEndDateTime = datetime.datetime.combine(myDate, myEndTime)
    
    myPartitionedTrades = [aTrades[n:n+aInterval-1] for n in range(0, len(aTrades), aInterval)]

    myIntervaledTrades = [{"startTime": min([x.dateTime for x in trades]), "endTime": max([x.dateTime for x in trades]), 
                            "trades": trades} for trades in myPartitionedTrades]
    
    theTradesPerInterval = [x for x in myIntervaledTrades if (x["startTime"] >= myStartDateTime and x["endTime"] <= myEndDateTime)]
    return(theTradesPerInterval)
def getClockIntervaledTrades(aTrades, aDateString, aTimeInterval, myStartTime, myEndTime):
    myDate = datetime.datetime.strptime(aDateString, "%d%m%Y")
    myStartDateTime = datetime.datetime.combine(myDate, myStartTime)
    myEndDateTime = datetime.datetime.combine(myDate, myEndTime)
    get_key = lambda x: int((x.dateTime - myStartDateTime) / aTimeInterval)
    theTradesPerInterval = [{"startTime":myStartDateTime + interv * aTimeInterval, "endTime":myStartDateTime + (interv+1) * aTimeInterval, "trades": list(d)} 
                            for interv, d in itertools.groupby(aTrades, get_key)]
    myPopulatedIntervals = [interv for interv, d in itertools.groupby(aTrades, get_key)]
    
    myFirstInterval = int((myStartDateTime - myStartDateTime) / aTimeInterval)
    myLastInterval = int((myEndDateTime - myStartDateTime) / aTimeInterval)
    theTradesPerInterval = addInEmptyIntervals(theTradesPerInterval, myStartDateTime, myPopulatedIntervals, aTimeInterval, myFirstInterval, myLastInterval)
    
    theTradesPerInterval = [x for x in theTradesPerInterval if (x["startTime"] >= myStartDateTime and x["endTime"] <= myEndDateTime)]
    return(theTradesPerInterval)    
def getExchangeVolumeDict(aTrades, aStartTime, aEndTime, aExchanges):
    myTimeRangeDict = {"startTime":aStartTime, "endTime":aEndTime}
    theExchangeVolumesDict = summarizeVolumeByExchange(aTrades, aExchanges)
    theExchangeVolumesDict.update(myTimeRangeDict)
    return(theExchangeVolumesDict)
def getExchangeVolumesPerInterval(aIntervaledTrades, aExchanges):
    theExchangeVolumesPerInterval = [getExchangeVolumeDict(t["trades"], t["startTime"], t["endTime"], aExchanges) for t in aIntervaledTrades]
    return(theExchangeVolumesPerInterval)
def getExchangeProportions(aExchangeVolumes):
    myStartTime = aExchangeVolumes.pop("startTime", None)
    myEndTime = aExchangeVolumes.pop("endTime", None)
    myTotalVolume = sum(aExchangeVolumes.values())
    for exchange, volume in aExchangeVolumes.items():
        if myTotalVolume == 0:
            aExchangeVolumes[exchange] = float('NaN')
        else:
            aExchangeVolumes[exchange] = volume / myTotalVolume
    aExchangeVolumes["isEmpty"] = 1 if myTotalVolume == 0 else 0
    aExchangeVolumes["startTime"] = myStartTime
    aExchangeVolumes["endTime"] = myEndTime
    aExchangeVolumes["totalVolume"] = myTotalVolume
    return(aExchangeVolumes)
def getExchangeProportionsPerInterval(aExchangeVolumesPerInterval):
    aExchangeProportionsPerInterval = [getExchangeProportions(t) for t in aExchangeVolumesPerInterval]
    aExchangeProportionsPerInterval = sorted(aExchangeProportionsPerInterval, key=lambda k: k['startTime'])
    theExchangeProportionsPerInterval, myEmptyCount, myIntervalCount = dealWithEmpties(aExchangeProportionsPerInterval)
    return(theExchangeProportionsPerInterval, myEmptyCount, myIntervalCount)
def dealWithEmpties(aExchangeProportionsPerInterval):
    myEmptyCount = sum([1 for x in aExchangeProportionsPerInterval if x["isEmpty"] == 1])
    myIntervalCount = sum([1 for x in aExchangeProportionsPerInterval])
    if myEmptyBehavior == "Persist":
        theExchangeProportionsPerInterval = persistThroughEmpties(aExchangeProportionsPerInterval)
    elif myEmptyBehavior == "NaN":
        theExchangeProportionsPerInterval = aExchangeProportionsPerInterval
    elif myEmptyBehavior == "ThrowOut":
        theExchangeProportionsPerInterval = [x for x in aExchangeProportionsPerInterval if x["isEmpty"] == 0]
    else:
        raise ValueError("invalid myEmptyBehavior")
    return(theExchangeProportionsPerInterval, myEmptyCount, myIntervalCount)
def persistThroughEmpties(aExchangeProportionsPerInterval):
    myLastRow = next(t for t in aExchangeProportionsPerInterval if t["isEmpty"]==0)
    for i in range(len(aExchangeProportionsPerInterval)):
        if aExchangeProportionsPerInterval[i]["isEmpty"]:
            myOriginalRow = dict(aExchangeProportionsPerInterval[i])
            aExchangeProportionsPerInterval[i] = dict(myLastRow)
            aExchangeProportionsPerInterval[i]["isEmpty"] = 1
            aExchangeProportionsPerInterval[i]["startTime"] = myOriginalRow["startTime"]
            aExchangeProportionsPerInterval[i]["endTime"] = myOriginalRow["endTime"]
        else:
            myLastRow = dict(aExchangeProportionsPerInterval[i])
    return(aExchangeProportionsPerInterval)
def writeExchangeBreakdownPerInterval(aBreakdownPerInterval, aFolder, aFileName, myEmptyCount, myIntervalCount):
    if not os.path.exists(aFolder):
        os.makedirs(aFolder)
    keys = aBreakdownPerInterval[0].keys()
    with open(aFolder+aFileName+".csv", 'wt') as aFile:
        dict_writer = csv.DictWriter(aFile, keys)
        dict_writer.writeheader()
        dict_writer.writerows(aBreakdownPerInterval)
    
    myCountsDict = [{"emptyCount":myEmptyCount, "intervalCount":myIntervalCount}]
    keysc = myCountsDict[0].keys()
    with open(aFolder+aFileName+"Counts.csv", 'wt') as aFile:
        dict_writer = csv.DictWriter(aFile, keysc)
        dict_writer.writeheader()
        dict_writer.writerows(myCountsDict)
def computeExchangeProportionsPerInterval(aTrades, aDateString, myStartTime, myEndTime, myExchanges, 
                                          aIntervalStyle, aInterval):
    print(aDateString + ":")
    print("grouping trades by interval")
    myIntervaledTrades = getIntervaledTrades(aTrades, aDateString, myStartTime, myEndTime, aIntervalStyle, aInterval)
    print("summarizing exchange volumes")
    myExchangeVolumesPerInterval = getExchangeVolumesPerInterval(myIntervaledTrades, myExchanges)
    print("converting to proportions")
    theExchangeProportionsPerInterval, myEmptyCount, myIntervalCount = getExchangeProportionsPerInterval(myExchangeVolumesPerInterval)
    return(theExchangeProportionsPerInterval, myEmptyCount, myIntervalCount)
def getAverageProportion(myExchangeProportionsPerInterval, exch):
    myProportions = [interval[exch] for interval in myExchangeProportionsPerInterval if not math.isnan(interval[exch])]
    theAverageProportion = sum(myProportions) / float(len(myProportions))
    return(theAverageProportion)
def transformProportionsToScores(myExchangeProportionsPerInterval, myExchanges):
    myAvgPropsPerExchange = dict()
    for exch in myExchanges:
        myAvgPropsPerExchange[exch] = getAverageProportion(myExchangeProportionsPerInterval, exch)
    for i in range(len(myExchangeProportionsPerInterval)):
        for exch, avgProp in myAvgPropsPerExchange.items():
            myProportion = myExchangeProportionsPerInterval[i][exch]
            myExchangeProportionsPerInterval[i][exch] = (myProportion - avgProp) / avgProp
    return(myExchangeProportionsPerInterval)
def computeTotalExchangeProportionsPerInterval(myPerDateTradeList, myStartTime, myEndTime, myExchanges,
                                                 aIntervalStyle, myInterval):
    myPerDateExchangeProportionsPerInterval = dict()
    for dateString, trades in myPerDateTradeList.items():
        myExchPropsPerInterval, myEmptyCount, myIntervalCount = computeExchangeProportionsPerInterval(trades, dateString,
                                                                                                      myStartTime, myEndTime,
                                                                                                      myExchanges, aIntervalStyle,
                                                                                                      myInterval)
        myPerDateExchangeProportionsPerInterval[dateString] = myExchPropsPerInterval
    print("sorting")
    myExchangeProportionsPerInterval = [line for dateList in myPerDateExchangeProportionsPerInterval.values() for line in dateList]
    myExchangeProportionsPerInterval = sorted(myExchangeProportionsPerInterval, key=lambda k: k['startTime'])
    
    print("transforming to scores")
    theScoresPerInterval = transformProportionsToScores(myExchangeProportionsPerInterval, myExchanges)
    
    return(theScoresPerInterval, myEmptyCount, myIntervalCount)
def createIntervaledFiles(myPerDateTradeList, myExchange, mySymbol, myStartTime, myEndTime, myDates, myIntervalStyle,
                          myEmptyBehavior, myIntervals):
    
    for i in myIntervals:
        if myIntervalStyle == "business":
            myInterval = i
        elif myIntervalStyle == "clock":
            myInterval = datetime.timedelta(seconds=i)
        else:
            raise ValueError("invalid interval style")
        myPerDateExchPropsPerInterv, myEmptyCount, myIntervalCount = computeTotalExchangeProportionsPerInterval(myPerDateTradeList,
                                                                                                                myStartTime, myEndTime,
                                                                                                                myExchanges, 
                                                                                                                myIntervalStyle, myInterval) 
        
        writeExchangeBreakdownPerInterval(myPerDateExchPropsPerInterv, os.path.join(os.getcwd(), "..\\output\\") + 
                                          myIntervalStyle + "Intervals\\" + mySymbol + "\\",
                                          myEmptyBehavior + str(i),
                                          myEmptyCount, myIntervalCount)
        return(myPerDateExchPropsPerInterv)
    
if __name__ == "__main__":
    mySymbols = ["AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    myDayCount = 30
    logging.basicConfig(filename="loglogloglog",
                            filemode='a',
                            format='%(asctime)s,%(msecs)d %(name)s %(levelname)s %(message)s',
                            datefmt='%H:%M:%S',
                            level=logging.DEBUG)
    logging.info("Partitioning Trades")
    
    for mySymbol in mySymbols:
        myExTime = datetime.datetime.now()
        myLogger = logging.getLogger(mySymbol)
        
        myStartTime = datetime.time(10,0,0)
        myEndTime = datetime.time(15,30,0)
        myDates = [datetime.date(2014,1,1) + datetime.timedelta(days=x) for x in range(myDayCount)]
        myEmptyBehavior = "ThrowOut"
        myEmptyBehavior = "NaN"
        myIntervals = [1, 30, 60, 300, 600, 3600]
        myBusinessIntervals = [1,5,10,20]
        myBusinessIntervals = [1]
        
        print("reading in trades")
        myPerDateTradeList = getPerDateTradeList(os.path.join(os.getcwd(), "..\\data\\2014SymbolData\\") + mySymbol + '.csv',
                                                 mySymbol, myDates, myStartTime, myEndTime)
        myExchanges = set(x.exchange for t in myPerDateTradeList.values() for x in t)
        myLogger.info("For " + str(myDayCount) + " days, reading data takes " + str(datetime.datetime.now() - myExTime))
        myExTime = datetime.datetime.now()
        """
        createIntervaledFiles(myPerDateTradeList, myExchanges, mySymbol, myStartTime, myEndTime, myDates, "clock", "ThrowOut", myIntervals)
        myLogger.info("For " + str(myDayCount) + " days, clock-ThrowOut takes " + str(datetime.datetime.now() - myExTime))
        myExTime = datetime.datetime.now()
        createIntervaledFiles(myPerDateTradeList, myExchanges, mySymbol, myStartTime, myEndTime, myDates, "clock", "NaN", myIntervals)
        myLogger.info("For " + str(myDayCount) + " days, clock-NaN takes " + str(datetime.datetime.now() - myExTime))
        myExTime = datetime.datetime.now()"""
        createIntervaledFiles(myPerDateTradeList, myExchanges, mySymbol, myStartTime, myEndTime, myDates, "business", "NaN", myBusinessIntervals)
        myLogger.info("For " + str(myDayCount) + " days, business-NaN takes " + str(datetime.datetime.now() - myExTime))
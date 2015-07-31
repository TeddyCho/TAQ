import datetime
import csv
import math
import time
import os
import itertools
import taqbreakdown

def getPerDateTradeList(aFileName, aSymbol, aDates, myStartTime, myEndTime):
    theTrades = dict((d.strftime("%d%m%Y"), list()) for d in aDates)
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
            i += 1
    return(theTrades)
def summarizeVolumeByExchange(myTrades, aExchanges):
    theExchangeVolumeSummary = dict.fromkeys(aExchanges, 0)
    for myTrade in myTrades:
        theExchangeVolumeSummary[myTrade.exchange] += myTrade.tradeVolume
    return(theExchangeVolumeSummary)
    # theExchangeVolumeSummary = dict(zip(aExchanges, myExchangeVolumes))
    # return(theExchangeVolumeSummary)
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
    theExchangeProportionsPerInterval = [getExchangeProportions(t) for t in aExchangeVolumesPerInterval]
    theExchangeProportionsPerInterval = sorted(theExchangeProportionsPerInterval, key=lambda k: k['startTime'])
    if myEmptyBehavior == "Persist":
        myLastRow = next(t for t in theExchangeProportionsPerInterval if t["isEmpty"]==0)
        for i in range(len(theExchangeProportionsPerInterval)):
            if theExchangeProportionsPerInterval[i]["isEmpty"]:
                myOriginalRow = dict(theExchangeProportionsPerInterval[i])
                theExchangeProportionsPerInterval[i] = dict(myLastRow)
                theExchangeProportionsPerInterval[i]["isEmpty"] = 1
                theExchangeProportionsPerInterval[i]["startTime"] = myOriginalRow["startTime"]
                theExchangeProportionsPerInterval[i]["endTime"] = myOriginalRow["endTime"]
            else:
                myLastRow = dict(theExchangeProportionsPerInterval[i])
    return(theExchangeProportionsPerInterval)
def writeExchangeBreakdownPerInterval(aBreakdownPerInterval, aFileName):
    keys = aBreakdownPerInterval[0].keys()
    with open(aFileName, 'wt') as aFile:
        dict_writer = csv.DictWriter(aFile, keys)
        dict_writer.writeheader()
        dict_writer.writerows(aBreakdownPerInterval)
def computeExchangeProportionsPerInterval(aTrades, aDateString, myStartTime, myEndTime, myExchanges, 
                                          aIntervalStyle, aInterval):
    print("grouping trades by interval")
    myIntervaledTrades = getIntervaledTrades(aTrades, aDateString, myStartTime, myEndTime, aIntervalStyle, aInterval)
    print("summarizing exchange volumes")
    myExchangeVolumesPerInterval = getExchangeVolumesPerInterval(myIntervaledTrades, myExchanges)
    print("converting to proportions")
    theExchangeProportionsPerInterval = getExchangeProportionsPerInterval(myExchangeVolumesPerInterval)
    return(theExchangeProportionsPerInterval)
def getAverageProportion(myExchangeProportionsPerInterval, exch):
    myProportions = [interval[exch] for interval in myExchangeProportionsPerInterval if not math.isnan(interval[exch])]
    theAverageProportion = sum(myProportions) / float(len(myProportions))
    return(theAverageProportion)
def transformProportionsToScores(myExchangeProportionsPerInterval):
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
        myExchPropsPerInterval = computeExchangeProportionsPerInterval(trades, dateString, myStartTime, myEndTime,
                                                                       myExchanges, aIntervalStyle, myInterval)
        myPerDateExchangeProportionsPerInterval[dateString] = myExchPropsPerInterval
    print("sorting")
    myExchangeProportionsPerInterval = [line for dateList in myPerDateExchangeProportionsPerInterval.values() for line in dateList]
    myExchangeProportionsPerInterval = sorted(myExchangeProportionsPerInterval, key=lambda k: k['startTime'])
    
    print("transforming to scores")
    theScoresPerInterval = transformProportionsToScores(myExchangeProportionsPerInterval)
    
    return(theScoresPerInterval)
if __name__ == "__main__":
    myFileName = "BACGOOGOneWeek"
    mySymbol = "BAC"
    myStartTime = datetime.time(10,0,0)
    myEndTime = datetime.time(15,30,0)
    myDates = [datetime.date(2014,3,3), datetime.date(2014,3,4), datetime.date(2014,3,5),
               datetime.date(2014,3,6), datetime.date(2014,3,7)]
    myIntervalStyle = "clock"
    myEmptyBehavior = "NaN"
    myTimeIntervals = [1, 10, 120, 1800, 3600, 10800, 19800]
    myTimeIntervals = [30]
    
    print("reading in trades")
    myPerDateTradeList = getPerDateTradeList(os.path.join(os.getcwd(), "..\\data\\") + myFileName + '.csv', mySymbol, 
                                             myDates, myStartTime, myEndTime)
    myExchanges = set(x.exchange for t in myPerDateTradeList.values() for x in t)
    
    for i in myTimeIntervals:
        myTimeInterval = datetime.timedelta(seconds=i)
        myPerDateExchPropsPerInterv = computeTotalExchangeProportionsPerInterval(myPerDateTradeList,
                                                                                 myStartTime, myEndTime, myExchanges, 
                                                                                 myIntervalStyle, myTimeInterval) 
        
        
        
        writeExchangeBreakdownPerInterval(myPerDateExchPropsPerInterv, os.path.join(os.getcwd(), "..\\output\\") + 
                                          myIntervalStyle + "Intervals\\" + 
                                          "oneWeek" + myEmptyBehavior + str(myTimeInterval.seconds) + mySymbol + ".csv")
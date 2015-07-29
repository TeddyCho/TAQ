import datetime
import csv
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
    aExchangeVolumes["startTime"] = myStartTime
    aExchangeVolumes["endTime"] = myEndTime
    aExchangeVolumes["totalVolume"] = myTotalVolume
    return(aExchangeVolumes)
def getExchangeProportionsPerInterval(aExchangeVolumesPerInterval):
    theExchangeProportionsPerInterval = [getExchangeProportions(t) for t in aExchangeVolumesPerInterval]
    return(theExchangeProportionsPerInterval)
def writeExchangeBreakdownPerInterval(aBreakdownPerInterval, aFileName):
    with open(aFileName, 'wt') as aFile:
        myCsvWriter = csv.writer(aFile)
        myColNames = list(aBreakdownPerInterval[0].keys())
        myCsvWriter.writerow(myColNames)
        for aBreakdown in aBreakdownPerInterval:
            myRow = list(aBreakdown.values())
            myCsvWriter.writerow(myRow)
def computeExchangeProportionsPerInterval(aTrades, aDateString, myStartTime, myEndTime, myExchanges, 
                                          aIntervalStyle, aInterval):
    print("grouping trades by interval")
    myIntervaledTrades = getIntervaledTrades(aTrades, aDateString, myStartTime, myEndTime, aIntervalStyle, aInterval)
    print("summarizing exchange volumes")
    myExchangeVolumesPerInterval = getExchangeVolumesPerInterval(myIntervaledTrades, myExchanges)
    print("converting to proportions")
    theExchangeProportionsPerInterval = getExchangeProportionsPerInterval(myExchangeVolumesPerInterval)
    return(theExchangeProportionsPerInterval)
def computePerDateExchangeProportionsPerInterval(myPerDateTradeList, myStartTime, myEndTime, myExchanges,
                                                 aIntervalStyle, myInterval):
    myPerDateExchangeProportionsPerInterval = dict()
    for dateString, trades in myPerDateTradeList.items():
        myExchPropsPerInterval = computeExchangeProportionsPerInterval(trades, dateString, myStartTime, myEndTime,
                                                                       myExchanges, aIntervalStyle, myInterval)
        myPerDateExchangeProportionsPerInterval[dateString] = myExchPropsPerInterval
    print("sorting")
    myExchangeProportionsPerInterval = [line for dateList in myPerDateExchangeProportionsPerInterval.values() for line in dateList]
    myExchangeProportionsPerInterval = sorted(myExchangeProportionsPerInterval, key=lambda k: k['startTime']) 
    return(myExchangeProportionsPerInterval)
if __name__ == "__main__":
    myFileName = "BACGOOGOneWeek"
    mySymbol = "BAC"
    myStartTime = datetime.time(10,0,0)
    myEndTime = datetime.time(15,30,0)
    myDates = [datetime.date(2014,3,3), datetime.date(2014,3,4), datetime.date(2014,3,5),
               datetime.date(2014,3,6), datetime.date(2014,3,7)]
    
    myIntervalStyle = "business"
    myBusinessIntervals = [10, 50, 100, 1000, 10000]
    myBusinessIntervals = [50000]
    myTimeIntervals = [1, 10, 120, 1800, 3600, 10800, 19800]
    
    print("reading in trades")
    myPerDateTradeList = getPerDateTradeList(os.path.join(os.getcwd(), "..\\data\\") + myFileName + '.csv', mySymbol, 
                                             myDates, myStartTime, myEndTime)
    myExchanges = set(x.exchange for t in myPerDateTradeList.values() for x in t)
    
    if myIntervalStyle == "business":
        for i in myBusinessIntervals:
            myPerDateExchPropsPerInterv = computePerDateExchangeProportionsPerInterval(myPerDateTradeList, myStartTime, myEndTime, 
                                                                                       myExchanges, myIntervalStyle, i)
            myCsvFile = os.path.join(os.getcwd(), "..\\output\\") + myIntervalStyle + "Intervals\\" + "exchangePropsOneWeek" + str(i) +  mySymbol + ".csv"
            writeExchangeBreakdownPerInterval(myPerDateExchPropsPerInterv, myCsvFile)
            print(myCsvFile)
    elif myIntervalStyle == "clock":
        for i in myTimeIntervals:
            myTimeInterval = datetime.timedelta(seconds=i)
            myPerDateExchPropsPerInterv = computePerDateExchangeProportionsPerInterval(myPerDateTradeList,
                                                                                       myStartTime, myEndTime, myExchanges, 
                                                                                       myIntervalStyle, myTimeInterval) 
            myCsvFile = os.path.join(os.getcwd(), "..\\output\\") +myIntervalStyle + "Intervals\\" + "exchangePropsOneWeek" + str(myTimeInterval.seconds) + mySymbol + ".csv"
            writeExchangeBreakdownPerInterval(myPerDateExchPropsPerInterv, myCsvFile)
            print(myCsvFile)
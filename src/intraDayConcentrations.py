import datetime
import csv
import time
import os
import itertools
import taqbreakdown

def getTradeList(aFileName, aSymbol, aDates):
    theTradesForDate = list()
    i=1
    with open(aFileName, 'r') as f:
        myReader = csv.DictReader(f)
        for myRow in myReader:
            myTrade = taqbreakdown.Trade(myRow)
            if myTrade.dateTime.date() in aDates and myTrade.symbol == aSymbol and \
            myTrade.dateTime.time() > datetime.time(9,30,0) and myTrade.dateTime.time() < datetime.time(16,0,0):
                theTradesForDate.append(myTrade)
            if i%10000 == 0:
                print(myTrade.dateTime)
            i += 1
    return(theTradesForDate)
def summarizeVolumeByExchange(myTrades, aExchanges):
    theExchangeVolumeSummary = dict.fromkeys(aExchanges, 0)
    for myTrade in myTrades:
        theExchangeVolumeSummary[myTrade.exchange] += myTrade.tradeVolume
    return(theExchangeVolumeSummary)
    # theExchangeVolumeSummary = dict(zip(aExchanges, myExchangeVolumes))
    # return(theExchangeVolumeSummary)
def perdelta(aStartTime, aEndTime, aDelta):
    curr = aStartTime
    while curr < aEndTime:
        yield curr
        curr += aDelta
def getTradesPerInterval(aTradeList, aExchanges, aTimeInterval):
    myStartTime = min([x.dateTime for x in aTradeList])
    myEndTime = max([x.dateTime for x in aTradeList])
    """myIntervalCount = sum(1 for _ in (perdelta(myStartTime, myEndTime, aTimeInterval)))
    i = 1
    theTradesPerInterval = list()
    for intervalStart in perdelta(myStartTime, myEndTime, aTimeInterval):
        if i%100 == 0:
            print(i / myIntervalCount)
        intervalEnd = intervalStart + aTimeInterval
        myIntervalTrades = [t for t in aTradeList if intervalStart <= t.dateTime and t.dateTime < intervalEnd]
        theTradesPerInterval.append({"startTime": intervalStart, "endTime": intervalEnd, "trades": myIntervalTrades})
        i+=1
    return(theTradesPerInterval)"""

    get_key = lambda x: int((x.dateTime - myStartTime) / aTimeInterval)
    theTradesPerInterval = [{"startTime":myStartTime + interv * aTimeInterval, "endTime":myStartTime + (interv+1) * aTimeInterval, "trades": list(d)} 
                            for interv, d in itertools.groupby(aTradeList, get_key)]
    return(theTradesPerInterval)
def getExchangeVolumeDict(aTrades, aStartTime, aEndTime, aExchanges):
    myTimeRangeDict = {"startTime":aStartTime, "endTime":aEndTime}
    theExchangeVolumesDict = summarizeVolumeByExchange(aTrades, aExchanges)
    theExchangeVolumesDict.update(myTimeRangeDict)
    return(theExchangeVolumesDict)
def getExchangeVolumesPerInterval(aTradesPerInterval, aExchanges):
    theExchangeVolumesPerInterval = [getExchangeVolumeDict(t["trades"], t["startTime"], t["endTime"], aExchanges) for t in aTradesPerInterval]
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
if __name__ == "__main__":
    myFileNameFolder = os.path.join(os.getcwd(), "..\\data\\")
    myFileName = "BACGOOGOneWeek"
    mySymbol = "BAC"
    myTimeInterval = datetime.timedelta(seconds=60)
    myDates = [datetime.date(2014,3,3), datetime.date(2014,3,4), datetime.date(2014,3,5), datetime.date(2014,3,6), datetime.date(2014,3,7)]
    #myDates = [datetime.date(2014,3,13)]
    
    print("reading in trades")
    myTradeList = getTradeList(myFileNameFolder + myFileName + '.csv', mySymbol, myDates)
    myFilteredTradeList = [x for x in myTradeList if x.exchange != "FINRA"]
    myExchanges = set(x.exchange for x in myFilteredTradeList)
    
    print("grouping trades by interval")
    myTradesPerInterval = getTradesPerInterval(myFilteredTradeList, myExchanges, myTimeInterval)
    print("summarizing exchange volumes")
    myExchangeVolumesPerInterval = getExchangeVolumesPerInterval(myTradesPerInterval, myExchanges)
    print("converting to proportions")
    myExchangeProportionsPerInterval = getExchangeProportionsPerInterval(myExchangeVolumesPerInterval)
    
    myCsvFile = os.path.join(os.getcwd(), "..\\output\\timeIntervals\\") + "exchangePropsOneWeek" + str(myTimeInterval.seconds) + mySymbol + ".csv"
    writeExchangeBreakdownPerInterval(myExchangeProportionsPerInterval, myCsvFile)
    print(myCsvFile)
import datetime
import csv
import zipfile
from itertools import groupby
import os
import taqbreakdown

def getTradeList(aFileName, aSymbol, aDate):
    theTradesForDate = list()
    i=1
    with open(aFileName, 'r') as f:
        myReader = csv.DictReader(f)
        for myRow in myReader:
            myTrade = taqbreakdown.Trade(myRow)
            if myTrade.dateTime.date() == aDate:
                if myTrade.symbol == aSymbol and myTrade.exchange != "FINRA" and \
                myTrade.dateTime.time() > datetime.time(10,30,0) and myTrade.dateTime.time() < datetime.time(15,0,0):
                    theTradesForDate.append(myTrade)
            if i%10000 == 0:
                print(i / 776352)
            i += 1
    return(theTradesForDate)

def get_key(aTrade):
    myDateTime = aTrade.dateTime
    return(datetime.datetime(myDateTime.year, myDateTime.month, myDateTime.day, myDateTime.hour, myDateTime.minute, myDateTime.second)) 

def volume_profile(myTrades, aExchanges):
    theExchangeProfile = dict.fromkeys(aExchanges, 0)
    for myTrade in myTrades:
        theExchangeProfile[myTrade.exchange] += myTrade.tradeVolume
    return(theExchangeProfile)

def writeExchangeProfileSeries(aFileName, aProfileSeries, aExchanges):
    with open(aFileName, 'wt') as aFile:
        myCsvWriter = csv.writer(aFile)
        myColNames = ["TimeInterval", "TradeCount", "Volume"]
        myColNames.extend(aExchanges)
        myCsvWriter.writerow(myColNames)
        for myProfile in aProfileSeries:
            myRow = [myProfile[0], myProfile[1], myProfile[2]]
            myVolumes = myProfile[3]
            myRow.extend([myVolumes[x] for x in aExchanges])
            myCsvWriter.writerow(myRow)
if __name__ == "__main__":
    os.chdir("..")
    myFileNameFolder = os.getcwd() + "\\data\\"
    myFileName = "f9036944a8673301"
    mySymbol = "GOOG"
    
    myTrades = getTradeList(myFileNameFolder + myFileName + '.csv', mySymbol, datetime.date(2014,3,13))
    print(len(myTrades))
    myExchanges = set(x.exchange for x in myTrades)
    
    myTradesGroupedBySecond = [(k, list(g)) for k, g in groupby(myTrades, get_key)]
    
    theExchangeProfileSeries = list()
    for myTimeTradeTuple in myTradesGroupedBySecond:
        myTradeList = myTimeTradeTuple[1]
        myVolume = sum([x.tradeVolume for x in myTradeList])
        myTradeCount = len(myTradeList)
        theExchangeProfileSeries.append((myTimeTradeTuple[0], myTradeCount, myVolume, volume_profile(myTradeList, myExchanges)))
    
    myCsvFile = os.getcwd() + "\\data\\profileSeries" + mySymbol + myFileName  + ".csv"
    writeExchangeProfileSeries(myCsvFile, theExchangeProfileSeries, myExchanges)
    print("done")
    
    
    
    
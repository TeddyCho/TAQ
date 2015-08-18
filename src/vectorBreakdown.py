import time
import datetime
import csv
import zipfile
import os
import pandas

def inferTimeOfDay(aSaleConditionString):
    if pandas.isnull(aSaleConditionString):
        return "Regular"
    #myCodeDict = {"O":"Open", "Q":"Open", "M":"Close", "6":"Close",
    #              "T":"After-Hours", "U":"After-Hours", "@":"Regular"}
    if ('O' in aSaleConditionString) or ('Q' in aSaleConditionString):
        return "Open"
    elif ('M' in aSaleConditionString) or ('6' in aSaleConditionString):
        return "Open"
    elif ('T' in aSaleConditionString) or ('U' in aSaleConditionString):
        return "Open"
    else:
        return "Regular"  
def inferExchange(aExchangeString):
    try:
        return myCodeDict[aExchangeString]
    except:
        return "Invalid Exchange"
def deriveDateTime(aDF):
    theDateTime = datetime.datetime.strptime(str(aDF["DATE"]) + ' ' + aDF["TIME"], '%Y%m%d %H:%M:%S')
    return(theDateTime)
def fallsWithinStartAndEnd(aDF, aStartTime, aEndTime):
    theIsWithin = (aStartTime <= aDF["DATETIME"].time()) and (aDF["DATETIME"].time() < aEndTime)
    return(theIsWithin)
def processTradesDF(aTradesDF, aSymbol):
    t=time.time()
    aTradesDF["DATETIME"] = pandas.to_datetime(aTradesDF["DATE"].map(str) + aTradesDF["TIME"],format='%Y%m%d%H:%M:%S')
    #print("DateTime derivation time: %s seconds." % (time.time() - t))
    t=time.time()
    aTradesDF['EXCHANGE'] = aTradesDF['EX'].apply(lambda x: myCodeDict[x])
    #print("Exchange decoding time: %s seconds." % (time.time() - t))
    t=time.time()
    aTradesDF['TOD'] = aTradesDF['COND'].apply(lambda x: inferTimeOfDay(x))
    #print("TOD decoding time: %s seconds." % (time.time() - t))
    t=time.time()
    aTradesDF = aTradesDF.drop(["G127", "CORR", "DATE", "TIME"], 1)
    #aTradesDF = aTradesDF.drop(["G127", "CORR", "DATE", "TIME", "EX"], 1)
    aTradesDF = aTradesDF.set_index('DATETIME')
    #print("Time indexing time: %s seconds." % (time.time() - t))
    t=time.time()
    
    aTradesDF = aTradesDF.between_time(myTimeStart, myTimeEnd)
    aTradesDF = aTradesDF[(aTradesDF["EXCHANGE"] != "FINRA") & (aTradesDF['TOD'] == "Regular") & (aTradesDF["SYMBOL"] == aSymbol)]
    #print("Filtering time: %s seconds." % (time.time() - t))
    return(aTradesDF)
def updateMasterWithRowCache(aMaster, aRowCache, aSymbol):
    myTrades = pandas.DataFrame(aRowCache)
    myTrades = processTradesDF(myTrades, aSymbol)
    if not myTrades.empty:
        myTrades['SIZE'] = myTrades['SIZE'].astype(float) 
        myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper('1D'), "EX"])['SIZE'].apply(lambda x: sum(x))
        #myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper('5M'), "EX"])['SIZE'].apply(lambda x: sum(x))
        #myProps = myGroupedVolumes.groupby(level=0).apply(lambda x: x/x.sum())
        myProps = myGroupedVolumes
        aMaster = pandas.concat([aMaster, myProps])
    return(aMaster)
def trawlCSV(aCSVFile, aStartDateTime, aEndDateTime, aTimeInterval, aSymbol, aStartTime, aEndTime):
    myMasterProps = pandas.Series()
    with open(aCSVFile, 'r') as f:
        myReader = csv.DictReader(f)
        myRowCache = []
        for myRow in myReader:
            myRowDateTime = datetime.datetime.strptime(myRow["DATE"] + ' ' + myRow["TIME"], '%Y%m%d %H:%M:%S')
            if aStartTime <= myRowDateTime.time() and myRowDateTime.time() < aEndTime:
                if myRowDateTime < aStartDateTime + aTimeInterval:
                    myRowCache.append(myRow)
                else:
                    if myRowCache:
                        print(aSymbol + ": " + str(aStartDateTime))
                        myMasterProps = updateMasterWithRowCache(myMasterProps, myRowCache, aSymbol)
                        del myRowCache[:]
                    while (aStartDateTime + aTimeInterval) < myRowDateTime:
                        aStartDateTime = aStartDateTime + aTimeInterval
                    myRowCache.append(myRow)
            if myRowDateTime >= aEndDateTime:
                break
    print(aStartDateTime)
    myMasterProps = updateMasterWithRowCache(myMasterProps, myRowCache, aSymbol)
    return(myMasterProps)
if __name__ == '__main__':
    myCodeDict = {"A":"NYSE MKT", "B":"NASDAQ BX", "C":"NSX", "D":"FINRA", "I":"ISE", "J":"BATS EDGA",
                  "K":"BATS EDGX", "M":"CHX", "N":"NYSE", "T":"NASDAQ T", "P":"NYSE Arca", "S":"Consolidated Tape System",
                  "T/Q":"NASDAQ TQ", "Q":"NASDAQ Q", "W":"CBSX", "X":"NASDAQ PSX", "Y":"BATS BYX", "Z":"BATS BZX"} 
    myFileNameFolder = os.getcwd() + "\\..\\data\\2014SymbolData\\"
    myOutputFolder = os.getcwd() + "\\..\\output\\2014SymbolShares\\"
    myTimeStart = datetime.datetime.strptime("09:30:00", "%H:%M:%S").time()
    myTimeEnd = datetime.datetime.strptime("16:00:00", "%H:%M:%S").time()
    myStartDateTime = datetime.datetime.strptime('20140101 00:00:00', '%Y%m%d %H:%M:%S')
    myEndDateTime = datetime.datetime.strptime('20141231 00:00:00', '%Y%m%d %H:%M:%S')
    myTimeInterval = datetime.timedelta(days=1)
    mySymbols = ["AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    mySymbols = ["MSFT", "RAD", "SPY"]
    mySymbols = ["AMD"]
    for mySymbol in mySymbols:
        myFileName = mySymbol
        
        myMaster = trawlCSV(myFileNameFolder+myFileName+".csv", myStartDateTime, myEndDateTime, myTimeInterval,
                            mySymbol, myTimeStart, myTimeEnd)
        
        print(myMaster)
        myMaster.to_csv(myOutputFolder + myFileName + ".csv")
    print("Finished.")
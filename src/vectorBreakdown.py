import time
import datetime
import csv
import zipfile
import os
import pandas

def inferTimeOfDay(aSaleConditionString):
    if pandas.isnull(aSaleConditionString):
        return "Regular"
    aSaleConditionString = aSaleConditionString.replace("@", "")
    myCodeDict = {"O":"Open", "Q":"Open", "M":"Close", "6":"Close",
                  "T":"After-Hours", "U":"After-Hours", "@":"Regular"}
    try:
        return myCodeDict[aSaleConditionString]
    except:
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
    aTradesDF = aTradesDF.drop(["G127", "CORR", "DATE", "TIME", "EX"], 1)
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
        #myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper('1D'), "EXCHANGE", "COND"])['SIZE'].apply(lambda x: sum(x))
        myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper('1D'), "EXCHANGE"])['SIZE'].apply(lambda x: sum(x))
        myProps = myGroupedVolumes.groupby(level=0).apply(lambda x: x/x.sum())
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
    mySymbol = "AMD"
    mySymbols = ["AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    mySymbols = ["GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    for mySymbol in mySymbols:
        myFileName = mySymbol
        myTimeStart = datetime.datetime.strptime("09:30:00", "%H:%M:%S").time()
        myTimeEnd = datetime.datetime.strptime("15:59:59", "%H:%M:%S").time()
        
        myStartDateTime = datetime.datetime.strptime('20140101 00:00:00', '%Y%m%d %H:%M:%S')
        myEndDateTime = datetime.datetime.strptime('20141231 00:00:00', '%Y%m%d %H:%M:%S')
        myTimeInterval = datetime.timedelta(days=1)
        myMaster = trawlCSV(myFileNameFolder+myFileName+".csv", myStartDateTime, myEndDateTime, myTimeInterval,
                            mySymbol, myTimeStart, myTimeEnd)
        
        print(myMaster)
        myMaster.to_csv(myOutputFolder + myFileName + ".csv")
        print("Finished.")
    """
    myTrades = pandas.read_csv(myFileNameFolder+myFileName)
    myTrades = processTradesDF(myTrades)
    
    t=time.time()
    myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper('5Min'), "EXCHANGE", "COND"])['SIZE'].apply(lambda x: sum(x))
    myProps = myGroupedVolumes.groupby(level=0).apply(lambda x: x/x.sum())
    print("Grouping time: %s seconds." % (time.time() - t))
    t=time.time()
    
    myGroupedVolumes.to_csv(myFileNameFolder + "MSFTDay18TODCondVol" + ".csv")
    print(myProps)
    print("Remaining time: %s seconds." % (time.time() - t))"""
import time
import datetime
import csv
import zipfile
import os
import pandas

def inferTimeOfDay(aSaleConditionString):
    if pandas.isnull(aSaleConditionString):
        return "Regular"
    if ('O' in aSaleConditionString) or ('Q' in aSaleConditionString):
        return "Open"
    elif ('M' in aSaleConditionString) or ('6' in aSaleConditionString):
        return "Open"
    elif ('T' in aSaleConditionString) or ('U' in aSaleConditionString):
        return "Open"
    else:
        return "Regular"  
def deriveDateTime(aDF):
    theDateTime = datetime.datetime.strptime(str(aDF["DATE"]) + ' ' + aDF["TIME"], '%Y%m%d %H:%M:%S')
    return(theDateTime)
def processTradesDF(aTradesDF, aSymbol):
    aTradesDF["DATETIME"] = pandas.to_datetime(aTradesDF["DATE"].map(str) + aTradesDF["TIME"],format='%Y%m%d%H:%M:%S')
    myCodeDict = {"A":"NYSE MKT", "B":"NASDAQ BX", "C":"NSX", "D":"FINRA", "I":"ISE", "J":"BATS EDGA",
                  "K":"BATS EDGX", "M":"CHX", "N":"NYSE", "T":"NASDAQ T", "P":"NYSE Arca", "S":"Consolidated Tape System",
                  "T/Q":"NASDAQ TQ", "Q":"NASDAQ Q", "W":"CBSX", "X":"NASDAQ PSX", "Y":"BATS BYX", "Z":"BATS BZX"} 
    aTradesDF['EXCHANGE'] = aTradesDF['EX'].apply(lambda x: myCodeDict[x])
    aTradesDF['TOD'] = aTradesDF['COND'].apply(lambda x: inferTimeOfDay(x))
    aTradesDF = aTradesDF.drop(["G127", "CORR", "DATE", "TIME"], 1)
    #aTradesDF = aTradesDF.drop(["G127", "CORR", "DATE", "TIME", "EX"], 1)
    aTradesDF = aTradesDF.set_index('DATETIME')
    
    aTradesDF = aTradesDF.between_time(myTimeStart, myTimeEnd)
    aTradesDF = aTradesDF[(aTradesDF["EXCHANGE"] != "FINRA") & (aTradesDF['TOD'] == "Regular") & (aTradesDF["SYMBOL"] == aSymbol)]
    return(aTradesDF)
def updateMasterWithRowCache(aMaster, aRowCache, aSymbol, aIntervalSize):
    myTrades = pandas.DataFrame(aRowCache)
    myTrades = processTradesDF(myTrades, aSymbol)
    if not myTrades.empty:
        myTrades['SIZE'] = myTrades['SIZE'].astype(float) 
        myGroupedVolumes = myTrades.groupby([pandas.TimeGrouper(aIntervalSize), 'EX'])['SIZE'].apply(lambda x: sum(x))
        #myProps = myGroupedVolumes.groupby(level=0).apply(lambda x: x/x.sum())
        
        aMaster = pandas.concat([aMaster, myGroupedVolumes])
    return(aMaster)
def trawlCSV(aCSVFile, aStartDateTime, aEndDateTime, aTimeInterval, aSymbol, aStartTime, aEndTime, aIntervalSize):
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
                        myMasterProps = updateMasterWithRowCache(myMasterProps, myRowCache, aSymbol, aIntervalSize)
                        del myRowCache[:]
                    while (aStartDateTime + aTimeInterval) < myRowDateTime:
                        aStartDateTime = aStartDateTime + aTimeInterval
                    myRowCache.append(myRow)
            if myRowDateTime >= aEndDateTime:
                break
    print(aSymbol + ": " + str(aStartDateTime))
    myMasterProps = updateMasterWithRowCache(myMasterProps, myRowCache, aSymbol, aIntervalSize)
    return(myMasterProps)
if __name__ == '__main__':
    myFileNameFolder = os.getcwd() + "\\..\\data\\2014SymbolData\\"
    myOutputFolder = os.getcwd() + "\\..\\output\\clockIntervals\\"
    myTimeStart = datetime.datetime.strptime("10:00:00", "%H:%M:%S").time()
    myTimeEnd = datetime.datetime.strptime("15:30:00", "%H:%M:%S").time()
    myStartDateTime = datetime.datetime.strptime('20140101 00:00:00', '%Y%m%d %H:%M:%S')
    myEndDateTime = datetime.datetime.strptime('20140108 00:00:00', '%Y%m%d %H:%M:%S')
    myCacheTimeInterval = datetime.timedelta(days=1)
    myIntervalSize = '1Min'
    mySymbols = ["AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    mySymbols = ["AMD"]
    for mySymbol in mySymbols:
        myFileName = mySymbol
        
        myMaster = trawlCSV(myFileNameFolder+myFileName+".csv", myStartDateTime, myEndDateTime, myCacheTimeInterval,
                            mySymbol, myTimeStart, myTimeEnd, myIntervalSize)
        
        input(myMaster)
        myMaster.to_csv(myOutputFolder + mySymbol + "\\" + myIntervalSize + ".csv")
    print("Finished.")
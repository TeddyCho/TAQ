import time
import datetime
import csv
import zipfile
import os

class Trade:
    def __init__(self, aRow):
        self.dateTime = datetime.datetime.strptime(aRow["DATE"] + ' ' + aRow["TIME"], '%Y%m%d %H:%M:%S')
        self.symbol = aRow["SYMBOL"]
        # self.price = aRow["PRICE"]
        self.tradeVolume = int(aRow["SIZE"])
        self.saleCondition = aRow["COND"]
        self.exchange = inferExchange(aRow["EX"])

class Breakdown:
    def __init__(self, aSymbols):
        myBreakdown = {}
        for sym in aSymbols:
            myBreakdown[sym] = {}
        
def inferTimeOfDay(aSaleConditionString):
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

def printProgress(aIter, aTotal):
    if aIter % 50000 == 0:
        print("Finished %s of %s, %s%%" % (aIter, int(aTotal), round((aIter/aTotal) * 100, 2)))

def updateBreakdownWithTrade(aBreakdown, aTrade):
    mySymbol = aTrade["SYMBOL"]
    myDate = aTrade["DATE"]
    myExchange = inferExchange(aTrade["EX"])
    mySize = int(aTrade["SIZE"])
    myTOD = inferTimeOfDay(aTrade["COND"])
    if(mySymbol not in aBreakdown.keys()):
        aBreakdown[mySymbol] = {}
    if(myDate not in aBreakdown[mySymbol].keys()):
        aBreakdown[mySymbol][myDate] = {}
        for exch in myCodeDict.values():
            aBreakdown[mySymbol][myDate][exch] = {"Open":0, "Regular":0,"Close":0, "After-Hours":0}
    aBreakdown[mySymbol][myDate][myExchange][myTOD] += mySize
    return(aBreakdown)

def writeBreakdownToCsv(aBreakdown, aFileName):
    with open(aFileName, 'wt') as aFile:
        myCsvWriter = csv.writer(aFile)
        myCsvWriter.writerow(["Symbol", "Date", "Exchange", "TOD", "Volume", "Proportion"])
        for mySymbol in aBreakdown.keys():
            for myDate in aBreakdown[mySymbol].keys():
                for myExchange in aBreakdown[mySymbol][myDate].keys():
                    for myTOD in aBreakdown[mySymbol][myDate][myExchange].keys():
                        myExchanges = set(myCodeDict.values())
                        myTotal = sum([aBreakdown[mySymbol][myDate][exch][myTOD] for exch in myExchanges if exch not in ["FINRA", "Consolidated Tape System"]])
                        myVolume = aBreakdown[mySymbol][myDate][myExchange][myTOD]
                        myProp = myVolume / myTotal if myTotal != 0 else float('nan')
                        myCsvWriter.writerow([mySymbol, myDate, myExchange, myTOD, myVolume, myProp])

def unzipFile(aFilePath):
    print("Unzipping %s" % aFilePath)
    myZipFile = zipfile.ZipFile(aFilePath, 'r')
    myZipFile.extractall()
    myZipFile.close()

def countRows(aCsvFile):
    with open(aCsvFile, 'r') as f:
        myReader = csv.DictReader(f)
        t = time.time()
        print("Counting rows...")
        theRowCount = sum(1 for _ in myReader)
        print("Row Count: " + str(theRowCount))
        print("Time elapsed: %s seconds" % (time.time() - t))
        return(theRowCount)
                
def updateBreakdown(aBreakdown, aCsvFile):
    i=1
    with open(aCsvFile, 'r') as f:
        myReader = csv.DictReader(f)
        for myTrade in myReader:
            aBreakdown = updateBreakdownWithTrade(aBreakdown, myTrade)
            i += 1
            if i%50000==0:
                print(myTrade["SYMBOL"] + " " + myTrade["DATE"] + " " + myTrade["TIME"])
    return(aBreakdown)

if __name__ == '__main__':
    myFileNameFolder = os.getcwd() + "\\..\\data\\2014SymbolData\\"
    myCodeDict = {"A":"NYSE MKT", "B":"NASDAQ BX", "C":"NSX", "D":"FINRA", "I":"ISE", "J":"BATS EDGA",
                  "K":"BATS EDGX", "M":"CHX", "N":"NYSE", "T":"NASDAQ", "P":"NYSE Arca", "S":"Consolidated Tape System",
                  "T/Q":"NASDAQ", "Q":"NASDAQ", "W":"CBSX", "X":"NASDAQ PSX", "Y":"BATS BYX", "Z":"BATS BZX"} 
    
    mySymbols = ["AMD", "BAC", "BRKA", "BRKB", "C", "GOOG", "GRPN", "JBLU", "MSFT", "RAD", "SPY"]
    
    for sym in mySymbols:
        myFileName = sym + ".csv"
        myBreakdown = {}
        t = time.time()
        
        myBreakdown = updateBreakdown(myBreakdown, myFileNameFolder + myFileName)
        print("Time elapsed: %s seconds" % (time.time() - t))
        
        myCsvFile = os.getcwd() + "\\..\\data\\breakdown" + myFileName
        writeBreakdownToCsv(myBreakdown, myCsvFile)
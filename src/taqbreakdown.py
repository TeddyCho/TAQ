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
    myCodeDict = {"A":"NYSE MKT", "B":"NASDAQ BX", "C":"National",
                  "D":"FINRA", "I":"ISE", "J":"Direct Edge A",
                  "K":"Direct Edge X", "M":"Chicago", "N":"NYSE",
                  "T":"NASDAQ", "P":"NYSE Arca SM", "S":"Consolidated Tape System",
                  "T/Q":"NASDAQ", "Q":"NASDAQ", "W":"CBOE", "X":"NASDAQ PSX",
                  "Y":"BATS Y", "Z":"BATS"} 
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
    if(myExchange not in aBreakdown[mySymbol][myDate].keys()):
        aBreakdown[mySymbol][myDate][myExchange] = {"Open":0, "Regular":0,"Close":0, "After-Hours":0}
    aBreakdown[mySymbol][myDate][myExchange][myTOD] += mySize
    return(aBreakdown)

def writeBreakdownToCsv(aBreakdown, aFileName):
    with open(aFileName, 'wt') as aFile:
        myCsvWriter = csv.writer(aFile)
        myCsvWriter.writerow(["Symbol", "Date", "Exchange", "TOD", "Volume"])
        for mySymbol in aBreakdown.keys():
            for myDate in aBreakdown[mySymbol].keys():
                for myExchange in aBreakdown[mySymbol][myDate].keys():
                    for myTOD in aBreakdown[mySymbol][myDate][myExchange].keys():
                        myVolume = aBreakdown[mySymbol][myDate][myExchange][myTOD]
                        myCsvWriter.writerow([mySymbol, myDate, myExchange, myTOD, myVolume])

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

def updateBreakdown(aBreakdown, aCsvFile, aRowCount):
    i=1
    with open(aCsvFile, 'r') as f:
        myReader = csv.DictReader(f)
        for myTrade in myReader:
            aBreakdown = updateBreakdownWithTrade(aBreakdown, myTrade)
            i += 1
            if i%50000==0:
                print(str(i/aRowCount))
    return(aBreakdown)

if __name__ == '__main__':
    myFileNameFolder = os.getcwd() + "\\..\\data\\"
    myFileName = "5aa0ce4b018e0067"
    
    temp = list()
    i=1
    with open(myFileNameFolder + myFileName + ".csv", 'r') as f:
        myReader = csv.DictReader(f)
        for myTrade in myReader:
            if myTrade["SYMBOL"] == "AMD" and time.strptime(myTrade["TIME"], "%H:%M:%S") > time.strptime("16:00:00", "%H:%M:%S"):
                print(myTrade["SYMBOL"])
                temp.append(myTrade)
            if i%5000 == 0:
                print(i/250000)
            if i%250000==0:
                break
            i+=1
    with open('temp.csv', 'w', newline = '') as output_file:
        print(temp[0].keys())
        dict_writer = csv.DictWriter(output_file, temp[0].keys())
        dict_writer.writeheader()
        dict_writer.writerows(temp)

    myBreakdown = {}
    myRowCount = 129048290 # countRows(myFileNameFolder + myFileName + ".csv")
    t = time.time()
    myBreakdown = updateBreakdown(myBreakdown, myFileNameFolder + myFileName + ".csv", myRowCount)
    print("Time elapsed: %s seconds" % (time.time() - t))
    
    myCsvFile = os.getcwd() + "\\..\\data\\breakdown" + myFileName  + ".csv"
    writeBreakdownToCsv(myBreakdown, myCsvFile)
    
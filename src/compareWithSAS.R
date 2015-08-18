library(reshape2)

myPropsSAS <- read.csv(paste("C:\\Users\\tcho\\Documents\\GitHub\\TAQ\\data\\SASMethod\\", 
                           "AMD14_dailyvolume", ".csv", sep =""),
                     header = TRUE, stringsAsFactors = FALSE)
myPropsSAS[is.na(myPropsSAS)] = 0

myPropsPy <- read.csv(paste("C:\\Users\\tcho\\Documents\\GitHub\\TAQ\\output\\2014SymbolShares\\", 
                          "AMD", ".csv", sep =""),
                    header = FALSE, stringsAsFactors = FALSE)
colnames(myPropsPy) <- c("DateTime", "Exchange", "Proportion")
myReshapedProps <- dcast(myPropsPy, DateTime ~ Exchange)
myReshapedProps$DATE <- gsub('-', '', substring(myReshapedProps[,"DateTime"], 1,10))
myReshapedProps$DateTime <- NULL
myReshapedProps[is.na(myReshapedProps)] = 0
myReshapedProps= myReshapedProps[,colnames(myPropsSAS)]
myReshapedProps$DATE = as.integer(myReshapedProps$DATE)

myReshapedProps[1,]
myPropsSAS[1,]

for(i in 1:dim(myPropsSAS)[1]){
  myRowDiff = 0
  for(j in 1:dim(myPropsSAS)[2]){
    myRowDiff = myRowDiff + myReshapedProps[i,j] - myPropsSAS[i,j]
  }
  print(myRowDiff)
}

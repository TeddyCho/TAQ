dataFrameToFormatOutput <- function(aDF){
  myDFInStrings <- sapply(aDF, as.character)
  cat(stringListToBracketed(names(aDF)))
  for(i in 1:dim(myDFInStrings)[1]) {
    myRowOfStrings <- myDFInStrings[i,]
    myRowFormatted <- stringListToBracketed(myRowOfStrings, i == dim(myDFInStrings)[1])
    cat(myRowFormatted)
  } 
}

stringListToBracketed <- function(aStringList, aIsLast=FALSE) {
  if(aIsLast) {
    mySuffix = ']\n'
  } else {
    mySuffix = '],\n'
  }
  return(paste('[', stringListWithSeps(aStringList), mySuffix, sep=""))
}
stringListWithSeps <- function(aStringList) {
  if(length(aStringList) > 1) {
    return(paste('"', aStringList[1], '",', stringListWithSeps(aStringList[-1]), sep=''))
  }
  else {
    return(paste('"', aStringList[1], '"'))
  }
}
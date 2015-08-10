a=c(1,3,5,1,NA,82,NA,4,7,2,2,4,NA,40,5,2,7,4,2,NA,NA,NA,6,4,4,0,2)
a = a[!is.na(a)]

b=acf(a, na.action = na.pass)
myLags = 0:10

NAPassedCor <- function(aArray, aLag){
  aUnLagged = aArray[1:(length(aArray)-aLag)]
  aLagged = aArray[(1+aLag):length(aArray)]
  myIsNumber = sapply(aUnLagged + aLagged, function(x) !is.na(x))
  
  myCorr = cor(aUnLagged[myIsNumber], aLagged[myIsNumber])
  return(myCorr)
}

NAPassedCor(a,0)
NAPassedCor(a,9)

b
sapply(myLags, function(x) NAPassedCor(a,x))

NAPassedCor(a,1)

apply(aLag0, aLag1)
cor(aLag0, aLag1)

b
c
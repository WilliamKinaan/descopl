#Load transationItems dataset.
transationItems <- read.csv(file = file.choose(), header = TRUE, sep = ",")

#Load transations dataset
transactions <- read.table(file = file.choose(), header = T, sep = ",")

#To check if transationID values in transactionItems dataset  are existed in transations dataset, and the answer is TRUE
areValuesExisted(transationItems$TransactionID, transactions$TransactionID)

#To check if all dates values are correct, and the answer is False
isCorrectedDate(transationItems$Date)
#There are 1124 wrong data, all of them is 20140229
wrongDatesInTransationDataset <- wrongDateValues(date = transationItems$Date)
#To check if all the dates values are correct after removing the wrong values, and the answer is TRUE
isCorrectedDate(transationItems$Date[-wrongDatesInTransationDataset$WrongIndexes])

#To check if all time values are correct, and the answer is FALSE
isCorrectTime(transationItems$Time)
#To get the wrong time values
wrongTimeValues <- wrongTimeValues(time = transationItems$Time)
#To check if al the wrong time values have length equals to three
isTheLengthOfAllTimeValuesThree(wrongTimeValues$WrongValues)
#To add zeros on the left
transationItems$Time[wrongTimeValues$WrongIndexes] <- correctMissingLeftZeroForTime( time = transationItems$Time[wrongTimeValues$WrongIndexes])
#To check again if the time values have been corrected, and the answer is true
isCorrectTime(transationItems$Time)

#Load items dataset
items <- read.csv(file=file.choose(), header =TRUE, sep = ";")
#To check if itemCode values in transactionItems dataset  are existed in items dataset, and the answer is FALSE
areValuesExisted(transationItems$ItemCode, items$ItemCode)
#To get the wrong itemCodes, which are codes exist in transationItems dataset but don't exist in items dataset
wrongItemCodes <- notExistedValues(transationItems$ItemCode, items$ItemCode)
#remove transationItems that their item codes do not exist in the items dataset
transationItems <- transationItems[-wrongItemCodes$WrongIndexes]
#check again if the itemCode values in transactionItems dataset are existed in items dataset, and the answer is TRUE
areValuesExisted(transationItems$ItemCode, items$ItemCode)

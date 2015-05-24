#Load Transation dataset
transactions <- read.table(file = file.choose(), header = T, sep = ",")

#To check if all dates values are correct, and the answer is False
isCorrectedDate(transactions$Date)
#There are 1031 wrong data, all of them is 20140229
wrongDatesInTransationDataset <- wrongDateValues(date = transactions$Date)
#To check if all the dates values are correct after removing the wrong values, and the answer is TRUE
isCorrectedDate(transactions$Date[-wrongDatesInTransationDataset$WrongIndexes])

#To check if all time values are correct, and the answer is FALSE
isCorrectTime(transactions$Time)
#To get the wrong time values
wrongTimeValues <- wrongTimeValues(time = transactions$Time)
#To check if al the wrong time values have length equals to three
isTheLengthOfAllTimeValuesThree(wrongTimeValues$WrongValues)
#To add zeros on the left
transactions$Time[wrongTimeValues$WrongIndexes] <- correctMissingLeftZeroForTime( time = transactions$Time[wrongTimeValues$WrongIndexes])
#To check again if the time values have been corrected, and the answer is true
isCorrectTime(transactions$Time)

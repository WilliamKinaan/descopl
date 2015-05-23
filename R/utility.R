#' Is Correct Date
#'
#' Input is a vector of integer/string values
#' @param date: a vector of integer/string values
#' @description Check if all the values of the input vectors have this format %Y%m%d
#' @return TRUE if all the values of the vector have the correct format, and otherwise it returns FALSE
#' @export 
isCorrectedDate <- function(date){
  dt <- strptime(date, "%Y%m%d")
  bad <- which(is.na(dt))
  if(length(bad) == 0){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

wrongDateValues<-function(date) {
  dt<-strptime(date, "%Y%m%d")
  bad<-which(is.na(dt))
  data.frame(WrongValues = date[bad], WrongIndexes=bad)
}

isCorrectDateTime <- function(dateTime){
  dt <- strptime(dateTime, "%Y%m%d%H%M")
  bad <- which(is.na(dt))
  if(length(bad) == 0){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

isCorrectTime <- function(time){
  dt <- strptime(time, "%H%M")
  bad <- which(is.na(dt))
  if(length(bad) == 0){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

wrongTimeValues <- function(time){
  dt<-strptime(time, " %H%M")
  bad<-which(is.na(dt))
  data.frame(WrongValues = time[bad], WrongIndexes=bad)
}

lengthOfIntegerValue <- function(oneValue){
  return (nchar(as.character(oneValue)))
}

isTheLengthOfAllTimeValuesThree <-function(time){
  lengthVector = lapply(X = time, FUN = lengthOfIntegerValue)
  return (all(lengthVector == 3))
}

correctMissingLeftZeroForOneValue <- function (oneValue){
  if(nchar(as.character(oneValue)) < 4){
    difference = 4 - nchar(as.character(oneValue))
    if(difference == 1)
      oneValue = paste("0", oneValue,sep = "")
    else if(difference ==2)
      oneValue = paste("00", oneValue,sep = "")
    else if (difference == 3)
      oneValue = paste("000", oneValue,sep = "")
  }
  return (oneValue)
}

correctMissingLeftZeroForTime <- function (time){
  return (lapply(X = time, FUN = correctMissingLeftZeroForOneValue))
}

areValuesExisted <- function (foriegnKeys, primaryKeys){
  return (all(foriegnKeys %in% primaryKeys))
}

notExistedValues <- function(foreignKeys, primaryKeys){
  differencesBooleanVector <- foreignKeys %in% primaryKeys
  return (foreignKeys[!differencesBooleanVector])
}
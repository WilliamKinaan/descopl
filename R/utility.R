#' Is Correct Date
#'
#' Input is a vector of integer/string values to be validated
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

#' Wrong Date Values
#'
#' Input is a vector of integer/string values
#' @param date: a vector of integer/string values
#' @description Returns the wrong date values and their indexes.
#' @return a data frame contains two columns; WrongValues is a vector contains the wrong values, and WrongIndexes contains the indexes of the wrong values
#' @export
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

#' Is Correct Time
#'
#' Input is a vector of integer/string values to be validated
#' @param time: a vector of integer/string values
#' @description Check if all the values of the input vectors have this format %H%M
#' @return TRUE if all the values of the vector have the correct format, and otherwise it returns FALSE
#' @export 
isCorrectTime <- function(time){
  dt <- strptime(time, "%H%M")
  bad <- which(is.na(dt))
  if(length(bad) == 0){
    return (TRUE)
  }else{
    return (FALSE)
  }
}


#' Wrong Time Values
#'
#' Input is a vector of integer/string values
#' @param time: a vector of integer/string values
#' @description Returns the wrong time values and their indexes.
#' @return a data frame contains two columns; WrongValues is a vector contains the wrong values, and WrongIndexes contains the indexes of the wrong values
#' @export
wrongTimeValues <- function(time){
  dt<-strptime(time, " %H%M")
  bad<-which(is.na(dt))
  data.frame(WrongValues = time[bad], WrongIndexes=bad)
}

lengthOfIntegerValue <- function(oneValue){
  return (nchar(as.character(oneValue)))
}

#' Is the length of all time values equal to three
#'
#' Input is a vector of integer/string values
#' @param time: a vector of integer/string values
#' @description Check if the length of all the values equal to three, we need that in order to know situations where the the leff zero is missing in the time value
#' @return TRUE when the lenght of all the values equal to three, and FALSE in the otherwise
#' @export
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
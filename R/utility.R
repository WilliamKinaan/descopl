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

#' Correct the missing zero at the left of a time value
#'
#' Input is a vector of integer/string values
#' @param time: a vector of integer/string values
#' @description There are some time values have this format 921 which should be 0921 but because it is integer values, the zero on the left will not appear. This function is to handle these cases
#' @return a vector of CHARACTORS contain the values with the correct format.
#' @export
correctMissingLeftZeroForTime <- function (time){
  return (lapply(X = time, FUN = correctMissingLeftZeroForOneValue))
}

#' Are the values of one vector contained in another vector
#'
#' It is like foreign key/primary key validation in relational database.
#' @param foreignKeys: vector contains the values that we need to validation
#' @param primaryKeys: vector contains the values that we need to validation throw out
#' @description Use this function to validation that the values of one vector are already existed in another vector. If this function returns FALSE, you can use the function notExistedValues to see which values are existed in the first vector but not in the second vector
#' @return  TRUE if all the values of the first vector are included in the second vector, and FALSE in the otherwise case.
#' @export
areValuesExisted <- function (foriegnKeys, primaryKeys){
  return (all(foriegnKeys %in% primaryKeys))
}

#' Not existed values
#'
#' @param foreignKeys: vector contains the values that we need to validation
#' @param primaryKeys: vector contains the values that we need to validation throw out
#' @description This function returns the values that exist in the first vector but don't exist in the second one
#' @export
notExistedValues <- function(foreignKeys, primaryKeys){
  differencesBooleanVector <- foreignKeys %in% primaryKeys
  return (foreignKeys[!differencesBooleanVector])
}

#' Does it contain null values
#' 
#' @param data: vector to be checked
#' @description check if all the values in the vector are not null
#' @examples
#' doesItContainNullValues(c(1:2))
#' doesItContainNullValues(c(1:2))
#' @export
doesItContainNullValues <- function(data){
  booleanVector <- lapply(data, is.na);
  return (!all(booleanVector == FALSE))
}
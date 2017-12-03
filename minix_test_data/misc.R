make.interFailures <- function(cumFailures){
  # Check to see if sorted.
  if(is.unsorted(cumFailures)){
    cumFailures <- sort(cumFailures)
  }
  
  nObservation <- length(cumFailures)
  # Pre-allocates memory
  interFailures <- numeric(nObservation)
  # Store first failure time
  interFailures[1] <- cumFailures[1]
  
  for (i in seq_along(cumFailures)) {
    interFailures[i + 1] <- cumFailures[i + 1] - cumFailures[i]  
  }
  
  # The for loop creates an NA, so just dropping it
  # There is a performance tradeoff according to:
  # https://stackoverflow.com/questions/12114439/remove-the-last-element-of-a-vector
  if(nObservation < 9000){
    interFailures <- interFailures[-length(interFailures)]
  }
  else{
    interFailures <- head(interFailures, -1)
  }
  
  t <- table(interFailures)
  if( t[names(t) == 0] > 1){
    # replacing in case there is a problem with parameter solving
    interFailures <- replace(interFailures, interFailures == 0, .0001)
  }
  rm(list = c("nObservation", "cumFailures", "t"))
  return(as.numeric(interFailures))
}

make.interval.Surv <- function(failureData){
  
  if(is.unsorted(failureData)){
    failureData <- sort(failureData)
  }
  t <- data.frame(table(failureData))
  t <- setDF(t)
  count <- 1
  status <- numeric(0)
  
  for(i in t$Freq){
    if(i == 1){
      status[count] <- i
      count = count + 1
    }
    else{
      status <- c(status, c(1, replicate(i - 1, 0)))
      count = count + 1 + (i - 1)
    }
  }
  rm(list = c("t", "count"))
  return(Surv(failureData, status))
}

#this function accepts a column, and a list of 2 items; 
#regex for grep, & corresponding replacements
clean.column <- function(column, solutions){
  # todo!
  for (i in seq_along(solutions[[1]])) {
    column[which(column %in% unique(grep(solutions[[1]][i], column, ignore.case = TRUE, value = TRUE)))] <- solutions[[2]][i] 
  }
  column[which(is.na(column))] <- "UNKNOWN"
  return(column)
}
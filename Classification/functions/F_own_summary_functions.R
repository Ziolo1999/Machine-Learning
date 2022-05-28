# fivenumSummary

fivenumSummary <- function(...) {
  c(defaultSummary(...), 
    twoClassSummary(...))
}

# mySummary

mySummary <- function(data, lev = NULL, model = NULL) {
  
  require(caret)
  
  # keep the classification table to the object table
  
  table <- confusionMatrix(data[,"pred"], # predictions
                            data[,"obs"],  # real
                            # assume that the 1st level is positive
                            lev[1])
  
  # return the result of twoClassSummary()
  # extended by the Accuracy (first statistic
  # in the $overall element)
  
  result <- c(twoClassSummary(data, lev, model)[1],
             table$overall[1:2],
             # we omit
             # Prevalence, Detection Rate, Detection Prevalence
             table$byClass[c(1:7, 11)])
  
  return(result)
  
}


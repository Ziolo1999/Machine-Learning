accuracy_multinom <- function(predicted, real) {
  ctable_m <- table(predicted, 
                    real)
  # accuracy
  accuracy <- (100 * sum(diag(ctable_m)) / sum(ctable_m))
  # recall
  recall <- 100 * diag(ctable_m) / colSums(ctable_m)
  # precision
  precision <- 100 * diag(ctable_m) / rowSums(ctable_m)
  # F1 - harmonic mean of each above pair
  F1 <- mapply(function(x, y) 1/mean(1/c(x, y)),
                         recall,
                         precision)
    
  return(c(accuracy = accuracy, 
           recall = recall,
           precision = precision,
           F1 = F1,
           avg_macro_recall = mean(recall),
           avg_macro_precision = mean(precision),
           avg_macro_F1 = mean(F1)))
}
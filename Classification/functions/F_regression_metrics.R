regressionMetrics <- function(real, predicted) {
  # Mean Square Error
  MSE <- mean((real - predicted)^2)
  # Root Mean Square Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - predicted))
  # Mean Absolute Percentage Error
  MAPE <- mean(abs(real - predicted)/real)
  # Median Absolute Error
  MedAE <- median(abs(real - predicted))
  # Mean Logarithmic Absolute Error
  MSLE <- mean((log(1 + real) - log(1 + predicted))^2)
  # R2
  R2 <- cor(predicted, real)^2

  result <- data.frame(MSE, RMSE, MAE, MAPE, MedAE, MSLE, R2)
  return(result)
}
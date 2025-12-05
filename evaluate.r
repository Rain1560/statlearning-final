evaluate_model <- function(y_true, y_pred) {
  
  log_y_true <- log(y_true)
  log_y_pred <- log(y_pred)
  
  rmse_log <- sqrt(mean((log_y_true - log_y_pred)^2))
  
  r2 <- 1 - sum((y_true - y_pred)^2) / sum((y_true - mean(y_true))^2)
  
  mae <- mean(abs(y_true - y_pred))
  
  residuals <- y_true - y_pred
  
  result <- list(
    RMSE_log = rmse_log,
    R2 = r2,
    MAE = mae,
    Residuals = residuals
  )
  
  return(result)
  
}

plot_residuals <- function(residuals, y_pred) {
  
  old_par <- par(mfrow = c(2, 2))
  on.exit(par(old_par))
  
  plot(y_pred, residuals, 
       xlab = "Predicted Values", 
       ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red")
  
  hist(residuals, 
       breaks = 30, 
       col = "lightblue",
       xlab = "Residuals", 
       main = "Histogram of Residuals")
  
  qqnorm(residuals, main = "Normal Q-Q Plot of Residuals")
  qqline(residuals, col = "red")
  
  plot(residuals, 
       type = "o", 
       pch = 19, 
       cex = 0.6,
       xlab = "Index", 
       ylab = "Residuals",
       main = "Residuals over Index")
  abline(h = 0, col = "red")
  
}

print_evaluation <- function(eval_result) {
  
  cat("Model Evaluation Results:\n")
  cat("==========================\n")
  cat(sprintf("RMSE (log scale): %.4f\n", eval_result$RMSE_log))
  cat(sprintf("R-squared: %.4f\n", eval_result$R2))
  cat(sprintf("MAE: %.2f\n", eval_result$MAE))
  cat("\n")
  cat("Residuals Summary:\n")
  print(summary(eval_result$Residuals))
  
}
library(dplyr)
library(glmnet)
library(ggplot2)

lasso <- function(y, data, family, cv.nfolds = 5){
  
  # define matrix of predictor variables
  x <- data %>%
    select(-c(eval(y), SalePrice)) %>%
    as.matrix()
  x <- cbind(1, x)
  
  # response variable
  y <- data %>%
    select(y) %>%
    as.matrix()

  # perform k-fold cross-validation to find optimal lambda value
  # lasso: alpha = 1
  cv.lasso.fit <- cv.glmnet(x, y, family = family, nfolds = cv.nfolds, alpha = 1, type.measure = "mse")
  
  # find optimal lambda value that minimizes train MSE
  best_lambda <- cv.lasso.fit$lambda.min
  
  cv.lasso.df <- data.frame(MSE = cv.lasso.fit$cvm, se.MSE = cv.lasso.fit$cvsd, lambda = cv.lasso.fit$lambda)
  
  # calculating se of RMSE using delta method
  cv.lasso.df$RMSE <- sqrt(cv.lasso.df$MSE)
  cv.lasso.df$se.RMSE <-  1/(2*cv.lasso.df$RMSE)*cv.lasso.df$se.MSE
  cv.lasso.df$upper.RMSE <- cv.lasso.df$RMSE + cv.lasso.df$se.RMSE
  cv.lasso.df$lower.RMSE <- cv.lasso.df$RMSE - cv.lasso.df$se.RMSE
  
  # plotting MSE vs. -log(lambda)
  cv.lasso.plot <- ggplot(cv.lasso.df, aes(x = -log(lambda), y = RMSE)) +
    geom_errorbar(aes(ymin = lower.RMSE, ymax = upper.RMSE), color = "gray") +
    geom_point(color = "blue") +
    labs(title = "Lasso: RMSE vs. -log(lambda)") +
    theme_bw()
  
  # find coefficients of best model
  lasso.model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
  
  coefficients <- as.matrix(coef(lasso.model)) %>%
    as.data.frame() 
  
  coefficients$Variable <- rownames(coefficients)
  coefficients <- coefficients %>%
    mutate(Coefficient = s0) %>%
    select(Variable, Coefficient) %>%
    filter(Coefficient != 0)
  
  return(list(plot = cv.lasso.plot, lambda = best_lambda, coefficients = coefficients, model = lasso.model))
  
}

train <- read.csv("data/train.csv")

na_cols_any <- colSums(is.na(train)) > 0
data <- train[, !na_cols_any] %>%
  select(-Id) %>%
  mutate(logY = log(SalePrice))

res <- lasso(y = "logY", data = data, family = "gaussian", cv.nfolds = 10)
res[["plot"]]
res[["lambda"]]
res[["coefficients"]]
res[["model"]]


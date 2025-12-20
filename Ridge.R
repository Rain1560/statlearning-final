library(dplyr)
library(glmnet)
library(ggplot2)

ridge <- function(y, data, family, cv.nfolds = 5){
  
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
  # ridge: alpha = 0
  cv.ridge.fit <- cv.glmnet(x, y, family = family, nfolds = cv.nfolds, alpha = 0, type.measure = "mse")
  
  # find optimal lambda value that minimizes train MSE
  best_lambda <- cv.ridge.fit$lambda.min
  
  cv.ridge.df <- data.frame(MSE = cv.ridge.fit$cvm, se.MSE = cv.ridge.fit$cvsd, lambda = cv.ridge.fit$lambda)
  
  # calculating se of RMSE using delta method
  cv.ridge.df$RMSE <- sqrt(cv.ridge.df$MSE)
  cv.ridge.df$se.RMSE <-  1/(2*cv.ridge.df$RMSE)*cv.ridge.df$se.MSE
  cv.ridge.df$upper.RMSE <- cv.ridge.df$RMSE + cv.ridge.df$se.RMSE
  cv.ridge.df$lower.RMSE <- cv.ridge.df$RMSE - cv.ridge.df$se.RMSE
  
  # plotting MSE vs. -log(lambda)
  cv.ridge.plot <- ggplot(cv.ridge.df, aes(x = -log(lambda), y = RMSE)) +
    geom_errorbar(aes(ymin = lower.RMSE, ymax = upper.RMSE), color = "gray") +
    geom_point(color = "blue") +
    labs(title = "Ridge: RMSE vs. -log(lambda)") +
    theme_bw()
  
  # find coefficients of best model
  ridge.model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
  
  coefficients <- as.matrix(coef(ridge.model)) %>%
    as.data.frame() 
  
  coefficients$Variable <- rownames(coefficients)
  coefficients <- coefficients %>%
    mutate(Coefficient = s0) %>%
    select(Variable, Coefficient) %>%
    filter(Coefficient != 0)
  
  return(list(plot = cv.ridge.plot, lambda = best_lambda, coefficients = coefficients, model = ridge.model))
  
}

train <- read.csv("data/train.csv")

na_cols_any <- colSums(is.na(train)) > 0
data <- train[, !na_cols_any] %>%
  select(-Id) %>%
  mutate(logY = log(SalePrice))

res <- ridge(y = "logY", data = data, family = "gaussian", cv.nfolds = 10)
res[["plot"]]
res[["lambda"]]
res[["coefficients"]]
res[["model"]]


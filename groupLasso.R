library(gglasso)
library(dplyr)
library(ggplot2)

glasso <- function(y, data, group, cv.nfolds = 5){
  
  # define matrix of predictor variables
  x <- data %>%
    select(-c(eval(y), SalePrice)) %>%
    as.matrix()
  x <- cbind(1, x)
  
  group <- c(1, group)
  
  # response variable
  y <- data %>%
    select(y) %>%
    as.matrix()
  
  # perform k-fold cross-validation to find optimal lambda value
  # MSE: pred.loss = "L1"
  cv.glasso.fit <- cv.gglasso(x, y, group = group, nfolds = cv.nfolds, pred.loss = "L1")
  
  # find optimal lambda value that minimizes train MSE
  best_lambda <- cv.glasso.fit$lambda.min
  
  cv.glasso.df <- data.frame(MSE = cv.glasso.fit$cvm, se.MSE = cv.glasso.fit$cvsd, lambda = cv.glasso.fit$lambda)
  
  # calculating se of RMSE using delta method
  cv.glasso.df$RMSE <- sqrt(cv.glasso.df$MSE)
  cv.glasso.df$se.RMSE <-  1/(2*cv.glasso.df$RMSE)*cv.glasso.df$se.MSE
  cv.glasso.df$upper.RMSE <- cv.glasso.df$RMSE + cv.glasso.df$se.RMSE
  cv.glasso.df$lower.RMSE <- cv.glasso.df$RMSE - cv.glasso.df$se.RMSE
  
  # plotting MSE vs. -log(lambda)
  cv.lasso.plot <- ggplot(cv.glasso.df, aes(x = -log(lambda), y = RMSE)) +
    geom_errorbar(aes(ymin = lower.RMSE, ymax = upper.RMSE), color = "gray") +
    geom_point(color = "blue") +
    labs(title = "Lasso: RMSE vs. -log(lambda)") +
    ylim(c(0, max(cv.glasso.df$RMSE))) +
    theme_bw()
  
}

train <- read.csv("data/train.csv")

data.train <- as.data.frame(sapply(train, as.numeric))

na_cols_any <- colSums(is.na(data.train)) > 0

data <- data.train[, !na_cols_any] %>%
  select(-Id) %>%
  mutate(logY = log(SalePrice))

y = "logY"
group = c(1:(ncol(data)-2)) + 1
cv.nfolds = 5

res <- lasso(y = "logY", data = data, family = "gaussian", cv.nfolds = 10)
res[["plot"]]
res[["lambda"]]
res[["coefficients"]]
res[["model"]]

library(xgboost)
library(caret)
library(dplyr)

xgb <- function(y, data, nrounds = 100, objective = "reg:squaredlogerror"){
  
  # define matrix of predictor variables
  x <- data %>%
    select(-c(eval(y), SalePrice)) %>%
    as.matrix()
  
  # response variable
  y <- data %>%
    select(y) %>%
    as.matrix()
  
  xgboost(data = x,
          label = y,
          nrounds = nrounds,
          objective = objective)
  
}

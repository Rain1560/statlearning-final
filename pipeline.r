source("preprocessing.r")
source("models.r")
source("evaluate.r")

train <- read.csv("data/train.csv")
x_test <- read.csv("data/test.csv")
# 设置随机种子
set.seed(123)

x <- subset(train, select = -SalePrice)
y <- train$SalePrice

train_ratio <- 0.8
train_size <- floor(train_ratio * nrow(train))

train_indices <- sample(seq_len(nrow(train)), size = train_size)

x_train <- x[train_indices, ]
x_val <- x[-train_indices, ]
y_train <- y[train_indices]
y_val <- y[-train_indices]

processed <- main_preprocess(x_train, x_val, x_test)

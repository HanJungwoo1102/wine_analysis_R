library(nnet)

wine_train_dataset <- read.csv("dataset/wine.csv", header = TRUE, sep = ",")
wine_train_dataset$wine <- as.factor(wine_train_dataset$wine)
wine_train_dataset[, 2:10] <- scale(
    wine_train_dataset[, 2:10], center = TRUE, scale = TRUE
)

nny_train <- class.ind(wine_train_dataset[, 1])
wine_train_dataset <- cbind(nny_train, wine_train_dataset[, -1])

nn_wine <- nnet(
    wine_train_dataset[, 3:11],
    wine_train_dataset[, 1:2],
    size = 5,
    maxit = 100
)

train_pred <- apply(nn_wine$fitted.value, 1, which.max)
train_true <- apply(wine_train_dataset[, 1:2], 1, which.max)
train_err <- sum(train_pred != train_true) / length(train_pred)
train_table <- table(train_pred, train_true)

print(train_table)

wine_test_dataset <- read.csv("dataset/wine_test.csv", header = TRUE, sep = ",")
wine_test_dataset[, 1:9] <- scale(
    wine_test_dataset[, 1:9], center = TRUE, scale = TRUE
)
test_pred <- apply(
    predict(
        nn_wine, wine_test_dataset, type = "raw"
    ),
    1,
    which.max
)

print(test_pred)

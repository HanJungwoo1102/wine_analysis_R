library(nnet)

wine_dataset <- read.csv("dataset/wine.csv", header = TRUE, sep = ",")
wine_dataset$wine <- as.factor(wine_dataset$wine)

wine_dataset[, 2:10] <- scale(wine_dataset[, 2:10], center = TRUE, scale = TRUE)

wine_a_dataset <- wine_dataset[wine_dataset$wine == "A", ]
wine_b_dataset <- wine_dataset[wine_dataset$wine == "B", ]

nr_a <- nrow(wine_a_dataset)
nr_b <- nrow(wine_b_dataset)

wine_a_dataset <- wine_a_dataset[sample(nr_a), ]
wine_b_dataset <- wine_b_dataset[sample(nr_b), ]

wine_a_train_dataset <- wine_a_dataset[1: round(nr_a * 0.8), ]
wine_a_test_dataset <- wine_a_dataset[(round(nr_a * 0.8) + 1) : nr_a, ]
wine_b_train_dataset <- wine_b_dataset[1: round(nr_b * 0.8), ]
wine_b_test_dataset <- wine_b_dataset[(round(nr_b * 0.8) + 1) : nr_b, ]

wine_train_dataset <- rbind(wine_a_train_dataset, wine_b_train_dataset)
wine_test_dataset <- rbind(wine_a_test_dataset, wine_b_test_dataset)

nny_train <- class.ind(wine_train_dataset[, 1])
wine_train_dataset <- cbind(nny_train, wine_train_dataset[, -1])
nny_test <- class.ind(wine_test_dataset[, 1])
wine_test_dataset <- cbind(nny_test, wine_test_dataset[, -1])

nn_wine <- nnet(wine_train_dataset[, 3:11], wine_train_dataset[, 1:2], size = 2, maxit = 100)

train_pred <- apply(nn_wine$fitted.value, 1, which.max)
train_true <- apply(wine_train_dataset[, 1:2], 1, which.max)
train_err <- sum(train_pred != train_true) / length(train_pred)
train_table <- table(train_pred, train_true)

print(train_table)

test_pred <- apply(predict(nn_wine, wine_test_dataset[, 3:11], type="raw"), 1, which.max)
test_true <- apply(wine_test_dataset[, 1:2], 1, which.max)
test_err <- sum(test_pred != test_true) / length(test_pred)
test_table <- table(test_pred, test_true)

print(test_table)

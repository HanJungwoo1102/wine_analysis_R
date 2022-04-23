library(rpart)
wine_dataset <- read.csv("dataset/wine.csv", header = TRUE, sep = ",")
rp_result <- rpart(
    wine ~ .,
    data = wine_dataset,
    method = "class"
)
plot(rp_result)
text(rp_result)

prediction_train <- predict(rp_result, wine_dataset[, -c(1:1)], type = "class")
cfm <- table(wine_dataset$wine, prediction_train)

print(cfm)
print(rp_result)

wine_test_dataset <- read.csv("dataset/wine_test.csv", header = TRUE, sep = ",")
prediction_test <- predict(rp_result, wine_test_dataset, type = "class")

print(prediction_test)

plot(
    x = wine_dataset$alcohol,
    y = wine_dataset$flavanoids,
    col = factor(wine_dataset$wine)
)
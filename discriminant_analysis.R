library(MASS)

wine_dataset <- read.csv("dataset/wine.csv", header = TRUE, sep = ",")
wine_a_dataset <- subset(wine_dataset, wine == "A")
wine_b_dataset <- subset(wine_dataset, wine == "B")
proportion <- 0.8
last_a <- nrow(wine_a_dataset) * proportion
last_b <- nrow(wine_b_dataset) * proportion
train_dataset <- wine_a_dataset[1:last_a, ]
test_dataset <- wine_a_dataset[-1:-last_a, ]
train_dataset <- rbind(train_dataset, wine_b_dataset[1:last_b, ])
test_dataset <- rbind(test_dataset, wine_b_dataset[-1:-last_b, ])

z <- lda(wine ~ ., data = train_dataset)

pred <- predict(z, train_dataset)
train_dataset$pred <- pred$class
pred <- predict(z, test_dataset)
test_dataset$pred <- pred$class

cfm <- table(train_dataset$wine, train_dataset$pred)
print(cfm)
cfm <- table(test_dataset$wine, test_dataset$pred)
print(cfm)
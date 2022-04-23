library(e1071)

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

model <- naiveBayes(wine ~ ., data = train_dataset)

pred <- predict(model, test_dataset[,-1], method="class")

cfm <- table(test_dataset$wine, pred)

print(cfm)
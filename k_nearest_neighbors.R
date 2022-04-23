library(class)

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

train <- train_dataset[,-1]
class <- train_dataset[,1]
test <- test_dataset[,-1]
test_labels <- test_dataset[,1]

md1 <- knn(
    train = train,
    cl = class,
    test = test,
    k = 5
)

cfm <- table(test_dataset$wine, md1)
print(cfm)
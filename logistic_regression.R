library(mlogit)

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

train_dataset$wine <- as.factor(train_dataset$wine)
test_dataset$wine <- as.factor(test_dataset$wine)
logr <- glm(
    wine ~ .,
    data = train_dataset,
    family = binomial("logit")
)

train_dataset$pred <- predict(logr, newdata = train_dataset, type = 'response')
test_dataset$pred <- predict(logr, newdata = test_dataset, type = 'response')

cfm <- table(test_dataset$wine, ifelse(test_dataset$pred < 0.5, 'A', 'B'))
print(test_dataset$wine)
print(test_dataset$pred)
print(cfm)

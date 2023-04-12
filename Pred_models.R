# Sources =============
source("data_preparation.R")

# Libraries ============
library(caret)
library(naivebayes)

# Seed =================
set.seed(12345)

# k-NN model ==========
# Automated paramenter tuning
grid <- expand.grid(k = c(1,3,5,7,15,21,27,35))
model <- train(label ~ ., data = norm_data,
               method = "knn", tuneGrid = grid)
model

grid <- expand.grid(k = c(17,19,21,23,25,27))
model <- train(label ~ ., data = norm_data,
               method = "knn", tuneGrid = grid)
model

# Prediction model
kNN_model21_pred <- knn(train = train[-1], test = test[-1],
                        cl = train[[1]], k = 21)
table(kNN_model21_pred == test[[1]])
confusionMatrix(kNN_model21_pred, test[[1]], 
                positive = "1")


# Naive Bayes ============
# Automated paramenter tuning
modelLookup("naive_bayes")
grid <- expand.grid(usekernel = c(TRUE, FALSE),
                    laplace = c(0,0.5,1),
                    adjust = c(0,0.5,1))
model <- train(label ~ ., data = norm_data,
               method = "naive_bayes", tuneGrid = grid)
model

# Prediction model
NaiveBayes_model <- naive_bayes(label ~ ., data=train,
                                laplace = 0, usekernel=F)
NaiveBayes_pred <- predict(NaiveBayes_model, test[-1])
table(NaiveBayes_pred == test[[1]])
confusionMatrix(NaiveBayes_pred, test[[1]], 
                positive = "1")
# Sources =============
source("data_preparation.R")

# Libraries ============
library(caret)
library(naivebayes)
library(kernlab)


# Control =============
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 5)


# k-NN model ==========
# Automated paramenter tuning
set.seed(12345)
grid <- expand.grid(k = c(1,3,5,7,15,21,27,35))
kNN_model <- train(label ~ ., data = norm_data, method = "knn",
                   tuneGrid = grid, trControl = ctrl)
kNN_model
plot(kNN_model)

# Predictions
kNN_model_pred <- predict(kNN_model, test[-1])
table(kNN_model_pred == test[[1]])
values_kNN_mod <- confusionMatrix(kNN_model_pred, test[[1]],
                                  positive = "1")


# Naive Bayes ============
# Automated paramenter tuning
set.seed(12345)
grid <- expand.grid(usekernel = c(FALSE,TRUE),
                    laplace = seq(0,1.5, length = 4),
                    adjust = seq(0,1.5, length = 4))
NB_model <- train(label ~ ., data = norm_data, method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model
plot(NB_model)

set.seed(12345)
grid <- expand.grid(usekernel = c(TRUE),
                    laplace = seq(0,1.5, length = 15),
                    adjust = seq(0,1.5, length = 15))
NB_model <- train(label ~ ., data = norm_data, method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model
plot(NB_model)

# Predictions
NaiveBayes_pred <- predict(NB_model, test[-1])
table(NaiveBayes_pred == test[[1]])
values_NB_mod <- confusionMatrix(NaiveBayes_pred, test[[1]], 
                                  positive = "1")


# Support Vector Machine ===========
# SVM linear classifier
svm_lin_model <- ksvm(label ~ ., data = norm_data, kernel = "vanilladot")
svm_lin_pred <- predict(svm_lin_model, test[-1])
table(svm_lin_pred == test[[1]])
values_lin_mod <- confusionMatrix(svm_lin_pred, test[[1]], 
                positive = "1")

# SVM with Radial Basis Function Kernel
svm_rbf_model <- ksvm(label ~ ., data = norm_data, kernel = "rbfdot")
svm_rbf_pred <- predict(svm_rbf_model, test[-1])
table(svm_rbf_pred == test[[1]])
values_rbf_mod <- confusionMatrix(svm_rbf_pred, test[[1]], 
                positive = "1")

# SVM with Polynomial Kernel
svm_poly_model <- ksvm(label ~ ., data = norm_data, kernel = "polydot")
svm_poly_pred <- predict(svm_poly_model, test[-1])
table(svm_poly_pred == test[[1]])
values_poly_mod <- confusionMatrix(svm_poly_pred, test[[1]], 
                positive = "1")

# Summary of results
SVM <- data.frame(model = c("Linear model","RBF model", "Polinomial model"),
                  accuracy = c(values_lin_mod[["overall"]][["Accuracy"]],
                               values_rbf_mod[["overall"]][["Accuracy"]],
                               values_poly_mod[["overall"]][["Accuracy"]]))
SVM %>% arrange(accuracy)


# Random forest ==========
set.seed(12345)
p <- round(sqrt(ncol(norm_data)))
grid <- expand.grid(mtry = seq(p,p+2*p))
RF_model <- train(label ~ ., data = norm_data, method = "rf",
                  tuneGrid = grid, trControl = ctrl)
RF_model
plot(RF_model)

# Predictions
RandomForest_pred <- predict(RF_model, test[-1])
table(RandomForest_pred == test[[1]])
values_NB_mod <- confusionMatrix(RandomForest_pred, test[[1]], 
                                 positive = "1")



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
grid <- expand.grid(k = c(1,3,5,7,15,21,27,35))
set.seed(12345)
kNN_model <- train(label ~ ., data = norm_data, method = "knn",
                   tuneGrid = grid, trControl = ctrl)
kNN_model

# Save model
'
kNN_model.txt <- file("outputs/kNN_model.txt")
sink(kNN_model.txt, append = TRUE, type = "output")
kNN_model
closeAllConnections()
'

# Performance plot
kNN_plot <- ggplot(kNN_model) +
  labs(title = "kNN models' performance", x = "Number of neighbors") +
  geom_text(aes(label=round(kNN_model[["results"]][["Accuracy"]],3)), hjust = 1.3) +
  theme(plot.title = element_text(size = 22, hjust = 0.5))
kNN_plot

# Save plot
'
pdf(file = "plots/performance_plot_kNN.pdf")
kNN_plot
dev.off()
'
# Predictions
kNN_model_pred <- predict(kNN_model, test[-1])
table(kNN_model_pred == test[[1]])
values_kNN_mod <- confusionMatrix(kNN_model_pred, test[[1]],
                                  positive = "1")
values_kNN_mod

# Save confusion matrix
'
kNN_matrix.txt <- file("outputs/kNN_matrix.txt")
sink(kNN_matrix.txt, append = TRUE, type = "output")
values_kNN_mod
closeAllConnections()
'


# Naive Bayes ============
# Automated paramenter tuning
grid <- expand.grid(usekernel = c(FALSE,TRUE),
                    laplace = seq(0,1.5, length = 5),
                    adjust = seq(0,1.5, length = 5))
set.seed(12345)
NB_model1 <- train(label ~ ., data = norm_data, method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model1

# Save model1
'
NB_model1.txt <- file("outputs/NB_model1.txt")
sink(NB_model1.txt, append = TRUE, type = "output")
NB_model1
closeAllConnections()
'

NB1_plot <- ggplot(NB_model1) +
  labs(title = "Naive Bayes models' performance") + 
  theme(plot.title = element_text(size = 22, hjust = 0.5))
NB1_plot

#Save plot
'
pdf(file = "plots/performance_plot_NaiveBayes1.pdf")
NB1_plot
dev.off()
'

grid <- expand.grid(usekernel = c(TRUE),
                    laplace = 0,
                    adjust = seq(0,1.5, length = 15))
set.seed(12345)
NB_model <- train(label ~ ., data = norm_data, method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model

# Save model
'
NB_model.txt <- file("outputs/NB_model.txt")
sink(NB_model.txt, append = TRUE, type = "output")
NB_model
closeAllConnections()
'

# Performance plot
NB_plot <- ggplot(NB_model) +
  labs(title = "Naive Bayes models' performance", subtitle = "Kernel = TRUE & Laplace = 0") +
  geom_text(aes(label=round(NB_model[["results"]][["adjust"]],3)), hjust = 1.2) +
  geom_text(aes(label=round(NB_model[["results"]][["Accuracy"]],3)), hjust = -0.3) +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))
NB_plot

#Save plot
'
pdf(file = "plots/performance_plot_NaiveBayes.pdf")
NB_plot
dev.off()
'

# Predictions
NaiveBayes_pred <- predict(NB_model, test[-1])
table(NaiveBayes_pred == test[[1]])
values_NB_mod <- confusionMatrix(NaiveBayes_pred, test[[1]], 
                                  positive = "1")
values_NB_mod

# Save confusion matrix
'
NB_matrix.txt <- file("outputs/NB_matrix.txt")
sink(NB_matrix.txt, append = TRUE, type = "output")
values_NB_mod
closeAllConnections()
'


# Support Vector Machine ===========
# SVM linear classifier
svm_lin_model <- ksvm(label ~ ., data = norm_data, kernel = "vanilladot")
svm_lin_pred <- predict(svm_lin_model, test[-1])
table(svm_lin_pred == test[[1]])
values_lin_mod <- confusionMatrix(svm_lin_pred, test[[1]], 
                positive = "1")

# Save model
'
SVM_lin_model.txt <- file("outputs/SVM_lin_model.txt")
sink(SVM_lin_model.txt, append = TRUE, type = "output")
svm_lin_model
closeAllConnections()
'
# Save confusion matrix
'
SVM_lin_matrix.txt <- file("outputs/SVM_lin_matrix.txt")
sink(SVM_lin_matrix.txt, append = TRUE, type = "output")
values_lin_mod
closeAllConnections()
'

# SVM with Radial Basis Function Kernel
svm_rbf_model <- ksvm(label ~ ., data = norm_data, kernel = "rbfdot")
svm_rbf_pred <- predict(svm_rbf_model, test[-1])
table(svm_rbf_pred == test[[1]])
values_rbf_mod <- confusionMatrix(svm_rbf_pred, test[[1]], 
                positive = "1")

# Save model
'
SVM_rbf_model.txt <- file("outputs/SVM_rbf_model.txt")
sink(SVM_rbf_model.txt, append = TRUE, type = "output")
svm_rbf_model
closeAllConnections()
'
# Save confusion matrix
'
SVM_rbf_matrix.txt <- file("outputs/SVM_rbf_matrix.txt")
sink(SVM_rbf_matrix.txt, append = TRUE, type = "output")
values_rbf_mod
closeAllConnections()
'

# SVM with Polynomial Kernel
svm_poly_model <- ksvm(label ~ ., data = norm_data, kernel = "polydot")
svm_poly_pred <- predict(svm_poly_model, test[-1])
table(svm_poly_pred == test[[1]])
values_poly_mod <- confusionMatrix(svm_poly_pred, test[[1]], 
                positive = "1")

# Save model
'
SVM_poly_model.txt <- file("outputs/SVM_poly_model.txt")
sink(SVM_poly_model.txt, append = TRUE, type = "output")
svm_poly_model
closeAllConnections()
'
# Save confusion matrix
'
SVM_poly_matrix.txt <- file("outputs/SVM_poly_matrix.txt")
sink(SVM_poly_matrix.txt, append = TRUE, type = "output")
values_poly_mod
closeAllConnections()
'

# Summary of results
SVM <- data.frame(model = c("Linear model","RBF model", "Polinomial model"),
                  accuracy = c(values_lin_mod[["overall"]][["Accuracy"]],
                               values_rbf_mod[["overall"]][["Accuracy"]],
                               values_poly_mod[["overall"]][["Accuracy"]]))
SVM %>% arrange(accuracy)

# Performance plot
SVM_plot <- ggplot(SVM, aes(x = model, y = accuracy, fill = model)) + geom_col() +
  labs(title = "Support Vector Machine models' performance") + 
  theme(plot.title = element_text(size = 22, hjust = 0.2))
SVM_plot

#Save plot
'
pdf(file = "plots/performance_plot_SVM.pdf")
SVM_plot
dev.off()
'

# Random forest ==========
p <- round(sqrt(ncol(norm_data)))
grid <- expand.grid(mtry = seq(p,p+2*p))
set.seed(12345)
RF_model <- train(label ~ ., data = norm_data, method = "rf",
                  tuneGrid = grid, trControl = ctrl)
RF_model

# Save model
'
RF_model.txt <- file("outputs/RF_model.txt")
sink(RF_model.txt, append = TRUE, type = "output")
RF_model
closeAllConnections()
'

# Performance plot
RF_plot <- ggplot(RF_model) +
  labs(title = "Random Forests models' performance", x = "Number of randomly selected variables") +
  geom_text(aes(label=round(RF_model[["results"]][["Accuracy"]],4)), hjust = 1.3) +
  theme(plot.title = element_text(size = 38, hjust = 0.5))
RF_plot

#Save plot
'
pdf(file = "plots/performance_plot_RF.pdf", width = 12, height = 12)
RF_plot
dev.off()
'

# Predictions
RandomForest_pred <- predict(RF_model, test[-1])
table(RandomForest_pred == test[[1]])
values_NB_mod <- confusionMatrix(RandomForest_pred, test[[1]], 
                                 positive = "1")
values_NB_mod

# Save confusion matrix
'
RF_matrix.txt <- file("outputs/RF_matrix.txt")
sink(RF_matrix.txt, append = TRUE, type = "output")
values_NB_mod
closeAllConnections()
'

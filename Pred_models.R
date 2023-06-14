# Sources =============
source("data_preparation.R")
source("functions.R")

# Libraries ============
library(dplyr)
library(caret)
library(naivebayes)
library(kernlab)
library(adabag)
library(nnet)
library(ROCR)
library(randomForest)


# Control =============
ctrl <- trainControl(method = "repeatedcv",
                     number = 10, repeats = 5)


# k-NN model ==========
# Automated paramenter tuning
grid <- expand.grid(k = c(1,3,5,7,15,21,27,35))
set.seed(12345)
kNN_model <- train(label ~ ., data = train[-1], method = "knn",
                   tuneGrid = grid, trControl = ctrl)
kNN_model


# Performance plot
kNN_plot <- ggplot(kNN_model) +
  labs(title = "kNN models' performance", x = "Number of neighbors") +
  geom_text(aes(label=round(kNN_model[["results"]][["Accuracy"]],3)), hjust = 1.3) +
  theme(plot.title = element_text(size = 22, hjust = 0.5))
kNN_plot

# Predictions
kNN_model_pred <- predict(kNN_model, test[-(1:2)])
table(kNN_model_pred == test[[2]])
values_kNN_mod <- confusionMatrix(kNN_model_pred, test[[2]],
                                  positive = "1")
values_kNN_mod


# Naive Bayes ============
# Automated paramenter tuning
grid <- expand.grid(usekernel = c(FALSE,TRUE),
                    laplace = seq(0,1.5, length = 5),
                    adjust = seq(0,1.5, length = 5))
set.seed(12345)
NB_model1 <- train(label ~ ., data = train[-1], method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model1

NB1_plot <- ggplot(NB_model1) +
  labs(title = "Naive Bayes models' performance") + 
  theme(plot.title = element_text(size = 22, hjust = 0.5))
NB1_plot

# useKernel = FALSE models
grid <- expand.grid(usekernel = c(FALSE),
                    laplace = seq(0,1.5, length = 15),
                    adjust = 0)
set.seed(12345)
NB_model <- train(label ~ ., data = train[-1], method = "naive_bayes",
                  tuneGrid = grid, trControl = ctrl)
NB_model

# Performance plot
NB_plot <- ggplot(NB_model) +
  labs(title = "Naive Bayes models' performance", subtitle = "Kernel = FALSE & Adjust = 0") +
  geom_text(aes(label=round(NB_model[["results"]][["Accuracy"]],3)), vjust = 1.5) +
  theme(plot.title = element_text(size = 22, hjust = 0.5),
        plot.subtitle = element_text(size = 15, hjust = 0.5))
NB_plot

# Predictions
NaiveBayes_pred <- predict(NB_model, test[-(1:2)])
table(NaiveBayes_pred == test[[2]])
values_NB_mod <- confusionMatrix(NaiveBayes_pred, test[[2]], 
                                  positive = "1")
values_NB_mod


# Support Vector Machine ===========
# SVM linear classifier
set.seed(12345)
svm_lin_model <- ksvm(label ~ ., data = train[-1], kernel = "vanilladot", prob.model = T)
svm_lin_pred <- predict(svm_lin_model, test[-(1:2)])
table(svm_lin_pred == test[[2]])
values_lin_mod <- confusionMatrix(svm_lin_pred, test[[2]], 
                positive = "1")

# SVM with Radial Basis Function Kernel
set.seed(12345)
svm_rbf_model <- ksvm(label ~ ., data = train[-1], kernel = "rbfdot", prob.model = T)
svm_rbf_pred <- predict(svm_rbf_model, test[-(1:2)])
table(svm_rbf_pred == test[[2]])
values_rbf_mod <- confusionMatrix(svm_rbf_pred, test[[2]], 
                positive = "1")

# SVM with Polynomial Kernel
set.seed(12345)
svm_poly_model <- ksvm(label ~ ., data = train[-1], kernel = "polydot", prob.model = T)
svm_poly_pred <- predict(svm_poly_model, test[-(1:2)])
table(svm_poly_pred == test[[2]])
values_poly_mod <- confusionMatrix(svm_poly_pred, test[[2]], 
                positive = "1")

# Summary of results
SVM <- data.frame(model = c("Linear model","RBF model", "Polinomial model"),
                  accuracy = c(values_lin_mod[["overall"]][["Accuracy"]],
                               values_rbf_mod[["overall"]][["Accuracy"]],
                               values_poly_mod[["overall"]][["Accuracy"]]))
SVM %>% arrange(desc(accuracy))

# Performance plot
SVM_plot <- ggplot(SVM, aes(x = model, y = accuracy, fill = model)) + geom_col() +
  geom_text(aes(label=round(SVM$accuracy,3)), vjust = -0.5) +
  labs(title = "Support Vector Machine models' performance") + 
  theme(plot.title = element_text(size = 22, hjust = 0.2))
SVM_plot


# Random forest ==========
p <- round(sqrt(ncol(train)))
grid <- expand.grid(mtry = seq(p,p+4*p))
set.seed(12345)
RF_model <- train(label ~ ., data = train[-1], method = "rf",
                  tuneGrid = grid, trControl = ctrl)
RF_model

# Performance plot
RF_plot <- ggplot(RF_model) +
  labs(title = "Random Forests models' performance", x = "Number of randomly selected variables") +
  geom_text(aes(label=round(RF_model[["results"]][["Accuracy"]],4)), hjust = 1.3) +
  theme(plot.title = element_text(size = 38, hjust = 0.5))
RF_plot

# Predictions
RandomForest_pred <- predict(RF_model, test[-(1:2)])
table(RandomForest_pred == test[[2]])
values_RF_mod <- confusionMatrix(RandomForest_pred, test[[2]], 
                                 positive = "1")
values_RF_mod


# Boosting decision trees ====================
grid <- expand.grid(mfinal = (1:4)*50,
                    maxdepth = c(1:3),
                    coeflearn = c("Breiman"))
set.seed(12345)
boost_model <- train(label ~ ., data = train[-1], method = "AdaBoost.M1",
                     tuneGrid = grid, trControl = ctrl)
boost_model

# Performance plot
boost_plot <- ggplot(boost_model) +
  labs(title = "Boosted decision tree models' performance", 
       x = "Number of trees") +
  geom_text(aes(label=round(boost_model[["results"]][["Accuracy"]],4)), hjust = 1.3) +
  theme(plot.title = element_text(size = 38, hjust = 0.5))
boost_plot

# Predictions
boost_pred <- predict(boost_model, test[-(1:2)])
table(boost_pred == test[[2]])
values_boost_mod <- confusionMatrix(boost_pred, test[[2]], 
                                 positive = "1")
values_boost_mod


# ANN =============
grid <- expand.grid(size = (1:16),
                    decay = sequence(5))
set.seed(12345)
ann_model <- train(label ~ ., data = train[-1], method = "nnet",
                     tuneGrid = grid, trControl = ctrl)
ann_model

# Performance plot
ann_plot <- ggplot(ann_model) +
  labs(title = "ANN models' performance", x = "Number of Hidden Units") +
  geom_text(aes(label=round(ann_model[["results"]][["Accuracy"]],4)), hjust = 1.3) +
  theme(plot.title = element_text(size = 38, hjust = 0.5))
ann_plot

# Predictions
ann_pred <- predict(ann_model, test[-(1:2)])
table(ann_pred == test[[2]])
values_ann_mod <- confusionMatrix(ann_pred, test[[2]], 
                                    positive = "1")
values_ann_mod


# Predicted probabilities ============
# kNN model
kNN_prob <- predict(kNN_model, test[-(1:2)], type = "prob")
kNN_predict <- prediction(predictions = kNN_prob[2],
                          labels = test[[2]])
kNN_perform <- performance(kNN_predict, measure = "tpr", x.measure = "fpr")
kNN_auc <- performance(kNN_predict, measure = "auc")

# Naive Bayes model
NB_prob <- predict(NB_model, test[-(1:2)], type = "prob")
NB_predict <- prediction(predictions = NB_prob[2],
                          labels = test[[2]])
NB_perform <- performance(NB_predict, measure = "tpr", x.measure = "fpr")
NB_auc <- performance(NB_predict, measure = "auc")

# SVM model
SVM_prob <- as.data.frame(predict(svm_lin_model, test[-(1:2)], type = "prob"))
SVM_predict <- prediction(predictions = SVM_prob[2],
                          labels = test[[2]])
SVM_perform <- performance(SVM_predict, measure = "tpr", x.measure = "fpr")
SVM_auc <- performance(SVM_predict, measure = "auc")

# RF model
RF_prob <- predict(RF_model, test[-(1:2)], type = "prob")
RF_predict <- prediction(predictions = RF_prob[2],
                          labels = test[[2]])
RF_perform <- performance(RF_predict, measure = "tpr", x.measure = "fpr")
RF_auc <- performance(RF_predict, measure = "auc")

# Boosted decision tree model
boost_prob <- predict(boost_model, test[-(1:2)], type = "prob")
boost_predict <- prediction(predictions = boost_prob[2],
                         labels = test[[2]])
boost_perform <- performance(boost_predict, measure = "tpr", x.measure = "fpr")
boost_auc <- performance(boost_predict, measure = "auc")

# ANN model
ann_prob <- predict(ann_model, test[-(1:2)], type = "prob")
ann_predict <- prediction(predictions = ann_prob[2],
                            labels = test[[2]])
ann_perform <- performance(ann_predict, measure = "tpr", x.measure = "fpr")
ann_auc <- performance(ann_predict, measure = "auc")


# AUC ==================
AUC <- data.frame(model = c("kNN model","NB model", "SVM model",
                            "RF model","Boost model", "ANN model"),
                  AUC = c(round(kNN_auc@y.values[[1]],5),round(NB_auc@y.values[[1]],5),
                          round(SVM_auc@y.values[[1]],5),round(RF_auc@y.values[[1]],5),
                          round(boost_auc@y.values[[1]],5),round(ann_auc@y.values[[1]],5)))
AUC %>% arrange(desc(AUC))


# ROC curves ==================
# kNN model ROC curve
plot(kNN_perform, main = "ROC curve for kNN model")
mtext("A", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(kNN_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# NB model ROC curve
plot(NB_perform, main = "ROC curve for Naive Bayes model")
mtext("B", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(NB_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# SVM model ROC curve
plot(SVM_perform, main = "ROC curve for SVM model")
mtext("C", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(SVM_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# RF model ROC curve
plot(RF_perform, main = "ROC curve for Random Forest model")
mtext("D", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(RF_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# Boosted decision trees model ROC curve
plot(boost_perform, main = "ROC curve for Boosted decision trees model")
mtext("E", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(boost_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# ANN model ROC curve
plot(ann_perform, main = "ROC curve for ANN model")
mtext("F", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(ann_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)


# Evaluating original RF model ================
# ROC curve
{plot(RF_perform, main = "ROC curve for Random Forest model")
mtext("D", side = 3, adj = -0.13, cex = 1.5, padj = -2.5)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(RF_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)}

# Best cutoff point to reduce FN
cost.perf = performance(RF_predict, measure = "cost") 
RF_predict@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]


# Improving RF model ===============
param_grid <- expand.grid(
  # Number of randomly selected variables
  mtry = seq(round(sqrt(ncol(train))),round(sqrt(ncol(train))) * 4),
  # Number of trees to be grown
  ntree = c(300, 500, 1000, 1500, 2000, 3000),
  # Cutoff values
  cutoff1 = c(0.35,0.4,0.45,0.5, 0.55, 0.6, 0.65, 0.7),
  # Sample with or without replacement
  replace = c(TRUE, FALSE),
  # Accuracy column
  ACC = NA,
  # RMSE column
  OOB_estimate = NA,
  # AUC value
  AUC = NA
)

#Setting second cutoff value (required by the randomForest function)
cutoff0 <- c()
for (i in seq_len(nrow(param_grid))) {
  diff <- 1 - param_grid$cutoff1[i]
    cutoff0 = append(cutoff0, diff)
}

# Join alltogether
param_grid <- cbind(param_grid, cutoff0)

#Reorder columns
order <- c("mtry","ntree","cutoff0","cutoff1","replace","ACC" ,"OOB_estimate","AUC")
param_grid <- param_grid[,order]

# Run the models
for (i in seq_len(nrow(param_grid))) {
  # Set seed for each iteration
  set.seed(12345)
  # RF model building
  model <- randomForest(
    formula = label ~ .,
    data = train[-1],
    mtry = param_grid$mtry[i],
    ntree = param_grid$ntree[i],
    replace = param_grid$replace[i],
    cutoff = c(param_grid$cutoff0[i], param_grid$cutoff1[i])
  )
  
  # Predictions
  model_pred <- predict(model, test[-(1:2)])
  values_model <- confusionMatrix(model_pred, test[[2]], 
                                   positive = "1")
  param_grid$ACC[i] <- values_model[["overall"]][["Accuracy"]]
  
  # Get OBB estimate
  param_grid$OOB_estimate[i] <- model[["err.rate"]][nrow(model[["err.rate"]]),1]

  # Calculate predictions
  model_prob <- as.data.frame(predict(model, test[-(1:2)], type = "prob"))
  model_predict <- prediction(predictions = model_prob[2],
                           labels = test[[2]])
  model_perform <- performance(model_predict, measure = "tpr", x.measure = "fpr")
  model_auc <- performance(model_predict, measure = "auc")
  
  # Store AUC value
  param_grid$AUC[i] <- round(model_auc@y.values[[1]],5)
  
}

param_grid %>% arrange(desc(AUC)) %>% head(10)


# Best Model ==================
set.seed(12345)
best.model <- randomForest(label ~.,
                           data = train[-1],
                           mtry = 9,
                           ntree = 300,
                           cutoff = c(0.3, 0.7),
                           replace = FALSE)

# Performance plot
layout(matrix(c(1,2),nrow=1),
       width=c(4,1))
par(mar=c(5,4,4,0)) #No margin on the right side
plot(best.model, main = "OBB error rates for the best tunned RF model")
par(mar=c(5,0,4,2)) #No margin on the left side
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("center", colnames(model$err.rate),col=1:4,cex=0.8,fill=1:4)


# Predictions
best.model_pred <- predict(best.model, test[-(1:2)])
table(best.model_pred == test[[2]])
values_best.model_mod <- confusionMatrix(best.model_pred, test[[2]], 
                                 positive = "1")
values_best.model_mod

# ROC curve and AUC
best.model_prob <- as.data.frame(predict(best.model, test[-(1:2)], type = "prob"))
best.model_predict <- prediction(predictions = best.model_prob[2],
                         labels = test[[2]])
best.model_perform <- performance(best.model_predict, measure = "tpr", x.measure = "fpr")
best.model_auc <- performance(best.model_predict, measure = "auc")

plot(best.model_perform, main = "ROC curve for the best Random Forest model found")
subtitle <- "mtry = 9, ntree = 300, cutoff = c(0.3,0.7), replace = F"
mtext(subtitle, side=3, line=0.3, at=-0.07, adj=-0.4, cex=1)
mtext("AUC = ", side = 1, adj = 0.8, padj = -3)
mtext(round(best.model_auc@y.values[[1]],5), side = 1, adj = 0.92, padj = -3)

# Variable importance plot
varImpPlot(best.model, n.var = 15,
           main = "Variable Importance plot best.model")

# Plot biggest tree
biggest_tree <- which(best.model$forest$ndbigtree == max(best.model$forest$ndbigtree))
tree_func(best.model, biggest_tree)

# Shortest tree
shortest_tree <- which(best.model$forest$ndbigtree == min(best.model$forest$ndbigtree))
tree_func(best.model, shortest_tree)

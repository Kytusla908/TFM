# Sources =============
source("data_preparation.R")
source("Pred_models.R")


# Normalization ========
pdf(file = "plots/boxplot_variables_1.pdf", width = 15, height = 8)
barplot
dev.off()

pdf(file = "plots/boxplot_variables_2.pdf", width = 15, height = 8)
norm_barplot
dev.off()


# Train Test split ==========
pdf(file = "plots/prop_table_sets.pdf")
prop_table
dev.off()


# k-NN model ==========
# Save model
kNN_model.txt <- file("outputs/kNN_model.txt")
sink(kNN_model.txt, append = TRUE, type = "output")
kNN_model
closeAllConnections()

# Save plot
pdf(file = "plots/performance_plot_kNN.pdf")
kNN_plot
dev.off()

# Save confusion matrix
kNN_matrix.txt <- file("outputs/kNN_matrix.txt")
sink(kNN_matrix.txt, append = TRUE, type = "output")
values_kNN_mod
closeAllConnections()


# Naive Bayes ============
# Save model1
NB_model1.txt <- file("outputs/NB_model1.txt")
sink(NB_model1.txt, append = TRUE, type = "output")
NB_model1
closeAllConnections()

#Save plot1
pdf(file = "plots/performance_plot_NaiveBayes1.pdf")
NB1_plot
dev.off()

# Save useKernel = TRUE models
NB_model.txt <- file("outputs/NB_model.txt")
sink(NB_model.txt, append = TRUE, type = "output")
NB_model
closeAllConnections()

#Save useKernel = TRUE plot
pdf(file = "plots/performance_plot_NaiveBayes.pdf")
NB_plot
dev.off()

# Save confusion matrix
NB_matrix.txt <- file("outputs/NB_matrix.txt")
sink(NB_matrix.txt, append = TRUE, type = "output")
values_NB_mod
closeAllConnections()


# Support Vector Machine ===========
# Save linear model
SVM_lin_model.txt <- file("outputs/SVM_lin_model.txt")
sink(SVM_lin_model.txt, append = TRUE, type = "output")
svm_lin_model
closeAllConnections()

# Save linear model confusion matrix
SVM_lin_matrix.txt <- file("outputs/SVM_lin_matrix.txt")
sink(SVM_lin_matrix.txt, append = TRUE, type = "output")
values_lin_mod
closeAllConnections()

# Save rbf model
SVM_rbf_model.txt <- file("outputs/SVM_rbf_model.txt")
sink(SVM_rbf_model.txt, append = TRUE, type = "output")
svm_rbf_model
closeAllConnections()

# Save rbf model confusion matrix
SVM_rbf_matrix.txt <- file("outputs/SVM_rbf_matrix.txt")
sink(SVM_rbf_matrix.txt, append = TRUE, type = "output")
values_rbf_mod
closeAllConnections()

# Save polinomial model
SVM_poly_model.txt <- file("outputs/SVM_poly_model.txt")
sink(SVM_poly_model.txt, append = TRUE, type = "output")
svm_poly_model
closeAllConnections()

# Save polinomial model confusion matrix
SVM_poly_matrix.txt <- file("outputs/SVM_poly_matrix.txt")
sink(SVM_poly_matrix.txt, append = TRUE, type = "output")
values_poly_mod
closeAllConnections()

#Save performance plot
pdf(file = "plots/performance_plot_SVM.pdf")
SVM_plot
dev.off()


# Random forest ==========
# Save model
RF_model.txt <- file("outputs/RF_model.txt")
sink(RF_model.txt, append = TRUE, type = "output")
RF_model
closeAllConnections()

#Save plot
pdf(file = "plots/performance_plot_RF.pdf", width = 12, height = 12)
RF_plot
dev.off()

# Save confusion matrix
RF_matrix.txt <- file("outputs/RF_matrix.txt")
sink(RF_matrix.txt, append = TRUE, type = "output")
values_RF_mod
closeAllConnections()


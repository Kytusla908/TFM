# Libraries ============
library(ggplot2)
library(reshape2)

# Input archives reading
data_chembl <- read.csv("data/data.csv",sep = ";",dec = ".")
data <- read.csv("data/CHEMBL4372_Desc.csv",sep = ",",dec = ".")
variables <- data[ ,9:127]  # Df containing only the descriptors

# DATA PREPARATION ============================
# Correlation based Feature Selection ==========
cor_matrix <- cor(variables)  # Calculate cor matrix
cor_matrix[!lower.tri(cor_matrix)] <- 0  # Erase half of the mirror matrix

# Eliminating highly correlated variables
variables_0.8 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.8, na.rm = TRUE))]
removed_var <- colnames(cor_matrix)[apply(cor_matrix, 2,
                                             function(x) any(abs(x) > 0.8, na.rm = TRUE))]
cat("No of variables after setting 0.8 as cut-off: ", ncol(variables_0.8),
    "\nEliminated variables:\n", removed_var)

'
variables_0.85 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.85, na.rm = TRUE))]
variables_0.9 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.9, na.rm = TRUE))]
cat("No of variables after setting 0.8 as cut-off: ", ncol(variables_0.8),
      "\nNo of variables after setting 0.85 as cut-off: ", ncol(variables_0.85),
      "\nNo of variables after setting 0.9 as cut-off: ", ncol(variables_0.9))
'


# Variance based Feature Selection ==========
variances <- data.frame(t(apply(variables_0.8, 2, var)))
removed_var <- colnames(variances)[which((!abs(variances) > 0))]
data_df <- variables_0.8[,apply(variances, 2,
                               function(x) any(abs(x) > 0, na.rm = TRUE))]
cat("Variable No. after eliminating variance = 0: ", ncol(data_df),
    "\nEliminated variables: ", removed_var)

# First description of the variables ==============
pdf(file = "plots/boxplot_1.pdf",
    width = 15, height = 8)
ggplot(melt(data_df), aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()

# Normalization =========
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - the results should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
input <- as.data.frame(lapply(data_df, normalize))

# boxplot normalized data
pdf(file = "plots/boxplot_2.pdf",
    width = 15, height = 8)
ggplot(melt(input), aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1))
dev.off()





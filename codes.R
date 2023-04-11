# Libraries ============
library(ggplot2)
library(reshape2)
library(dplyr)

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
# pdf(file = "plots/boxplot_variables_1.pdf", width = 15, height = 8)
ggplot(melt(data_df), aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1))
# dev.off()


# Normalization =========
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - the results should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
norm_data <- as.data.frame(lapply(data_df, normalize))

# boxplot normalized data
# pdf(file = "plots/boxplot_variables_2.pdf", width = 15, height = 8)
ggplot(melt(norm_data), aes(x = variable, y = value)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1))
# dev.off()


# Define labels =========
for (i in 1:nrow(data)) {
  #Std.value equal or bigger than 11000 will set as active
  if (data$Standard.Value[i] >= 11000){  
    norm_data$label[i] = 1  # label 1 = active
  }
  else {
    norm_data$label[i] = 0  # label 0 = inactive
    }
}
table(norm_data$label)
prop.table(table(norm_data$label))


# Train Test Split ========
set.seed(12345)
norm_data$id <- 1:nrow(norm_data)  # create temporary variable to facilitate sampling
train <- norm_data %>% dplyr::sample_frac(0.7)  # Split train set
test  <- dplyr::anti_join(norm_data, train, by = "id")  # Split test set

# Proportion tables
complete_set_prop <- as.data.frame(prop.table(table(norm_data$label)))
colnames(complete_set_prop)[1] <- c("Complete_set_prop")
train_prop <- as.data.frame(prop.table(table(train$label)))
colnames(train_prop)[1] <- c("train_prop")
test_prop <- as.data.frame(prop.table(table(test$label)))
colnames(test_prop)[1] <- c("test_prop")

# Barplot to compare proportions
all_sets <- list(complete_set_prop, train_prop, test_prop)
all_sets <- lapply(all_sets, function(dat) {
  dat$type <- colnames(dat)[1]
  colnames(dat)[1] <- "Label"
  dat
})
all_sets <- do.call(rbind, all_sets)
# pdf(file = "plots/prop_table_sets.pdf")
ggplot(all_sets,aes(x=Label, y=Freq, fill = Label)) +
  geom_col() + 
  facet_wrap(~ type, scales = "free_x")
# dev.off()


# Remove temporary variable
norm_data <- subset(norm_data, select = -c(id))
train <- subset(train, select = -c(id))
test <- subset(test, select = -c(id))


# k-NN model ==========










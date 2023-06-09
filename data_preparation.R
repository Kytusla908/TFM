source("functions.R")

# Libraries ============
library(ggplot2)
library(reshape2)
library(dplyr)


# Input archives reading
data_chembl <- read.csv("data/data.csv",sep = ";",dec = ".")
data <- read.csv("data/CHEMBL4372_Desc.csv",sep = ",",dec = ".")
variables <- data[ ,9:127]  # Df containing only the descriptors


# DATA PREPARATION ============================
#Missing values verification
table(is.na(variables))

# Correlation based Feature Selection ==========
cor_matrix <- cor(variables)  # Calculate cor matrix
cor_matrix[!lower.tri(cor_matrix)] <- 0  # Erase half of the mirror matrix

# Eliminating highly correlated variables
variables_0.8 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.8, na.rm = TRUE))]
corr_removed_var <- colnames(cor_matrix)[apply(cor_matrix, 2,
                                             function(x) any(abs(x) > 0.8, na.rm = TRUE))]
cat("No of variables after setting 0.8 as cut-off: ", ncol(variables_0.8),
    "\nEliminated variables:\n", corr_removed_var)


# Variance based Feature Selection ==========
variances <- data.frame(t(apply(variables_0.8, 2, var)))
var_removed_var <- colnames(variances)[which((!abs(variances) > 0))]
data_df <- variables_0.8[,apply(variances, 2,
                               function(x) any(abs(x) > 0, na.rm = TRUE))]
cat("Variable No. after eliminating variance = 0: ", ncol(data_df),
    "\nEliminated variables: ", var_removed_var)


# First description of the variables ==============
barplot <- ggplot(melt(data_df), aes(x = variable, y = value)) +
  labs(title = "Descriptors values distribution boxplot", x = "Descriptors") + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),
                         plot.title = element_text(size = 22, hjust = 0.5))


# Normalization =========
# test normalization function - the results should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
norm_data <- as.data.frame(lapply(data_df, normalize))

# boxplot normalized data
norm_barplot <- ggplot(melt(norm_data), aes(x = variable, y = value)) +
  labs(title = "Normalized descriptors values distribution boxplot", x = "Descriptors") + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),
                         plot.title = element_text(size = 22, hjust = 0.5))

# print out into the same pdf
'
library(gridExtra)
p <- ggplot(melt(data_df), aes(x = variable, y = value)) +
  labs(title = "Descriptors values distribution boxplot", x = "Descriptors", tag = "A") + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),
                         plot.title = element_text(size = 22, hjust = 0.5),
                         plot.tag = element_text(size = 16))
  
q <- ggplot(melt(norm_data), aes(x = variable, y = value)) +
  labs(title = "Normalized descriptors values distribution boxplot", x = "Descriptors", tag = "B") +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust=1),
                         plot.title = element_text(size = 22, hjust = 0.5),
                         plot.tag = element_text(size = 16))
pdf(file = "plots/melted_boxplot.pdf", onefile = TRUE, width = 15, height = 16)
grid.arrange(p,q)
dev.off()
'

# Define labels =========
label <- c()
for (i in 1:nrow(data)) {
  #Std.value equal or bigger than 11000 will set as active
  if (data$Standard.Value[i] >= 11000){  
    label[i] = 0  # label 0 = inactive
  }
  else {
    label[i] = 1  # label 1 = active
    }
}
norm_data <- cbind(label, norm_data)
norm_data <- cbind(data$Molecule.ChEMBL.ID, norm_data)
norm_data$label <- as.factor(norm_data$label)
table(norm_data$label)
prop.table(table(norm_data$label))


# Train Test Split ========
norm_data$id <- 1:nrow(norm_data)  # create temporary variable to facilitate sampling
set.seed(12345)
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
all_sets <- list(complete_set_prop, train_prop, test_prop)  # Setting all tables to the same data frame
all_sets <- lapply(all_sets, function(dat) {  # Add "type" and "label" column
  dat$type <- colnames(dat)[1]
  colnames(dat)[1] <- "Label"
  dat
})
all_sets <- do.call(rbind, all_sets)  # Include all tables to the same data frame

# store no of observations in each group
no_of_obs <- c(table(norm_data$label)[[1]],table(norm_data$label)[[2]],
               table(train$label)[[1]],table(train$label)[[2]],
               table(test$label)[[1]],table(test$label)[[2]])

# Bind all data about the proportions
all_sets <- cbind(all_sets, no_of_obs)

# Create barplot
prop_table <- ggplot(all_sets,aes(x=Label, y=Freq, fill = Label)) +
  geom_col() + 
  geom_label( label = all_sets$no_of_obs) +
  facet_wrap(~ type, scales = "free_x")
prop_table

# Remove temporary variable
norm_data <- subset(norm_data, select = -c(id))
train <- subset(train, select = -c(id))
test <- subset(test, select = -c(id))


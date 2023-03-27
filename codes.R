# Input archives reading
data_chembl <- read.csv("data/data.csv",sep = ";",dec = ".")
data <- read.csv("data/CHEMBL4372_Desc.csv",sep = ",",dec = ".")
variables <- data[ ,9:127]  # Df containing only the descriptors

# DATA PREPARATION ============================
# Correlation based Feature Selection ==========
cor_matrix <- cor(variables)  # Calculation cor matrix
cor_matrix[!lower.tri(cor_matrix)] <- 0  # Erasing half of the mirror matrix

# Eliminating highly correlated variables
variables_0.8 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.8, na.rm = TRUE))]
variables_0.85 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.85, na.rm = TRUE))]
variables_0.9 <- variables[, !apply(cor_matrix, 2,
                                    function(x) any(abs(x) > 0.9, na.rm = TRUE))]
cat("N? Variables usando 0.8 como punto de corte: ", ncol(variables_0.8),
      "\nN? Variables usando 0.85 como punto de corte: ", ncol(variables_0.85),
      "\nN? Variables usando 0.9 como punto de corte: ", ncol(variables_0.9))

# Variance based Feature Selection ==========




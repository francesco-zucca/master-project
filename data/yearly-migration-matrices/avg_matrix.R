# Load the required package
library(tidyverse)

# Select years
years <- 2014:2021
matrix_list <- list()

# Load all matrices
for (year in years) {
  file_name <- paste0("data/yearly-migration-matrices/migration_matrix_", year, ".csv")
  if (file.exists(file_name)) {
    matrix_list[[as.character(year)]] <- read.csv(file_name, check.names = FALSE)
  } else {
    warning(paste("File missing:", file_name))
  }
}

# Detach the ID columns to keep only the numeric part
id_cols <- matrix_list[[1]][, c("mx_state", "mx_municipality")]

# Isolate just the numeric columns for all years in the list
numeric_list <- lapply(matrix_list, function(df) select(df, -mx_state, -mx_municipality))

# Stack the list of dataframes into an array
arr <- array(
  unlist(numeric_list), 
  dim = c(nrow(numeric_list[[1]]), ncol(numeric_list[[1]]), length(numeric_list))
)

# Calculate the mean
avg_numeric_matrix <- apply(arr, c(1, 2), mean, na.rm = TRUE)

# Convert back to a dataframe and restore the column names
avg_numeric <- as.data.frame(avg_numeric_matrix)
colnames(avg_numeric) <- colnames(numeric_list[[1]])

# Recombine with the mx_state and mx_municipality columns
avg_matrix <- cbind(id_cols, avg_numeric)

# Make matrix with rows summing to 1
row_totals <- rowSums(avg_numeric, na.rm = TRUE)
clean_rows_num <- avg_numeric / ifelse(row_totals == 0, 1, row_totals) 
row_matrix <- cbind(id_cols, clean_rows_num)

# Make matrix with columns summing to 1
col_totals <- colSums(avg_numeric, na.rm = TRUE)
clean_cols_num <- sweep(avg_numeric, MARGIN = 2, STATS = col_totals, FUN = "/")
col_matrix <- cbind(id_cols, clean_cols_num)

# Sanity checks
print(paste("NAs in average matrix:", sum(is.na(avg_matrix))))
print(paste("NAs in row matrix:", sum(is.na(row_matrix))))
print(paste("NAs in col matrix:", sum(is.na(col_matrix))))

# Check that rows actually sum to 1
sanity_check_rows <- row_matrix %>% mutate(row_total = rowSums(across(-c(mx_state, mx_municipality)), na.rm = TRUE))
print(head(sanity_check_rows))

# Check that columns actually sum to 1
sanity_check_cols <- colSums(select(col_matrix, -mx_state, -mx_municipality), na.rm = TRUE)
print(head(sanity_check_cols))

# Export the matrices as clean CSVs
write.csv(avg_matrix, "data/migration_matrix_avg.csv", row.names = FALSE)
write.csv(row_matrix, "data/migration_matrix_rows.csv", row.names = FALSE)
write.csv(col_matrix, "data/migration_matrix_cols.csv", row.names = FALSE)

cat("Successfully calculated and exported your final CSV matrices!\n")
# Install the required R packages
install.packages(c("zoo","tibble", "dplyr", "readr", "ggplot2", "lubridate", "caret", "randomForest", "viridis", "zoo", "mice", "purrr"))

# Load the required libraries
library(tibble)
library(mice)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(caret)
library(randomForest)
library(zoo)
library(purrr)
library(zoo)


# Define the file paths for your CSV files
file_paths <- c(
  "C:/Users/GN898ZK/OneDrive - EY/sales.csv",
  "C:/Users/GN898ZK/OneDrive - EY/inventory.csv",
  "C:/Users/GN898ZK/OneDrive - EY/feedback.csv",
  "C:/Users/GN898ZK/OneDrive - EY/products.csv"
)

# Initialize a list to store data frames
data_frames <- list()

# Function to read and process CSV files
read_csv_file <- function(file_path) {
  tryCatch({
    data <- read.csv(file_path)
    cat("Processing file:", file_path, "\n")
    
    # Perform operations specific to each file
    if (file_path == "C:/Users/GN898ZK/OneDrive - EY/sales.csv") {
      data$Date <- as.Date(data$Date)
      data <- na.omit(data)
    } else if (file_path == "C:/Users/GN898ZK/OneDrive - EY/inventory.csv") {
      # Operations specific to inventory.csv
    } else if (file_path == "C:/Users/GN898ZK/OneDrive - EY/feedback.csv") {
      # Operations specific to feedback.csv
    } else if (file_path == "C:/Users/GN898ZK/OneDrive - EY/products.csv") {
      # Operations specific to products.csv
    }
    
    return(data)
  }, error = function(e) {
    cat("Error:", file_path, "not found or could not be read.", e$message, "\n")
    return(NULL)
  }, warning = function(w) {
    cat("Warning:", file_path, "is empty. Please provide a non-empty file.", "\n")
    return(NULL)
  })
}

# Loop through file paths and read data into data frames
for (file_path in file_paths) {
  data <- read_csv_file(file_path)
  if (!is.null(data)) {
    data_frames[[basename(file_path)]] <- data
  }
}

# Now, you can access your data frames using data_frames["sales.csv"], data_frames["inventory.csv"], etc.

# Check if "sales.csv" and "products.csv" are in the data frames list
if ("sales.csv" %in% names(data_frames) && "products.csv" %in% names(data_frames)) {
  # Extract the relevant data frames
  sales <- data_frames[["sales.csv"]]
  products <- data_frames[["products.csv"]]
  
  # Function to calculate top products
  calculate_top_products <- function(sales, products) {
    product_sales <- sales %>%
      group_by(Product_ID) %>%
      summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE))
    top_products <- product_sales %>%
      left_join(products, by = "Product_ID")
    return(top_products)
  }
  
  # Calculate top products
  top_products <- calculate_top_products(sales, products)
  
  # Sort products by sales amount in descending order and select top 10
  top_products <- top_products %>%
    arrange(desc(Total_Revenue)) %>%
    head(10)
  
  # Create the ggplot with the "viridis" color palette
  library(viridis)
  ggplot(top_products, aes(x = Total_Revenue, y = Product_Name)) +
    geom_bar(stat = "identity", aes(fill = Total_Revenue)) +
    scale_fill_viridis() +  # Use a continuous color scale
    labs(x = "Total Revenue", y = "Product Name", title = "Top 10 Products by Total Revenue")
} else {
  cat("Please make sure you have 'sales.csv' and 'products.csv' in your data frames.", "\n")
}

# Analyzing Sales Over Time

# Read the data with the correct date format
sales <- read.csv("sales.csv")

# Convert the "Date" column to Date format
sales$Date <- as.Date(sales$Date, format = "%d-%m-%Y")
sales$Year <- factor(year(sales$Date))
sales$Month <- factor(month(sales$Date, label = TRUE))

# Now, use the correct Year and Month columns in the group_by function:
monthly_sales <- sales %>%
  group_by(Year, Month) %>%
  summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE), .groups = 'keep')

# Finally, create the ggplot visualization with the corrected columns.
ggplot(monthly_sales, aes(x = Month, y = Total_Revenue, fill = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Monthly Sales Over Time")


# Analyzing Sales by Category
products <- read.csv("products.csv")

tryCatch({
  category_sales <- sales %>%
    left_join(products, by = "Product_ID") %>%
    group_by(Category) %>%
    summarise(Total_Revenue = sum(Total_Revenue, na.rm = TRUE))
  
  ggplot(category_sales, aes(x = Total_Revenue, y = Category)) +
    geom_bar(stat = "identity") +
    labs(title = "Sales by Category")
}, error = function(e) {
  print(paste0("Error analyzing sales by category: ", e$message))
})

# Time-Based Features
tryCatch({
  sales$Day_of_Week <- wday(sales$Date)
  sales$Week_of_Year <- isoweek(sales$Date)
  sales$Month <- month(sales$Date)
  sales$Quarter <- quarter(sales$Date)
}, error = function(e) {
  print(paste0("Error creating time-based features: ", e$message))
})

# Lagged Features (let's create for 1, 3, and 7 days lag)
tryCatch({
  for (lag in c(1, 3, 7)) {
    sales[paste0("lag_", lag)] <- lag(sales$Total_Revenue, lag)
  }
}, error = function(e) {
  print(paste0("Error creating lagged features: ", e$message))
})

# Rolling Window Features (let's create rolling sum & mean for windows of 3 and 7 days)
tryCatch({
  for (window in c(3, 7)) {
    # Calculate rolling sums and means
    rolling_sum <- zoo::rollsum(sales$Total_Revenue, window, fill = NA)
    rolling_mean <- zoo::rollmean(sales$Total_Revenue, window, fill = NA)
    
    # Define the column names
    sum_col_name <- paste0("rolling_sum_", window)
    mean_col_name <- paste0("rolling_mean_", window)
    
    # Check if the columns exist and have the correct length
    if (!sum_col_name %in% colnames(sales)) {
      sales[, sum_col_name] <- NA
    }
    if (!mean_col_name %in% colnames(sales)) {
      sales[, mean_col_name] <- NA
    }
    
    # Ensure that the length of rolling_sum and rolling_mean matches the number of rows in sales
    if (length(rolling_sum) != nrow(sales)) {
      rolling_sum <- c(rep(NA, window - 1), rolling_sum)
    }
    if (length(rolling_mean) != nrow(sales)) {
      rolling_mean <- c(rep(NA, window - 1), rolling_mean)
    }
    
    # Assign the rolling vectors to the sales dataframe
    sales[, sum_col_name] <- rolling_sum
    sales[, mean_col_name] <- rolling_mean
  }
}, error = function(e) {
  print(paste0("Error creating rolling window features: ", e$message))
})

# Drop NA values created due to lagged features
tryCatch({
  sales <- na.omit(sales)
}, error = function(e) {
  print(paste0("Error dropping NA values: ", e$message))
})

# Define your features and target variable
features <- c(
  'Units_Sold', 'Year', 'Month', 'Day_of_Week', 'Week_of_Year', 'Quarter',
  'lag_1', 'lag_3', 'lag_7', 'rolling_sum_3', 'rolling_mean_3',
  'rolling_sum_7', 'rolling_mean_7'
)
target <- 'Total_Revenue'

# Split the data into training and testing sets
set.seed(42)
train_index <- createDataPartition(sales$Total_Revenue, p = 0.8, list = FALSE)
X_train <- sales[features][train_index, ]
X_test <- sales[features][-train_index, ]

# Define your target variable for the test set
y_test <- sales$Total_Revenue[-train_index]

# Identify categorical variables (assuming they are of character or factor type)
categorical_vars <- sapply(X_train, is.factor) | sapply(X_train, is.character)

# Convert categorical variables to factors
X_train[categorical_vars] <- lapply(X_train[categorical_vars], as.factor)

# Check for missing values in predictor variables in X_train
if (anyNA(X_train)) {
  stop("There are missing values in predictor variables in X_train. Please check your data.")
} else {
  cat("No missing values in predictor variables in X_train.\n")
}

# Define your target variable
y_train <- sales$Total_Revenue[train_index]

# Load the necessary library for caret
library(caret)

# Define hyperparameter ranges
ntree_range <- seq(50, 300, by = 50)  # Example range for ntree
mtry_range <- seq(2, ncol(X_train), by = 1)  # Example range for mtry

# Initialize variables to store results
best_ntree <- NULL
best_mtry <- NULL
best_rmse <- Inf  # Initialize with a high value

# Perform grid search using cross-validation
for (ntree in ntree_range) {
  for (mtry in mtry_range) {
    # Perform k-fold cross-validation (e.g., 5-fold)
    set.seed(42)  # Set a seed for reproducibility
    folds <- createFolds(y_train, k = 5, list = TRUE)
    rmse_values <- c()
    
    for (fold in folds) {
      train_indices <- unlist(folds[-fold])
      test_indices <- fold
      
      train_data <- X_train[train_indices, ]
      test_data <- X_train[test_indices, ]
      
      # Ensure that train_data and test_data have the same columns
      common_columns <- intersect(colnames(train_data), colnames(test_data))
      train_data <- train_data[, common_columns]
      test_data <- test_data[, common_columns]
      
      # Check if 'Units_Sold' column exists in both train_data and test_data
      if (!"Units_Sold" %in% colnames(train_data) || !"Units_Sold" %in% colnames(test_data)) {
        stop("The 'Units_Sold' column is missing in train_data or test_data.")
      }
      
      # Create a random forest model with the current hyperparameters
      rf_model <- randomForest(y_train[train_indices] ~ ., data = train_data, ntree = ntree, mtry = mtry)
      
      # Make predictions on the test data
      predictions <- predict(rf_model, newdata = test_data)
      
      # Calculate RMSE (Root Mean Squared Error)
      rmse <- sqrt(mean((predictions - y_train[test_indices])^2))
      rmse_values <- c(rmse_values, rmse)
    }
    
    # Calculate the mean RMSE across folds
    mean_rmse <- mean(rmse_values)
    
    # Check if this combination of hyperparameters is better
    if (mean_rmse < best_rmse) {
      best_rmse <- mean_rmse
      best_ntree <- ntree
      best_mtry <- mtry
    }
  }
}

# Print the best hyperparameters
cat("Best ntree:", best_ntree, "\n")
cat("Best mtry:", best_mtry, "\n")


# Train the final model using the best hyperparameters
final_rf_model <- randomForest(y_train ~ ., data = X_train, ntree = best_ntree, mtry = best_mtry)

# Check if best_ntree and best_mtry are properly defined
if (is.na(best_ntree) || is.na(best_mtry)) {
  stop("Please define best_ntree and best_mtry before fitting the final model.")
} else {
  cat("best_ntree and best_mtry are defined. Continuing with fitting the final model.\n")
}


# Add the 'Total_Revenue' column to X_train
X_train$Total_Revenue <- y_train

# Create a vector of Week_of_Year values (replace with your actual data)
week_of_year_values <- c(30, 31, 32)  # Replace with your data

# Create a data frame with the same columns as X_train
new_data <- data.frame(
  Units_Sold = c(25, 30, 15),  # Replace with your data
  Year = factor(c(2023, 2023, 2023), levels = levels(X_train$Year)),
  Month = factor(c("January", "February", "March"), levels = levels(X_train$Month)),
  Day_of_Week = factor(c("Monday", "Tuesday", "Wednesday"), levels = levels(X_train$Day_of_Week)),
  Temperature = c(10, 15, 8),  # Replace with your actual temperature values
  Quarter = factor(c(1, 1, 1), levels = levels(X_train$Quarter)),  # Add Quarter as a factor
  Week_of_Year = week_of_year_values,  # Add Week_of_Year as a vector
  lag_1 = c(5, 8, 10),  # Replace with your lag_1 values
  lag_3 = c(15, 20, 12),  # Add lag_3 values
  lag_7 = c(35, 40, 28),  # Add lag_7 values
  rolling_sum_3 = c(75, 85, 62),  # Add rolling_sum_3 values
  rolling_mean_3 = c(25, 28.33, 20.67),  # Add rolling_mean_3 values
  rolling_sum_7 = c(175, 200, 140),  # Add rolling_sum_7 values
  rolling_mean_7 = c(25, 28.57, 20)  # Add rolling_mean_7 values
)

# Assuming you have the Total_Revenue values, add them to new_data
new_data$Total_Revenue <- c(1000, 1200, 800)  # Replace with your actual Total_Revenue values


# Add the 'Week_of_Year' column to 'new_data'
new_data$Week_of_Year <- week_of_year_values

# Add the 'Total_Revenue' column to X_train
X_train$Total_Revenue <- y_train

# Check column names of X_train
colnames_X_train <- colnames(X_train)

# Check column names of new_data
colnames_new_data <- colnames(new_data)

# Compare column names
if (identical(colnames_X_train, colnames_new_data)) {
  cat("Column names match between X_train and new_data.\n")
} else {
  cat("Column names do not match between X_train and new_data.\n")
}

# Set column names of new_data to match X_train
colnames(new_data) <- colnames_X_train

####################Checking##########

# Check data types in X_train for categorical variables
categorical_vars_train <- sapply(X_train, is.factor)
data_types_train <- sapply(X_train[categorical_vars_train], class)
data_types_train

# Check data types in new_data for categorical variables
categorical_vars_new <- sapply(new_data, is.factor)
data_types_new <- sapply(new_data[categorical_vars_new], class)
data_types_new

# List of categorical variables in your data
categorical_vars <- c("Year", "Month", "Day_of_Week", "Quarter")

# Loop through each categorical variable
for (var in categorical_vars) {
  # Check levels in X_train
  levels_train <- levels(X_train[[var]])
  
  # Check levels in new_data
  levels_new <- levels(new_data[[var]])
  
  # Compare levels
  if (!identical(levels_train, levels_new)) {
    cat("Warning: Levels of", var, "do not match between X_train and new_data.\n")
    
    # Optionally, you can choose to align or preprocess the data here.
    
    # For example, to align levels, you can use the following code:
    # levels(new_data[[var]]) <- levels_train
  } else {
    cat("Levels of", var, "match between X_train and new_data.\n")
  }
}


# Load the dplyr package
library(dplyr)

# Check for missing values in new_data
if (anyNA(new_data)) {
  cat("There are missing values in new_data. Imputing...\n")
  
  # Identify columns with missing values
  missing_cols <- colnames(new_data)[apply(is.na(new_data), 2, any)]
  
  # Loop through missing columns
  for (col in missing_cols) {
    if (anyNA(new_data[, col])) {  # Check if there are missing values in this column
      
      # Check the data type of the column (numeric, character, factor, etc.)
      col_data_type <- class(new_data[, col])
      
      if (col_data_type == "numeric") {
        # For numeric columns, impute with mean
        new_data <- new_data %>%
          mutate({{col}} := ifelse(is.na({{col}}), mean({{col}}, na.rm = TRUE), {{col}}))
      } else if (col_data_type == "character") {
        # For character columns, impute with mode (most frequent value)
        mode_val <- names(sort(table(new_data[, col]), decreasing = TRUE))[1]
        new_data <- new_data %>%
          mutate({{col}} := ifelse(is.na({{col}}), mode_val, {{col}}))
      } else if (col_data_type == "factor") {
        # For factor columns, impute with the most frequent level
        mode_level <- levels(new_data[, col])[which.max(table(new_data[, col]))]
        new_data <- new_data %>%
          mutate({{col}} := ifelse(is.na({{col}}), mode_level, {{col}}))
      } else {
        # Handle other data types as needed or show a warning
        cat("Warning: Unsupported data type in column", col, "\n")
      }
    }
  }
  
  cat("Missing values imputed.\n")
} else {
  cat("No missing values in predictor variables in new_data.\n")
}


# Loop through missing columns
for (col in missing_cols) {
  if (anyNA(new_data[, col])) {  # Check if there are missing values in this column
    if (is.numeric(new_data[, col])) {
      # For numeric columns, impute with mean
      if (anyNA(new_data[, col])) {  # Check again for NA values
        new_data[is.na(new_data[, col]), col] <- mean(new_data[, col], na.rm = TRUE)
      }
    } else if (is.character(new_data[, col])) {
      # For character columns, impute with mode (most frequent value)
      if (anyNA(new_data[, col])) {  # Check again for NA values
        mode_val <- names(sort(table(new_data[, col]), decreasing = TRUE))[1]
        new_data[is.na(new_data[, col]), col] <- mode_val
      }
    } else if (is.factor(new_data[, col])) {
      # For factor columns, impute with the most frequent level
      if (anyNA(new_data[, col])) {  # Check again for NA values
        mode_level <- levels(new_data[, col])[which.max(table(new_data[, col]))]
        new_data[is.na(new_data[, col]), col] <- mode_level
      }
    } else {
      # Handle other data types as needed
      cat("Warning: Unsupported data type in column", col, "\n")
    }
  } else {
    cat("No missing values in column", col, "\n")
  }
}

# Check data types of predictors in X_train
data_types_train <- sapply(X_train, class)

# Check data types of predictors in new_data
data_types_new <- sapply(new_data, class)

# Compare data types
identical(data_types_train, data_types_new)

# Check levels of categorical variables in X_train
for (var in categorical_vars) {
  levels_train <- levels(X_train[[var]])
  levels_new <- levels(new_data[[var]])
  cat("Variable:", var, "\n")
  cat("Levels in X_train:", levels_train, "\n")
  cat("Levels in new_data:", levels_new, "\n")
}

# Impute missing values in new_data if necessary (you've already done this)

# Ensure levels match for categorical variables in new_data
for (var in categorical_vars) {
  levels_train <- levels(X_train[[var]])
  levels_new <- levels(new_data[[var]])
  if (!identical(levels_train, levels_new)) {
    cat("Warning: Levels of", var, "do not match between X_train and new_data.\n")
    # Optionally, align or preprocess the data here.
    # For example, to align levels, you can use the following code:
    # levels(new_data[[var]]) <- levels_train
  } else {
    cat("Levels of", var, "match between X_train and new_data.\n")
  }
}



# Check data types of predictors in X_train
data_types_train <- sapply(X_train, class)

# Check data types of predictors in new_data
data_types_new <- sapply(new_data, class)

# Compare data types
data_type_mismatches <- data_types_train != data_types_new

# Identify which variables have data type mismatches
mismatched_vars <- names(data_types_train[data_type_mismatches])

# Display the mismatched variables
mismatched_vars

# Check for data type mismatches
if (any(data_type_mismatches)) {
  cat("Data type mismatches found between X_train and new_data.\n")
  # Output the details of the mismatches
  print(data_type_mismatches)
} else {
  cat("No data type mismatches between X_train and new_data.\n")
}


# Check levels of categorical variables in X_train
for (var in categorical_vars) {
  if (var %in% names(X_train)) {
    levels_train <- levels(X_train[[var]])
    levels_new <- levels(new_data[[var]])
    
    cat("Variable:", var, "\n")
    cat("Levels in X_train:", levels_train, "\n")
    cat("Levels in new_data:", levels_new, "\n")
    
    if (!identical(levels_train, levels_new)) {
      cat("Warning: Levels of", var, "do not match between X_train and new_data.\n")
      # Optionally, align or preprocess the data here.
      # For example, to align levels, you can use the following code:
      # levels(new_data[[var]]) <- levels_train
    } else {
      cat("Levels of", var, "match between X_train and new_data.\n")
    }
  } else {
    cat("Variable:", var, "is not present in X_train.\n")
  }
}

# Loop through each categorical variable
for (var in categorical_vars) {
  if (var %in% names(X_train)) {
    levels_train <- levels(X_train[[var]])
    levels_new <- levels(new_data[[var]])
    
    cat("Variable:", var, "\n")
    cat("Levels in X_train:", levels_train, "\n")
    cat("Levels in new_data:", levels_new, "\n")
    
    # Check if levels are different
    if (!identical(levels_train, levels_new)) {
      cat("Warning: Levels of", var, "do not match between X_train and new_data.\n")
      # You can choose to align or preprocess the data here.
      # To align levels, you can use the following code:
      levels(new_data[[var]]) <- levels_train
      cat("Fixed levels for", var, "in new_data.\n")
    } else {
      cat("Levels of", var, "match between X_train and new_data.\n")
    }
  } else {
    cat("Variable:", var, "is not present in X_train.\n")
  }
}

# Define variables with data type mismatches and their correct data types
data_type_fixes <- list(
  "Units_Sold" = "numeric",
  "Month" = "factor",
  "Day_of_Week" = "factor",
  "Quarter" = "factor",
  "lag_1" = "numeric",
  "lag_3" = "numeric",
  "lag_7" = "numeric",
  "rolling_sum_3" = "numeric",
  "rolling_sum_7" = "numeric",
  "Total_Revenue" = "numeric"  # Include Total_Revenue with its correct data type
)
# Check if "Total_Revenue" variable exists in new_data
if ("Total_Revenue" %in% colnames(new_data)) {
  cat("The 'Total_Revenue' variable is present in new_data.\n")
  
  # Optionally, you can print the first few rows to inspect the data
  head(new_data["Total_Revenue"])
} else {
  cat("The 'Total_Revenue' variable is not present in new_data.\n")
}

# Loop through variables with data type mismatches and fix the data types
for (var in mismatched_vars) {
  if (var %in% names(data_type_fixes)) {
    # Check if the variable exists in new_data
    if (var %in% colnames(new_data)) {
      if (data_type_fixes[[var]] == "factor") {
        # Convert to factor
        new_data[[var]] <- factor(new_data[[var]])
      } else {
        # For other data types, use as()
        new_data[[var]] <- as(new_data[[var]], data_type_fixes[[var]])
      }
      cat("Fixed data type for", var, "in new_data.\n")
    } else {
      cat("Variable", var, "is not present in new_data.\n")
    }
  } else {
    cat("No data type fix defined for", var, "in new_data.\n")
  }
}

# Verify that data types now match
data_types_new_fixed <- sapply(new_data, class)
identical(data_types_train, data_types_new_fixed)


# Identify numeric columns in new_data
numeric_cols <- sapply(new_data, is.numeric)

# Impute missing values in numeric columns with the mean value
new_data[, numeric_cols][is.na(new_data[, numeric_cols])] <- colMeans(new_data[, numeric_cols], na.rm = TRUE)



# Fit the final model using best_ntree and best_mtry
final_model <- randomForest(Total_Revenue ~ ., data = X_train, ntree = best_ntree, mtry = best_mtry)

# Check the formula of the final model
model_formula <- formula(final_model)
cat("Model Formula:", as.character(model_formula), "\n")

# Make predictions using the final model and new_data
predicted_value <- predict(final_model, newdata = new_data)

# Check column names
colnames(new_data)
colnames(X_train)


# Predictions on the test set using the final model
y_pred <- predict(final_model, newdata = X_test)

# Calculate and print metrics
mae <- mean(abs(y_test - y_pred))
rmse <- sqrt(mean((y_test - y_pred)^2))
r2 <- cor(y_test, y_pred)^2

print(paste0('MAE: ', mae))
print(paste0('RMSE: ', rmse))
print(paste0('R2 Score: ', r2))

# Initialize the scaler
scaler <- tryCatch(
  preProcess(X_train, method = c("center", "scale")),
  error = function(e) {
    cat("Error occurred during scaling:", conditionMessage(e), "\n")
    NULL
  }
)

# Check if the scaler object is not empty
if (!is.null(scaler)) {
  # The scaler object is not empty
  # Extract center and scale components from the scaler object
  center <- scaler$center
  scale <- scaler$scale
  
  # Scale the test data using the scaler object
  X_test_scaled <- predict(scaler, newdata = X_test)
  
  # Predictions on the scaled test set using the final model
  y_pred_scaled <- predict(final_model, newdata = X_test_scaled)
  
  # Calculate and print scaled metrics
  mae_scaled <- mean(abs(y_test - y_pred_scaled))
  rmse_scaled <- sqrt(mean((y_test - y_pred_scaled)^2))
  r2_scaled <- cor(y_test, y_pred_scaled)^2
  
  print(paste0('Scaled MAE: ', mae_scaled))
  print(paste0('Scaled RMSE: ', rmse_scaled))
  print(paste0('Scaled R2 Score: ', r2_scaled))
} else {
  # The scaler object is empty
  cat("Scaler object is NULL. Check your data and preprocessing steps.\n")
}


# Identify numeric columns in X_train
numeric_cols <- sapply(X_train, is.numeric)

# Calculate center and scale vectors based on training data for numeric columns
center <- colMeans(X_train[, numeric_cols], na.rm = TRUE)
scale <- apply(X_train[, numeric_cols], 2, sd, na.rm = TRUE)


required_vars <- c("Units_Sold", "Year", "Month", "Day_of_Week", "Week_of_Year", "Quarter", "lag_1", "lag_3", "lag_7", "rolling_sum_3", "rolling_mean_3", "rolling_sum_7", "rolling_mean_7")

missing_vars <- setdiff(required_vars, colnames(new_data))

if (length(missing_vars) > 0) {
  stop(paste("The following variables are missing in 'new_data':", paste(missing_vars, collapse = ", ")))
}

# Identify numeric columns in new_data
numeric_cols_new <- sapply(new_data, is.numeric)

# Verify that numeric_cols_new identifies the correct columns
cat("Numeric Columns in new_data:\n")
cat(colnames(new_data)[numeric_cols_new])

# Verify the center vector has values for each numeric column
cat("Center vector:\n")
cat(center)

# Verify the scale vector has values for each numeric column
cat("Scale vector:\n")
cat(scale)


if ("Week_of_Year" %in% colnames(new_data)) {
  # Continue with scaling and removing missing values
} else {
  stop("Column 'Week_of_Year' is missing in new_data.")
}


# Create a copy of new_data for scaling
new_data_scaled <- new_data

# Scale only the numeric variables in new_data_scaled
new_data_scaled[, numeric_cols_new] <- scale(new_data_scaled[, numeric_cols_new])

# Convert new_data_scaled to a matrix
new_data_scaled_matrix <- as.matrix(new_data_scaled)

# Remove rows with missing values
new_data_scaled_matrix_no_na <- na.omit(new_data_scaled_matrix)

# Check if 'Week_of_Year' is in the column names of 'new_data_scaled_matrix_no_na'
if ("Week_of_Year" %in% colnames(new_data_scaled_matrix_no_na)) {
  cat("The 'Week_of_Year' variable exists in 'new_data_scaled_matrix_no_na'.\n")
} else {
  stop("The 'Week_of_Year' variable is missing in 'new_data_scaled_matrix_no_na'.")
}


# Check if 'Units_Sold' is in the column names of 'new_data_scaled_matrix_no_na'
if ("Units_Sold" %in% colnames(new_data_scaled_matrix_no_na)) {
  cat("The 'Units_Sold' variable exists in 'new_data_scaled_matrix_no_na'.\n")
  
  # Get the column index of 'Units_Sold'
  col_index <- which(colnames(new_data_scaled_matrix_no_na) == "Units_Sold")
  
  # Check the data type of 'Units_Sold'
  if (!is.numeric(new_data_scaled_matrix_no_na[, col_index])) {
    # If it's not numeric, try converting it to numeric
    new_data_scaled_matrix_no_na[, col_index] <- as.numeric(new_data_scaled_matrix_no_na[, col_index])
  }
  
  # Use the trained model to make a prediction
  predicted_value <- predict(final_model, newdata = new_data_scaled_matrix_no_na)
  
  # Print the predicted value
  print(paste0('Predicted Value: ', predicted_value))
} else {
  stop("The 'Units_Sold' variable is missing in 'new_data_scaled_matrix_no_na'. Please add it.")
}

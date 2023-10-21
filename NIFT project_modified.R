# Install devtools if you don't have it
install.packages("devtools")
# Load devtools
library(devtools)
# Install imputeTS from GitHub
install_github("SteffenMoritz/imputeTS")
install.packages("VIM")
install.packages("prophet")
install.packages("lpSolve")

# Load Necessary Libraries
library(readr)
library(dplyr)
library(forecast)
library(ggplot2)
library(zoo)  # for na.approx function
library(imputeTS)  # for na.knn
library(VIM)
library(prophet)
library(lpSolve)



# Set Working Directory
setwd("C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App")

# Define the file paths for your CSV files
file_paths <- list(
  sales = "sales.csv",
  inventory = "inventory.csv",
  feedback = "feedback.csv",
  products = "products.csv",
  customer_metrics = "customer_metrics.csv",
  customer_demographics = "customer_demographics.csv"
)

# Read the CSV files into a list of data frames
datasets <- lapply(file_paths, function(fp) read_csv(file.path(getwd(), fp)))

# Assign datasets to meaningful variable names
names(datasets) <- names(file_paths)

# Define a list of product IDs you want to analyze
product_ids <- c("P001", "P002", "P003")  # Add more product IDs as needed

# Define a list of forecasting methods you want to compare
forecast_methods <- c("SMA", "ETS")  # Add more methods as needed

# Define the forecast horizon (e.g., 5 days)
forecast_horizon <- 5

# Print working directory
print(paste("Working Directory:", getwd()))

# Directory to save plots
plot_dir <- "plots"
dir.create(file.path(getwd(), plot_dir), showWarnings = FALSE)

# Modified create_plot function
create_plot <- function(plot_data, product_id, method) {
  p <- ggplot(plot_data, aes(x = Date)) +
        geom_line(aes(y = Actual_Units_Sold, color = "Actual")) +
        geom_point(aes(y = Forecasted_Units_Sold, color = "Forecasted")) +
        labs(
          title = paste("Product:", product_id, "Method:", method),
          x = "Date",
          y = "Units Sold"
        ) +
        scale_color_manual(
          name = "Legend",
          values = c("Actual" = "blue", "Forecasted" = "red")
        ) +
        theme_minimal()
  return(p)
}

# Loop through products and forecasting methods

for (product_id in product_ids) {
  for (method in forecast_methods) {
    # Re-initialize forecast_values to NULL at the beginning of each method loop
    forecast_values <- NULL  
    
    # Filter the data for the current product
    product_sales <- datasets$sales %>%
      filter(Product_ID == product_id) %>%
      mutate(Date = as.Date(Date))  # Convert Date to a date type
    
    # Calculate the forecast using the current method
    if (method == "ETS") {
      ets_model <- ets(product_sales$Units_Sold)
      forecast_values <- as.numeric(forecast(ets_model, h = forecast_horizon)$mean)
    } else if (method == "SMA") {  
      # For simplicity, using a rolling mean with a window of 3 as an example for SMA
      forecast_values <- rep(mean(tail(product_sales$Units_Sold, 3)), forecast_horizon)
    }
    
    # Extract actual Units_Sold for the forecasted period
    min_forecast_date <- max(product_sales$Date) + 1
    max_forecast_date <- max(product_sales$Date) + forecast_horizon
    
    actual_sales <- datasets$sales %>%
      filter(Product_ID == product_id & Date >= min_forecast_date & Date <= max_forecast_date) %>%
      summarise(Actual_Units_Sold = sum(Units_Sold))
    
    # Check if actual sales data is available
    if (nrow(actual_sales) > 0) {
      # Create a data frame for plotting
      plot_data <- data.frame(
        Date = c(product_sales$Date, seq.Date(from = max(product_sales$Date) + 1, by = "days", length.out = forecast_horizon)),
        Actual_Units_Sold = c(product_sales$Units_Sold, rep(NA, forecast_horizon)),
        Forecasted_Units_Sold = c(rep(NA, length(product_sales$Date)), forecast_values)
      )
      
      plot_data_imputed <- plot_data
      plot_data_imputed$Forecasted_Units_Sold <- imputeTS::na_interpolation(plot_data$Forecasted_Units_Sold)
      plot_data_imputed$Actual_Units_Sold <- imputeTS::na_interpolation(plot_data$Actual_Units_Sold)
      
    
      # Check for length mismatch before plotting
      if (length(plot_data$Date) != length(plot_data$Actual_Units_Sold) || length(plot_data$Date) != length(plot_data$Forecasted_Units_Sold)) {
        cat("Length mismatch in plot_data for Product", product_id, "using", method, "method.\n")
      } else {
        # Create a time series plot using the create_plot function
        p <- create_plot(plot_data_imputed, product_id, method)
        
        # Print the plot
        print(p)
        
        # Save the plot as an image file
        filename <- paste("product_", product_id, "_method_", method, "_plot.png", sep = "")
        filepath <- file.path(getwd(), plot_dir, filename)  # Updated to save in the plots directory
        ggsave(filename = filepath, plot = p, width = 8, height = 6)
      }
    }
  }
}


# Get summary statistics for sales data
summary_stats_sales <- datasets$sales %>% 
  summarise(
    Min_Date = min(Date),
    Max_Date = max(Date),
    Total_Sales = sum(Units_Sold),
    Avg_Daily_Sales = mean(Units_Sold),
    Median_Daily_Sales = median(Units_Sold)
  )
print(summary_stats_sales)

# Trend Analysis
ggplot(datasets$sales, aes(x = as.Date(Date, format = "%d-%m-%Y"), y = Units_Sold)) +
  geom_line() +
  labs(title = "Daily Sales Trend", x = "Date", y = "Units Sold") +
  theme_minimal()

## Seasonal Decomposition:
# Assuming daily data
sales_ts <- ts(datasets$sales$Units_Sold, frequency = 7)  # Weekly seasonality
decomposed_sales <- stl(sales_ts, s.window = "periodic")
plot(decomposed_sales)

# Create a new Price_per_Unit column
datasets$sales <- datasets$sales %>%
  mutate(Price_per_Unit = Total_Revenue / Units_Sold)

## Correlation with Other Variables:
# Now perform the correlation test using the new Price_per_Unit column
cor_test <- cor.test(datasets$sales$Units_Sold, datasets$sales$Price_per_Unit, method = "pearson")

# Print the results
print(cor_test)


##  Distribution Analysis:
# Distribution of sales per product

ggplot(datasets$sales, aes(x = Units_Sold, fill = Product_ID)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Distribution of Units Sold by Product", x = "Units Sold", y = "Frequency") +
  theme_minimal()


## Seasonal Analysis:
# Aggregate Sales by Season
seasonal_sales <- datasets$sales %>%
  group_by(Season) %>%
  summarise(Total_Sales = sum(Units_Sold))

# Plotting
ggplot(data=seasonal_sales, aes(x=Season, y=Total_Sales)) +
  geom_bar(stat='identity') +
  labs(title="Sales by Season", x="Season", y="Total Sales")

## Brand Analysis:
# Aggregate Sales by Brand

brand_sales <- datasets$sales %>%
  group_by(Brand) %>%
  summarise(Total_Sales = sum(Units_Sold))

# Plotting
ggplot(data=brand_sales, aes(x=Brand, y=Total_Sales)) +
  geom_bar(stat='identity') +
  labs(title="Sales by Brand", x="Brand", y="Total Sales") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##  Price Sensitivity Analysis:
# Aggregating data by Date to get average price per unit and total units sold per day

price_time_analysis <- datasets$sales %>%
  group_by(Date) %>%
  summarise(
    Avg_Price_per_Unit = mean(Price_per_Unit),
    Total_Units_Sold = sum(Units_Sold)
  )

# Plotting
ggplot(data=price_time_analysis, aes(x=as.Date(Date, format="%d-%m-%Y"))) +
  geom_line(aes(y=Avg_Price_per_Unit, color="Average Price per Unit")) +
  geom_line(aes(y=Total_Units_Sold, color="Total Units Sold")) +
  scale_y_continuous(sec.axis = sec_axis(~./max(price_time_analysis$Total_Units_Sold)*max(price_time_analysis$Avg_Price_per_Unit), name="Total Units Sold")) +
  labs(title="Price Changes Over Time vs Sales", x="Date") +
  theme(legend.position="bottom")

## Price Points Effect:

# Aggregating data by Product_ID to get average price per unit and total units sold per product
price_point_analysis <- datasets$sales %>%
  group_by(Product_ID) %>%
  summarise(
    Avg_Price_per_Unit = mean(Price_per_Unit),
    Total_Units_Sold = sum(Units_Sold)
  )

# Plotting
ggplot(data=price_point_analysis, aes(x=Avg_Price_per_Unit, y=Total_Units_Sold)) +
  geom_point() +
  labs(title="Price Points vs Sales", x="Average Price per Unit", y="Total Units Sold")

## Customer Feedback Analysis:

# Joining sales data with feedback data
sales_feedback <- datasets$sales %>%
  left_join(datasets$feedback, by="Product_ID", relationship = "many-to-many")


# Aggregating data by Product_ID to get average rating and total units sold per product
rating_sales_analysis <- sales_feedback %>%
  group_by(Product_ID) %>%
  summarise(
    Avg_Rating = mean(Rating, na.rm=TRUE),
    Total_Units_Sold = sum(Units_Sold)
  )

# Removing rows with NA ratings (if any)
rating_sales_analysis <- na.omit(rating_sales_analysis)

# Plotting
ggplot(data=rating_sales_analysis, aes(x=Avg_Rating, y=Total_Units_Sold)) +
  geom_point() +
  labs(title="Customer Ratings vs Sales", x="Average Rating", y="Total Units Sold")

# Display the structure of the inventory data frame
str(datasets$inventory)

## Inventory Analysis:
# Joining sales data with inventory data
sales_inventory <- datasets$sales %>%
  group_by(Product_ID) %>%
  summarise(Total_Sales = sum(Units_Sold)) %>%
  left_join(datasets$inventory, by = "Product_ID")

# Aggregate Inventory by Product
inventory_analysis <- sales_inventory %>%
  group_by(Product_ID) %>%
  summarise(
    Avg_Stock_Level = mean(Stock_Level),
    Total_Sales = sum(Total_Sales)
  )

# Plotting
ggplot(data=inventory_analysis, aes(x=Avg_Stock_Level, y=Total_Sales)) +
  geom_point() +
  labs(title="Inventory Levels vs Sales", x="Average Inventory Level", y="Total Sales")

## Gap Analysis:

# Identifying products with stock-outs
stock_outs <- datasets$inventory %>%
  filter(Stock_Level == 0) %>%
  group_by(Product_ID) %>%
  summarise(Stock_Out_Days = n())

# Joining with sales data to see impact on sales
stock_outs_sales <- left_join(stock_outs, datasets$sales, by="Product_ID") %>%
  group_by(Product_ID) %>%
  summarise(
    Stock_Out_Days = first(Stock_Out_Days),
    Total_Sales = sum(Units_Sold),
    .groups = 'drop'  # this will drop the grouping structure, returning a regular tibble
  )

## Demand Forecasting:

# Assume datasets$sales has Date and Units_Sold columns
sales_data <- datasets$sales %>%
  rename(ds = Date, y = Units_Sold)  # prophet requires the columns to be named ds and y

# Create a prophet model
prophet_model <- prophet(sales_data)

# Make future predictions
future_dates <- make_future_dataframe(prophet_model, periods = 365)  # predict the next year
forecast <- predict(prophet_model, future_dates)

# Plot the forecast
plot(prophet_model, forecast)

## Optimization

library(lpSolve)

# Assume cost per unit of inventory is $10, and you have a forecasted demand of 100 units for a particular product
cost_per_unit <- 10
forecasted_demand <- 100

# Assume other costs
ordering_cost <- 50  # Cost of placing an order
holding_cost_per_unit <- 2  # Cost of holding a unit in inventory
lead_time_days <- 7  # Lead time in days

# The objective function now becomes the sum of ordering, holding, and unit costs
# Let's assume x[1] is the number of orders and x[2] is the number of units to order
objective_function <- c(ordering_cost * (forecasted_demand) + (holding_cost_per_unit / 2))

# The constraint is that the order quantity must be greater than or equal to the forecasted demand
constraints <- matrix(c(1), nrow = 1, byrow = TRUE)
dir <- c(">=")
rhs <- c(forecasted_demand)

# Solve the LP problem
lp_solution <- lp("min", objective_function, constraints, dir, rhs)

# Check the status of the solution (0 means the solution was found)
solution_status <- lp_solution$status
if (solution_status == 0) {
  # Get the optimal order quantity from the solution
  optimal_order_quantity <- lp_solution$solution
  print(paste("Optimal Order Quantity: ", optimal_order_quantity))
} else {
  print("No solution found")
}


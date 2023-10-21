# List of required packages
packages <- c(
  "xts", "shiny", "readr", "dplyr", "ggplot2", "stringr", "forecast", "tm",
  "zoo", "imputeTS", "VIM", "prophet", "lpSolve"
)


# Load packages with error handling
for (package in packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
    if (!requireNamespace(package, quietly = TRUE)) {
      cat("Error: Package", package, "could not be installed.\n")
    }
  }
  library(package, character.only = TRUE)
}

# Define the directory where your CSV files are located
directory <- "C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App/"

# List of CSV file names
csv_files <- c("sales.csv", "customer_demographics.csv", "customer_metrics.csv", "feedback.csv", "inventory.csv", "products.csv")

# Create a function to read CSV files
read_csv_file <- function(filename, directory) {
  file_path <- file.path(directory, filename)
  if (file.exists(file_path)) {
    return(read.csv(file_path))
  } else {
    cat("Error: File", filename, "not found in directory", directory, "\n")
    return(NULL)
  }
}

# Load CSV files
sales_data <- read_csv_file("sales.csv", directory)
customer_demographics <- read_csv_file("customer_demographics.csv", directory)
customer_metrics <- read_csv_file("customer_metrics.csv", directory)
feedback_data <- read_csv_file("feedback.csv", directory)
inventory_data <- read_csv_file("inventory.csv", directory)
products_data <- read_csv_file("products.csv", directory)

# Server Code
server <- function(input, output, session) {
  
  # Define sales_data as a reactive expression
  sales_data <- reactive({
    # Assuming Date column is in the format "dd-mm-yyyy"
    sales_data$Date <- as.Date(sales_data$Date, format = "%d-%m-%Y")
    
    # Filter data based on the input date range
    filtered_data <- sales_data %>%
      filter(Date >= input$dates[1] & Date <= input$dates[2])
    
    return(filtered_data)
  })
  
  # Define feedback_data as a reactive expression
  feedback_data <- reactive({
    # Clean missing values
    feedback_data[is.na(feedback_data)] <- "Unknown"
    
    # Feature Engineering: Calculate the average rating for each product
    feedback_data <- feedback_data %>%
      group_by(Customer_ID) %>%
      mutate(Avg_Rating = mean(Rating))
    
    return(feedback_data)
  })
  
  # Define inventory_data as a reactive expression
  inventory_data <- reactive({
    return(inventory_data)
  })
  
  # Define products_data as a reactive expression
  products_data <- reactive({
    return(products_data)
  })
  
  # Define sentiment_analysis as a reactive expression
  sentiment_analysis <- reactive({
    # Assuming feedback data has columns named "Rating" and "Review"
    feedback_data <- feedback_data()
    
    feedback_data <- feedback_data %>%
      mutate(Sentiment = case_when(
        str_detect(Review, "love|excellent|best") ~ "Positive",
        str_detect(Review, "not comfortable|poor quality|size is not accurate") ~ "Negative",
        TRUE ~ "Neutral"
      ))
    
    return(feedback_data)
  })  
  
  # Forecasting function
  calculate_forecast_errors <- function(data, horizon, method) {
    if (method == "SMA") {
      # Simple Moving Average (SMA) calculation
      forecast_values <- forecast::sma(data, h = horizon)
    } else if (method == "ETS") {
      # Exponential Smoothing (ETS) calculation
      ets_model <- ets(data)
      forecast_values <- forecast(ets_model, h = horizon)
    } else if (method == "ARIMA") {
      # ARIMA forecasting
      arima_model <- auto.arima(data)
      forecast_values <- forecast(arima_model, h = horizon)
    } else {
      # Add more forecasting methods as needed
      forecast_values <- NULL
      cat("Unsupported forecasting method")
    }
    
    # Calculate forecast errors or perform other operations as needed
    if (!is.null(forecast_values)) {
      # Calculate Mean Absolute Error (MAE)
      actual_values <- tail(data, horizon)
      mae <- mean(abs(forecast_values$mean - actual_values))
      cat("Mean Absolute Error (MAE):", mae, "\n")
      
      # Print forecast values
      cat("Forecast Values:", forecast_values$mean, "\n")
    }
    
    # Return the forecast values or other results as needed
    return(forecast_values)
  }
  
  
  
  # Define sentiment_analysis as a reactive expression
  sentiment_analysis <- reactive({
    req(feedback_data())  # Ensure feedback data is available
    feedback <- feedback_data()
    
    feedback <- feedback %>%
      mutate(Sentiment = case_when(
        str_detect(Review, "love|excellent|best") ~ "Positive",
        str_detect(Review, "not comfortable|poor quality|size is not accurate") ~ "Negative",
        TRUE ~ "Neutral"
      ))
    
    return(feedback)
  })
  
  # Word Cloud: Generate a word cloud based on review text
  output$wordCloudPlot <- renderPlot({
    # Check if sentiment analysis has valid data
    if (is.null(sentiment_analysis()) || is.null(sentiment_analysis()$Review)) {
      return(NULL)
    }
    
    # Create a corpus from the review text
    corpus <- Corpus(VectorSource(sentiment_analysis()$Review))
    
    # Preprocess the text (customize as needed)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Create a term-document matrix and calculate word frequencies
    dtm <- DocumentTermMatrix(corpus)
    word_freq <- row_sums(as.matrix(dtm))
    
    # Create a word cloud
    wordcloud(names(word_freq), word_freq)
  })
  
  output$topics <- renderPrint({
    # Check if sentiment analysis has valid data
    if (is.null(sentiment_analysis()) || is.null(sentiment_analysis()$Review)) {
      return(NULL)
    }
    
    # Create a document-term matrix
    dtm <- DocumentTermMatrix(corpus)  # Ensure that 'corpus' is defined and contains preprocessed text
    
    # Perform topic modeling using LDA (customize 'k' as needed)
    lda_model <- LDA(dtm, k = 3)  # Assuming you want to identify 3 topics
    
    # Get the top words for each topic
    terms(lda_model, 10)
  })

  # Define a function to calculate costs
  calculate_cost <- function(sales_data, feedback_data, inventory_data) {
    # Calculate costs related to overstocking
    overstock_cost <- sum(ifelse(inventory_data$Stock_Level > inventory_data$Reorder_Point, 
                                 (inventory_data$Stock_Level - inventory_data$Reorder_Point) * inventory_data$Overstock_Cost, 0))
    
    # Calculate costs related to understocking
    understock_cost <- sum(ifelse(inventory_data$Stock_Level < inventory_data$Reorder_Point, 
                                  (inventory_data$Reorder_Point - inventory_data$Stock_Level) * inventory_data$Understock_Cost, 0))
    
    # Calculate costs related to customer ratings and seasonal gap analysis
    feedback_ratings <- feedback_data$Rating
    rating_effect <- sum((feedback_ratings - 3) * sales_data$Sales * 0.05)  # Adjust the weight (0.05) as needed
    
    # Calculate costs related to seasonal gap analysis (customize as needed)
    seasonal_gap_cost <- sum(ifelse(inventory_data$Seasonal_gap > 0, inventory_data$Seasonal_gap * 5, 0))
    
    # Calculate overall costs by summing all components
    total_cost <- overstock_cost + understock_cost + rating_effect + seasonal_gap_cost
    
    return(total_cost)
  }

  # Output the logo
  output$logo <- renderUI({
    tags$img(src = "www/OIP.jpg", height = "50px")  # Use a relative path within your Shiny app directory
  })

  # Define an optimization function
  optimize_function <- function(costs, constraints, constraint_dir, rhs) {
    # Convert constraint direction to lpSolve format
    constraint_dir_lp <- ifelse(constraint_dir == ">=", ">=", "<=")
    
    # Create a model using the lp function
    lp_model <- lp(
      direction = "min",            # Minimize or maximize (use "max" for maximization)
      objective.in = costs,
      const.mat = matrix(unlist(constraints), nrow = length(constraints), byrow = TRUE),
      const.dir = constraint_dir_lp,
      const.rhs = rhs
    )
    
    # Check for LP solve errors
    if (lp_model$status != 0) {
      stop("LP solve error: ", lp_model$status)
    }
    
    # Return the optimized values
    return(lp_model$solution)
  }
  
  # Define constraint directions
  constraint_dir <- c(">=", ">=")  # Replace with your actual constraint direction values
  
  # Define constraints as a list or vector with appropriate values
  constraints <- list(c(1, 2, 3), c(4, 5, 6))  # Replace with your actual constraint values
  
  # Define costs as a vector or data frame with appropriate values
  costs <- c(1, 2, 3, 4)  # Replace with your actual cost values
  
  # Define rhs as a vector with appropriate values
  rhs <- c(10, 20)  # Replace with your actual RHS values
  
  
  # Define a reactive expression for filtered data based on your criteria
  filtered_data <- reactive({
    req(sales_data(), input$filterSeason, input$filterBrand, input$filterSupplier, input$filterMaterial, input$filterStoreLocation)
    
    data <- sales_data()
    
    if (!is.null(data)) {
      # Create a list of conditions for filtering
      filters <- list(
        Season = input$filterSeason,
        Brand = input$filterBrand,
        Supplier = input$filterSupplier,
        Material = input$filterMaterial,
        StoreLocation = input$filterStoreLocation
      )
      
      # Use dplyr to filter the data based on conditions
      data <- data %>%
        filter(
          (Season == "All" | Season == filters$Season) &
            (Brand == "All" | Brand == filters$Brand) &
            (Supplier == "All" | Supplier == filters$Supplier) &
            (Material == "All" | Material == filters$Material) &
            (StoreLocation == "All" | StoreLocation == filters$StoreLocation)
        )
      
      return(data)
    } else {
      return(NULL)
    }
  })
  
  # Define optimization_result as a reactive expression
  optimization_result <- reactive({
    req(input$enableOptimization)
    
    if (!input$enableOptimization) {
      return(NULL)  # Return NULL when optimization is disabled
    }
    
    req(filtered_data(), input$forecast_horizon, input$forecast_method)
    
    # Calculate forecast errors (e.g., MAE or MSE) based on your forecasting method
    forecast_errors <- calculate_forecast_errors(filtered_data(), input$forecast_horizon, input$forecast_method)
    
    # Calculate 'costs' based on the forecast errors
    costs <- calculate_inventory_costs(filtered_data(), inventory_data()) + sum(forecast_errors^2)
    
    return(costs)
  })
  
  # Render the optimization result in the UI
  output$optimizationOutput <- renderPrint({
    optimization_result()
  })
  
  # Updating the filters dynamically
  observe({
    updateSelectInput(session, "filterSeason", choices = c("All", unique(sales_data()$Season)))
    updateSelectInput(session, "filterBrand", choices = c("All", unique(sales_data()$Brand)))
    updateSelectInput(session, "filterSupplier", choices = c("All", unique(sales_data()$Supplier)))
    updateSelectInput(session, "filterMaterial", choices = c("All", unique(sales_data()$Material)))
    updateSelectInput(session, "filterStoreLocation", choices = c("All", unique(sales_data()$StoreLocation)))
  })
  
  # Render the sorted table
  output$sortedTable <- renderDataTable({
    req(input$filterSeason, input$filterBrand, input$filterSupplier, input$filterMaterial, input$filterStoreLocation)
    
    data <- filtered_data()
    
    if (is.null(data)) {
      return(NULL)  # Return NULL if data is NULL
    }
    
    # Apply filtering based on user inputs
    if (input$filterSeason != "All") {
      data <- data[data$Season == input$filterSeason,]
    }
    if (input$filterBrand != "All") {
      data <- data[data$Brand == input$filterBrand,]
    }
    if (input$filterSupplier != "All") {
      data <- data[data$Supplier == input$filterSupplier,]
    }
    if (input$filterMaterial != "All") {
      data <- data[data$Material == input$filterMaterial,]
    }
    if (input$filterStoreLocation != "All") {
      data <- data[data$StoreLocation == input$filterStoreLocation,]
    }
    
    if (nrow(data) == 0) {
      return(NULL)  # Return NULL if no data after filtering
    }
    
    # Sort the data by Date
    data <- data[order(data$Date),]
    
    datatable(data)  # Using DT package for interactive tables
  })

  # Render the sales trend plot
  output$salesTrendPlot <- renderPlot({
    data <- filtered_data()  # Retrieve filtered data
    
    # Check if data is NULL (no data points in range)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(
          title = "No data in selected date range",
          x = "Date",
          y = "Total Revenue"
        )
    } else {
      # Create your plot using filtered_data() as the dataset
      ggplot(data = data, aes(x = Date, y = Total_Revenue)) +
        geom_line() +
        labs(
          title = "Total Revenue Trend Over Time",
          x = "Date",
          y = "Total Revenue"
        )
    }
  })

  # Render the seasonal decomposition plot
  output$seasonalDecompPlot <- renderPlot({
    data <- sales_data()  # Retrieve sales data
    
    # Check if data is NULL (no data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No sales data available for seasonal decomposition")
    } else {
      ggplot(data = data, aes(x = Date, y = Total_Revenue, group = Product_ID)) +
        geom_smooth(se = FALSE) +  # Disable standard error shading
        labs(title = "Seasonal Decomposition of Sales Data",
             x = "Date",
             y = "Total Revenue")
    }
  })
  
  # Render the inventory table
  output$inventoryTable <- renderDataTable({
    data <- inventory_data()  # Retrieve inventory data
    
    # Check if data is NULL (no inventory data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      datatable() +
        labs(title = "No inventory data available")
    } else {
      # Create the inventory table using inventory_data() as the dataset
      datatable(data)
    }
  })
  
  # Render the product information table
  output$productTable <- renderDataTable({
    data <- products_data()  # Retrieve product data
    
    # Check if data is NULL (no product data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      datatable() +
        labs(title = "No product data available")
    } else {
      # Create the product information table using products_data() as the dataset
      datatable(data)
    }
  })
  
  # Render the feedback table
  output$feedbackTable <- renderDataTable({
    data <- feedback_data()  # Retrieve feedback data
    
    # Check if data is NULL (no feedback data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      datatable() +
        labs(title = "No feedback data available")
    } else {
      # Create the feedback table using feedback_data() as the dataset
      datatable(data)
    }
  })
  
  # Render the inventory costs
  output$inventoryCostPlot <- renderPlot({
    data <- inventory_data()  # Retrieve inventory data
    
    # Check if data is NULL (no inventory data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No inventory data available for cost analysis")
    } else {
      # Create a bar plot of inventory costs
      ggplot(data, aes(x = Product, y = Inventory_Cost)) +
        geom_bar(stat = "identity") +
        labs(title = "Inventory Costs by Product",
             x = "Product",
             y = "Inventory Cost")
    }
  })
  
  # Render the inventory levels plot
  output$inventoryLevelsPlot <- renderPlot({
    data <- inventory_data()  # Retrieve inventory data
    
    # Check if data is NULL (no inventory data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No inventory data available for analysis")
    } else {
      # Create a line plot of inventory levels
      ggplot(data, aes(x = Date, y = Stock_Level, color = Product)) +
        geom_line() +
        labs(title = "Inventory Levels Over Time",
             x = "Date",
             y = "Stock Level")
    }
  })
  
  # Render the price sensitivity plot
  output$priceSensitivityPlot <- renderPlot({
    data <- sales_data()  # Retrieve sales data
    
    # Check if data is NULL (no data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No sales data available for price sensitivity analysis")
    } else {
      ggplot(data = data, aes(x = Total_Revenue, y = Units_Sold, group = Product_ID)) +
        geom_point() +
        labs(title = "Price Sensitivity")
    }
  })
  
  # Render the demand forecast plot
  output$demandForecastPlot <- renderPlot({
    data <- sales_data()  # Retrieve sales data
    
    # Check if data is NULL (no data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No sales data available for demand forecast")
    } else if (is.null(input$forecast_horizon) || is.null(input$forecast_method)) {
      # Check if forecast horizon or method is NULL
      ggplot() +
        geom_blank() +
        labs(title = "Please select a forecast horizon and method")
    } else {
      # Check if forecast horizon is a positive integer
      if (!is.numeric(input$forecast_horizon) || input$forecast_horizon <= 0 || !is.integer(input$forecast_horizon)) {
        ggplot() +
          geom_blank() +
          labs(title = "Invalid forecast horizon value")
      } else {
        ggplot(data = data, aes(x = Date, y = Units_Sold, group = Product_ID)) +
          geom_line() +
          labs(title = "Demand Forecast")
      }
    }
  })

  # Render the rating effect plot
  output$ratingEffectPlot <- renderPlot({
    data <- sales_data()
    feedback <- feedback_data()  # Retrieve feedback data
    
    # Check if data is NULL (no data)
    if (is.null(data) || is.null(feedback)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No sales or feedback data available for rating effect analysis")
    } else if (!all(c("Rating", "Product_ID") %in% colnames(feedback))) {
      # Check if feedback data does not contain required columns
      ggplot() +
        geom_blank() +
        labs(title = "Feedback data is missing required columns (Rating and/or Product_ID)")
    } else {
      # Assuming your feedback data has columns named "Rating" and "Product_ID"
      if (!all(c("Rating", "Product_ID") %in% colnames(data))) {
        # Check if sales data does not contain required columns
        ggplot() +
          geom_blank() +
          labs(title = "Sales data is missing required columns (Rating and/or Product_ID)")
      } else {
        joined_data <- inner_join(data, feedback, by = "Product_ID", relationship = "many-to-many")
        
        ggplot(data = joined_data, aes(x = Rating, y = Total_Revenue)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE) +
          labs(title = "Effect of Customer Ratings on Sales")
      }
    }
  })
  
  # Render the decomposition plot
  output$decompPlot <- renderPlot({
    data <- sales_data()  # Retrieve sales data
    
    # Check if data is NULL (no data)
    if (is.null(data)) {
      # Provide a default visualization or error message
      ggplot() +
        geom_blank() +
        labs(title = "No sales data available for decomposition")
    } else if (!all(c("Total_Revenue", "Date") %in% colnames(data))) {
      # Check if sales data does not contain required columns
      ggplot() +
        geom_blank() +
        labs(title = "Sales data is missing required columns (Total_Revenue and/or Date)")
    } else {
      # Assuming Date is in Date format and Total_Revenue is numeric
      ts_data <- ts(data$Total_Revenue, frequency = 12)  # Adjust frequency as needed
      
      stl_decomp <- stl(ts_data, s.window = "periodic")
      decomposed <- as.data.frame(stl_decomp$time.series)
      decomposed$Date <- as.Date(index(ts_data))  # Correct the date conversion
      
      ggplot(data = decomposed, aes(x = Date)) +
        geom_line(aes(y = seasonal), color = "blue") +
        geom_line(aes(y = trend), color = "red") +
        labs(title = "Decomposition")
    }
  })
}

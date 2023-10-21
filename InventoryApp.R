library(xfun)

packages <- c(
  "xts", "shiny", "readr", "dplyr", "ggplot2", "stringr", "forecast", "tm",
  "lda", "shinythemes", "prophet", "lpSolve", "plotly", "shinycssloaders",
  "DT", "shinyBS", "stats"
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

setwd("C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App")

# Expose the folder containing your image to Shiny
addResourcePath("images", "C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App/www")

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel(
    title = div(
      img(src = "images/OIP.jpg", height = "50px", align = "left"),
      HTML("&nbsp;"), 
      "AI-Driven Inventory Management",
      style = "font-weight: bold; color: #2c3e50;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("enableUpload", "Enable File Upload", value = FALSE),
      
      conditionalPanel(
        condition = "input.enableUpload == true",
        fileInput("salesInput", "Upload Sales Data (CSV file)"),
        fileInput("feedbackInput", "Upload Feedback Data (CSV file)"),
        fileInput("inventoryInput", "Upload Inventory Data (CSV file)"),
        uiOutput("productSelect"),
        
        # Allow users to enter any date and year they want
        dateRangeInput("dates", "Select Date Range:", format = "yyyy-mm-dd", startview = "month"),
        
        numericInput("forecast_horizon", "Forecast Horizon (days)", value = 5),
        selectInput("forecast_method", "Forecasting Method", choices = c("SMA", "ETS")),
        actionButton("analyseBtn", "Run Analysis"),
        bsTooltip(id = "analyseBtn", title = "Click to run the analysis", placement = "right", trigger = "hover"),
        selectInput("filterSeason", "Filter by Season:", choices = NULL),
        selectInput("filterBrand", "Filter by Brand:", choices = NULL),
        selectInput("filterSupplier", "Filter by Supplier:", choices = NULL),
        selectInput("filterMaterial", "Filter by Material:", choices = NULL),
        selectInput("filterStoreLocation", "Filter by Store Location:", choices = NULL),
        checkboxInput("enableOptimization", "Enable Optimization"),
        dataTableOutput("sortedTable"),
        tags$hr(),
        tags$div(
          "Developed by Sandeep Prasad",
          style = "font-size: 10px; text-align: center; padding-top: 10px;"
        )
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Sales Trend", 
          plotOutput("salesTrendPlot"),
          helpText("This plot shows the trend of total revenue over time. 
                A rising line indicates increasing sales, while a falling 
                line indicates decreasing sales.")
        ),
        tabPanel(
          "Seasonal Decomposition", 
          plotOutput("seasonalDecompPlot"),
          helpText("This plot shows the seasonal pattern in the data. 
                The smooth line represents the trend, while the 
                fluctuating line represents the seasonal component.")
        ),
        tabPanel(
          "Price Sensitivity", 
          plotOutput("priceSensitivityPlot"),
          helpText("This plot shows how the quantity sold is affected by the 
                total revenue, which can help to understand price 
                sensitivity of the products.")
        ),
        tabPanel(
          "Demand Forecast", 
          plotOutput("demandForecastPlot"),
          helpText("This plot shows the forecasted demand based on past 
                sales data. The line represents the estimated number of 
                units that will be sold in the future.")
        ),
        tabPanel(
          "Optimization", 
          verbatimTextOutput("optimizationOutput"),
          helpText("This section displays the results of an optimization analysis. 
             Optimization is a mathematical technique used to find the best 
             outcome or solution from a set of possible choices, subject to 
             certain constraints. The results here might include the optimal 
             order quantities, pricing or scheduling to maximize profit or 
             minimize costs, based on the data provided.")
        ),
        tabPanel(
          "Decomposition", 
          plotOutput("decompPlot"),
          helpText("This plot shows the decomposition of the total revenue 
                into its seasonal and trend components, which can help 
                to understand the underlying patterns in the data.")
        ),
        tabPanel(
          "Rating Effect",
          plotOutput("ratingEffectPlot"),
          helpText("This plot shows the effect of customer ratings on sales. 
                The scatter points represent the actual data, while the 
                line represents the fitted linear regression model.")
        ),
        tabPanel(
          "Word Cloud",
          plotOutput("wordCloudPlot"),
        ),
        tabPanel(
          "Topics",
          verbatimTextOutput("topics"),
        )
      )  # This closing parenthesis is for tabsetPanel
    )  # This closing parenthesis is for mainPanel
  )  # This closing parenthesis is for sidebarLayout
)  # This closing parenthesis is for fluidPage



# Define a reactive expression to read CSV files
uploaded_data <- reactive({
  uploaded <- list()
  
  # Check if each CSV file is available and a CSV file
  for (file_name in c("sales.csv", "inventory.csv", "feedback.csv", "customer_metrics.csv", "customer_demographics.csv")) {
    req(input[[file_name]])
    validate(need(file_ext(input[[file_name]]$name) == ".csv", paste("Please upload", file_name)))
    
    # Attempt to read the CSV data
    data <- tryCatch({
      read.csv(input[[file_name]]$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      cat("Error reading the CSV file", file_name, ":", conditionMessage(e), "\n")
      return(NULL)
    })
    
    # Check if data is successfully read
    validate(need(!is.null(data), paste("Failed to read the CSV file", file_name)))
    
    uploaded[[file_name]] <- data
  }
  
  return(uploaded)
})

# Server Code
server <- function(input, output, session) {
  # Define sales_data as a reactive expression
  sales_data <- reactive({
    # Check if the salesInput is available and a CSV file
    req(input$salesInput)
    validate(need(file_ext(input$salesInput$name) == ".csv", "Please upload a CSV file"))
    
    # Set a breakpoint here
    browser()
    
    # Attempt to read the CSV data
    data <- tryCatch({
      read.csv(input$salesInput$datapath, stringsAsFactors = FALSE)
    }, error = function(e) {
      cat("Error reading the CSV file:", conditionMessage(e), "\n")
      return(NULL)
    })
    
    print(data)
    
    # Check if data is successfully read
    validate(need(!is.null(data), "Failed to read the CSV file"))
    
    tryCatch({
      data$date_column <- as.Date(data$date_column, format = "%m/%d/%Y")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Handle the error, e.g., provide a default value or display an error message.
    })
    
    
    # Filter data based on the input date range
    filtered_data <- data[data$Date >= input$dates[1] & data$Date <= input$dates[2], ]
    
    # Check if there's data after filtering
    validate(need(nrow(filtered_data) > 0, "No data available for the selected date range"))
    
    # Perform data manipulations or calculations as needed
    # Example 1: Calculate Total Sales by Product
    total_sales_by_product <- filtered_data %>%
      group_by(Product) %>%
      summarise(Total_Sales = sum(Sales))
    
    # Example 2: Calculate Monthly Sales Trends
    monthly_sales_trends <- filtered_data %>%
      mutate(YearMonth = format(Date, "%Y-%m")) %>%
      group_by(YearMonth) %>%
      summarise(Total_Sales = sum(Sales))
    
    # Example 3: Filter Data by Product Category
    if (!is.null(input$productCategory)) {
      filtered_data <- filtered_data[filtered_data$Category == input$productCategory, ]
    }
    
    # You can add more data manipulations or calculations here based on your requirements
    
    return(filtered_data)
  })
  
  # Define feedback_data as a reactive expression
  feedback_data <- reactive({
    tryCatch({
      req(input$feedbackInput)  # Ensure file is uploaded before trying to read
      feedback <- read_csv(input$feedbackInput$datapath)
      
      # Check if feedback is empty
      validate(need(nrow(feedback) > 0, "No feedback data available"))
      
      # Cleaning Data: Check for missing values and handle if necessary
      if (anyNA(feedback)) {
        feedback[is.na(feedback)] <- "Unknown"  # Replace missing values with "Unknown" or other appropriate values
      }
      
      # Feature Engineering: Calculate the average rating for each product
      feedback <- feedback %>%
        group_by(Product_ID) %>%
        mutate(Avg_Rating = mean(Rating))
      
      return(feedback)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      return(NULL)  # Return NULL in case of an error
    })
  })
  
observeEvent(input$calculate_button, {
    tryCatch({
      # Check if sales_data() is available
      validate(need(!is.null(sales_data()), "No sales data available"))
      
      if (input$method == "SMA") {
        # Simple Moving Average (SMA) calculation
        forecast_values <- forecast::sma(sales_data(), h = input$horizon)
      } else if (input$method == "ETS") {
        # Exponential Smoothing (ETS) calculation
        ets_model <- ets(sales_data())
        forecast_values <- forecast(ets_model, h = input$horizon)
      } else if (input$method == "ARIMA") {
        # ARIMA forecasting
        arima_model <- auto.arima(sales_data())
        forecast_values <- forecast(arima_model, h = input$horizon)
      } else {
        # Handle other forecasting methods as needed
        cat("Unsupported forecasting method\n")
        forecast_values <- NULL
      }
      
      # Check if forecast_values is not NULL
      validate(need(!is.null(forecast_values), "Forecasting failed"))
      
      # Calculate forecast errors or perform other operations as needed
      actual_values <- tail(sales_data(), input$horizon)
      mae <- mean(abs(forecast_values$mean - actual_values))
      cat("Mean Absolute Error (MAE):", mae, "\n")
      
      # Print forecast values
      cat("Forecast Values:", forecast_values$mean, "\n")
      
      # Usage example:
      # Replace 'sales_data' with your actual sales time series data
      # Replace 'your_method' with the desired forecasting method ("SMA", "ETS", "ARIMA", etc.)
      # Replace 'your_horizon' with the desired forecast horizon (e.g., 5 for a 5-step forecast)
      result <- calculate_forecast_errors(sales_data(), input$horizon, input$method)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
  })
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
    # Handle unsupported forecasting methods
    forecast_values <- NULL
    cat("Unsupported forecasting method\n")
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
  feedback <- feedback_data()
  
  if (is.null(feedback)) {
    cat("No feedback data available for sentiment analysis.\n")
    return(NULL)  # Handle the case where feedback data is not available
  }
  
  feedback <- tryCatch({
    feedback %>%
      mutate(Sentiment = case_when(
        str_detect(Review, "love|excellent|best") ~ "Positive",
        str_detect(Review, "not comfortable|poor quality|size is not accurate") ~ "Negative",
        TRUE ~ "Neutral"
      ))
  }, error = function(e) {
    cat("Error in sentiment analysis:", conditionMessage(e), "\n")
    return(NULL)  # Handle the error as needed
  })
  
  return(feedback)
})



# Define cost_function based on your project topic
calculate_cost <- function(sales_data, feedback_data, inventory_data) {
  # Check if all required data frames are available
  if (is.null(sales_data) || is.null(feedback_data) || is.null(inventory_data)) {
    cat("Missing data for cost calculation.\n")
    return(NULL)  # Handle missing data gracefully
  }
  
  # Compute costs related to your inventory management based on your project's objectives.
  # You can include calculations related to overstocking, understocking, and customer experience.
  # You might also consider factors like customer ratings and seasonal gap analysis.
  
  # Calculate costs related to overstocking
  overstock_cost <- sum(ifelse(inventory_data$Stock_Level > inventory_data$Reorder_Point,
                               (inventory_data$Stock_Level - inventory_data$Reorder_Point) * inventory_data$Overstock_Cost, 0))
  
  # Calculate costs related to understocking
  understock_cost <- sum(ifelse(inventory_data$Stock_Level < inventory_data$Reorder_Point,
                                (inventory_data$Reorder_Point - inventory_data$Stock_Level) * inventory_data$Understock_Cost, 0))
  
  # Calculate costs related to customer ratings and seasonal gap analysis
  # You can define your own logic for these calculations based on your project's objectives.
  # For example, let's assume you have a feedback rating scale from 1 to 5.
  # Calculate costs based on customer ratings and their impact on sales.
  feedback_ratings <- feedback_data$Rating
  rating_effect <- sum((feedback_ratings - 3) * sales_data$Sales * 0.05)  # Adjust the weight (0.05) as needed
  
  # Calculate costs related to seasonal gap analysis
  # Seasonal_gap is a variable that represents the gap between demand and supply in each season.
  seasonal_gap_cost <- sum(ifelse(inventory_data$Seasonal_gap > 0, inventory_data$Seasonal_gap * 5, 0))
  
  # Calculate overall costs by summing all components
  total_cost <- overstock_cost + understock_cost + rating_effect + seasonal_gap_cost
  
  return(total_cost)
}

optimize_function <- function(costs, constraints, constraint_dir, rhs) {
  # Check if inputs are valid
  if (is.null(costs) || length(costs) == 0 || is.null(constraints) || length(constraints) == 0 ||
      is.null(constraint_dir) || length(constraint_dir) == 0 || is.null(rhs) || length(rhs) == 0) {
    cat("Invalid input data for optimization.\n")
    return(NULL)  # Handle invalid inputs gracefully
  }
  
  # Convert constraint direction to lpSolve format
  constraint_dir_lp <- ifelse(constraint_dir == ">=", ">=", "<=")
  
  # Create a model using the lp function
  lp_model <- lp(
    direction = "min",  # Minimize or maximize (use "max" for maximization)
    objective.in = costs,
    const.mat = matrix(unlist(constraints), nrow = length(constraints), byrow = TRUE),
    const.dir = constraint_dir_lp,
    const.rhs = rhs
  )
  
  # Check for errors
  if (lp_model$status != 0) {
    cat("LP solve error: ", lp_model$status, "\n")
    return(NULL)  # Handle LP solve errors gracefully
  }
  
  # Return the optimized values
  return(lp_model$solution)
}

# Define constraint_dir with appropriate values
constraint_dir <- c(">=", ">=", ">=", ">=")  # Replace with your actual constraint direction values

# Define constraints as a list or vector with appropriate values
constraints <- list(c(1, 2, 3), c(4, 5, 6))  # Replace with your actual constraint values

# Define costs as a vector or data frame with appropriate values
costs <- c(1, 2, 3, 4)  # Replace with your actual cost values

# Define rhs as a vector with appropriate values
rhs <- c(10, 20)  # Replace with your actual RHS values

# Define a basic cost function to calculate inventory holding costs
calculate_inventory_costs <- function(sales_data, inventory_data) {
  # Check if required data frames are available
  if (is.null(sales_data) || is.null(inventory_data)) {
    cat("Missing data for inventory cost calculation.\n")
    return(NULL)  # Handle missing data gracefully
  }
  
  # Calculate the total number of items sold
  total_items_sold <- sum(sales_data$Units_Sold)
  
  # Calculate the total cost of holding inventory
  holding_cost_per_item <- 0.1  # You can adjust this value based on your data
  inventory_cost <- total_items_sold * holding_cost_per_item
  
  return(inventory_cost)
}

# Define a reactive expression for filtered data based on your criteria
filtered_data <- reactive({
  req(sales_data(), input$filterSeason, input$filterBrand, input$filterSupplier, input$filterMaterial, input$filterStoreLocation)
  data <- sales_data()
  if (!is.null(data)) {
    if (input$filterSeason != "All" && !is.null(input$filterSeason)) {
      data <- data[data$Season == input$filterSeason, ]
    }
    if (input$filterBrand != "All" && !is.null(input$filterBrand)) {
      data <- data[data$Brand == input$filterBrand, ]
    }
    if (input$filterSupplier != "All" && !is.null(input$filterSupplier)) {
      data <- data[data$Supplier == input$filterSupplier, ]
    }
    if (input$filterMaterial != "All" && !is.null(input$filterMaterial)) {
      data <- data[data$Material == input$filterMaterial, ]
    }
    if (input$filterStoreLocation != "All" && !is.null(input$filterStoreLocation)) {
      data <- data[data$StoreLocation == input$filterStoreLocation, ]
    }
    return(data)
  } else {
    cat("No sales data available.\n")
    return(NULL)
  }
})
  
# Define output$salesTrendPlot within the server function
output$salesTrendPlot <- renderPlot({
  req(sales_data())  # Ensure data is loaded before trying to access
  data <- sales_data()
  
  if (!is.null(data)) {
    # Check if the data contains at least two rows for plotting
    if (nrow(data) >= 2) {
      ggplot(data = data, aes(x = Date, y = Sales)) +
        geom_line() +
        labs(title = "Sales Trend")
    } else {
      # Handle the case where there are insufficient data points to plot
      plot(NULL, xlim = as.Date(input$dates()[1]), ylim = c(0, 1), type = "n",
           main = "Insufficient data for sales trend",
           xlab = "Date", ylab = "Sales")
    }
  } else {
    # Handle the case where data is NULL
    plot(NULL, xlim = as.Date(input$dates()[1]), ylim = c(0, 1), type = "n",
         main = "No sales data available",
         xlab = "Date", ylab = "Sales")
  }
})

# Define output$seasonalDecompPlot within the server function
output$seasonalDecompPlot <- renderPlot({
  tryCatch({
    req(sales_data())  # Ensure data is loaded before trying to access
    data <- sales_data()
    
    # Check if data is NULL (no data points)
    if (is.null(data)) {
      # Provide an error message or default visualization
      plot(NULL, xlim = as.Date(input$dates()[1]), ylim = c(0, 100), type = "n",
           main = "No data available for seasonal decomposition",
           xlab = "Date", ylab = "Total Revenue")
    } else {
      ggplot(data = data, aes(x = Date, y = Total_Revenue, group = Product_ID)) +
        geom_smooth(se = FALSE) +  # Seasonal decomposition using a smoother
        labs(title = "Seasonal Decomposition")
    }
  }, error = function(e) {
    cat("Error:", conditionMessage(e), "\n")
    # Provide an error message or default visualization in case of an error
    plot(NULL, xlim = as.Date(input$dates()[1]), ylim = c(0, 100), type = "n",
         main = "An error occurred while rendering the seasonal decomposition plot",
         xlab = "Date", ylab = "Total Revenue")
  })
})

# Define optimization_result as a reactive expression
optimization_result <- reactive({
    tryCatch({
      req(filtered_data(), input$forecast_horizon, input$forecast_method, input$enableOptimization)
      
      # Calculate forecast errors (e.g., MAE or MSE) based on your forecasting method
      forecast_errors <- calculate_forecast_errors(filtered_data(), input$forecast_horizon, input$forecast_method)
      
      # Check if inventory_data() is available and not NULL
      if (!is.null(inventory_data())) {
        # Calculate 'costs' based on the forecast errors and inventory data
        costs <- calculate_inventory_costs(filtered_data(), inventory_data()) + sum(forecast_errors^2)
      } else {
        # Provide a message if inventory data is missing
        cat("Inventory data is missing. Unable to calculate costs.\n")
        costs <- NULL
      }
      
      return(costs)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
})

# Updating the filters dynamically
observe({
  req(sales_data())
  updateSelectInput(session, "filterSeason", choices = c("All", unique(sales_data()$Season)))
  updateSelectInput(session, "filterBrand", choices = c("All", unique(sales_data()$Brand)))
  updateSelectInput(session, "filterSupplier", choices = c("All", unique(sales_data()$Supplier)))
  updateSelectInput(session, "filterMaterial", choices = c("All", unique(sales_data()$Material)))
  updateSelectInput(session, "filterStoreLocation", choices = c("All", unique(sales_data()$StoreLocation)))
})


# Render the optimization result in the UI
output$optimizationOutput <- renderPrint({
    tryCatch({
      optimization_result()
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
})
  
# Define output$topics within the server function
output$topics <- renderPrint({
    tryCatch({
      req(sentiment_analysis())  # Ensure sentiment analysis is performed
      
      # Create a document-term matrix
      dtm <- DocumentTermMatrix(corpus)
      
      # Perform topic modeling using LDA
      lda_model <- LDA(dtm, k = 3)  # Assuming you want to identify 3 topics
      
      # Get the top words for each topic
      terms(lda_model, 10)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
})
  
# Define output$logo within the server function
output$logo <- renderUI({
    tryCatch({
      tags$img(src = "C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App/www/OIP.jpg", height = "50px")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
})
  
# Word Cloud: Generate a word cloud based on review text
output$wordCloudPlot <- renderPlot({
    tryCatch({
      req(sentiment_analysis())  # Ensure sentiment analysis is performed
      
      # Create a corpus from the review text
      corpus <- Corpus(VectorSource(sentiment_analysis()$Review))
      
      # Preprocess the text (e.g., remove punctuation and common stopwords)
      corpus <- tm_map(corpus, content_transformer(tolower))
      corpus <- tm_map(corpus, removePunctuation)
      corpus <- tm_map(corpus, removeWords, stopwords("english"))
      
      # Create a term-document matrix and calculate word frequencies
      dtm <- DocumentTermMatrix(corpus)
      word_freq <- row_sums(as.matrix(dtm))
      
      # Create a word cloud
      wordcloud(names(word_freq), word_freq)
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
})
  
output$priceSensitivityPlot <- renderPlot({
    tryCatch({
      req(sales_data())  # Ensure data is loaded before trying to access
      data <- sales_data()
      ggplot(data = data, aes(x = Total_Revenue, y = Units_Sold, group = Product_ID)) +
        geom_point() +
        labs(title = "Price Sensitivity")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
  })
  
  output$demandForecastPlot <- renderPlot({
    tryCatch({
      req(sales_data())  # Ensure data is loaded before trying to access
      data <- sales_data()
      ggplot(data = data, aes(x = Date, y = Units_Sold, group = Product_ID)) +
        geom_line() +
        labs(title = "Demand Forecast")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
  })
  
  output$ratingEffectPlot <- renderPlot({
    tryCatch({
      req(sales_data(), feedback_data())  # Ensure both datasets are loaded
      
      # Assuming your feedback data has columns named "Rating" and "Product_ID"
      joined_data <- inner_join(sales_data(), feedback_data(), by = "Product_ID", relationship = "many-to-many")
      
      ggplot(data = joined_data, aes(x = Rating, y = Total_Revenue)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = "Effect of Customer Ratings on Sales")
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
  })
  
  output$decompPlot <- renderPlot({
    tryCatch({
      req(sales_data())  # Ensure data is loaded before trying to access
      data <- sales_data()
      
      # Check if data is NULL (no data points in range)
      if (is.null(data)) {
        # Provide an error message or default visualization
        plot(NULL, xlim = as.Date(input$dates[1]), ylim = c(0, 100), type = "n",
             main = "No data in selected date range",
             xlab = "Date", ylab = "Total Revenue")
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
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
  })
  # Render the sorted table
  output$sortedTable <- renderDataTable({
    tryCatch({
      req(filtered_data(), input$filterSeason, input$filterBrand, input$filterSupplier, input$filterMaterial, input$filterStoreLocation)
      data <- filtered_data()
      
      if (!is.null(data)) {
        if (input$filterSeason != "All" && !is.null(input$filterSeason)) {
          data <- data[data$Season == input$filterSeason,]
        }
        if (input$filterBrand != "All" && !is.null(input$filterBrand)) {
          data <- data[data$Brand == input$filterBrand,]
        }
        if (input$filterSupplier != "All" && !is.null(input$filterSupplier)) {
          data <- data[data$Supplier == input$filterSupplier,]
        }
        if (input$filterMaterial != "All" && !is.null(input$filterMaterial)) {
          data <- data[data$Material == input$filterMaterial,]
        }
        if (input$filterStoreLocation != "All" && !is.null(input$filterStoreLocation)) {
          data <- data[data$StoreLocation == input$filterStoreLocation,]
        }
        
        datatable(data[order(data$Date),])  # Using DT package for interactive tables
      } else {
        return(NULL)
      }
    }, error = function(e) {
      cat("Error:", conditionMessage(e), "\n")
      # Provide an error message or handle the error as needed
      # You can return NULL or an appropriate default value in case of an error
      NULL
    })
  })
  
}

# Launch the Shiny app
shinyApp(ui, server)

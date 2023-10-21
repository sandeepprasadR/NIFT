library(shiny)
library(shinythemes)
library(readr)
library(dplyr)
library(forecast)
library(ggplot2)
library(prophet)
library(lpSolve)
library(plotly)
library(shinycssloaders)
library(DT)
library(shinyBS)

# Expose the folder containing your image to Shiny
addResourcePath("images", "C:/Users/GN898ZK/OneDrive - EY/5. My Learnings/32. R/Inventory_App/www")

# UI
ui <- fluidPage(
  titlePanel(
    title = div(
      img(src = "images/OIP.jpg", height = "50px", align = "left"),
      HTML("&nbsp;"), 
      "Inventory Analysis App",
      style = "font-weight: bold; color: #2c3e50;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("enableUpload", "Enable File Upload", value = FALSE),
      
      conditionalPanel(
        condition = "input.enableUpload == true",
        fileInput("salesInput", "Upload Sales Data", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        fileInput("feedbackInput", "Upload Feedback Data", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        fileInput("inventoryInput", "Upload Inventory Data", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
        uiOutput("productSelect"),
        uiOutput("dateRangeSelect"),
        numericInput("forecast_horizon", "Forecast Horizon (days)", value = 5),
        selectInput("forecast_method", "Forecasting Method", choices = c("SMA", "ETS")),
        actionButton("analyseBtn", "Run Analysis"),
        bsTooltip(id = "analyseBtn", title = "Click to run the analysis", placement = "right", trigger = "hover"),
        selectInput("filterSeason", "Filter by Season:", choices = NULL),
        selectInput("filterBrand", "Filter by Brand:", choices = NULL),
        selectInput("filterSupplier", "Filter by Supplier:", choices = NULL),
        selectInput("filterMaterial", "Filter by Material:", choices = NULL),
        selectInput("filterStoreLocation", "Filter by Store Location:", choices = NULL),
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
        ),  # This closing parenthesis should come after helpText
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
        )
      )  # This closing parenthesis is for tabsetPanel
    )
  )  # This closing parenthesis is for sidebarLayout
)

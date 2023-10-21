library(shiny)

# Load the UI and server code from the respective files
source("ui.R")
source("server.R")

# Launch the Shiny app
shinyApp(ui = ui, server = server)

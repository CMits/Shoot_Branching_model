library(shiny)
source("global.R")   # Load global settings
source("ui.R")       # Load UI
source("server.R")   # Load Server

# Launch Shiny App
shinyApp(ui = ui, server = server)

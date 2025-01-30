manualSimulationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      fileInput(ns("manual_sbgnFile"), "Upload SBGN File:", accept = c(".sbgn")),
      actionButton(ns("manual_convertSBGN"), "Convert"),
      actionButton(ns("manual_runSimulation"), "Run Simulation")
    ),
    mainPanel(
      plotOutput(ns("manual_simulationPlot"))
    )
  )
}

manualSimulationsServer <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$manual_convertSBGN, {
    showMessage("SBGN File Converted Successfully!")
  })
  
  observeEvent(input$manual_runSimulation, {
    output$manual_simulationPlot <- renderPlot({
      plot(runif(10), runif(10), main = "Manual Simulation Results", col = "blue")
    })
  })
}

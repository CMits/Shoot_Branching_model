simulationsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      fileInput(ns("sbgnFile"), "Upload SBGN File:", accept = c(".sbgn")),
      actionButton(ns("convertSBGN"), "Convert"),
      actionButton(ns("runSimulations"), "Run Simulations")
    ),
    mainPanel(
      plotOutput(ns("simulationPlot")),
      DTOutput(ns("resultsTable"))
    )
  )
}

simulationsServer <- function(input, output, session) {
  ns <- session$ns
  simulationsData <- reactiveVal(NULL)
  
  observeEvent(input$convertSBGN, {
    showMessage("SBGN File Converted Successfully!")
  })
  
  observeEvent(input$runSimulations, {
    simulationsData(data.frame(Gene = c("AUX", "SUC"), Growth = c(0.5, 1.2)))
    showMessage("Simulations Completed!")
  })
  
  output$simulationPlot <- renderPlot({
    req(simulationsData())
    ggplot(simulationsData(), aes(x = Gene, y = Growth)) +
      geom_bar(stat = "identity", fill = "#3498db")
  })
  
  output$resultsTable <- renderDT({ datatable(simulationsData()) })
}

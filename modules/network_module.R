networkHomeUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, DTOutput(ns("networkTable"))),
      actionButton(ns("addRow"), "Add Row", class = "btn-primary"),
      actionButton(ns("createNetwork"), "Create Network", class = "btn-success")
    ),
    visNetworkOutput(ns("networkPlot"), height = "600px")
  )
}

networkHomeServer <- function(input, output, session) {
  ns <- session$ns
  networkData <- reactiveVal(data.frame(Node1 = character(), Node2 = character(), Relationship = character()))
  
  observeEvent(input$addRow, {
    newRow <- data.frame(Node1 = "", Relationship = "promotion", Node2 = "")
    networkData(rbind(networkData(), newRow))
  })
  
  output$networkTable <- renderDT({ datatable(networkData()) })
}

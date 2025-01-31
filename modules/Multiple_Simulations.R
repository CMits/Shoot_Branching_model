# modules/multiple_simulations.R

multipleSimulationsUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Multiple Simulations",
    fluidPage(
      titlePanel(tags$div(
        style = "color: #3498db; text-align: center; font-size: 2.5rem; font-weight: bold;",
        "PSoup Network Analysis"
      )),
      tags$head(
        tags$script(
          HTML("function toggleSidebar() {
                  var sidebar = document.getElementById('sidebarPanel');
                  var mainPanel = document.getElementById('mainPanel');
                  if (sidebar.style.display === 'none') {
                    sidebar.style.display = 'block';
                    mainPanel.classList.remove('expanded');
                  } else {
                    sidebar.style.display = 'none';
                    mainPanel.classList.add('expanded');
                  }
                }")
        ),
        tags$style(
          HTML("#mainPanel.expanded {
                    width: 100% !important;
                  }
                  #sidebarPanel {
                    width: 100%;
                  }
                  .content {
                    transition: width 0.3s;
                  }")
        )
      ),
      sidebarLayout(
        sidebarPanel(
          id = ns("sidebarPanel"),
          
          # Step 1: Upload network
          tags$div(class = "panel-heading", "Step 1: Upload network"),
          tags$div(
            class = "panel-body",
            fileInput(ns("sbgnFile"), "Upload SBGN File:", accept = c(".sbgn")),
            textInput(ns("networkName"), "Network Name:"),
            selectInput(ns("predefinedNetwork"), "Or Select a Predefined Network:",
                        choices = c("None", "Benchmark Model"),
                        selected = "None"),
            actionButton(ns("convertSBGN"), "Upload Network", class = "btn-primary")
          ),
          
          hr(),
          
          # Step 2: Encode network into a mathematical model
          tags$div(class = "panel panel-success",
                   tags$div(class = "panel-heading", "Step 2: Encode network into mathematical model"),
                   tags$div(class = "panel-body",
                            textInput(ns("outputFolder"), "Folder for model:",
                                      "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data/CBmodel"),
                            actionButton(ns("buildModel"), "Build Model", class = "btn-success")
                   )
          ),
          
          hr(),
          
          # Step 3: Extract modifier and exogenous list
          tags$div(class = "panel panel-success",
                   tags$div(class = "panel-heading", "Extract modifier and exogenous list"),
                   tags$div(class = "panel-body",
                            textInput(ns("saveCSV"), "Save data as CSV:",
                                      "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data"),
                            actionButton(ns("saveFiles"), "Save modifiers and nodes", class = "btn-danger")
                   )
          ),
          
          hr(),
          
          # Step 4: Define experimental treatments
          tags$div(class = "panel panel-info",
                   tags$div(class = "panel-heading", "Step 4: Define experimental treatments"),
                   tags$div(class = "panel-body",
                            fileInput(ns("exogenousFile"), "Upload Exogenous Definitions CSV:"),
                            fileInput(ns("genotypeFile"), "Upload Genotype Definitions CSV:"),
                            actionButton(ns("saveDefinitions"), "Save Definitions", class = "btn-info")
                   )
          ),
          
          hr(),
          
          # Step 5: Simulation settings
          tags$div(class = "panel panel-warning",
                   tags$div(class = "panel-heading", "Step 5: Simulation settings"),
                   tags$div(class = "panel-body",
                            numericInput(ns("maxStep"), "Max Steps:", 200, min = 1),
                            numericInput(ns("delay"), "Delay:", 20, min = 0),
                            checkboxInput(ns("exogenousSupply"), "Enable Exogenous Supply", TRUE),
                            checkboxInput(ns("combinatorial"), "Enable Combinatorial", FALSE),
                            checkboxInput(ns("preventDrop"), "Prevent Drop", TRUE),
                            actionButton(ns("runSimulations"), "Run Simulations", class = "btn-warning")
                   )
          ),
          
          hr(),
          
          # Step 6: Results
          tags$div(class = "panel panel-danger",
                   tags$div(class = "panel-heading", "Step 6: Results"),
                   tags$div(class = "panel-body",
                            textInput(ns("saveCSVResults"), "Save Combined Data as CSV:",
                                      "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data/Test_simulation_data.csv"),
                            actionButton(ns("saveResults"), "Save Results to CSV", class = "btn-danger"),
                            actionButton(ns("showFilters"), "Show/Hide Filters", class = "btn-secondary"),
                            actionButton(ns("applyFilters"), "Apply Filters", class = "btn-secondary")
                   )
          )
        ),
        
        # Main Panel
        mainPanel(
          id = ns("mainPanel"),
          actionButton(ns("toggleSidebar"), "Toggle Sidebar", onclick = "toggleSidebar()", class = "btn-secondary mb-3"),
          tabsetPanel(
            tabPanel("Status", verbatimTextOutput(ns("status"))),
            tabPanel("Preview", plotOutput(ns("simulationPlot"), height = "400px")),
            tabPanel("Results", DTOutput(ns("resultsTable"))),
            tabPanel("Visualizations", 
                     selectInput(ns("yAxis"), "Select Y-axis:", choices = NULL), 
                     plotOutput(ns("barGraph"), height = "600px"))
          )
        )
      )
    )
  )
      
}

multipleSimulationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactiveVals <- reactiveValues(
      CBnetwork = NULL,
      folder = NULL,
      allSim = NULL,
      Hormones = NULL,
      Gene = NULL,
      exogenous = NULL,
      combined_data = NULL,
      filtered_data = NULL,
      show_filters = FALSE
    )
    
    # Convert SBGN to network
    observeEvent(input$convertSBGN, {
      file <- if (input$predefinedNetwork == "Benchmark Model") {
        "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data/Benchmark_Model.sbgn"
      } else {
        req(input$sbgnFile)
        input$sbgnFile$datapath
      }
      networkName <- ifelse(input$predefinedNetwork == "Benchmark Model", "Benchmark Model", input$networkName)
      reactiveVals$CBnetwork <- convertSBGNdiagram(file, networkName)
      output$status <- renderText("Network uploaded successfully!")
    })
    
    # Build model
    observeEvent(input$buildModel, {
      req(reactiveVals$CBnetwork, input$outputFolder)
      reactiveVals$folder <- input$outputFolder
      buildModel(reactiveVals$CBnetwork, reactiveVals$folder, forceOverwrite = TRUE)
      output$status <- renderText("Model built successfully!")
    })
    
    #Extract genotype and exogenous
    observeEvent(input$saveFiles, {
      # Get the folder path from the user input
      folder <- input$outputFolder
      save_path <- input$saveCSV
      
      # Debug: Print save path
      print(paste("Saving files to:", save_path))
      
      # Load RData files from the specified folder
      genotype_file <- file.path(folder, "genotypeDef.RData")
      nodes_file <- file.path(folder, "nodestartDef.RData")
      
      print(paste("Genotype file path:", genotype_file))
      print(paste("Nodes file path:", nodes_file))
      
      # Check if files exist before loading
      if (file.exists(genotype_file) && file.exists(nodes_file)) {
        print("Loading genotype and nodes files...")
        load(genotype_file)
        load(nodes_file)
        
        # Verify if objects were loaded correctly
        if (exists("genotypeDef") && exists("nodestartDef")) {
          print("Objects loaded successfully!")
          
          # Convert lists to data frames
          genotype_df <- as.data.frame(genotypeDef)
          nodes_df <- as.data.frame(nodestartDef)
          
          # Define CSV file paths
          genotype_csv <- file.path(save_path, "genotype.csv")
          nodes_csv <- file.path(save_path, "nodes.csv")
          
          # Save CSV files
          write.csv(genotype_df, genotype_csv, row.names = FALSE)
          write.csv(nodes_df, nodes_csv, row.names = FALSE)
          
          # Debug: Print success message
          print(paste("Files saved successfully at:", save_path))
          showNotification("Files saved successfully!", type = "message")
          
        } else {
          print("Error: Objects not loaded correctly!")
          showNotification("Error: Objects not loaded correctly.", type = "error")
        }
      } else {
        print("Error: RData files not found!")
        showNotification("Error: RData files not found in the specified folder.", type = "error")
      }
    })
    
    observeEvent(input$saveResults, {
      req(reactiveVals$allSim, reactiveVals$Gene, reactiveVals$exogenous)
      
      # Debug: Print file path to console
      print(paste("🟢 Save Path Entered:", input$saveCSVResults))
      
      # If input is NULL or empty, stop execution
      if (is.null(input$saveCSVResults) || input$saveCSVResults == "") {
        output$status <- renderText("❌ Error: No save path provided!")
        print("❌ ERROR: No save path provided!")
        return()
      }
      
      # Ensure folder exists
      folder_path <- dirname(input$saveCSVResults)
      if (!dir.exists(folder_path)) {
        dir.create(folder_path, recursive = TRUE)
        print(paste("✅ Created directory:", folder_path))
      }
      
      # Generate and print data before saving
      Hormones <- finalStates(reactiveVals$allSim$screen)
      combined_data <- data.frame(Hormones = Hormones, Gene = reactiveVals$Gene, exogenous = reactiveVals$exogenous)
      reactiveVals$combined_data <- combined_data
      
      
      # Debug: Print first few rows of data to console
      print( "Data Preview:")
      print(head(combined_data))
      
      # Try saving the CSV file and catch errors
      tryCatch({
        write.csv(combined_data, file = input$saveCSVResults, row.names = FALSE)
        output$status <- renderText("Results saved successfully!")
        print(paste("Successfully saved results to:", input$saveCSVResults))
      }, error = function(e) {
        output$status <- renderText(paste("Error saving file:", e$message))
        print(paste("ERROR: Could not save results to", input$saveCSVResults, "Error:", e$message))
      })
    })
    
    output$resultsTable <- renderDT({
      if (is.null(reactiveVals$combined_data)) {
        print("ERROR: No Data Available for Results Table!")
        return(datatable(data.frame(Message = "No data available"), options = list(pageLength = 10)))
      }
      print("Rendering Results Table...")
      datatable(reactiveVals$combined_data, options = list(pageLength = 10))
    })
    
    
    
    # Run simulations
    observeEvent(input$runSimulations, {
      req(reactiveVals$folder)
      reactiveVals$allSim <- setupSims(
        reactiveVals$folder,
        maxStep = input$maxStep,
        delay = input$delay,
        exogenousSupply = input$exogenousSupply,
        combinatorial = input$combinatorial,
        preventDrop = input$preventDrop
      )
      output$status <- renderText({ "Simulations completed successfully!" })
    })
    
    # Plot simulation results
    output$simulationPlot <- renderPlot({
      req(reactiveVals$allSim)
      fastPlot(reactiveVals$allSim$screen[[2]]$simulation, logTransform = TRUE)
    })
    
    # Save results to CSV
    observeEvent(input$saveResults, {
      req(reactiveVals$allSim, reactiveVals$Gene, reactiveVals$exogenous)
      Hormones <- finalStates(reactiveVals$allSim$screen)
      combined_data <- data.frame(Hormones = Hormones, Gene = reactiveVals$Gene, exogenous = reactiveVals$exogenous)
      reactiveVals$combined_data <- combined_data
      reactiveVals$filtered_data <- combined_data
      write.csv(combined_data, file = input$saveCSV, row.names = FALSE)
      output$status <- renderText("Results saved successfully!")
    })
    
    # Show or hide filters
    observeEvent(input$showFilters, {
      reactiveVals$show_filters <- !reactiveVals$show_filters
    })
    
    # Generate UI for filtering options dynamically
    output$filterUI <- renderUI({
      req(reactiveVals$combined_data, reactiveVals$show_filters)
      if (reactiveVals$show_filters) {
        colnames <- names(reactiveVals$combined_data)
        filterUI <- lapply(seq(1, length(colnames), by = 3), function(i) {
          fluidRow(
            lapply(colnames[i:min(i+2, length(colnames))], function(col) {
              if (is.numeric(reactiveVals$combined_data[[col]])) {
                column(4, sliderInput(ns(paste0("filter_", col)), 
                                      label = paste("Filter Range for", col), 
                                      min = min(reactiveVals$combined_data[[col]], na.rm = TRUE), 
                                      max = max(reactiveVals$combined_data[[col]], na.rm = TRUE), 
                                      value = c(min(reactiveVals$combined_data[[col]], na.rm = TRUE), 
                                                max(reactiveVals$combined_data[[col]], na.rm = TRUE))))
              }
            })
          )
        })
        do.call(tagList, filterUI)
      }
    })
  })
}

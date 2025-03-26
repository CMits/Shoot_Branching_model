# UI Module
manualSimulationsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Manual Simulations",
           fluidPage(
             titlePanel(
               tags$div(
                 style = "color: #3498db; text-align: center; font-size: 2.5rem; font-weight: bold;",
                 "PSoup Network Analysis"
               )
             ),
             tags$head(
               # Build a unique JS function for toggling the sidebar using the module's namespace
               tags$script(HTML(sprintf("
                  function toggleSidebar_%s() {
                    var sidebar = document.getElementById('%s');
                    var mainPanel = document.getElementById('%s');
                    if (sidebar.style.display === 'none') {
                      sidebar.style.display = 'block';
                      mainPanel.classList.remove('expanded');
                    } else {
                      sidebar.style.display = 'none';
                      mainPanel.classList.add('expanded');
                    }
                  }
                  ", id, ns("sidebarPanel"), ns("mainPanel")
               ))),
               tags$style(HTML(sprintf("
                   #%s.expanded {
                     width: 100%% !important;
                   }
                   #%s {
                     width: 100%%;
                   }
                   .content {
                     transition: width 0.3s;
                   }
                   ", ns("mainPanel"), ns("sidebarPanel")
               )))
             ),
             sidebarLayout(
               sidebarPanel(
                 id = ns("sidebarPanel"),
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
                 tags$div(
                   class = "panel panel-success",
                   tags$div(class = "panel-heading", "Step 2: Encode network into mathematical model"),
                   tags$div(
                     class = "panel-body",
                     textInput(ns("outputFolder"),
                               "Folder for model:",
                               "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data/Manual"),
                     actionButton(ns("buildModel"), "Build Model", class = "btn-success")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-success",
                   tags$div(class = "panel-heading", "Extract modifier and exogenous list"),
                   tags$div(
                     class = "panel-body",
                     textInput(
                       ns("saveModifiersCSV"),
                       "Save data as CSV:",
                       "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/Testing Data"
                     ),
                     actionButton(ns("saveFiles"), "Save modifiers and nodes", class = "btn-danger")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-info",
                   tags$div(class = "panel-heading", "Step 3: Define experimental treatments"),
                   tags$div(
                     class = "panel-body",
                     actionButton(ns("showFiles"), "Show Genotype and Exogenous", class = "btn-warning"),
                     uiOutput(ns("slidersUI")),  # Placeholder for dynamically generated sliders
                     actionButton(ns("saveModifiedValues"), "Save Modified Values", class = "btn-success")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-warning",
                   tags$div(class = "panel-heading", "Step 4: Simulation settings"),
                   tags$div(
                     class = "panel-body",
                     # Added simulationName input
                     textInput(ns("simulationName"), "Simulation Name:", "Simulation 1"),
                     numericInput(ns("maxStep"), "Max Steps:", 200, min = 1),
                     numericInput(ns("delay"), "Delay:", 20, min = 0),
                     checkboxInput(ns("exogenousSupply"), "Enable Exogenous Supply", TRUE),
                     actionButton(ns("runSimulations"), "Run Simulation", class = "btn-warning")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-danger",
                   tags$div(class = "panel-heading", "Step 5: Results"),
                   tags$div(
                     class = "panel-body",
                     textInput(
                       ns("saveResultsCSV"),
                       "Save Combined Data as CSV:",
                       "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/Testing Data/Test_simulation_data.csv"
                     ),
                     actionButton(ns("saveResults"), "Save Results to CSV", class = "btn-danger")
                   )
                 )
               ),
               # Main Panel
               mainPanel(
                 id = ns("mainPanel"),
                 actionButton(ns("toggleSidebar"), "Toggle Sidebar", 
                              onclick = sprintf("toggleSidebar_%s()", id), 
                              class = "btn-secondary mb-3"),
                 textOutput(ns("status")),  # For status messages
                 plotOutput(ns("barGraph"), height = "400px"),  # Placeholder for bar graph
                 DTOutput(ns("resultsTable"))                   # Table to display results
               )
             )
           )
  )
}
# Server Module
manualSimulationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive values to store simulation data, slider settings, etc.
    reactiveVals <- reactiveValues(
      genotype = NULL,
      exogenous = NULL,
      sliders = NULL,
      folder = NULL,
      allSim = data.frame(),  # Data frame to store all simulation results
      simulation = NULL,
      showSliders = FALSE,
      CBnetwork = NULL
    )
    
    # Convert SBGN to network
    observeEvent(input$convertSBGN, {
      req(input$sbgnFile)
      file <- input$sbgnFile$datapath
      reactiveVals$CBnetwork <- convertSBGNdiagram(file, input$networkName)
      output$status <- renderText({ "Network converted successfully!" })
    })
    
    
    
    # Build model
    observeEvent(input$buildModel, {
      req(reactiveVals$CBnetwork, input$outputFolder)
      reactiveVals$folder <- input$outputFolder
      buildModel(reactiveVals$CBnetwork, reactiveVals$folder, forceOverwrite = TRUE)
      output$status <- renderText({ "Model built successfully!" })
    })
    
    # Toggle display of genotype and exogenous sliders when the button is pressed
    observeEvent(input$showFiles, {
      reactiveVals$showSliders <- !reactiveVals$showSliders  # Toggle state
      
      if (reactiveVals$showSliders) {
        req(reactiveVals$folder)
        
        # Build file paths using file.path for cross-platform compatibility
        genotype_file <- file.path(reactiveVals$folder, "genotypeDef.RData")
        # Load exogenous data from nodestartDef.RData
        exogenous_file <- file.path(reactiveVals$folder, "nodestartDef.RData")
        
        if (file.exists(genotype_file) && file.exists(exogenous_file)) {
          load(genotype_file)    # Expects to load an object named 'genotypeDef'
          load(exogenous_file)   # Expects to load an object named 'nodestartDef'
          
          if (exists("genotypeDef") && exists("nodestartDef")) {
            reactiveVals$genotype <- as.data.frame(genotypeDef)
            reactiveVals$exogenous <- as.data.frame(nodestartDef)
            
            # Dynamically create sliders for each column of genotype and exogenous data
            reactiveVals$sliders <- tagList(
              lapply(names(reactiveVals$genotype), function(col) {
                sliderInput(
                  inputId = ns(paste0("genotype_", col)),
                  label = paste("Genotype -", col),
                  min = 0, max = 3, 
                  value = ifelse(is.null(reactiveVals$genotype[[col]][1]), 0, reactiveVals$genotype[[col]][1]), 
                  step = 0.1
                )
              }),
              lapply(names(reactiveVals$exogenous), function(col) {
                sliderInput(
                  inputId = ns(paste0("exogenous_", col)),
                  label = paste("Exogenous -", col),
                  min = 0, max = 3, 
                  value = 0,  # Default value for exogenous sliders
                  step = 0.1
                )
              })
            )
          } else {
            showNotification("Genotype or exogenous data not loaded properly.", type = "error")
          }
        } else {
          showNotification("Genotype or exogenous files not found in the folder.", type = "error")
        }
      }
      
      # Render (or hide) the sliders UI based on the toggle state
      output$slidersUI <- renderUI({
        if (reactiveVals$showSliders) {
          reactiveVals$sliders
        } else {
          NULL
        }
      })
      
      # Update the button label dynamically
      updateActionButton(session, "showFiles",
                         label = if (reactiveVals$showSliders) "Hide Genotype and Exogenous" else "Show Genotype and Exogenous"
      )
    })
    
    # Save modified slider values back to the RData files
    observeEvent(input$saveModifiedValues, {
      req(reactiveVals$genotype, reactiveVals$exogenous)
      
      # Update each value based on the corresponding slider input
      lapply(names(reactiveVals$genotype), function(col) {
        reactiveVals$genotype[[col]] <- input[[paste0("genotype_", col)]]
      })
      lapply(names(reactiveVals$exogenous), function(col) {
        reactiveVals$exogenous[[col]] <- input[[paste0("exogenous_", col)]]
      })
      
      # Save updated genotype back to its file
      if (!is.null(reactiveVals$genotype)) {
        genotypeDef <- reactiveVals$genotype
        save(genotypeDef, file = file.path(reactiveVals$folder, "genotypeDef.RData"))
      }
      
      # Save updated exogenous data as a new RData file named "exogenous.RData"
      if (!is.null(reactiveVals$exogenous) && ncol(reactiveVals$exogenous) > 0) {
        exogenous <- reactiveVals$exogenous
        save(exogenous, file = file.path(reactiveVals$folder, "exogenousDef.RData"))
      } else {
        showNotification("Warning: No exogenous data to save!", type = "warning")
      }
      
      showNotification("Modified values saved successfully!", type = "message")
    })
    
    # Run simulations when the corresponding button is clicked
    observeEvent(input$runSimulations, {
      req(input$simulationName, reactiveVals$folder)
      
      output$status <- renderText({ "Simulations are loading..." })
      
      # Run the simulation (assumes simulateNetwork() and finalStates() are defined elsewhere)
      reactiveVals$simulation <- simulateNetwork(
        reactiveVals$folder,
        maxStep = input$maxStep,
        delay = input$delay,
        exogenousSupply = input$exogenousSupply
      )
      
      output$status <- renderText({ "Simulation completed successfully!" })
      
      # Extract hormone levels from the simulation results
      Hormones <- finalStates(reactiveVals$simulation$screen)
      
      # Build a new data frame for this simulation's results.
      # Storing the genotype and exogenous data as list columns.
      new_data <- data.frame(
        Simulation_Name = input$simulationName,
        Hormones = Hormones,
        stringsAsFactors = FALSE
      )
      new_data$Gene <- I(list(reactiveVals$genotype))
      new_data$Exogenous <- I(list(reactiveVals$exogenous))
      
      # Append the new simulation data to the existing results
      reactiveVals$allSim <- rbind(reactiveVals$allSim, new_data)
    })
    
    # Save all simulation results to a CSV file
    observeEvent(input$saveResults, {
      req(nrow(reactiveVals$allSim) > 0)
      
      write.csv(reactiveVals$allSim, file = input$saveResultsCSV, row.names = FALSE)
      
      output$status <- renderText({ "Results saved successfully!" })
    })
    
    # Render the simulation results table
    output$resultsTable <- renderDT({
      req(nrow(reactiveVals$allSim) > 0)
      
      datatable(
        reactiveVals$allSim,
        options = list(pageLength = 10),
        rownames = FALSE
      )
    })
    
    # Render a placeholder bar graph of hormone levels by simulation
    output$barGraph <- renderPlot({
      req(nrow(reactiveVals$allSim) > 0)
      
      ggplot(reactiveVals$allSim, aes(x = Simulation_Name, y = Hormones, fill = Simulation_Name)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        labs(title = "Hormone Levels by Simulation", x = "Simulation Name", y = "Hormones") +
        theme_minimal()
    })
    
    # Predifined model
    observeEvent(input$convertSBGN, {
      if (input$predefinedNetwork == "Benchmark Model") {
        file <- "C:/Users/uqcmitsa/OneDrive - The University of Queensland/Desktop/PhD/PSoup_Shiny/Shoot_Branching_model/data/Benchmark_Model.sbgn"
        networkName <- "Benchmark Model"
      } else {
        req(input$sbgnFile)
        file <- input$sbgnFile$datapath
        networkName <- input$networkName
      }
      reactiveVals$CBnetwork <- convertSBGNdiagram(file, networkName)
      output$status <- renderText({ "Network uploaded successfully!" })
    })
    
    # (Optional) Download handler for comparison results can be added here if needed.
    
  })
}

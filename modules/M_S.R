multipleSimulationsUI <- function(id) {
  ns <- NS(id)
  
  tabPanel("Multiple Simulations",
           fluidPage(
             titlePanel(tags$div(
               style = "color: #3498db; text-align: center; font-size: 2.5rem; font-weight: bold;",
               "PSoup Network Analysis"
             )),
             tags$head(
               tags$script(
                 # Inject namespaced IDs into the JS code
                 HTML(sprintf("function toggleSidebar() {
                      var sidebar = document.getElementById('%s');
                      var mainPanel = document.getElementById('%s');
                      if (sidebar.style.display === 'none') {
                        sidebar.style.display = 'block';
                        mainPanel.classList.remove('expanded');
                      } else {
                        sidebar.style.display = 'none';
                        mainPanel.classList.add('expanded');
                      }
                    }", ns("sidebarPanel"), ns("mainPanel")))
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
                     # Inform the user that the model will be built automatically in a temporary folder
                     tags$p("The model will be built automatically in a temporary folder on the server."),
                     actionButton(ns("buildModel"), "Build Model", class = "btn-success"),
                     # Download button for retrieving the model output
                     downloadButton(ns("downloadModel"), "Download Model Output", class = "btn-primary")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-success",
                   tags$div(class = "panel-heading", "Extract modifier and exogenous list"),
                   tags$div(
                     class = "panel-body",
                     # Using a relative path that will work on the server
                     textInput(ns("saveCSV"), "Save data as CSV (subfolder name):",
                               value = "data"),
                     actionButton(ns("saveFiles"), "Save modifiers and nodes", class = "btn-danger")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-info",
                   tags$div(class = "panel-heading", "Step 3: Define experimental treatments"),
                   tags$div(
                     class = "panel-body",
                     fileInput(ns("exogenousFile"), "Upload Exogenous Definitions CSV:"),
                     fileInput(ns("genotypeFile"), "Upload Genotype Definitions CSV:"),
                     actionButton(ns("saveDefinitions"), "Save Definitions", class = "btn-info")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-warning",
                   tags$div(class = "panel-heading", "Step 4: Simulation settings"),
                   tags$div(
                     class = "panel-body",
                     numericInput(ns("maxStep"), "Max Steps:", 200, min = 1),
                     numericInput(ns("delay"), "Delay:", 20, min = 0),
                     checkboxInput(ns("exogenousSupply"), "Enable Exogenous Supply", TRUE),
                     checkboxInput(ns("combinatorial"), "Enable Combinatorial", FALSE),
                     checkboxInput(ns("preventDrop"), "Prevent Drop", TRUE),
                     actionButton(ns("runSimulations"), "Run Simulations", class = "btn-warning")
                   )
                 ),
                 hr(),
                 tags$div(
                   class = "panel panel-danger",
                   tags$div(class = "panel-heading", "Step 5: Results"),
                   tags$div(
                     class = "panel-body",
                     textInput(ns("saveCSVResults"), "Save Combined Data as CSV:",
                               value = "Test_simulation_data.csv"),
                     actionButton(ns("saveResults"), "Save Results to CSV", class = "btn-danger"),
                     actionButton(ns("showFilters"), "Show/Hide Filters", class = "btn-secondary"),
                     actionButton(ns("applyFilters"), "Apply Filters", class = "btn-secondary")
                   )
                 )
               ),
               mainPanel(
                 id = ns("mainPanel"),
                 actionButton(ns("toggleSidebar"), "Toggle Sidebar", onclick = "toggleSidebar()", class = "btn-secondary mb-3"),
                 tabsetPanel(
                   tabPanel(
                     "Status",
                     verbatimTextOutput(ns("status")),
                     tags$style(
                       "#status { background-color: #ecf0f1; padding: 15px; border-radius: 10px; font-size: 1.1rem; }"
                     )
                   ),
                   tabPanel(
                     "Preview",
                     plotOutput(ns("simulationPlot"), height = "400px")
                   ),
                   tabPanel(
                     "Results",
                     DTOutput(ns("resultsTable")),
                     hr(),
                     uiOutput(ns("filterUI"))
                   ),
                   tabPanel(
                     "Visualizations",
                     selectInput(ns("yAxis"), "Select Y-axis:", choices = NULL),
                     plotOutput(ns("barGraph"), height = "600px")
                   ),
                   tabPanel(
                     "Analysis",
                     sliderInput(ns("rangeSustainedGrowth"), "Select Range for Sustained Growth:",
                                 min = 0,
                                 max = 1,
                                 value = c(0, 1),
                                 step = 0.01),
                     selectInput(ns("xGene"), "Select X-axis Gene:", choices = NULL),
                     selectInput(ns("yGene"), "Select Y-axis Gene:", choices = NULL),
                     plotOutput(ns("geneCombinationPlot"), height = "600px")
                   ),
                   tabPanel(
                     "Training",
                     fluidRow(
                       column(6,
                              h4("Step 1: Upload CSV"),
                              fileInput(ns("inputFile"), "Upload Experimental Treatments CSV", accept = c(".csv")),
                              textInput(ns("saveLocation"), "Save Location:", "Comparison_treatments.csv"),
                              actionButton(ns("processFile"), "Process File", class = "btn-primary")
                       ),
                       column(6,
                              h4("Step 2: Download Processed File"),
                              downloadButton(ns("downloadOutput"), "Download Processed CSV", class = "btn-success")
                       )
                     ),
                     hr(),
                     fluidRow(
                       column(6,
                              h4("Step 3: Upload Comparison File"),
                              fileInput(ns("comparisonFile"), "Upload Comparison CSV", accept = c(".csv")),
                              actionButton(ns("compareBins"), "Compare Bins", class = "btn-primary")
                       ),
                       column(6,
                              h4("Step 4: Download Comparison Results"),
                              verbatimTextOutput(ns("accuracyOutput")),
                              downloadButton(ns("downloadComparison"), "Download Comparison Results", class = "btn-success")
                       )
                     ),
                     hr(),
                     DTOutput(ns("nonMatchingTable")),
                     verbatimTextOutput(ns("processStatus"))
                   )
                 )
               )
             )
           )
  )
}
multipleSimulationsServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Separate reactiveValues for comparison result
    comparisonResult <- reactiveValues(data = NULL, accuracy = NULL)
    
    # Reactive values to store data
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
      req(input$sbgnFile)
      file <- input$sbgnFile$datapath
      reactiveVals$CBnetwork <- convertSBGNdiagram(file, input$networkName)
      output$status <- renderText({ "Network converted successfully!" })
    })
    
    # Build model
    observeEvent(input$buildModel, {
      req(reactiveVals$CBnetwork)
      
      # Create a unique safe folder inside tempdir()
      safeFolder <- file.path(tempdir(), paste0("model_", format(Sys.time(), "%Y%m%d%H%M%S")))
      
      tryCatch({
        if (!dir.exists(safeFolder)) {
          dir.create(safeFolder, recursive = TRUE)
        }
        
        reactiveVals$folder <- safeFolder
        # Call your buildModel() function to write outputs to safeFolder
        buildModel(reactiveVals$CBnetwork, safeFolder, forceOverwrite = TRUE)
        
        output$status <- renderText("Model built successfully!")
      }, error = function(e) {
        showNotification(paste("Error in building model:", e$message), type = "error")
        output$status <- renderText(paste("Error:", e$message))
      })
    })
    
    output$downloadModel <- downloadHandler(
      filename = function() {
        paste0("model_output_", format(Sys.time(), "%Y%m%d%H%M%S"), ".zip")
      },
      content = function(file) {
        req(reactiveVals$folder)
        files_to_zip <- list.files(reactiveVals$folder, full.names = TRUE)
        if (length(files_to_zip) == 0) {
          stop("No files found in the model folder.")
        }
        # Use utils::zip instead of zip::zipr
        utils::zip(zipfile = file, files = files_to_zip, flags = "-j")
      }
    )
    
    
    
    # Save files (modifiers and nodes)
    observeEvent(input$saveFiles, {
      req(reactiveVals$folder)
      
      # Use the safe folder stored in reactiveVals
      folder <- reactiveVals$folder
      save_path <- input$saveCSV
      
      # Define file paths within the safe folder
      genotype_file <- file.path(folder, "genotypeDef.RData")
      nodes_file <- file.path(folder, "nodestartDef.RData")
      
      if (file.exists(genotype_file) && file.exists(nodes_file)) {
        load(genotype_file)
        load(nodes_file)
        
        if (exists("genotypeDef") && exists("nodestartDef")) {
          genotype_df <- as.data.frame(genotypeDef)
          nodes_df <- as.data.frame(nodestartDef)
          
          genotype_csv <- paste0(save_path, "_genotype.csv")
          nodes_csv <- paste0(save_path, "_nodes.csv")
          
          write.csv(genotype_df, genotype_csv, row.names = FALSE)
          write.csv(nodes_df, nodes_csv, row.names = FALSE)
          
          showNotification("Files saved successfully!", type = "message")
        } else {
          showNotification("Error: Required objects not loaded correctly.", type = "error")
        }
      } else {
        showNotification("Error: RData files not found in the specified folder.", type = "error")
      }
    })
    
    # Save genotype and exogenous definitions
    observeEvent(input$saveDefinitions, {
      req(input$exogenousFile, input$genotypeFile, reactiveVals$folder)
      exogenousDef <- read.csv(input$exogenousFile$datapath)
      genotypeDef <- read.csv(input$genotypeFile$datapath)
      reactiveVals$exogenous <- exogenousDef
      reactiveVals$Gene <- genotypeDef
      save(genotypeDef, file = file.path(reactiveVals$folder, "genotypeDef.RData"))
      save(exogenousDef, file = file.path(reactiveVals$folder, "exogenousDef.RData"))
      output$status <- renderText({ "Definitions saved successfully!" })
    })
    
    # Run simulations
    observeEvent(input$runSimulations, {
      req(reactiveVals$folder)
      output$status <- renderText({ "Simulations are loading..." })
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
    
    # Save results
    observeEvent(input$saveResults, {
      req(reactiveVals$allSim, reactiveVals$Gene, reactiveVals$exogenous)
      Hormones <- finalStates(reactiveVals$allSim$screen)
      combined_data <- data.frame(Hormones = Hormones, Gene = reactiveVals$Gene, exogenous = reactiveVals$exogenous)
      reactiveVals$combined_data <- combined_data
      reactiveVals$filtered_data <- combined_data
      
      # Save using the user-specified CSV filename within the safe folder
      write.csv(combined_data, file = file.path(reactiveVals$folder, input$saveCSVResults), row.names = FALSE)
      
      output$status <- renderText({ "Results saved successfully!" })
      
      updateSliderInput(session, "rangeSustainedGrowth", 
                        min = min(combined_data$Hormones.Sustained_growth, na.rm = TRUE), 
                        max = max(combined_data$Hormones.Sustained_growth, na.rm = TRUE), 
                        value = c(min(combined_data$Hormones.Sustained_growth, na.rm = TRUE), 
                                  max(combined_data$Hormones.Sustained_growth, na.rm = TRUE)))
    })
    
    # Show/hide filters
    observeEvent(input$showFilters, {
      reactiveVals$show_filters <- !reactiveVals$show_filters
    })
    
    output$filterUI <- renderUI({
      req(reactiveVals$combined_data, reactiveVals$show_filters)
      if (reactiveVals$show_filters) {
        cols <- names(reactiveVals$combined_data)
        filterUI <- lapply(seq(1, length(cols), by = 3), function(i) {
          fluidRow(
            lapply(cols[i:min(i+2, length(cols))], function(col) {
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
    
    # Apply filters to the data
    observeEvent(input$applyFilters, {
      req(reactiveVals$combined_data)
      filtered_data <- reactiveVals$combined_data
      isolate({
        lapply(names(filtered_data), function(col) {
          if (is.numeric(filtered_data[[col]])) {
            range_filter <- input[[paste0("filter_", col)]]
            if (!is.null(range_filter)) {
              filtered_data <- filtered_data[filtered_data[[col]] >= range_filter[1] & 
                                               filtered_data[[col]] <= range_filter[2], ]
            }
          }
        })
      })
      reactiveVals$filtered_data <- filtered_data
    })
    
    # Render combined data table
    output$resultsTable <- renderDT({
      req(reactiveVals$filtered_data)
      data_to_show <- reactiveVals$filtered_data %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
      datatable(data_to_show, options = list(pageLength = 10))
    })
    
    # Update select inputs for plotting dynamically
    observe({
      req(reactiveVals$filtered_data)
      updateSelectInput(session, "yAxis", choices = names(reactiveVals$filtered_data), selected = "Hormones.Sustained_growth")
      updateSelectInput(session, "xGene", choices = names(reactiveVals$filtered_data), selected = "Gene.1")
      updateSelectInput(session, "yGene", choices = names(reactiveVals$filtered_data), selected = "Gene.2")
    })
    
    # Generate bar graph
    output$barGraph <- renderPlot({
      req(reactiveVals$filtered_data, input$yAxis)
      data <- reactiveVals$filtered_data
      data$row_id <- seq_len(nrow(data))
      data$RowName <- apply(data, 1, function(row) {
        gene_labels <- names(row[grep("Gene.", names(row))])[row[grep("Gene.", names(row))] != 1]
        exo_labels <- names(row[grep("Exogenous.", names(row))])[row[grep("Exogenous.", names(row))] != 0]
        paste(c(paste(gene_labels, collapse = ", "), paste(exo_labels, collapse = ", ")), collapse = " | ")
      })
      data$RowName <- make.unique(data$RowName)
      
      ggplot(data, aes(x = factor(RowName, levels = RowName), y = .data[[input$yAxis]])) +
        geom_bar(stat = "identity", fill = "#85c1e9", color = "#3498db", width = 0.8) +
        theme_minimal(base_size = 16) +
        theme(
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
          panel.grid = element_blank(),
          plot.margin = margin(20, 20, 20, 20)
        ) +
        labs(title = "Scenario Bar Graph", x = "Row Description", y = input$yAxis) +
        coord_cartesian(expand = TRUE)
    })
    
    # Generate gene combination plot
    output$geneCombinationPlot <- renderPlot({
      req(reactiveVals$filtered_data, input$xGene, input$yGene, input$rangeSustainedGrowth)
      data <- reactiveVals$filtered_data
      filtered_data <- data[data$Hormones.Sustained_growth >= input$rangeSustainedGrowth[1] &
                              data$Hormones.Sustained_growth <= input$rangeSustainedGrowth[2], ]
      
      ggplot(filtered_data, aes_string(x = input$xGene, y = input$yGene)) +
        geom_point(size = 4, color = "#3498db") +
        theme_minimal(base_size = 16) +
        labs(title = "Gene Combination Plot", x = input$xGene, y = input$yGene) +
        theme(
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5)
        )
    })
    
    # Handle predefined model
    observeEvent(input$convertSBGN, {
      if (input$predefinedNetwork == "Benchmark Model") {
        file <- file.path(getwd(), "data", "Benchmark_Model.sbgn")
        networkName <- "Benchmark Model"
      } else {
        req(input$sbgnFile)
        file <- input$sbgnFile$datapath
        networkName <- input$networkName
      }
      reactiveVals$CBnetwork <- convertSBGNdiagram(file, networkName)
      output$status <- renderText({ "Network uploaded successfully!" })
    })
    
    # Compare bins and calculate accuracy
    observeEvent(input$compareBins, {
      req(input$comparisonFile)
      df <- read.csv(input$comparisonFile$datapath, stringsAsFactors = FALSE)
      matches <- df$PSoup.bin == df$Biological.bin
      accuracy_percentage <- mean(matches) * 100
      non_matching_rows <- df[!matches, ]
      
      result_df <- data.frame(
        Name = "",
        Network.explanation = "",
        Psoup.treatment = non_matching_rows$Psoup.treatment,
        Compare.with = non_matching_rows$Compare.with,
        Hormones.Sustained_growth.Psoup = non_matching_rows$Hormones.Sustained_growth.Psoup,
        Hormones.Sustained_growth.Compare = non_matching_rows$Hormones.Sustained_growth.Compare,
        PSoup.bin = non_matching_rows$PSoup.bin,
        Biological.bin = non_matching_rows$Biological.bin,
        Percentage.accuracy = accuracy_percentage
      )
      
      comparisonResult$data <- result_df
      comparisonResult$accuracy <- accuracy_percentage
      
      output$accuracyOutput <- renderText({
        paste0("Accuracy: ", sprintf("%.2f", accuracy_percentage), "%")
      })
      
      output$nonMatchingTable <- renderDT({
        req(comparisonResult$data)
        datatable(comparisonResult$data, options = list(pageLength = 10))
      })
    })
    
    # Download comparison result
    output$downloadComparison <- downloadHandler(
      filename = function() { "Comparison_Bio_Sim_Results.csv" },
      content = function(file) {
        write.csv(comparisonResult$data, file, row.names = FALSE)
      }
    )
  })
}

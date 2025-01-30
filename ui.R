source("modules/network_module.R")    # Load Network Analysis UI
source("modules/simulations_module.R") # Load Simulations UI
source("modules/manual_simulations.R") # Load Manual Simulations UI

ui <- navbarPage(
  theme = my_theme,
  title = "PSoup Network Analysis",
  
  tabPanel("Home",
           fluidPage(
             div(
               style = "background-color: #3498db; padding: 20px; text-align: center; color: white; font-size: 2.5rem; font-weight: bold;",
               "PSoup Network Analysis"
             ),
             includeCSS("www/styles.css"),  # Load custom styles (optional)
             networkHomeUI("network_home")   # Call Network UI Module
           )
  ),
  
  tabPanel("Multiple Simulations", simulationsUI("simulations")),
  tabPanel("Manual Simulations", manualSimulationsUI("manual_simulations"))
)

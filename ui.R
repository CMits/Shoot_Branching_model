source("modules/network_module.R")    # Load Network Analysis UI
source("modules/simulations_module.R") # Load Simulations UI
source("modules/manual_simulations.R") # Load Manual Simulations UI
source("modules/M_S.R") # Load multiple simulations

ui <- navbarPage(
  theme = my_theme,
  title = "PSoup Network Analysis",
  
  tabPanel("Home", networkHomeUI("home")),
  tabPanel("Multiple Simulations", multipleSimulationsUI("multiple_simulations")),
  tabPanel("Manual Simulations", manualSimulationsUI("manual_simulations"))
)

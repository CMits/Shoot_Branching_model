source("modules/network_module.R")
source("modules/Multiple_S.R")
source("modules/manual_simulations.R")

# In your main server function
server <- function(input, output, session) {
  # Use moduleServer() to call your modules
  networkHomeServer("home")
  multipleSimulationsServer("multiple_simulations")
  manualSimulationsServer("manual_simulations")
}

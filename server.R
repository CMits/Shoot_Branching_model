source("modules/network_module.R")
source("modules/simulations_module.R")
source("modules/manual_simulations.R")

server <- function(input, output, session) {
  callModule(networkHomeServer, "network_home")
  callModule(simulationsServer, "simulations")
  callModule(manualSimulationsServer, "manual_simulations")
}

source("modules/network_module.R")
source("modules/Multiple_Simulations.R")
source("modules/manual_simulations.R")

server <- function(input, output, session) {
  callModule(networkHomeServer, "home")
  multipleSimulationsServer("multiple_simulations")
  callModule(manualSimulationsServer, "manual_simulations")
}

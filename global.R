# Load required libraries
# Check if PSoup is installed, if not install it from GitHub
if (!requireNamespace("PSoup", quietly = TRUE)) {
  remotes::install_github("NicoleZFortuna/PSoup")
}
library(future)
plan(multisession)
# Load the package after installation
library(PSoup)

library(shiny)
library(DT)
library(ggplot2)
library(bslib)
library(visNetwork)
library(dplyr)


# Define a custom theme
my_theme <- bs_theme(
  bootswatch = "flatly",
  primary = "#3498db",
  secondary = "#f7dc6f",
  success = "#2ecc71",
  warning = "#f39c12",
  danger = "#e74c3c",
  font_scale = 1.1,
  heading_font = "Roboto",
  base_font = "Open Sans"
)

# Common function for notifications
showMessage <- function(message, type = "message") {
  showNotification(message, type = type, duration = 3)
}

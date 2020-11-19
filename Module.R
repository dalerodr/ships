# Shiny Module
library(shinyWidgets)

ships <- readRDS(file = "ships.rds")

# Function for module UI
dropdown_fieldUI <- function(id, label, choices) {
  ns <- NS(id)
  
  column(6, dropdown_input(label, sort(choices), value = NULL, type = "selection unique"))
  
}

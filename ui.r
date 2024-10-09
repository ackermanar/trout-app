# List of required packages
required_packages <- c("shiny", "shinydashboard", "shinyjs", "DT", "kinship2", "openxlsx", "tidyverse")

# Function to check and install missing packages
install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Check and install each package
lapply(required_packages, install_if_missing)

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(kinship2)
library(openxlsx)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Trout app"),
  sidebarLayout(
    sidebarPanel(
      h3("Generate EBV matrix and filter with designated kinship coefficient"),
      fileInput("candidate_file", "Upload candidate mates in csv format", accept = c(".csv", ".txt")),
      h4("Calculate kinship matrix"),
      fileInput("pedigree_file", "Choose pedigree file, e.g. 'even_year_ped.txt'",
                accept = ".txt"),
      h4("Calculate EBVs"),
      fileInput("weight_file", "Upload EBVs for weights", accept = c(".csv", ".txt")),
      numericInput("weight1", "EBV weight for fish weight:", 0.5, min = 0, max = 1, step = 0.1),
      verbatimTextOutput("weight1"),
      fileInput("length_file", "Upload EBVs for length", accept = c(".csv", ".txt")),
      numericInput("weight2", "EBV weight for fish length:", 0.5, min = 0, max = 1, step = 0.1),
      h4("Select level to threshold Kinship"),
      numericInput("thresh", "Threshold to filter kinship:", 1, min = 0, max = 1, step = 0.1),
      h3("Export results"),
      downloadButton("download", label = "Download")
    ),
    mainPanel(
      h5("User feedback for calclulating kinship:"),
      verbatimTextOutput("message1"),
      h5("User feedback for calclulating EBVs:"),
      verbatimTextOutput("message2"),
      DTOutput("quadrants_table"),
      DTOutput("matrix")
    )
  )
)
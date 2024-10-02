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
      h4("Upload candidate mates"),
      fileInput("candidate_file", "Upload candidate parents in csv format", accept = c(".csv", ".txt")),
      h4("Calculate kinship matrix"),
      fileInput("pedigree_file", "Choose pedigree file, e.g. 'even_year_ped.txt'",
                accept = ".txt"),
      h4("Calculate EBVs"),
      fileInput("weight_file", "Upload weights", accept = c(".csv", ".txt")),
      numericInput("weight1", "EBV weight for fish weight:", 0.5, min = 0, max = 1, step = 0.1),
      verbatimTextOutput("weight1"),
      fileInput("length_file", "Upload length", accept = c(".csv", ".txt")),
      numericInput("weight2", "EBV weight for fish length:", 0.5, min = 0, max = 1, step = 0.1),
      h4("Download results"),
      downloadButton("download", label = "Download")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Parents",
          h5("User feedback:"),
          verbatimTextOutput("message1"),
          DTOutput("quadrants_table"),
          DTOutput("matrix")
        ),
        tabPanel("EBVs",
          h5("User feedback:"),
          verbatimTextOutput("message2"),
          verbatimTextOutput("weightText1"),
          verbatimTextOutput("weightText2"),
          tableOutput("matrix2")
        )
      )
    )
  )
)

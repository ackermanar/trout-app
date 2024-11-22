# List of required packages
required_packages <- c("shiny", "shinydashboard", "shinyjs", "DT", "janitor", "kinship2", "openxlsx", "tidyverse")

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
library(janitor)
library(kinship2)
library(openxlsx)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Trout app"),
  sidebarLayout(
    sidebarPanel(
      h3("Estimate progeny genetic merit"),
      fileInput("candidate_file", "Upload list of candidates", accept = c(".csv", ".txt")),
      h4("Calculate kinship matrix"),
      fileInput("pedigree_file", "Upload pedigree file",
                accept = ".txt"),
      h4("Calculate EBVs"),
      fileInput("weight_file", "Upload EBVs for live weight", accept = c(".csv", ".txt")),
      numericInput("weight1", "Relative weight for live weight EBV:", 0.5, min = 0, max = 1, step = 0.1),
      verbatimTextOutput("weight1"),
      fileInput("length_file", "Upload EBVs for body length", accept = c(".csv", ".txt")),
      numericInput("weight2", "Relative weight for body length EBV:", 0.5, min = 0, max = 1, step = 0.1),
      h4("Select max kinship allowed between mates"),
      numericInput("thresh", "Kinship threshold:", 1, min = 0, max = 1, step = 0.1),
      h4("Export results"),
      downloadButton("download1", label = "Download"),
      br(),
      br(),
      h3("Generate cross reports"),
      fileInput("running_spawners", "Upload list of crosses for report", accept = c(".csv", ".txt")),
      fileInput("unsuccesful", "Upload list of culled crosses", accept = c(".csv", ".txt")),
      h4("Export reports"),
      downloadButton("download2", label = "Download spawner list")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Kinship and EBV",
          h5("User feedback for calclulating kinship:"),
          verbatimTextOutput("message1"),
          h5("User feedback for calclulating EBVs:"),
          verbatimTextOutput("message2"),
          DTOutput("quadrants_table"),
          DTOutput("matrix")
        ),
        tabPanel("Running spawners",
          h5("User feedback for tracking running spawners:"),
          verbatimTextOutput("message3"),
          fluidRow(
            column(6, DTOutput("cross_counter")),
            column(6, DTOutput("family_counter"))
          )
        )
      )
    )
  )
)

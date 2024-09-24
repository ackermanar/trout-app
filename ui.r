  library(shiny)
  library(shinydashboard)
  library(DT)
  library(kinship2)
  library(tidyverse)

  ui <- fluidPage(
    titlePanel("Trout app"),
    sidebarLayout(
      sidebarPanel(
        h4("Calculate kinship matrix"),
        fileInput("file1", "Choose pedigree file, e.g. 'even_year_ped.txt'",
                  accept = ".txt"),
        fileInput("file2", "Upload available parents in csv format", accept = c(".csv", ".txt")),
        downloadButton("download", label = "Download"),
        # New section
        h4("Calculate EBVs"),
        checkboxInput("choice", "Use available parent list", value = FALSE),
        fileInput("file3", "Upload available candidates", accept = c(".csv", ".txt")),
        fileInput("file4", "Upload weights", accept = c(".csv", ".txt")),
        numericInput("weight1", "EBV weight for fish weight:", 0.5, min = 0, max = 1),
        verbatimTextOutput("weight1"),
        fileInput("file5", "Upload length", accept = c(".csv", ".txt")),
        numericInput("weight2", "EBV weight for fish length:", 0.5, min = 0, max = 1),
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Parents",
            h3("Parents:"),
            tableOutput("matrix")
          ),
          tabPanel("EBVs",
            h3("EBVs:"),
            verbatimTextOutput("weight1"),
            verbatimTextOutput("weight2"),
            tableOutput("matrix2")
          )
        )
      )
    )
  )

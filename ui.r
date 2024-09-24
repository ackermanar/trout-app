  library(shiny)
  library(shinydashboard)
  library(DT)
  library(kinship2)
  library(openxlsx)
  library(tidyverse)

  ui <- fluidPage(
    titlePanel("Trout app"),
    sidebarLayout(
      sidebarPanel(
        h4("Upload candidate mates"),
        fileInput("candidate_file", "Upload available parents in csv format", accept = c(".csv", ".txt")),
        h4("Calculate kinship matrix"),
        fileInput("pedigree_file", "Choose pedigree file, e.g. 'even_year_ped.txt'",
                 accept = ".txt"),
        h4("Calculate EBVs"),
        fileInput("weight_file", "Upload weights", accept = c(".csv", ".txt")),
        numericInput("weight1", "EBV weight for fish weight:", 0.5, min = 0, max = 1),
        verbatimTextOutput("weight1"),
        fileInput("length_file", "Upload length", accept = c(".csv", ".txt")),
        numericInput("weight2", "EBV weight for fish length:", 0.5, min = 0, max = 1),
        h4("Download results"),
        downloadButton("download", label = "Download")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Parents",
            tableOutput("matrix")
          ),
          tabPanel("EBVs",
            verbatimTextOutput("weight1"),
            verbatimTextOutput("weight2"),
            tableOutput("matrix2")
          )
        )
      )
    )
  )


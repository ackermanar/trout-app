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
        sliderInput("slider", "Select EBV weight for trout weight", min = 0, max = 1, value = 0.5),
        fileInput("file5", "Upload length", accept = c(".csv", ".txt")),
        sliderInput("slider", "Select EBV weight for trout length", min = 0, max = 1, value = 0.5)
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Parents",
            h3("Parents:"),
            verbatimTextOutput("text1"),
            tableOutput("matrix")
          ),
          tabPanel("EBVs",
            h3("EBVs:"),
            verbatimTextOutput("text2"),
            tableOutput("matrix2")
          )
        )
      )
    )
  )

  server <- function(input, output, session) { # nolint
    observe({
      file1 <- input$file1
      req(file1)
      raw_ped <- read_table(file1$datapath) %>%
        mutate(id = as.factor(id),
          sire = as.factor(sire),
          dam = as.factor(dam)
        )

      sex_ped <-raw_ped %>%
        mutate(sex = case_when(
               id %in% sire ~ 0,
               id %in% dam ~ 1,
               .default = 2
        ))

      #check for ids that appear as both sire and dam
      messy_parents <- as.data.frame(intersect(sex_ped$sire, sex_ped$dam)) %>%
        rename(id = 1) %>%
        filter(id != 0)

      #change values in messy_parents to 0 in cols for sire and dam
      #create new datafram
      parents_fixed_ped <- sex_ped
      # Fix values in sire column
      parents_fixed_ped$sire[parents_fixed_ped$sire %in% messy_parents[, 1]] <- 0
      # Fix values in dam column
      parents_fixed_ped$dam[parents_fixed_ped$dam %in% messy_parents[, 1]] <- 0

      #check for duplicate IDs
      n_occur <- data.frame(table(parents_fixed_ped$id)) %>%
        rename(id = 1, freq = 2) #count occurrence of each ID

      double_ids <- n_occur[n_occur$freq > 1,] #print ids with more than 1 occurrence

      #create new dataframe without doubled ids(all instances are removed to avoid data errors)
      nodup_ped <- parents_fixed_ped[!parents_fixed_ped$id %in% double_ids$id, ]

  #check for individuals that appear as offspring and sire or offspring and dam of themselves

      circ_deps <- nodup_ped %>%
        mutate(id = as.character(id),
               sire = as.character(sire),
               dam = as.character(dam)) %>%
        filter(id == sire | id == dam)

      no_circ_deps <- anti_join(nodup_ped, circ_deps, by = "id")

      #run FixPedigree to add parents that are not found as founders
      ready_ped <- with(no_circ_deps, fixParents(id, sire, dam, sex, missid = "0"))
      
      #create a pedigree object
      final_ped <- with(ready_ped, pedigree(id, dadid, momid, sex, missid = "0"))

      #calculate kinship matrix
      kinship_matrix <- kinship(final_ped) #check Amatrix
              # Render tbl
      # read in list of candidates with cols
      req(input$file2)
      file2 <- input$file2
      candidates <- read_table(file2$datapath)
      # Make lists of males and females
      males_to_select <- candidates %>%
        subset(sex == "M") %>%
        pull(id)

      females_to_select <- candidates %>%
        subset(sex == "F") %>%
        pull(id)

      selected_matrix <- kinship_matrix[males_to_select, females_to_select]

      output$text1 <- renderPrint({
        paste(selected_matrix)
      })

      # Select rows and columns based on the list of ids
      selected_matrix <- kinship_matrix[males_to_select, females_to_select]

      # Add labels to the rows and columns
      rownames(selected_matrix) <- paste("male", rownames(selected_matrix), sep = "_")
      colnames(selected_matrix) <- paste("female", colnames(selected_matrix), sep = "_")

      output$download <- downloadHandler(
        filename = function() {
        paste0("kinship_matrix-", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(selected_matrix, file)
      }
    )
    }) %>%
    bindEvent(input$file1, input$file2)
  }
  shinyApp(ui, server)

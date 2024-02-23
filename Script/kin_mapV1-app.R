library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(kinship2)

ui <- fluidPage(
  titlePanel("Pedigree Kinship Calculator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose pedigree file, e.g. 'sealice.txt.'",
                accept = ".txt"),
      br(),
      fileInput("file2", "Upload available parents in csv format", accept = c(".csv")),
      br(),
      downloadButton("download", label = "Download")
    ),
    mainPanel(
      h3("Parents:"),
      verbatimTextOutput("text1"),
      tableOutput("matrix")
    )
  )
)

server <- function(input, output, session) { # nolint
  observe({
    file1 <- input$file1
    req(file1)
    raw_ped <- read_table(file1$datapath) %>%
      rename(id = 1, sire = 2, dam = 3) %>%
      mutate(id = as.factor(id),
        sire = as.factor(sire),
        dam = as.factor(dam)
      )

    sex_ped <- raw_ped %>%
      mutate(sex = ifelse(id %in% sire, 1, 2))

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

    #run FixPedigree to add parents that are not found as founders
    ready_ped <- with(nodup_ped, fixParents(id, sire, dam, sex, missid = "0"))
    
    #create a pedigree object
    final_ped <- with(ready_ped, pedigree(id, dadid, momid, sex, missid = "0"))

    #calculate kinship matrix
    kinship_matrix <- kinship(final_ped) #check Amatrix

    # Make lists of males and females
    males <- ready_ped %>%
      filter(sex == 1) %>%
      pull(id)

    females <- ready_ped %>%
      filter(sex == 2) %>%
      pull(id)

    file2 <- input$file2
    req(input$file2)

    ids_to_select <- read_csv("/Users/aja294-admin/Library/CloudStorage/Box-Box/aja294_box/salmonoids/trout/kin_calc-app/Data/parents.csv", col_names = FALSE) %>%
      select(1) %>%
      pull() %>%
      as.character()

    output$text1 <- renderPrint({
      paste(ids_to_select)
    })

    # Assign ids entered to males and females
    males_to_select <- ids_to_select[ids_to_select %in% males]
    females_to_select <- ids_to_select[ids_to_select %in% females]

    # Select rows and columns based on the list of ids
    selected_matrix <- kinship_matrix[males_to_select, females_to_select]
    
    # Render tbl
    output$matrix <- renderTable({
      selected_matrix
    }, rownames = TRUE)

    output$download <- downloadHandler(
      filename = function() {
      paste0("kinship_matrix-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(selected_matrix, file)
    }
  )
  }) %>%
  bindEvent(input$file1, input$file2, input$download)
}
shinyApp(ui, server)

library(shiny)
library(shinydashboard)
library(DT)
library(kinship2)
library(tidyverse)

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

    # Select rows and columns based on the list of ids
    selected_matrix <- kinship_matrix[males_to_select, females_to_select]

    # Add labels to the rows and columns
    rownames(selected_matrix) <- paste("male", rownames(selected_matrix), sep = "_")
    colnames(selected_matrix) <- paste("female", colnames(selected_matrix), sep = "_")

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

  # EBV calculations

#If checkbox is checked use previous candidate list if not use file3
if(input$choice == FALSE){

    #read in list of candidates with cols id and sex(male 0, female 1)
  file3 <- input$file3
  req(file3)

  candidates <- read_table(file3$datapath)
  # Assign ids entered to males and females
  males_to_select <- candidates %>%
    filter(sex == "M") %>%
    pull(id)

  females_to_select <- candidates %>%
    filter(sex == "F") %>%
    pull(id)
}

  #read in EBV files
  file4 <- input$file4
  req(file4)
  weight_ebvs <- read_table(file4$datapath)

  file5 <- input$file5
  req(file5)
  length_ebvs <- read_table(file5$datapath)

    #### EBV calculations ####
    # calculate index function
    calculate_index <- function(...) {
    args <- list(...)
    
    # Ensure the number of arguments is even (each dataset must have a corresponding weight)
    if (length(args) %% 2 != 0) {
        stop("Each EBV dataset must be followed by a corresponding weight.")
    }
    
    # Separate the datasets and weights
    ebv_list <- args[seq(1, length(args), by = 2)]
    rel_weights <- unlist(args[seq(2, length(args), by = 2)])
    
    # Check that the sum of the weights is 1
    if (sum(rel_weights) != 1.00) {
        stop("The sum of relative weights must equal 1.00.")
    }
    
    # Start with the first dataset in the list
    joint_ebvs <- ebv_list[[1]]
    
    # Merge all EBV datasets by 'ID'
    for (i in 2:length(ebv_list)) {
        joint_ebvs <- merge(joint_ebvs, ebv_list[[i]], by = "ID", suffixes = c("", paste0(".", i)))
    }
    
    # Calculate the index value
    joint_ebvs$index_val <- 0
    for (i in 1:length(rel_weights)) {
        if (i == 1) {
        ebv_col_name <- "EBV"
        } else {
        ebv_col_name <- paste0("EBV.", i)
        }
        joint_ebvs$index_val <- joint_ebvs$index_val + (rel_weights[i] * joint_ebvs[[ebv_col_name]])
    }
    
    return(joint_ebvs)
    }

  output$weight1 <- renderText({
    paste("EBV weight for fish weight:", input$weight1)
  })

  output$weight2 <- renderText({
    paste("EBV weight for fish length:", input$weight2)
  })

    weight_length_index <- calculate_index(weight_ebvs, input$weight1, length_ebvs, input$weight2)

    # subset EBVs to those of candidates only and by sex
    candidate_ebvs <- candidates %>%
      left_join(weight_length_index, by = c("id" = "ID")) %>%
    select(c(1,2,5))

    male_candidate_ebvs <- candidate_ebvs[candidate_ebvs$id %in% males_to_select, ] %>% 
    select(c(1,3)) %>%
    mutate(
        id = paste("male", id, sep = "_")
    )
    female_candidate_ebvs <- candidate_ebvs[candidate_ebvs$id %in% females_to_select, ] %>% 
    select(c(1,3)) %>%
    mutate(
        id = paste("female", id, sep = "_")
    )

    # make matrix of EBVs
    # Initialize an empty matrix
    ebv_matrix <- matrix(NA, nrow = nrow(male_candidate_ebvs), ncol = nrow(female_candidate_ebvs))
    rownames(ebv_matrix) <- male_candidate_ebvs$id
    colnames(ebv_matrix) <- female_candidate_ebvs$id

    # Populate the matrix with average EBV values
    for(i in 1:nrow(male_candidate_ebvs)) {
    for(j in 1:nrow(female_candidate_ebvs)) {
        ebv_matrix[i, j] <- round(mean(c(male_candidate_ebvs$index_val[i], female_candidate_ebvs$index_val[j])),2)
    }
    }
    output$matrix2 <- renderTable({
      ebv_matrix
    }, rownames = TRUE)
  }) %>%
  bindEvent(input$file1, input$file2, input$download, input$file3, input$file4, input$file5)
}

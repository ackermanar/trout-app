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

server <- function(input, output, session) { # nolint
  observe({
    # User feedback messages
    output$message1 <- renderText({
      if (is.null(input$pedigree_file)) {
        return("Please upload a candidate file and a pedigree file.")
      }
    })

    output$message2 <- renderText({
      if (is.null(input$pedigree_file)) {
        return("Please upload weights for length and weight.")
      }
    })

    output$message3 <- renderText({
      if (is.null(input$running_spawners)) {
        return("Please upload a running spawner list.")
      }
    })

    # read in list of candidates with cols
    req(input$candidate_file)
    tryCatch({
      candidates <- read_table(input$candidate_file$datapath)

      # Make lists of males and females
      males_to_select <- candidates %>%
        subset(sex == "M") %>%
        pull(id)

      females_to_select <- candidates %>%
        subset(sex == "F") %>%
        pull(id)
    }, error = function(e) {
      output$message1 <- renderText({
        paste("Please inspect the candidate file formatting, an error occurred:", e$message)
      })
    })

    output$message1 <- renderText({
      paste("Candidate file upload successful, please upload a pedigree file.")
    })

    output$message2 <- renderText({
      paste("Candidate file upload successful, please upload weight and length files.")
    })

    # Kinship matrix calculation -------------------------------------
    if (!is.null(input$pedigree_file)) {
      tryCatch({
        raw_ped <- read_table(input$pedigree_file$datapath) %>%
          mutate(id = as.factor(id),
            sire = as.factor(sire),
            dam = as.factor(dam)
          )

        sex_ped <- raw_ped %>%
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

        double_ids <- n_occur[n_occur$freq > 1, ] #print ids with more than 1 occurrence

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

        selected_matrix <- kinship_matrix[males_to_select, females_to_select]

        # Select rows and columns based on the list of ids
        selected_matrix <- kinship_matrix[males_to_select, females_to_select]

        quantilesKinship <- list(
          Data = "Kinship",
          Q25 = quantile(selected_matrix, 0.25),
          Q50 = quantile(selected_matrix, 0.50),
          Q75 = quantile(selected_matrix, 0.75),
          Q100 = quantile(selected_matrix, 1.00)
        )

        # Convert the list of quantiles to a tibble column
        quadrants <- as_tibble(quantilesKinship) %>%
          column_to_rownames(var = "Data")

        output$quadrants_table <- renderDT({
          datatable(quadrants,
                    options = list(ordering = FALSE, dom = "t"),
                    rownames = TRUE) %>%
            formatStyle(
              "Q25",
              backgroundColor = "lightgreen"
            ) %>%
            formatStyle(
              "Q50",
              backgroundColor = "yellow"
            ) %>%
            formatStyle(
              "Q75",
              backgroundColor = "orange"
            ) %>%
            formatStyle(
              "Q100",
              backgroundColor = "coral"
            )
        })

        results <- as_tibble(selected_matrix, rownames = "Row") %>%
          pivot_longer(-Row, names_to = "Female", values_to = "Kinship") %>%
          select(Female, Row, Kinship) %>%
          rename(Male = Row) %>%
          arrange(Kinship)

        # Render tbl
        output$matrix <- renderDT({
          datatable(results, rownames = TRUE) %>%
            formatStyle(
              "Kinship",
              backgroundColor = styleInterval(
                c(
                  quantilesKinship$Q25[[1]],
                  quantilesKinship$Q50[[1]],
                  quantilesKinship$Q75[[1]]
                ),
                c("lightgreen", "yellow", "orange", "coral")  # Four colors for three intervals
              )
            )
        })
        if(!is.null(results)) {
          output$message1 <- renderText({
            paste("Kinship matrix generated succesfully.")
          })
        }
      }, error = function(e) {
        output$message1 <- renderText({
          paste("Please inspect the pedigree file formatting, an error occurred:", e$message)
        })
      }) # End second tryCatch
    } # End kinship matrix calculation

    # EBV matrix calculation -------------------------------------

    if (!is.null(input$weight_file) && !is.null(input$length_file)) {
      tryCatch({
        #read in EBV files
        req(input$candidate_file, input$weight_file, input$length_file)
        weight_ebvs <- read_table(input$weight_file$datapath)
        length_ebvs <- read_table(input$length_file$datapath)
        testTot <- input$weight1 + input$weight2

        # calculate index function
        calculate_index <- function(...) {
          args <- list(...)

          # Ensure the number of arguments is even (each dataset must have a corresponding weight)
          if (length(args) %% 2 != 0) {
            output$message2 <- renderText({
              paste("Each EBV dataset must be followed by a corresponding weight.")
            })
            stop("Each EBV dataset must be followed by a corresponding weight.")
          }

          # Separate the datasets and weights
          ebv_list <- args[seq(1, length(args), by = 2)]
          rel_weights <- unlist(args[seq(2, length(args), by = 2)])

          # Start with the first dataset in the list
          joint_ebvs <- ebv_list[[1]]

          # Merge all EBV datasets by "ID"
          for (i in 2:length(ebv_list)) {
            joint_ebvs <- merge(joint_ebvs, ebv_list[[i]], by = "ID", suffixes = c("", paste0(".", i)))
          }

          # Calculate the index value
          joint_ebvs$index_val <- 0
          for (i in seq_along(rel_weights)) {
            if (i == 1) {
              ebv_col_name <- "EBV"
            } else {
              ebv_col_name <- paste0("EBV.", i)
            }
            joint_ebvs$index_val <- joint_ebvs$index_val + (rel_weights[i] * joint_ebvs[[ebv_col_name]])
          }
          return(joint_ebvs)
        }

        weight_length_index <- calculate_index(weight_ebvs, input$weight1, length_ebvs, input$weight2)

        # subset EBVs to those of candidates only and by sex
        candidate_ebvs <- candidates %>%
          left_join(weight_length_index, by = c("id" = "ID")) %>%
          select(c(1, 2, 5))

        male_candidate_ebvs <- candidate_ebvs[candidate_ebvs$id %in% males_to_select, ] %>% 
          select(c(1, 3))

        female_candidate_ebvs <- candidate_ebvs[candidate_ebvs$id %in% females_to_select, ] %>% 
          select(c(1, 3))

        # make matrix of EBVs
        # Initialize an empty matrix
        ebv_matrix <- matrix(NA, nrow = nrow(male_candidate_ebvs), ncol = nrow(female_candidate_ebvs))
        rownames(ebv_matrix) <- male_candidate_ebvs$id
        colnames(ebv_matrix) <- female_candidate_ebvs$id

        # Populate the matrix with average EBV values
        for (i in seq_len(nrow(male_candidate_ebvs))) {
          for (j in seq_len(nrow(female_candidate_ebvs))) {
            ebv_matrix[i, j] <- round(mean(c(male_candidate_ebvs$index_val[i], female_candidate_ebvs$index_val[j])),2)
          }
        }

        # Calculate quantiles for the EBV matrix
        quantilesEBV <- list(
          Data = "EBV",
          Q25 = quantile(ebv_matrix, 1.00),
          Q50 = quantile(ebv_matrix, 0.70),
          Q75 = quantile(ebv_matrix, 0.50),
          Q100 = quantile(ebv_matrix, 0.25)
        )

        # Convert the list of quantiles to a tibble column
        quadrants <- as_tibble(quantilesEBV) %>%
          column_to_rownames(var = "Data") %>%
          bind_rows(quadrants) %>%
           rownames_to_column(var = "Data") %>%
          arrange(desc(Data == "Kinship"), Data) %>%
          column_to_rownames(var = "Data")

        # Render full quantile summary with formatting
        output$quadrants_table <- renderDT({
          datatable(quadrants,
                    options = list(ordering = FALSE, dom = "t"),
                    rownames = TRUE) %>%
            formatStyle(
              "Q25",
              backgroundColor = "lightgreen"
            ) %>%
            formatStyle(
              "Q50",
              backgroundColor = "yellow"
            ) %>%
            formatStyle(
              "Q75",
              backgroundColor = "orange"
            ) %>%
            formatStyle(
              "Q100",
              backgroundColor = "coral"
            )
        })

        results <- as_tibble(ebv_matrix, rownames = "Row") %>%
          pivot_longer(-Row, names_to = "Female", values_to = "EBV") %>%
          rename(Male = Row) %>%
          left_join(results, by = c("Female", "Male")) %>%
          select(Female, Male, Kinship, EBV) %>%
          arrange(Kinship, desc(EBV))

        filtered_results <- results %>%
          filter(Kinship < input$thresh & EBV > 0)

        output$matrix <- renderDT({
          datatable(filtered_results, rownames = TRUE) %>%
            formatStyle(
              "Kinship",
              backgroundColor = styleInterval(
                c(
                  quantilesKinship$Q25[[1]],
                  quantilesKinship$Q50[[1]],
                  quantilesKinship$Q75[[1]]
                ),
                c("lightgreen", "yellow", "orange", "coral")  # Four colors for three intervals
              )
            ) %>%
            formatStyle(
              "EBV",
              backgroundColor = styleInterval(
                c(
                  quantilesEBV$Q100[[1]],
                  quantilesEBV$Q75[[1]],
                  quantilesEBV$Q50[[1]]
                ),
                c("coral", "orange", "yellow", "lightgreen")  # Four colors for three intervals
              )
            )
        })

        matDL <- results %>%
          mutate(EBV = case_when(
                                 Kinship > input$thresh | EBV < 0 ~ NA_real_,
                                 .default = EBV)
          ) %>%
          select(Female, Male, EBV) %>%
          pivot_wider(names_from = Female, values_from = EBV) %>%
          column_to_rownames(var = "Male") %>%
          as.matrix(.)

        if (testTot != 1 && !is.null(ebv_matrix)) {
          output$message2 <- renderText({
            paste("Warning: EBV weights are not equal to 1, current value:", testTot)
          })
        } else if (!is.null(ebv_matrix) && testTot == 1) {
          output$message2 <- renderText({
            paste("EBV matrix generated succesfully.")
          })
        }
      } , error = function(e) {
        output$message2 <- renderText({
          paste("An error occurred:", e$message)
        })
      }) # End third tryCatch
    } # End EBV matrix calculation

    # Running spawners calculation -------------------------------------
    if (!is.null(input$running_spawners)) {
      tryCatch({
        # read in ebv list (this is where the family and full ID will be pulled from)
        ebvs <- length_ebvs %>%
          select(c(1)) %>%
          separate(ID, into = c("tag", "family"), sep = "_") %>%
          mutate(last_four = str_sub(tag, -4, -1))

        # read list of broodstock used
        spawners <- read.table(input$running_spawners$datapath, sep = "\t", header = T) %>%
          janitor::clean_names() %>%
          select(c(1,2,4,7,8))

        # pull info from previously updated ebv file and generate a full report
        spawners <- spawners %>%
          left_join(ebvs, by = c("female" = "last_four")) %>%
          rename(female_fam = family, female_tag = tag) %>%
          left_join(ebvs, by = c("male" = "last_four")) %>%
          rename(male_fam = family, male_tag = tag,`2024_cross` = cross) %>%
          mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
          select(c(4, 1, 6, 7, 8, 9, 5)) %>%
          arrange(date)

        # count how many times each cross between families (regardless of reciprocals) has been made
        cross_counter <- spawners %>%
          mutate(
            cross_id = ifelse(
              as.numeric(gsub("CX-", "", female_fam)) < as.numeric(gsub("CX-", "", male_fam)),
              paste(female_fam, male_fam, sep = " x "),
              paste(male_fam, female_fam, sep = " x ")
            )
          ) %>%
          group_by(cross_id) %>%
          summarise(count = n(), .groups = "drop") %>%
          arrange(desc(count))

        # count how many times each family has been used regardless of male and female
        family_counter <- as.data.frame(rbind(
          data.frame(family = spawners$male_fam),
          data.frame(family = spawners$female_fam)
        )) %>%
          group_by(family) %>%  # Correctly referencing the column name
          summarise(count = n(), .groups = "drop") %>%
          arrange(desc(count))

        output$message3 <- renderText({
          paste("Running spawners list uploaded successfully.")
        })

        output$cross_counter <- renderDT({
          datatable(cross_counter, rownames = FALSE) %>%
            formatStyle(
              "count",
              backgroundColor = styleInterval(
                c(4, 3, 2, 1),
              ),
              c("coral", "orange", "yellow", "lightgreen")
            )
        })

        output$family_counter <- renderDT({
          datatable(family_counter, rownames = FALSE) %>%
            formatStyle(
              "count",
              backgroundColor = styleInterval(
                c(4, 8, 12),
                c("lightgreen", "yellow", "orange", "coral")
              )
            )
        })

      }, error = function(e) {
        output$message3 <- renderText({
          paste("An error occurred in running spawners:", e$message)
        })

      })
    } # End running spawners calculation

    output$download1 <- downloadHandler(
      filename = function() {
        paste0("trout_app_results-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()

        # Add a worksheet for the selected matrix
        addWorksheet(wb, "Mate Selection")
        writeData(wb, sheet = "Mate Selection", results, rowNames = TRUE)

        # Add a worksheet for the matrix
        addWorksheet(wb, "EBV Matrix")
        writeData(wb, sheet = "EBV Matrix", matDL, rowNames = TRUE)

        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$download2 <- downloadHandler(
      filename = function() {
        paste0("running_spawners-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        wb <- createWorkbook()

        # Add a worksheet for the selected matrix
        addWorksheet(wb, "cross-to-date report")
        writeData(wb, sheet = "cross-to-date report", spawners, rowNames = TRUE)

        # Add a worksheet for the matrix
        addWorksheet(wb, "cross counter")
        writeData(wb, sheet = "cross counter", cross_counter, rowNames = TRUE)

        addWorksheet(wb, "Family counter")
        writeData(wb, sheet = "Family counter", family_counter, rowNames = TRUE)

        # Save the workbook
        saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  }) %>%
    bindEvent(input$pedigree_file, input$candidate_file, input$thresh, input$download1, input$weight_file, input$weight1, input$length_file, input$weight2, input$running_spawners, input$download2)
}
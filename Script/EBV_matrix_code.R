
library(kinship2)
library(tidyverse)
setwd("/Users/jc3635/Library/CloudStorage/Box-Box/Breeding Insight/Species/Salmonids/Idaho Group/Trout/EBV calculations/EBV_app/")
set.seed(101919)

#### Data ####

#read in list of candidates with cols id and sex(male 0, female 1)
candidates = read.table("demo_ids.txt", header = T)
#read in EBV files
weight_ebvs = read.table("weight_2016phenos_2016ped_ebvs.txt", header = T) 
length_ebvs = read.table("length_ebvs_2020pheno_2020ped.txt", header = T) 

# Assign ids entered to males and females
males_to_select = candidates %>%
  subset(sex == "M")
males_to_select = males_to_select$id

females_to_select <- candidates %>%
  subset(sex == "F") 
females_to_select = females_to_select$id

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
weight_length_index <- calculate_index(weight_ebvs, 0.5, length_ebvs, 0.5)

# subset EBVs to those of candidates only and by sex#
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

# Display the ebv matrix
ebv_matrix

# print out 
write.table(ebv_matrix, "EBV_matrix.txt", sep = "\t", col.names = NA)


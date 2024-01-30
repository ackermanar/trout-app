set.seed(101919)
library(kinship2)
library(tidyverse)

#read in raw data and rename cols to id sire and dam
#sealice.txt has parents with conflcts
#YC24_idaho.txt has no parent conflicts but the ids to search for are from this dataset
raw_ped = read.table("sealice.txt", header = T) %>%
  rename(id =1, sire =2, dam =3) %>%
  mutate(id = as.factor(id),
         sire = as.factor(sire),
         dam = as.factor(dam))

#add sex column (if id on sire col, sex =0)
sex_ped = raw_ped %>%
  mutate(sex = ifelse(id%in%sire,1,2))


#check for ids that appear as both sire and dam
messy_parents <- as.data.frame(intersect(sex_ped$sire, sex_ped$dam)) %>%
  rename(id = 1) %>%
  filter(id != 0)

print(messy_parents)
#change values in messy_parents to 0 in cols for sire and dam
#create new datafram
parents_fixed_ped = sex_ped
# Fix values in sire column
parents_fixed_ped$sire[parents_fixed_ped$sire %in% messy_parents[,1]] <- 0
# Fix values in dam column
parents_fixed_ped$dam[parents_fixed_ped$dam %in% messy_parents[,1]] <- 0

#check for duplicate IDs
n_occur <- data.frame(table(parents_fixed_ped$id)) %>%
  rename(id =1, freq =2) #count occurrence of each ID
  
double_ids = n_occur[n_occur$freq > 1,] #print ids with more than 1 occurrence
print(double_ids)
#create new dataframe without doubled ids(all instances are removed to avoid data errors)
nodup_ped <- parents_fixed_ped[!parents_fixed_ped$id %in% double_ids$id, ]

#run FixPedigree to add parents that are not found as founders
ready_ped <-with(nodup_ped, fixParents(id, sire, dam, sex, missid="0"))

#create a pedigree object
final_ped <- with(ready_ped, pedigree(id, dadid, momid,sex, missid="0"))


#calculate kinship matrix
kinship_matrix = kinship(final_ped). #check Amatrix

# Make lists of males and females
males <- ready_ped %>%
  filter(sex == 1) %>%
  pull(id)

females <- ready_ped %>%
  filter(sex == 2) %>%
  pull(id)

# Input ids to print
ids_to_select <- c("3DD.003D58661E_2018",
                   "3DD.003D586631_2018",
                   "3DD.003D58666E_2020",
                   "3DD.003D586683_2020",
                   "3DD.003D586691_2020",
                   "3DD.003D58669A_2020",
                   "3DD.003D5866AB_2020",
                   "3DD.003D5866AD_2020",
                   "3DD.003D5866B0_2020",
                   "3DD.003D5866B4_2020",
                   "3DD.003D5866BF_2020")

# Assign ids entered to males and females
males_to_select <- ids_to_select[ids_to_select %in% males]
females_to_select <- ids_to_select[ids_to_select %in% females]

# Select rows and columns based on the list of ids
selected_matrix <- kinship_matrix[males_to_select, females_to_select]

# Print the selected ids
print(selected_matrix)


write.table(selected_matrix,"mating_matrix.txt", sep = "\t")

set.seed(101919)
library(kinship2)
library(tidyverse)

#### kinship calculations ####
#read in pedigree with cols id dam sire in any order
raw_ped = read.table("/Users/aja294/Documents/Trout_local/kin_calc-app/Data/even_year_ped.txt", header = T) %>%
  mutate(id = as.factor(id),
         sire = as.factor(sire),
         dam = as.factor(dam))

raw_ped <- read_table("/Users/aja294/Documents/Trout_local/kin_calc-app/Data/even_year_ped.txt") %>%
  mutate(id = as.factor(id),
         sire = as.factor(sire),
         dam = as.factor(dam))

#read in list of candidates with cols id and sex(male 0, female 1)
candidates = read.table("/Users/aja294/Documents/Trout_local/kin_calc-app/Data/demo_ids.txt", header = T)

candidates <- read_table("/Users/aja294/Documents/Trout_local/kin_calc-app/Data/demo_ids.txt")

#add sex column (male 0, female 1, unknown 2)

sex_ped = raw_ped %>%
  mutate(sex = ifelse(id%in%sire,0,ifelse(id%in%dam,1,2)))

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
  rename(id = 1, freq = 2) #count occurrence of each ID
  
double_ids = n_occur[n_occur$freq > 1,] #print ids with more than 1 occurrence
print(double_ids)
#create new dataframe without doubled ids(all instances are removed to avoid data errors)
nodup_ped <- parents_fixed_ped[!parents_fixed_ped$id %in% double_ids$id, ]

#check for individuals that appear as offspring and sire or offspring and dam of themselves

circ_deps <- nodup_ped %>%
  mutate(id = as.character(id), 
         sire = as.character(sire), 
         dam = as.character(dam)) %>%
  filter(id == sire | id == dam)

print(circ_deps)

no_circ_deps <- anti_join(nodup_ped, circ_deps, by = "id")

#run FixPedigree to add parents that are not found as founders
ready_ped <- with(no_circ_deps, fixParents(id, sire, dam, sex, missid="0"))

#create a pedigree object
final_ped <- with(ready_ped, pedigree(id, dadid, momid,sex, missid="0"))

#write out clean pedigree
write.table(final_ped, file="" , row.names = F, quote = F, col.na)
#calculate kinship matrix
kinship_matrix = kinship(final_ped) #check Amatrix

# Assign ids entered to males and females
males_to_select = candidates %>%
  subset(sex == "M")

males_to_select = males_to_select$id

females_to_select <- candidates %>%
  subset(sex == "F") 

females_to_select = females_to_select$id

# Select rows (males) and columns (females) based on the list of ids. 
selected_matrix <- kinship_matrix[males_to_select, females_to_select]

# Add labels to the rows and columns
rownames(selected_matrix) <- paste("male", rownames(selected_matrix), sep = "_")
colnames(selected_matrix) <- paste("female", colnames(selected_matrix), sep = "_")

# Print the selected matrix
print(selected_matrix)

# Write the matrix to a file with a header indicating "males" and "females"
write.table(selected_matrix, "mating_matrix_v2.txt", sep = "\t", col.names = NA)


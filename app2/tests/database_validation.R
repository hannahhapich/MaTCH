library(dplyr)
library(data.table)
library(DT)
library(tidyr)
library(stringr)
library(tibble)
library(chRoma)



#Data cleaning function
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

removeslashclean <- function(x){
  gsub("/", "or", x)
}

#Read in relational tables
material_alias <- read.csv("data/PrimeMaterials.csv")
material_hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
item_alias <- read.csv("data/PrimeItems.csv")
item_hierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
use_case_alias <- read.csv("data/PrimeUnclassifiable.csv")
material_item_relation <- read.csv("data/MaterialItemRelationship.csv")
use_case_item_relation <- read.csv("data/Item_Use_Case.csv")


##Validate alias sheet
#check for duplicate item aliases
duplicate_item_alias <- item_alias[duplicated(item_alias$Alias), ]
print(duplicate_item_alias)
#check for duplicate material aliases
duplicate_material_alias <- material_alias[duplicated(material_alias$Alias), ]
print(duplicate_material_alias)
#check for duplicate use case aliases
duplicate_use_case_alias <- use_case_alias[duplicated(use_case_alias$Alias), ]
print(duplicate_use_case_alias)
#check for duplicate use case aliases
duplicate_relation_alias <- material_item_relation[duplicated(material_item_relation), ]
print(duplicate_relation_alias)
#should return zero

#check that all prime items are also aliases
prime_item_terms <- data.frame(prime = unique(item_alias$Item)) %>%
  add_column("invalid" = NA)
prime_item_terms <- mutate_all(prime_item_terms, cleantext)
alias_item_terms <- data.frame(alias = unique(item_alias$Alias))
alias_item_terms <- mutate_all(alias_item_terms, cleantext)
for(x in 1:nrow(prime_item_terms)){
  if(any(alias_item_terms == prime_item_terms$prime[[x]]) == FALSE){
    prime_item_terms$invalid[[x]] <- TRUE
  }
}
prime_item_terms <- prime_item_terms %>% drop_na(invalid)
print(prime_item_terms)
#should return NA

#check that all prime materials are also aliases
prime_material_terms <- data.frame(prime = unique(material_alias$Material)) %>%
  add_column("invalid" = NA)
prime_material_terms <- mutate_all(prime_material_terms, cleantext)
alias_material_terms <- data.frame(alias = unique(material_alias$Alias))
alias_material_terms <- mutate_all(alias_material_terms, cleantext)
for(x in 1:nrow(prime_material_terms)){
  if(any(alias_material_terms == prime_material_terms$prime[[x]]) == FALSE){
    prime_material_terms$invalid[[x]] <- TRUE
  }
}
prime_material_terms <- prime_material_terms %>% drop_na(invalid)
print(prime_material_terms)
#should return NA

#check that all prime use cases are also aliases
prime_use_terms <- data.frame(prime = unique(use_case_alias$Use)) %>%
  add_column("invalid" = NA)
prime_use_terms <- mutate_all(prime_use_terms, cleantext)
alias_use_terms <- data.frame(alias = unique(use_case_alias$Use))
alias_use_terms <- mutate_all(alias_use_terms, cleantext)
for(x in 1:nrow(prime_use_terms)){
  if(any(alias_use_terms == prime_use_terms$prime[[x]]) == FALSE){
    prime_use_terms$invalid[[x]] <- TRUE
  }
}
prime_use_terms <- prime_use_terms %>% drop_na(invalid)
print(prime_use_terms)
#should return NA


#check that all prime items are in hierarchy and visa vera
item_hierarchy_terms <- data.frame(unique = unique(unlist(item_hierarchy)))
item_hierarchy_terms <- mutate_all(item_hierarchy_terms, cleantext)
prime_item_terms <- data.frame(unique = unique(item_alias$Item))
prime_item_terms <- mutate_all(prime_item_terms, cleantext)
item_hierarchy_only <- setdiff(item_hierarchy_terms, prime_item_terms)
print(item_hierarchy_only)
item_prime_only <- setdiff(prime_item_terms, item_hierarchy_terms)
print(item_prime_only)

#check that all prime materials are in hierarchy and visa vera
material_hierarchy_terms <- data.frame(unique = unique(unlist(material_hierarchy)))
material_hierarchy_terms <- mutate_all(material_hierarchy_terms, cleantext)
prime_material_terms <- data.frame(unique = unique(material_alias$Material))
prime_material_terms <- mutate_all(prime_material_terms, cleantext)
material_hierarchy_only <- setdiff(material_hierarchy_terms, prime_material_terms)
print(material_hierarchy_only)
material_prime_only <- setdiff(prime_material_terms, material_hierarchy_terms)
print(material_prime_only)

#check that all prime use cases and prime items are in item-use relational table and visa vera
prime_use_terms <- data.frame(unique = unique(use_case_alias$Prime))
prime_use_terms <- mutate_all(prime_use_terms, cleantext)
use_relational <- data.frame(unique = unique(use_case_item_relation$Prime))
use_relational <- mutate_all(use_relational, cleantext)
item_relational <- data.frame(unique = unique(use_case_item_relation$Item))
item_relational <- mutate_all(item_relational, cleantext)
item_prime_only <- setdiff(prime_item_terms, item_relational)
print(item_prime_only)
item_relational_only <- setdiff(item_relational, prime_item_terms)
print(item_relational_only)
use_prime_only <- setdiff(prime_use_terms, use_relational)
print(use_prime_only)
use_relational_only <- setdiff(use_relational, prime_use_terms)
print(use_relational_only)

#check that all materials and items in relation sheet are in alias sheets and visa versa for items
material_relation <- data.frame(unique = unique(material_item_relation$Material))
material_relation <- mutate_all(material_relation, cleantext)
item_relation <- data.frame(unique = unique(material_item_relation$Item))
item_relation <- mutate_all(item_relation, cleantext)
item_relation_only <- setdiff(item_relation, prime_item_terms)
print(item_relation_only)
material_relation_only <- setdiff(material_relation, prime_material_terms)
print(material_relation_only)
item_relation_extra <- setdiff(prime_item_terms, item_relation)
print(item_relation_extra)



##Test openAI matching against actual
#Read in API key
Sys.setenv(OPENAI_API_KEY = readLines("data/openai.txt"))

#Retrieve embeddings for prime items
item_alias_clean <- similarity_update %>% mutate(Alias = removeslashclean(Alias)) %>%
  distinct(Alias, .keep_all = TRUE)
words_to_retrieve <- item_alias_clean %>%
  distinct(Item) %>%
  rename("text" = "Item") %>%
  as.data.table(.)
items_vDB <- add_collection(metadata = words_to_retrieve)

#Retrieve embeddings for alias items and match to prime items
words_to_match <- item_alias_clean %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  left_join(words_to_retrieve, by = "text", keep = TRUE) %>%
  as.data.table(.)
words_to_match <- words_to_match %>% filter(is.na(text.y)) %>%
  rename(text = text.x) %>%
  select(-text.y)
new_item_vDB <- add_collection(metadata = words_to_match)

#Calculate similarity, match alias and item names, clean
similarity <- query_collection(db = items_vDB, query_embeddings = new_item_vDB, top_n = 5, type = "dotproduct") %>%
  left_join(items_vDB$metadata, by = c("db_id" = "id")) %>%
  rename(item_predicted = text)
similarity <- similarity %>% 
  left_join(new_item_vDB$metadata, by = c("query_id" = "id")) %>%
  rename(alias = text) %>%
  select(-c(file.x, file.y))
similarity <- similarity %>% 
  left_join(item_alias_clean, by = c("alias" = "Alias"), relationship = "many-to-many") %>%
  rename(item_actual = Item)

#Calculate % match true by rank
similarity_rank <- similarity %>%
  arrange(by_group = "alias", desc("similarity"))
number_alias <- nrow(similarity_rank)/5
similarity_rank <- similarity_rank %>%
  add_column(rank = rep(seq(5), nrow(similarity_rank)/5))
#similarity_rank$rank <- rep(seq(5), nrow(similarity_rank)/5)
rank_total_count <- data.frame(matrix(ncol=1, nrow=5, dimnames = list(c("1","2","3","4","5"), "rank_total")))
rank_total_count$rank_total <- c(0,0,0,0,0)

for(x in 1:nrow(similarity_rank)){
  if(similarity_rank$item_predicted[[x]] == similarity_rank$item_actual[[x]]){
    rank_total_count[similarity_rank$rank[[x]], 1] <- rank_total_count[similarity_rank$rank[[x]], 1] + 1
  }
}

rank_total_count <- rank_total_count %>% add_column(percentage = (as.numeric(rank_total_count$rank_total)/nrow(words_to_match))*100)

#Save item vector DB
words_to_match <- item_alias_clean %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  as.data.table(.)
item_DB_full <- add_collection(metadata = words_to_match)
#save to rda file
saveRDS(item_DB_full, file = "data/items_vectorDB.rda")



#Retrieve embeddings for prime materials
material_alias <- read.csv("data/PrimeMaterials_no_abrv.csv")
words_to_retrieve <- material_alias %>%
  distinct(Material) %>%
  rename("text" = "Material") %>%
  as.data.table(.)
material_vDB <- add_collection(metadata = words_to_retrieve)

#Retrieve embeddings for alias materials and match to prime materials
words_to_match <- material_alias %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  left_join(words_to_retrieve, by = "text", keep = TRUE) %>%
  as.data.table(.)
words_to_match <- words_to_match %>% filter(is.na(text.y)) %>%
  rename(text = text.x) %>%
  select(-text.y)

new_material_vDB <- add_collection(metadata = words_to_match)

#Calculate similarity, match alias and item names, clean
similarity <- query_collection(db = material_vDB, query_embeddings = new_material_vDB, top_n = 5, type = "dotproduct") %>%
  left_join(material_vDB$metadata, by = c("db_id" = "id")) %>%
  rename(material_predicted = text)
similarity <- similarity %>% 
  left_join(new_material_vDB$metadata, by = c("query_id" = "id")) %>%
  rename(alias = text) %>%
  select(-c(file.x, file.y))
similarity <- similarity %>% 
  left_join(material_alias, by = c("alias" = "Alias"), relationship = "many-to-many") %>%
  rename(material_actual = Material)

#Calculate % match true by rank
similarity_rank <- similarity %>%
  arrange(by_group = "alias", desc("similarity"))
number_alias <- nrow(similarity_rank)/5
similarity_rank <- similarity_rank %>%
  add_column("rank")
similarity_rank$rank <- rep(seq(5), nrow(similarity_rank)/5)
rank_total_count <- data.frame(matrix(ncol=1, nrow=5, dimnames = list(c("1","2","3","4","5"), "rank_total")))
rank_total_count$rank_total <- c(0,0,0,0,0)


for(x in 1:nrow(similarity_rank)){
  if(similarity_rank$material_predicted[[x]] == similarity_rank$material_actual[[x]]){
    rank_total_count[similarity_rank$rank[[x]], 1] <- rank_total_count[similarity_rank$rank[[x]], 1] + 1
  }
}

rank_total_count <- rank_total_count %>% add_column(percentage = (as.numeric(rank_total_count$rank_total)/nrow(words_to_match))*100)

#Save material vector DB
words_to_match <- material_alias %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  as.data.table(.)
material_DB_full <- add_collection(metadata = words_to_match)
#save to rda file
saveRDS(material_DB_full, file = "data/materials_vectorDB.rda")





# Load the openai package
#library(openai)

# Load the pre-trained model
#model <- load_model(model_name = "gpt-3.5")

# Prepare and preprocess the additional data
#additional_data <- read_additional_data()  # Replace with your data loading/preprocessing code

# Fine-tune the model
#fine_tuned_model <- fine_tune_model(model = model, data = additional_data)

# Evaluate the fine-tuned model
#evaluation_results <- evaluate_model(fine_tuned_model, test_data)

# Make predictions with the fine-tuned model
#predictions <- predict(fine_tuned_model, new_data)

# Further iterations, adjustments, or improvements as necessary

ourclean <- read.csv("tests/our_clean_community_particle.csv")
ourclean_clean <- mutate_all(ourclean, cleantext)
ourclean_clean <- ourclean_clean %>% left_join(Items_Alias, by = c("items" = "Alias"))
dataframeclean <- ourclean_clean %>% filter(is.na(Item)) %>% distinct()



















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

#Read in relational tables
material_alias <- read.csv("data/validation/PrimeMaterials_Clean.csv")
material_hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
item_alias <- read.csv("data/validation/PrimeItems_Clean.csv")
item_hierarchy <- read.csv("data/validation/ITEMSHierarchyLower.csv")
use_case_alias <- read.csv("data/validation/PrimeUnclassifiable.csv")
material_item_relation <- read.csv("data/validation/MaterialItemRelationship.csv")
use_case_item_relation <- read.csv("data/validation/Item_Use_Case.csv")


##Validate alias sheet
#check for duplicate item aliases
duplicate_item_alias <- item_alias[duplicated(item_alias$Alias), ]
#check for duplicate material aliases
duplicate_material_alias <- material_alias[duplicated(material_alias$Alias), ]
#check for duplicate use case aliases
duplicate_use_case_alias <- use_case_alias[duplicated(use_case_alias$Alias), ]
#check for duplicate use case aliases
duplicate_relation_alias <- material_item_relation[duplicated(material_item_relation), ]
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
prime_use_terms <- data.frame(prime = unique(use_case_alias$Prime)) %>%
  add_column("invalid" = NA)
prime_use_terms <- mutate_all(prime_use_terms, cleantext)
alias_use_terms <- data.frame(alias = unique(use_case_alias$Alias))
alias_use_terms <- mutate_all(alias_use_terms, cleantext)
for(x in 1:nrow(prime_use_terms)){
  if(any(alias_use_terms == prime_use_terms$prime[[x]]) == FALSE){
    prime_use_terms$invalid[[x]] <- TRUE
  }
}
prime_use_terms <- prime_use_terms %>% drop_na(invalid)
print(prime_use_terms)
#should return NA

#check for recursive material hierarchy terms
clean_material_hierarchy <- mutate_all(material_hierarchy, cleantext)
for(y in 1:ncol(clean_material_hierarchy)){
  
}


#check that all prime items are in hierarchy and visa vera
item_hierarchy_terms <- data.frame(unique = unique(unlist(item_hierarchy)))
item_hierarchy_terms <- mutate_all(item_hierarchy_terms, cleantext)
prime_item_terms <- data.frame(unique = unique(item_alias$Item))
prime_item_terms <- mutate_all(prime_item_terms, cleantext)
item_hierarchy_only <- setdiff(item_hierarchy_terms, prime_item_terms)
item_prime_only <- setdiff(prime_item_terms, item_hierarchy_terms)

#check that all prime materials are in hierarchy and visa vera
material_hierarchy_terms <- data.frame(unique = unique(unlist(material_hierarchy)))
material_hierarchy_terms <- mutate_all(material_hierarchy_terms, cleantext)
prime_material_terms <- data.frame(unique = unique(material_alias$Material))
prime_material_terms <- mutate_all(prime_material_terms, cleantext)
material_hierarchy_only <- setdiff(material_hierarchy_terms, prime_material_terms)
material_prime_only <- setdiff(prime_material_terms, material_hierarchy_terms)

#check that all prime use cases and prime items are in item-use relational table and visa vera
prime_use_terms <- data.frame(unique = unique(use_case_alias$Prime))
prime_use_terms <- mutate_all(prime_use_terms, cleantext)
use_relational <- data.frame(unique = unique(use_case_item_relation$Prime))
use_relational <- mutate_all(use_relational, cleantext)
item_relational <- data.frame(unique = unique(use_case_item_relation$Item))
item_relational <- mutate_all(item_relational, cleantext)
item_prime_only <- setdiff(prime_item_terms, item_relational)
item_relational_only <- setdiff(item_relational, prime_item_terms)
use_prime_only <- setdiff(prime_use_terms, use_relational)
use_relational_only <- setdiff(use_relational, prime_use_terms)

#check that all materials and items in relation sheet are in alias sheets and visa versa for items
material_relation <- data.frame(unique = unique(material_item_relation$Material))
material_relation <- mutate_all(material_relation, cleantext)
item_relation <- data.frame(unique = unique(material_item_relation$Item))
item_relation <- mutate_all(item_relation, cleantext)
item_relation_only <- setdiff(item_relation, prime_item_terms)
material_relation_only <- setdiff(material_relation, prime_material_terms)
item_relation_extra <- setdiff(prime_item_terms, item_relation)




##Test openAI matching against actual
#Read in API key
Sys.setenv(OPENAI_API_KEY = readLines("data/validation/openai.txt"))

#Retrieve embeddings for prime items
words_to_retrieve <- item_alias %>%
  distinct(Item) %>%
  rename("text" = "Item") %>%
  as.data.table(.)
items_vDB <- add_collection(metadata = words_to_retrieve)

#Retrieve embeddings for alias items and match to prime items
words_to_match <- item_alias %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  as.data.table(.)
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
  left_join(item_alias, by = c("alias" = "Alias"), relationship = "many-to-many") %>%
  rename(item_actual = Item)

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
  if(similarity_rank$item_predicted[[x]] == similarity_rank$item_actual[[x]]){
    rank_total_count[similarity_rank$rank[[x]], 1] <- rank_total_count[similarity_rank$rank[[x]], 1] + 1
  }
}

rank_total_count <- rank_total_count %>% add_column(percentage = (as.numeric(rank_total_count$rank_total)/nrow(item_alias))*100)


#Retrieve embeddings for prime materials
words_to_retrieve <- material_alias %>%
  distinct(Material) %>%
  rename("text" = "Material") %>%
  as.data.table(.)
material_vDB <- add_collection(metadata = words_to_retrieve)

#Retrieve embeddings for alias materials and match to prime materials
words_to_match <- material_alias %>%
  distinct(Alias) %>%
  rename("text" = "Alias") %>%
  as.data.table(.)
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

rank_total_count <- rank_total_count %>% add_column(percentage = (as.numeric(rank_total_count$rank_total)/nrow(material_alias))*100)




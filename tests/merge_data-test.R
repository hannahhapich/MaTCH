#Isolated tests for the merge_data function. 
#Build cleaning functions
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

Sys.setenv(OPENAI_API_KEY = readLines("data/openai.txt"))

items_vectorDB <- readRDS("data/items_vectorDB.rda")
materials_vectorDB <- readRDS("data/materials_vectorDB.rda")

alias <- read.csv("data/PrimeMaterials.csv")

#aliasclean <- read.csv("data/PrimeMaterials.csv")|>
#                  mutate_all(cleantext)

aliasi <- read.csv("data/PrimeItems.csv")

#aliascleani <- read.csv("data/PrimeItems.csv")|>
#                  mutate_all(cleantext)

use_cases <- read.csv("data/Item_Use_Case.csv")
prime_unclassifiable <- read.csv("data/PrimeUnclassifiable.csv")

merge_data <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable){
  dataframe <- lapply(file_paths, 
                      function(x){fread(x) %>%
                          mutate(proportion = count/sum(count))
                }) %>%
    rbindlist(., fill = T) %>%
    select(material, items, count, proportion) %>%
    mutate(material = as.character(material),
           items = as.character(items), 
           count = as.numeric(count),
           proportion = as.numeric(proportion)) %>%
    group_by(material, items) %>%
    summarise(proportion = sum(proportion),
              count = sum(count)) %>%
    ungroup() %>%
    mutate(proportion = proportion/sum(proportion))
  
  dataframeclean <- dataframe %>%
                  mutate(material = cleantext(material), 
                             items = cleantext(items)) 
  
  material_key <- inner_join(dataframeclean %>% select(material), 
                             alias, by = c("material" = "Alias")) %>%
    distinct()
  
  materials_left <- anti_join(dataframeclean %>% select(material), 
                              alias, by = c("material" = "Alias")) %>%
    distinct() %>%
    rename(text = material) %>%
    as.data.table()
  
  if(nrow(materials_left) > 0){
    new_material_vDB <- add_collection(metadata = materials_left)
    
    material_key <- query_collection(db = materials_vectorDB, query_embeddings = new_material_vDB, top_n = 1, type = "dotproduct") %>%
      left_join(materials_vectorDB$metadata, by = c("db_id" = "id")) %>%
      rename(Alias = text) %>%
      left_join(new_material_vDB$metadata, by = c("query_id" = "id")) %>%
      rename(material = text) %>%
      inner_join(alias, by = c("Alias")) %>%
      select(material, Material)   %>%
      bind_rows(material_key)
  }
  
  
  #Run for items
  items_key <- inner_join(dataframeclean %>% select(items), 
                          aliasi, by = c("items" = "Alias")) %>%
    distinct()
  
  items_left <- anti_join(dataframeclean %>% select(items), 
                          aliasi, by = c("items" = "Alias")) %>%
    distinct() %>%
    rename(text = items) %>%
    as.data.table()
  
  if(nrow(items_left) > 0){
    new_items_vDB <- add_collection(metadata = items_left)
    
    items_key <- query_collection(db = items_vectorDB, query_embeddings = new_items_vDB, top_n = 1, type = "dotproduct") %>%
      left_join(items_vectorDB$metadata, by = c("db_id" = "id")) %>%
      rename(Alias = text) %>%
      left_join(new_items_vDB$metadata, by = c("query_id" = "id")) %>%
      rename(items = text) %>%
      inner_join(aliasi, by = "Alias") %>%
      select(items, Item)   %>%
      bind_rows(items_key)
  }
  
  #Replace old material with merged material
  #Combine any new identical terms
  dataframeclean2 <- dataframeclean %>%
    mutate(count = as.numeric(count)) %>%
    group_by(material, items) %>%
    summarise(count = sum(count), 
              proportion = sum(proportion)) %>%
    ungroup() %>% 
    rename(Item = items, 
           Material = material) %>%
    left_join(use_cases, by = c("Item"), keep = NULL)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), Item) 
  dataframeclean2[aliasi, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-Item) %>% rename(Item = readable)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), Material) 
  dataframeclean2[alias, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-Material) %>% rename(Material = readable)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), Use) 
  dataframeclean2[prime_unclassifiable, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-Use) %>% rename(Use = readable)
  
  return(dataframeclean2)
}


#Test starts

test <- merge_data(file_paths = c("data/Test_Survey_1.csv", "data/Test_Survey_3.csv"), 
                   materials_vectorDB = materials_vectorDB,
                   items_vectorDB = items_vectorDB, 
                   alias = alias, 
                   aliasi = aliasi, 
                   use_cases = use_cases,
                   prime_unclassifiable = prime_unclassifiable)




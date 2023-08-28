#Isolated tests for the merge_data function. 


#Build cleaning functions
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

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

hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
hierarchyi <- read.csv("data/ITEMSHierarchyLower.csv")

hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)

#make item and material pathstrings for merging tool
pathstrings_items <- data.frame(matrix(ncol=2, dimnames = list("", c("items", "pathString"))))
for(y in 1:ncol(hierarchycleani)){
  for(x in 1:nrow(hierarchycleani)){
    pathstrings_items[nrow(pathstrings_items) + 1, 1] <- hierarchycleani[x,y]
    if(!is.na(hierarchycleani[x,y]) && y == 1){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], sep = "/")}
    if(!is.na(hierarchycleani[x,y]) && y == 2){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], hierarchycleani[x,2], sep = "/")}
    if(!is.na(hierarchycleani[x,y]) && y == 3){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], hierarchycleani[x,2], hierarchycleani[x,3], sep = "/")}
    if(!is.na(hierarchycleani[x,y]) && y == 4){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], hierarchycleani[x,2], hierarchycleani[x,3], hierarchycleani[x,4], sep = "/")}
    if(!is.na(hierarchycleani[x,y]) && y == 5){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], hierarchycleani[x,2], hierarchycleani[x,3], hierarchycleani[x,4], hierarchycleani[x,5], sep = "/")}
    if(!is.na(hierarchycleani[x,y]) && y == 6){pathstrings_items[nrow(pathstrings_items), 2] <- paste("Trash", hierarchycleani[x,1], hierarchycleani[x,2], hierarchycleani[x,3], hierarchycleani[x,4], hierarchycleani[x,5], hierarchycleani[x,6], sep = "/")}
  }
  pathstrings_items <- pathstrings_items %>% distinct() %>% drop_na()
}

pathstrings_materials <- data.frame(matrix(ncol=2, dimnames = list("", c("materials", "pathString"))))
for(y in 1:ncol(hierarchyclean)){
  for(x in 1:nrow(hierarchyclean)){
    pathstrings_materials[nrow(pathstrings_materials) + 1, 1] <- hierarchyclean[x,y]
    if(!is.na(hierarchyclean[x,y]) && y == 1){pathstrings_materials[nrow(pathstrings_materials), 2] <- paste("Trash", hierarchyclean[x,1], sep = "/")}
    if(!is.na(hierarchyclean[x,y]) && y == 2){pathstrings_materials[nrow(pathstrings_materials), 2] <- paste("Trash", hierarchyclean[x,1], hierarchyclean[x,2], sep = "/")}
    if(!is.na(hierarchyclean[x,y]) && y == 3){pathstrings_materials[nrow(pathstrings_materials), 2] <- paste("Trash", hierarchyclean[x,1], hierarchyclean[x,2], hierarchyclean[x,3], sep = "/")}
    if(!is.na(hierarchyclean[x,y]) && y == 4){pathstrings_materials[nrow(pathstrings_materials), 2] <- paste("Trash", hierarchyclean[x,1], hierarchyclean[x,2], hierarchyclean[x,3], hierarchyclean[x,4], sep = "/")}
    if(!is.na(hierarchyclean[x,y]) && y == 5){pathstrings_materials[nrow(pathstrings_materials), 2] <- paste("Trash", hierarchyclean[x,1], hierarchyclean[x,2], hierarchyclean[x,3], hierarchyclean[x,4], hierarchyclean[x,5], sep = "/")}
  }
  pathstrings_materials <- pathstrings_materials %>% distinct() %>% drop_na()
}



merge_data <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable){
  dataframe <- lapply(file_paths, fread) %>%
    rbindlist(., fill = T) %>%
    select(material, items, count) %>%
    mutate(material = as.character(material),
           items = as.character(items), 
           count = as.numeric(count)) 
  
  dataframeclean <- mutate_all(dataframe, cleantext) 
  
  material_key <- inner_join(dataframeclean %>% select(material), 
                             alias, by = c("material" = "Alias")) %>%
    distinct()
  
  materials_left <- anti_join(dataframeclean %>% select(material), 
                              alias, by = c("material" = "Alias")) %>%
    distinct() %>%
    rename(text = material)
  
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
    rename(text = items)
  
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
    group_by(Material, Item) %>%
    summarise(count = sum(count)) %>%
    ungroup() %>% 
    left_join(use_cases, by = "Item", keep = NULL)
  
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

test <- merge_data(file_paths = c("data/Test_Survey_1.csv", "data/Test_Survey_2.csv"), 
                   materials_vectorDB = materials_vectorDB,
                   items_vectorDB = items_vectorDB, 
                   alias = alias, 
                   aliasi = aliasi, 
                   use_cases = use_cases,
                   prime_unclassifiable = prime_unclassifiable)


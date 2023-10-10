library(tidyr)
library(rgpt3)
library(httr)
library(caret)
library(qs)
library(randomForest)
library(stringr)
library(gridExtra)
library(ggplot2)
library(readr)
library(ggrepel)
library(ggforce)
library(skimr)
library(ggdark)
library(ggdist)
library(ggthemes)
library(chRoma)
library(tibble)
library(aws.s3)
library(digest)
library(shiny)
library(dplyr)
library(data.table)
library(shinyjs)
library(shinythemes)
library(DT)
library(shinyhelper)
library(shinyTree)
library(data.tree)
library(collapsibleTree)
library(plotly)
library(shinyWidgets)
library(shinyBS)

merge_data <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable){
  .confidence_interval_width <- function(proportion, sample_size, population_size){
    1.96*sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1))
  }
  sample_size = length(file_paths)
  population_size = 100000 
  
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
    mutate(proportion_width = .confidence_interval_width(proportion = proportion, sample_size = sample_size, population_size = population_size)) %>%
    mutate(min_proportion = proportion - proportion_width, 
           max_proportion = proportion + proportion_width) %>%
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



use_cases <- read.csv("data/Item_Use_Case.csv")
prime_unclassifiable <- read.csv("data/PrimeUnclassifiable.csv")

#Build cleaning functions
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

removeslash <- function(x){
  gsub("/", " OR ", x)
}

cleanmaterials <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5")]
  if(is.list(x)) lapply(x, cleanmaterials)
}

cleanitems <- function(x) {
  x <- x[!names(x) %in% c("X1", "X2", "X3", "X4", "X5", "X6")]
  if(is.list(x)) lapply(x, cleanitems)
}

#Build bootstrapping functions

BootMean <- function(data) {
  B <- 10000
  mean <- numeric(B)
  n = length(data)
  
  set.seed(34347)
  for (i in 1:B) {
    boot <- sample(1:n, size=n, replace = TRUE)
    mean[i] <- mean(data[boot], na.rm = T)
  }
  return(quantile(mean, c(0.025, 0.5, 0.975), na.rm = T))
}

confidence_interval_width <- function(data){
  proportion = 0.95
  #sample_size = sum(data$count)
  sample_size = length(data)
  population_size = 10000
  1.96*sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1))
}

AggregateTrees <- function(DF, Alias, Hierarchy){
  
  DF <- mutate_all(DF, cleantext) %>%
    mutate(Count = as.numeric(Count))
  
  colnames(DF) <- c("Alias", "Count")
  
  DF$Alias <- as.character(DF$Alias)
  
  Hierarchy <- mutate_all(Hierarchy, cleantext)
  
  colnames(Hierarchy) <- c("from", "Key")
  
  Alias <- mutate_all(Alias, cleantext) 
  
  DF <- DF %>% group_by(Alias) %>%
    summarise(across(Count, sum))
  
  #duplicated(Hierarchy$Key)
  
  #Hierarchy$from[!Hierarchy$from %in% Hierarchy$Key]
  
  DF_v2 <- DF %>%
    left_join(Alias) %>%
    dplyr::select(Key, Count) %>%
    mutate(Key = ifelse(is.na(Key), "other", Key)) %>%
    right_join(Hierarchy) %>%
    select(from, Key, Count) #%>%
    #add_row(from = "trash", Key = "missing", Count = sum(DF$Count, na.rm = T) - sum(.$Count, na.rm = T))%>%
    #mutate(Count = Count/sum(Count, na.rm = T))
  
  
  DF_network <- FromDataFrameNetwork(DF_v2, check = "check")
  
  DF_network$Do(function(x) x$totalsum <- ifelse(is.null(x$Count), 0, x$Count) + sum(Get(x$children, "totalsum")), traversal = "post-order")
  
  Treedf <- ToDataFrameNetwork(DF_network, "totalsum") 
  
  Treedf %>%
    add_row(from = "trash", to = "", totalsum = sum(Treedf %>%
                                                      filter(from == "trash") %>% 
                                                      pull(totalsum)))
  
  
}

grouped_uncertainty <- function(DF_group, Group_Alias, Group_Hierarchy, type){
  
  df_join = data.frame(from = character(), 
                       to = character(), 
                       count = numeric())
  DF_group$Count = as.numeric(DF_group$Count)
  
  groups <- DF_group
  
  for(row in 1:nrow(groups)){
    df_subset <- DF_group %>%
      #inner_join(groups[row,]) %>%
      select(Class, Count)
    
    df_join <- AggregateTrees(DF = df_subset, Alias = Group_Alias, Hierarchy = Group_Hierarchy) %>%
      mutate(from = ifelse(from == "trash", type, from)) %>%
      bind_rows(df_join)
  }
  
  df_join_boot <- df_join %>%
    group_by(from, to) %>%
    summarise(mean_prop = mean(totalsum, na.rm = T), 
              min_prop = confidence_interval_width(totalsum) - mean(totalsum, na.rm = T), 
              max_prop = confidence_interval_width(totalsum) + mean(totalsum, na.rm = T))
  
}

sunburstplot <-function(df_join_boot){
  
  values <- paste(df_join_boot$to, 
                  "<br>", 
                  round(df_join_boot$mean_prop, 2) * 100, 
                  " (", 
                  round(df_join_boot$min_prop, 2) * 100, 
                  "-", 
                  round(df_join_boot$max_prop, 2) * 100, 
                  ")%", 
                  sep = "")
  
  values[df_join_boot$mean_prop < 0.1] <- NA
  
  plot_ly() %>%
    add_trace(
      labels = df_join_boot$to,
      parents = df_join_boot$from,
      type = "sunburst",
      maxdepth = 6,
      domain = list(column = 1), 
      branchvalues = 'total',
      texttemplate = values,
      values = df_join_boot$mean_prop) 
}

###create function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

#Files for tool
alias <- read.csv("data/PrimeMaterials.csv")
hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
aliasi <- read.csv("data/PrimeItems.csv")
hierarchyi <- read.csv("data/ITEMSHierarchyLower.csv")
microcolor <- read.csv("data/Microplastics_Color.csv")
aliasclean <- mutate_all(alias, cleantext)
aliascleani <- mutate_all(aliasi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)
microcolorclean <- mutate_all(microcolor, cleantext)

#Creating materials hierarchy
Materials <- hierarchy
Materials[is.na(Materials)] <- ""
Materials <- mutate_all(Materials, removeslash)
Materials$pathString <- paste("Trash", Materials$X1, Materials$X2, Materials$X3, Materials$X4, Materials$X5, sep = "/")
Materials <- as.Node(Materials[,])
Materials <- as.list(Materials)
Materials <- Materials[-1]
Materials <- cleanmaterials(Materials)

Materials_hierarchy <- hierarchy
Materials_hierarchy[is.na(Materials_hierarchy)] <- ""
Materials_hierarchy <- mutate_all(Materials_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Materials_hierarchy <- as.Node(Materials_hierarchy, pathDelimiter = "/")
Materials_hierarchy <- as.list(Materials_hierarchy)
Materials_hierarchy <- Materials_hierarchy[-1]

#Creating items hierarchy
Items <- hierarchyi
Items[is.na(Items)] <- ""
Items <- mutate_all(Items, removeslash)
Items$pathString <- paste("Trash", Items$X1, Items$X2, Items$X3, Items$X4, Items$X5, Items$X6, sep = "/")
Items <- as.Node(Items)
Items <- as.list(Items)
Items <- Items[-1]
Items <- cleanitems(Items)

Items_hierarchy <- hierarchyi
Items_hierarchy[is.na(Items_hierarchy)] <- ""
Items_hierarchy <- mutate_all(Items_hierarchy, removeslash) %>%
  mutate(key = "trash") %>%
  relocate(key) %>%
  unite(pathString, sep = "/")
Items_hierarchy <- as.Node(Items_hierarchy, pathDelimiter = "/")
Items_hierarchy <- as.list(Items_hierarchy)
Items_hierarchy <- Items_hierarchy[-1]

#Files for display
Materials_Alias <- read.csv("data/PrimeMaterials.csv")
Materials_Hierarchy <- read.csv("data/MaterialsHierarchyLower.csv")
Items_Alias <- read.csv("data/PrimeItems.csv")
Items_Hierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
Material_Item_Relation <- read.csv("data/MaterialItemRelationship.csv")
Brand_Manufacturer_Relation <- read.csv("data/BrandManufacturer.csv")
Brand_Item_Relation <- read.csv("data/BrandItem.csv")
NOAA <- read.csv("data/NOAA.csv")
Micro_Color_Display <-read.csv("data/Microplastics_Color.csv")
MicroOnly <- read.csv("data/PremadeSurveys/Most_Specific_Microplastics.csv")
AllMore <- read.csv("data/PremadeSurveys/Most_Specific_All.csv")
AllLess <- read.csv("data/PremadeSurveys/Least_Specific_All.csv")
polymer_db <- read.csv("data/median_polymer_density.csv")

#Data for embeddings generation via chRoma
items_vectorDB <- readRDS(file = "data/items_vectorDB.rda")
materials_vectorDB <- readRDS(file = "data/materials_vectorDB.rda")
Sys.setenv(OPENAI_API_KEY = readLines("data/openai.txt"))
creds <- read.csv("data/s3_cred.csv")
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = creds$Access.key.ID,
  "AWS_SECRET_ACCESS_KEY" = creds$Secret.access.key,
  "AWS_DEFAULT_REGION" = "us-east-2"
)
primeItems <- read.csv("data/PrimeItems.csv")
primeMaterials <- read.csv("data/PrimeMaterials.csv")

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

pathstrings_materials <- data.frame(matrix(ncol=2, dimnames = list("", c("material", "pathString"))))
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

#Files for bootstrapping routine and sunburst plots
ItemsHierarchy_sunburst <- data.frame(matrix(ncol=2, dimnames = list("", c("from", "to"))))
for(y in 1:ncol(hierarchycleani)){
  for(x in 1:nrow(hierarchycleani)){
    ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst) + 1, 2] <- hierarchycleani[x,y]
    if(!is.na(hierarchycleani[x,y]) && y == 1){ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst), 1] <- paste("Trash")}
    if(!is.na(hierarchycleani[x,y]) && y != 1){ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst), 1] <- paste(hierarchycleani[x, y -1])}
  }
  ItemsHierarchy_sunburst <- ItemsHierarchy_sunburst %>% distinct() %>% drop_na()
}

MaterialsHierarchy_sunburst <- data.frame(matrix(ncol=2, dimnames = list("", c("from", "to"))))
for(y in 1:ncol(hierarchyclean)){
  for(x in 1:nrow(hierarchyclean)){
    MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst) + 1, 2] <- hierarchyclean[x,y]
    if(!is.na(hierarchyclean[x,y]) && y == 1){MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst), 1] <- paste("Trash")}
    if(!is.na(hierarchyclean[x,y]) && y != 1){MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst), 1] <- paste(hierarchyclean[x, y -1])}
  }
  MaterialsHierarchy_sunburst <- MaterialsHierarchy_sunburst %>% distinct() %>% drop_na()
}

ItemsAlias_sunburst <- read.csv("data/PrimeItems.csv")%>%
  rename(Key = Item) %>%
  select(-readable)
MaterialsAlias_sunburst <- read.csv("data/PrimeMaterials.csv") %>%
  rename(Key = Material) %>%
  select(-readable)


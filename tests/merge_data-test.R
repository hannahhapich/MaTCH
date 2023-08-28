#Isolated tests for the merge_data function. 

items_vectorDB <- readRDS("data/items_vectorDB.rda")
materials_vectorDB <- readRDS("data/materials_vectorDB.rda")
aliusclean <- read.csv("data/PrimeMaterials.csv")|>
                  mutate_all(cleantext)
aliuscleani <- read.csv("data/PrimeItems.csv")|>
                  mutate_all(cleantext)

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

test <- merge_data(file_paths = c("data/Test_Survey_1.csv", "data/Test_Survey_2.csv"), materials_vectorDB = materials_vectorDB,items_vectorDB = items_vectorDB, aliasclean = aliasclean, aliascleani = aliascleani, pathstrings_materials = pathstrings_materials, pathstrings_items = pathstrings_items)

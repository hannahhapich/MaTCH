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
#library(shinyBS)
library(bs4Dash)
library(classInt)

#setwd("/Users/hannahhapich/Documents/R_Scripts/TTT2.0")

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

#file_paths <- read.csv("app2/tests/count_mass_particle.csv")
merge_terms <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable){
  dataframe <- file_paths %>%
    mutate(material = as.character(material),
           morphology = as.character(morphology))
  
  dataframeclean <- dataframe %>%
    mutate(material = cleantext(material), 
           morphology = cleantext(morphology)) 
  
  dataframe <- dataframe %>%
    rename(material_raw = material,
           morphology_raw = morphology) %>%
    add_column(material = cleantext(dataframe$material),
               morphology = cleantext(dataframe$morphology))
  
  material_key <- inner_join(dataframeclean %>% select(material), 
                             alias, by = c("material" = "Alias")) %>%
    distinct()
  
  materials_left <- anti_join(dataframeclean %>% select(material), 
                              alias, by = c("material" = "Alias")) %>%
    distinct() %>%
    drop_na() %>%
    rename(text = material) %>%
    as.data.table()
  
  if(nrow(materials_left) > 0){
    new_material_vDB <- add_collection(metadata = materials_left)
    
    material_unknown_key <- query_collection(db = materials_vectorDB, query_embeddings = new_material_vDB, top_n = 5, type = "dotproduct") %>%
      left_join(materials_vectorDB$metadata, by = c("db_id" = "id")) %>%
      rename(Alias = text) %>%
      left_join(new_material_vDB$metadata, by = c("query_id" = "id")) %>%
      rename(material = text) 
    
    material_embedding <- material_unknown_key %>%
      add_column(rank = rep(seq(5), nrow(material_unknown_key)/5)) %>%
      select(Alias, material, rank) %>% 
      pivot_wider(names_from = rank, values_from = Alias)
    
    material_embedding <- material_embedding %>%
      rename(material_raw = 1,
             material_match_1 = 2,
             material_match_2 = 3,
             material_match_3 = 4,
             material_match_4 = 5,
             material_match_5 = 6)
    
    material_unknown_key <- material_unknown_key %>% 
      distinct(material, .keep_all = T) %>%
      select(Alias, material)
    
    material_key <- material_unknown_key %>%
      inner_join(alias, by = c("Alias")) %>%
      select(material, Material, readable) %>%
      bind_rows(material_key)
    
    
  }
  
  
  #Run for items
  items_key <- inner_join(dataframeclean %>% select(morphology), 
                          aliasi, by = c("morphology" = "Alias")) %>%
    distinct()
  
  items_left <- anti_join(dataframeclean %>% select(morphology), 
                          aliasi, by = c("morphology" = "Alias")) %>%
    distinct() %>%
    drop_na() %>%
    rename(text = morphology) %>%
    as.data.table()
  
  if(nrow(items_left) > 0){
    new_items_vDB <- add_collection(metadata = items_left)
    
    items_unknown_key <- query_collection(db = items_vectorDB, query_embeddings = new_items_vDB, top_n = 5, type = "dotproduct") %>%
      left_join(items_vectorDB$metadata, by = c("db_id" = "id"))  %>%
      rename(Alias = text) %>%
      left_join(new_items_vDB$metadata, by = c("query_id" = "id")) %>%
      rename(morphology = text) 
    
    items_embedding <- items_unknown_key %>%
      add_column(rank = rep(seq(5), nrow(items_unknown_key)/5)) %>%
      select(Alias, morphology, rank) %>% 
      pivot_wider(names_from = rank, values_from = Alias)
    
    items_embedding <- items_embedding %>%
      rename(morphology_raw = 1,
             morphology_match_1 = 2,
             morphology_match_2 = 3,
             morphology_match_3 = 4,
             morphology_match_4 = 5,
             morphology_match_5 = 6)
    
    items_unknown_key <- items_unknown_key %>% 
      distinct(morphology, .keep_all = T) %>%
      select(Alias, morphology)
    
    items_key <- items_unknown_key %>%
      inner_join(aliasi, by = "Alias") %>%
      select(morphology, Item, readable)   %>%
      bind_rows(items_key)
  }
  
  
  #Replace old material with merged material
  dataframeclean <- dataframeclean %>% 
    left_join(material_key, by = "material") %>%
    select(-Material) %>%
    rename(material_new = readable)
  
  #Replace old item with merged item
  dataframeclean <- dataframeclean %>% 
    left_join(items_key, by = c("morphology")) %>%
    select(-Item) %>%
    rename(morphology_new = readable)
  
  dataframeclean <- dataframeclean %>% 
    select("material", "morphology", "material_new", "morphology_new") 
  dataframeclean <- unique(dataframeclean[c("material", "morphology", "material_new", "morphology_new")])

  dataframe <- dataframe %>%
    left_join(dataframeclean, by = c("material", "morphology"))
  dataframe <- dataframe %>%
    select(-c(material, morphology)) %>%
    rename(material = material_new,
           morphology = morphology_new)
  
  if(nrow(materials_left) > 0){
    dataframe <- dataframe %>%
      left_join(material_embedding, by = "material_raw")
  }
  if(nrow(items_left) > 0){
    dataframe <- dataframe %>%
      left_join(items_embedding, by = "morphology_raw")
  }
  
  
  dataframe <- dataframe %>% select(morphology_raw, everything()) 
  dataframe <- dataframe %>% select(material_raw, everything()) 
  dataframe <- dataframe %>% select(morphology, everything()) 
  dataframe <- dataframe %>% select(material, everything())
  
  return(dataframe)
}

#file_paths <- dataframe
merge_data <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable){
  .confidence_interval_width <- function(proportion, sample_size, population_size){
    1.96*sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1))
  }
  sample_size = length(file_paths)
  population_size = 100000 
  
  dataframe <- file_paths %>%
    mutate(proportion = count/sum(count))%>%
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
      select(material, Material, readable)   %>%
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
      select(items, Item, readable)   %>%
      bind_rows(items_key)
  }
  
  
  #Replace old material with merged material
  dataframeclean <- dataframeclean %>% 
    left_join(material_key, by = "material") %>%
    select(-c(material, readable)) %>%
    rename(material = Material)
  
  #Replace old item with merged item
  dataframeclean <- dataframeclean %>% 
    left_join(items_key, by = "items") %>%
    select(-c(items, readable)) %>%
    rename(items = Item)
  
  dataframeclean <- dataframeclean %>%
    left_join(use_cases, by = c("items"="Item"), keep = NULL)
  
  #Combine any new identical terms
  dataframeclean2 <- dataframeclean %>%
    mutate(count = as.numeric(count)) %>%
    group_by(material, items, Use) %>%
    summarise(count = sum(count), 
              proportion = sum(proportion)) %>%
    ungroup() %>% 
    mutate(proportion_width = .confidence_interval_width(proportion = proportion, sample_size = sample_size, population_size = population_size)) %>%
    mutate(min_proportion = proportion - proportion_width, 
           max_proportion = proportion + proportion_width)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), items) 
  dataframeclean2[aliasi, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-items) %>% rename(items = readable)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), material) 
  dataframeclean2[alias, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-material) %>% rename(material = readable)
  
  dataframeclean2 <- setkey(setDT(dataframeclean2), Use) 
  dataframeclean2[prime_unclassifiable, readable := i.readable]
  dataframeclean2 <- dataframeclean2 %>% select(-Use) %>% rename(use = readable) %>% select(-proportion_width)
  
  dataframeclean2$min_proportion <- as.numeric(dataframeclean2$min_proportion)
  dataframeclean2$max_proportion <- as.numeric(dataframeclean2$max_proportion)
  
  for(x in 1:nrow(dataframeclean2)){
    if(dataframeclean2$min_proportion[[x]] < 0){
      dataframeclean2$min_proportion[[x]] <- 0
    }
    if(dataframeclean2$max_proportion[[x]] > 1){
      dataframeclean2$max_proportion[[x]] <- 1
    }
  }
  
  dataframeclean2 <- dataframeclean2[,c("material","items","use","count","proportion","min_proportion","max_proportion")]
  
  return(dataframeclean2)
}

#dataframe <- read.csv("tests/count_mass_particle.csv")
particle_count_mass <- function(dataframe, morphology_shape, polymer_density, trash_mass_clean, polymer_avg_decision, morph_weight, sample_weight){
  dataframe$length_um <- as.numeric(dataframe$length_um)
  dataframe$morphology <- as.character(dataframe$morphology)
  dataframe$material <- as.character(dataframe$material)
  if("width_um" %in% colnames(dataframe) == TRUE){dataframe$width_um <- as.numeric(dataframe$width_um)}
  if("height_um" %in% colnames(dataframe) == TRUE){dataframe$height_um <- as.numeric(dataframe$height_um)}
  if("density" %in% colnames(dataframe) == TRUE){dataframe$density <- as.numeric(dataframe$density)}
  if("sample_ID" %in% colnames(dataframe) == TRUE){dataframe$sample_ID <- as.character(dataframe$sample_ID)}
  dataframeclean <- mutate_all(dataframe, cleantext) 
  
  dataframeclean <- left_join(dataframeclean, morphology_shape, by = "morphology", copy = F)
  dataframeclean <- left_join(dataframeclean, polymer_density, by = "material", copy = F) %>%
    select(-material) %>%
    rename(material = readable)
  
  if("width_um" %in% colnames(dataframeclean)){
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x, "width_um"])) {
        dataframeclean[x, "W_min"] <- as.numeric(dataframeclean[x, "W_min"]) * as.numeric(dataframeclean[x, "length_um"])
        dataframeclean[x, "W_mean"] <- (as.numeric(dataframeclean[x, "W_min"]) + as.numeric(dataframeclean[x, "W_max"]))/2
        dataframeclean[x, "W_max"] <- as.numeric(dataframeclean[x, "W_max"]) * as.numeric(dataframeclean[x, "length_um"])
      }else{
        dataframeclean[x, "W_min"] <- as.numeric(dataframeclean[x, "width_um"]) * meas_min
        dataframeclean[x, "W_mean"] <- as.numeric(dataframeclean[x, "width_um"])
        dataframeclean[x, "W_max"] <- as.numeric(dataframeclean[x, "width_um"]) * meas_max
      }
    }
  }
  if("height_um" %in% colnames(dataframeclean)){
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x, "height_um"])) {
        dataframeclean[x, "H_min"] <- as.numeric(dataframeclean[x, "H_min"]) * as.numeric(dataframeclean[x, "length_um"])
        dataframeclean[x, "H_mean"] <- (as.numeric(dataframeclean[x, "H_min"]) + as.numeric(dataframeclean[x,"H_max"]))/2
        dataframeclean[x, "H_max"] <- as.numeric(dataframeclean[x, "H_max"]) * as.numeric(dataframeclean[x, "length_um"])
      }else{
        dataframeclean[x, "H_min"] <- as.numeric(dataframeclean[x, "height_um"]) * meas_min
        dataframeclean[x, "H_mean"] <- as.numeric(dataframeclean[x, "height_um"])
        dataframeclean[x, "H_max"] <- as.numeric(dataframeclean[x, "height_um"]) * meas_max
      }
    }
  }
  
  dataframeclean <- dataframeclean %>%
    mutate(L_min = as.numeric(L_min) * as.numeric(length_um),
           L_mean = as.numeric(length_um),
           L_max = as.numeric(L_max) * as.numeric(length_um))
  
  dataframeclean <- data.frame(dataframeclean) %>%
    mutate(volume_min_um_3 = L_min * W_min* H_min,
           volume_mean_um_3 = L_mean * W_mean* H_mean,
           volume_max_um_3 = L_max * W_max * H_max) 
  
  if("density" %in% colnames(dataframeclean)){
    for(x in 1:nrow(dataframeclean)){
      #x <- 1
      if(is.na(dataframeclean[x, "density"]) && is.na(dataframeclean[x, "density_max"])) {
        dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x, "volume_min_um_3"])
        dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x,"volume_mean_um_3"])
        dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x, "volume_max_um_3"])
      }else if(is.na(dataframeclean[x, "density"]) && !is.na(dataframeclean[x, "density_max"])){
        dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_min"]) * as.numeric(dataframeclean[x, "volume_min_um_3"])
        dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x,"volume_mean_um_3"])
        dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_max"]) * as.numeric(dataframeclean[x, "volume_max_um_3"])
      }else{
        dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x, "volume_min_um_3"])
        dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x,"volume_mean_um_3"])
        dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x, "volume_max_um_3"])
      }
    }
  }else{
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x, "density_max"]) && is.na(dataframeclean[x, "density_min"])){
        dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x, "volume_min_um_3"])
        dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x,"volume_mean_um_3"])
        dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x, "volume_max_um_3"])
      }else{
        dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_min"]) * as.numeric(dataframeclean[x, "volume_min_um_3"])
        dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um_3"]) * as.numeric(dataframeclean[x,"volume_mean_um_3"])
        dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_max"]) * as.numeric(dataframeclean[x, "volume_max_um_3"])
      }
    }
  }
  
  dataframeclean_particles <- data.frame(dataframeclean)
  
  dataframeclean_particles <- dataframeclean_particles %>%
    left_join(trash_mass_clean, by = c("morphology" = "items",
                                       "material" = "material"))
  dataframeclean_particles$weight_estimate_g <- as.numeric(dataframeclean_particles$weight_estimate_g)
  for(x in 1:nrow(dataframeclean_particles)){
    if(! is.na(dataframeclean_particles[x, "weight_estimate_g"])){
      dataframeclean_particles[x, "mean_mass_mg"] <- (dataframeclean_particles[x, "weight_estimate_g"]*1000)
    }
  }
  # polymer_avg_decision = T
  # morph_weight = F
  # sample_weight = F
  if(polymer_avg_decision == T){
    dataframeclean_particles$density_mg_um_3 <- as.numeric(dataframeclean_particles$density_mg_um_3)
    dataframeclean_particles$volume_mean_um_3 <- as.numeric(dataframeclean_particles$volume_mean_um_3)
    dataframeclean_particles$mean_mass_mg <- as.numeric(dataframeclean_particles$mean_mass_mg)
    dataframeclean_particles$density_max <- as.numeric(dataframeclean_particles$density_max)
    dataframeclean_particles$density_min <- as.numeric(dataframeclean_particles$density_min)
    dataframeclean_particles_avg <- dataframeclean_particles
    for(x in 1:nrow(dataframeclean_particles)){
      if(is.na(dataframeclean_particles$mean_mass_mg[[x]]) && ! is.na(dataframeclean_particles$volume_mean_um_3[[x]])){
        # if(all(weighted_choice == c("morph_weight", "sample_weight")) && "sample_ID" %in% colnames(dataframeclean_particles) == TRUE){
        if(morph_weight == T && sample_weight == T && "sample_ID" %in% colnames(dataframeclean_particles) == TRUE){
          dataframeclean_particles_avg <- dataframeclean_particles_avg %>% filter(sample_ID == dataframeclean_particles$sample_ID[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um_3, na.rm = T)
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_mean_um_3[[x]])*density
          upper_diff <- (dataframeclean_particles_avg$density_max) - (dataframeclean_particles_avg$density_mg_um_3)
          lower_diff <- (dataframeclean_particles_avg$density_mg_um_3) - (dataframeclean_particles_avg$density_min)
        }else if(morph_weight == T || morph_weight == T && sample_weight == T && !("sample_ID" %in% colnames(dataframeclean_particles))){
          dataframeclean_particles_avg <- dataframeclean_particles %>% filter(morphology == dataframeclean_particles$morphology[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um_3, na.rm = T)
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_mean_um_3[[x]])*density
          upper_diff <- (dataframeclean_particles_avg$density_max) - (dataframeclean_particles_avg$density_mg_um_3)
          lower_diff <- (dataframeclean_particles_avg$density_mg_um_3) - (dataframeclean_particles_avg$density_min)
        }else if(morph_weight == T && "sample_ID" %in% colnames(dataframeclean_particles) == TRUE){
          dataframeclean_particles_avg <- dataframeclean_particles %>% filter(morphology == dataframeclean_particles$morphology[[x]])
          dataframeclean_particles_avg <- dataframeclean_particles_avg %>% filter(sample_ID == dataframeclean_particles$sample_ID[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um_3, na.rm = T)
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_mean_um_3[[x]])*density
          upper_diff <- (dataframeclean_particles_avg$density_max) - (dataframeclean_particles_avg$density_mg_um_3)
          lower_diff <- (dataframeclean_particles_avg$density_mg_um_3) - (dataframeclean_particles_avg$density_min)
        }else{
          density <- mean(dataframeclean_particles$density_mg_um_3, na.rm = T)
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_mean_um_3[[x]])*density
          upper_diff <- (dataframeclean_particles$density_max) - (dataframeclean_particles$density_mg_um_3)
          lower_diff <- (dataframeclean_particles$density_mg_um_3) - (dataframeclean_particles$density_min)
        }
        upper_conf <- mean(upper_diff, na.rm = T) 
        lower_conf <- mean(lower_diff, na.rm = T)
        dataframeclean_particles$max_mass_mg[[x]] <- dataframeclean_particles$mean_mass_mg[[x]] + upper_conf
        dataframeclean_particles$min_mass_mg[[x]] <- dataframeclean_particles$mean_mass_mg[[x]] + lower_conf
      }
    }
  }
  
  dataframeclean_particles <- dataframeclean_particles %>% 
    select(-c(weight_estimate_g, L_min, L_max, W_min, W_max, H_min, H_max, density_mg_um_3, density_min, density_max, W_mean, H_mean, L_mean))
  
   dataframeclean_particles <- dataframeclean_particles %>%
     mutate_if(is.numeric, signif, digits=3)
  
  return(dataframeclean_particles)
}

#dataframe <- read.csv("tests/count_mass_concentration.csv")

concentration_count_mass <- function(dataframe, morphology_shape, polymer_density){
  dataframe$concentration_particle_vol <- as.numeric(dataframe$concentration_particle_vol)
  dataframe$avg_length_um <- as.numeric(dataframe$avg_length_um)
  dataframe$morphology <- as.character(dataframe$morphology)
  dataframe$material <- as.character(dataframe$material)
  dataframe$material_percent <- as.numeric(dataframe$material_percent)
  dataframe$morphology_percent <- as.numeric(dataframe$morphology_percent)
  dataframe$sample_ID <- as.character(dataframe$sample_ID)
  if("avg_width_um" %in% colnames(dataframe) == TRUE){dataframe$avg_width_um <- as.numeric(dataframe$avg_width_um)}
  if("avg_height_um" %in% colnames(dataframe) == TRUE){dataframe$avg_height_um <- as.numeric(dataframe$avg_height_um)}
  if("avg_density" %in% colnames(dataframe) == TRUE){dataframe$avg_density <- as.numeric(dataframe$avg_density)}
  if("error_SD" %in% colnames(dataframe) == TRUE){dataframe$error_SD <- as.numeric(dataframe$error_SD)}
  if("error_upper" %in% colnames(dataframe) == TRUE){dataframe$error_upper <- as.numeric(dataframe$error_upper)}
  if("error_lower" %in% colnames(dataframe) == TRUE){dataframe$error_lower <- as.numeric(dataframe$error_lower)}
  dataframeclean <- mutate_all(dataframe, cleantext) 
  
  dataframeclean <- left_join(dataframeclean, morphology_shape, by = "morphology", copy = F)
  dataframeclean <- left_join(dataframeclean, polymer_density, by = "material", copy = F)
  
  dataframeclean_trash <- dataframeclean %>%
    left_join(trash_mass_clean, by = c("morphology" = "items",
                                 "material" = "material"))
  
  if("avg_width_um" %in% colnames(dataframeclean)){
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x, "avg_width_um"])) {
        dataframeclean[x, "W_mean"] <- ((as.numeric(dataframeclean[x, "W_min"]) + as.numeric(dataframeclean[x, "W_max"]))/2) * as.numeric(dataframeclean[x, "avg_length_um"])
        dataframeclean[x, "W_min"] <- as.numeric(dataframeclean[x, "W_min"]) * as.numeric(dataframeclean[x, "avg_length_um"])
        dataframeclean[x, "W_max"] <- as.numeric(dataframeclean[x, "W_max"]) * as.numeric(dataframeclean[x, "avg_length_um"])
      }else{
        dataframeclean[x, "W_min"] <- as.numeric(dataframeclean[x, "avg_width_um"]) * meas_min
        dataframeclean[x, "W_mean"] <- as.numeric(dataframeclean[x, "avg_width_um"])
        dataframeclean[x, "W_max"] <- as.numeric(dataframeclean[x, "avg_width_um"]) * meas_max
      }
    }
  }else{
    for(x in 1:nrow(dataframeclean)){
      dataframeclean[x, "W_mean"] <- ((as.numeric(dataframeclean[x, "W_min"]) + as.numeric(dataframeclean[x, "W_max"]))/2) * as.numeric(dataframeclean[x, "avg_length_um"])
      dataframeclean[x, "W_min"] <- as.numeric(dataframeclean[x, "W_min"]) * as.numeric(dataframeclean[x, "avg_length_um"])
      dataframeclean[x, "W_max"] <- as.numeric(dataframeclean[x, "W_max"]) * as.numeric(dataframeclean[x, "avg_length_um"])
    }
  }
  if("avg_height_um" %in% colnames(dataframeclean)){
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x, "avg_height_um"])) {
        dataframeclean[x, "H_mean"] <- ((as.numeric(dataframeclean[x, "H_min"]) + as.numeric(dataframeclean[x,"H_max"]))/2) * as.numeric(dataframeclean[x, "avg_length_um"])
        dataframeclean[x, "H_min"] <- as.numeric(dataframeclean[x, "H_min"]) * as.numeric(dataframeclean[x, "avg_length_um"])
        dataframeclean[x, "H_max"] <- as.numeric(dataframeclean[x, "H_max"]) * as.numeric(dataframeclean[x, "avg_length_um"])
      }else{
        dataframeclean[x, "H_min"] <- as.numeric(dataframeclean[x, "avg_height_um"]) * meas_min
        dataframeclean[x, "H_mean"] <- as.numeric(dataframeclean[x, "avg_height_um"])
        dataframeclean[x, "H_max"] <- as.numeric(dataframeclean[x, "avg_height_um"]) * meas_max
      }
    }
  }else{
    for(x in 1:nrow(dataframeclean)){
      dataframeclean[x, "H_mean"] <- ((as.numeric(dataframeclean[x, "H_min"]) + as.numeric(dataframeclean[x,"H_max"]))/2) * as.numeric(dataframeclean[x, "avg_length_um"])
      dataframeclean[x, "H_min"] <- as.numeric(dataframeclean[x, "H_min"]) * as.numeric(dataframeclean[x, "avg_length_um"])
      dataframeclean[x, "H_max"] <- as.numeric(dataframeclean[x, "H_max"]) * as.numeric(dataframeclean[x, "avg_length_um"])
    }
  }
  
  dataframeclean <- dataframeclean %>%
    mutate(L_min = as.numeric(L_min) * as.numeric(avg_length_um),
           L_mean = as.numeric(avg_length_um),
           L_max = as.numeric(L_max) * as.numeric(avg_length_um))
  
  dataframeclean <- data.frame(dataframeclean) %>%
    mutate(volume_min_um_3 = L_min * W_min* H_min,
           volume_mean_um_3 = L_mean * W_mean* H_mean,
           volume_max_um_3 = L_max * W_max * H_max) 
  
  dataframeclean <- dataframeclean %>%
    add_column(density_upper = as.numeric(dataframeclean$density_max) - as.numeric(dataframeclean$density_mg_um_3),
               density_lower = as.numeric(dataframeclean$density_mg_um_3) - as.numeric(dataframeclean$density_min))
  
  dataframeclean <- dataframeclean %>%
    select(-c(L_min, L_mean, L_max, W_min, W_mean, W_max, H_min, H_mean, H_max, density_max, density_min, avg_height_um, avg_width_um, avg_length_um, material, morphology))
  
  dataframeclean <- dataframeclean %>%
    mutate(density_mg_um_3 = as.numeric(dataframeclean$density_mg_um_3) * as.numeric(dataframeclean$material_percent) * 0.01,
           density_upper = as.numeric(dataframeclean$density_upper) * as.numeric(dataframeclean$material_percent) * 0.01,
           density_lower = as.numeric(dataframeclean$density_lower) * as.numeric(dataframeclean$material_percent) * 0.01,
               volume_min_um_3 = as.numeric(dataframeclean$volume_min_um_3) * as.numeric(dataframeclean$morphology_percent) * 0.01,
               volume_mean_um_3 = as.numeric(dataframeclean$volume_mean_um_3) * as.numeric(dataframeclean$morphology_percent) * 0.01,
               volume_max_um_3 = as.numeric(dataframeclean$volume_max_um_3) * as.numeric(dataframeclean$morphology_percent) * 0.01) %>%
    select(-c(material_percent, morphology_percent))
  
  dataframeclean <- replace(dataframeclean, is.na(dataframeclean), 0)
  dataframeclean$concentration_particle_vol <- as.numeric(dataframeclean$concentration_particle_vol)
  dataframeclean$sample_ID <- as.character(dataframeclean$sample_ID)
  
  if("avg_density" %in% colnames(dataframeclean) && "error_SD" %in% colnames(dataframeclean) && "error_upper" %in% colnames(dataframeclean) && "error_lower" %in% colnames(dataframeclean)){
    dataframeclean$avg_density <- as.numeric(dataframeclean$avg_density)
    dataframeclean$error_SD <- as.numeric(dataframeclean$error_SD)
    dataframeclean$error_upper <- as.numeric(dataframeclean$error_upper)
    dataframeclean$error_lower <- as.numeric(dataframeclean$error_lower)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "avg_density", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_SD","error_upper","error_lower"), mean)
  }else if("error_SD" %in% colnames(dataframeclean) && "error_upper" %in% colnames(dataframeclean) && "error_lower" %in% colnames(dataframeclean)){
    dataframeclean$error_SD <- as.numeric(dataframeclean$error_SD)
    dataframeclean$error_upper <- as.numeric(dataframeclean$error_upper)
    dataframeclean$error_lower <- as.numeric(dataframeclean$error_lower)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_SD","error_upper","error_lower"), mean)
  }else if("avg_density" %in% colnames(dataframeclean) && "error_SD" %in% colnames(dataframeclean)){
    dataframeclean$avg_density <- as.numeric(dataframeclean$avg_density)
    dataframeclean$error_SD <- as.numeric(dataframeclean$error_SD)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "avg_density", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_SD"), mean)
  }else if("error_SD" %in% colnames(dataframeclean)){
    dataframeclean$error_SD <- as.numeric(dataframeclean$error_SD)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_SD"), mean)
  }else if("avg_density" %in% colnames(dataframeclean)){
    dataframeclean$avg_density <- as.numeric(dataframeclean$avg_density)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "avg_density", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3"), mean)
  }else if("avg_density" %in% colnames(dataframeclean) && "error_upper" %in% colnames(dataframeclean) && "error_lower" %in% colnames(dataframeclean)){
    dataframeclean$avg_density <- as.numeric(dataframeclean$avg_density)
    dataframeclean$error_upper <- as.numeric(dataframeclean$error_upper)
    dataframeclean$error_lower <- as.numeric(dataframeclean$error_lower)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "avg_density", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_upper","error_lower"), mean)
  }else if("error_upper" %in% colnames(dataframeclean) && "error_lower" %in% colnames(dataframeclean)){
    dataframeclean$error_upper <- as.numeric(dataframeclean$error_upper)
    dataframeclean$error_lower <- as.numeric(dataframeclean$error_lower)
    summary_table <- dataframeclean %>% group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3", "error_upper","error_lower"), mean)
  }else{
    summary_table <- dataframeclean %>%
      group_by(sample_ID) %>%
      summarise_at(c("concentration_particle_vol", "density_mg_um_3", "density_upper", "density_lower", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3"), mean)
  }
  
  summary_table[summary_table==0]<-NA
  dataframeclean_summary <- summary_table
  
  if("avg_density" %in% colnames(dataframeclean_summary)){
    for(x in 1:nrow(dataframeclean_summary)){
      if(is.na(dataframeclean_summary[x, "avg_density"])) {
        dataframeclean_summary[x, "min_mass_mg"] <- (as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) - as.numeric(dataframeclean_summary[x, "density_lower"])) * as.numeric(dataframeclean_summary[x, "volume_min_um_3"])
        dataframeclean_summary[x, "mean_mass_mg"] <- as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) * as.numeric(dataframeclean_summary[x,"volume_mean_um_3"])
        dataframeclean_summary[x, "max_mass_mg"] <- (as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) + as.numeric(dataframeclean_summary[x, "density_upper"])) * as.numeric(dataframeclean_summary[x, "volume_max_um_3"])
      }else{
        dataframeclean_summary[x, "min_mass_mg"] <- as.numeric(dataframeclean_summary[x, "avg_density"]) * as.numeric(dataframeclean_summary[x, "volume_min_um_3"])
        dataframeclean_summary[x, "mean_mass_mg"] <- as.numeric(dataframeclean_summary[x, "avg_density"]) * as.numeric(dataframeclean_summary[x,"volume_mean_um_3"])
        dataframeclean_summary[x, "max_mass_mg"] <- as.numeric(dataframeclean_summary[x, "avg_density"]) * as.numeric(dataframeclean_summary[x, "volume_max_um_3"])
      }
    }
  }else{
    for(x in 1:nrow(dataframeclean_summary)){
      dataframeclean_summary[x, "min_mass_mg"] <- (as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) - as.numeric(dataframeclean_summary[x, "density_lower"])) * as.numeric(dataframeclean_summary[x, "volume_min_um_3"])
      dataframeclean_summary[x, "mean_mass_mg"] <- as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) * as.numeric(dataframeclean_summary[x,"volume_mean_um_3"])
      dataframeclean_summary[x, "max_mass_mg"] <- (as.numeric(dataframeclean_summary[x, "density_mg_um_3"]) + as.numeric(dataframeclean_summary[x, "density_upper"])) * as.numeric(dataframeclean_summary[x, "volume_max_um_3"])
      }
  }
  
  dataframeclean_summary <- dataframeclean_summary %>% select(-c(density_upper, density_lower)) %>% add_column(min_concentration_particle_vol = NA,
                                                                                                              max_concentration_particle_vol = NA)
  
  if("error_SD" %in% colnames(dataframeclean_summary) == TRUE){
    dataframeclean_summary <- dataframeclean_summary %>%
      add_column(min_concentration_particle_vol_SD = as.numeric(dataframeclean_summary$concentration_particle_vol) - as.numeric(dataframeclean_summary$error_SD),
                 max_concentration_particle_vol_SD = as.numeric(dataframeclean_summary$concentration_particle_vol) + as.numeric(dataframeclean_summary$error_SD))
    for(x in 1:nrow(dataframeclean_summary)){
      if(!(is.na(dataframeclean_summary[x, "error_SD"]))){
        dataframeclean_summary[x, "min_concentration_particle_vol"] = dataframeclean_summary[x, "min_concentration_particle_vol_SD"]
        dataframeclean_summary[x, "max_concentration_particle_vol"] = dataframeclean_summary[x, "max_concentration_particle_vol_SD"]
      }
    }
  }
    
  if("error_upper" %in% colnames(dataframeclean_summary) == TRUE && "error_lower" %in% colnames(dataframeclean_summary) == TRUE){
    dataframeclean_summary <- dataframeclean_summary %>%
      add_column(min_concentration_particle_vol_er = as.numeric(dataframeclean_summary$error_lower),
                 max_concentration_particle_vol_er = as.numeric(dataframeclean_summary$error_upper))
    for(x in 1:nrow(dataframeclean_summary)){
      if(!(is.na(dataframeclean_summary[x, "error_SD"]))){
        dataframeclean_summary[x, "min_concentration_particle_vol"] = dataframeclean_summary[x, "min_concentration_particle_vol_er"]
        dataframeclean_summary[x, "max_concentration_particle_vol"] = dataframeclean_summary[x, "max_concentration_particle_vol_er"]
      }
    }
  }
    
  for(x in 1:nrow(dataframeclean_summary)){
    if(is.na(dataframeclean_summary[x, "min_concentration_particle_vol"]) && is.na(dataframeclean_summary[x, "max_concentration_particle_vol"])){
      dataframeclean_summary[x, "min_concentration_particle_vol"] = dataframeclean_summary[x, "concentration_particle_vol"]
      dataframeclean_summary[x, "max_concentration_particle_vol"] = dataframeclean_summary[x, "concentration_particle_vol"]
    }
  }
    
  dataframeclean_summary <- dataframeclean_summary %>%
    add_column(min_concentration_mg_vol = as.numeric(dataframeclean_summary$min_mass_mg) * as.numeric(dataframeclean_summary$min_concentration_particle_vol),
               mean_concentration_mg_vol = as.numeric(dataframeclean_summary$mean_mass_mg) * as.numeric(dataframeclean_summary$concentration_particle_vol),
               max_concentration_mg_vol = as.numeric(dataframeclean_summary$max_mass_mg) * as.numeric(dataframeclean_summary$max_concentration_particle_vol)) %>%
    select(sample_ID, min_concentration_mg_vol, mean_concentration_mg_vol, max_concentration_mg_vol, volume_min_um_3, volume_max_um_3, volume_mean_um_3)
  
  
  dataframe <- left_join(dataframe, dataframeclean_summary, by = "sample_ID")
  
  
  dataframeclean_trash$weight_estimate_g <- as.numeric(dataframeclean_trash$weight_estimate_g)
  for(x in 1:nrow(dataframeclean_trash)){
    if(! is.na(dataframeclean_trash[x, "weight_estimate_g"])){
      dataframeclean_trash[x, "mean_mass_mg"] <- (dataframeclean_trash[x, "weight_estimate_g"]*1000)
    }
  }
  dataframeclean_trash <- dataframeclean_trash %>% drop_na(mean_mass_mg) %>%
    select(sample_ID, concentration_particle_vol, material, morphology, morphology_percent, mean_mass_mg)
  
  dataframeclean_trash$mean_mass_mg <- as.numeric(dataframeclean_trash$mean_mass_mg)
  dataframeclean_trash$morphology_percent <- as.numeric(dataframeclean_trash$morphology_percent)
  dataframeclean_trash$concentration_particle_vol <- as.numeric(dataframeclean_trash$concentration_particle_vol)
  dataframeclean_trash$mean_concentration_mg <- (dataframeclean_trash$mean_mass_mg * dataframeclean_trash$morphology_percent * 0.01 * dataframeclean_trash$concentration_particle_vol)
  
  summary_table_trash <- dataframeclean_trash %>%
    group_by(sample_ID) %>%
    summarise_at(c("mean_concentration_mg"), mean) 
  
  dataframeclean_particles <- left_join(dataframe, summary_table_trash, by = "sample_ID", copy = F)
  
  for(x in 1:nrow(dataframeclean_particles)){
    if(! is.na(dataframeclean_particles[x, "mean_concentration_mg"])){
      dataframeclean_particles[x, "mean_concentration_mg_vol"] <- (dataframeclean_particles[x, "mean_concentration_mg"])
    }
  }
  
  dataframeclean_particles <- dataframeclean_particles %>% select(-mean_concentration_mg)
    
  dataframeclean_particles <- data.frame(dataframeclean_particles)
  
  dataframeclean_particles <- dataframeclean_particles %>%
    mutate_if(is.numeric, signif, digits=3)
  
  return(dataframeclean_particles)
}

#dataframe <- read.csv("tests/rescaling_concentration.csv")
#dataframe <- read.csv("tests/rescaling_binned.csv")
correctionFactor_conc <- function(dataframe, alpha_vals, metric, corrected_min, corrected_max){
  
  dataframe$concentration_particle_vol <- as.numeric(dataframe$concentration_particle_vol)
  dataframe$size_min <- as.numeric(dataframe$size_min)
  dataframe$size_max <- as.numeric(dataframe$size_max)
  if("study_media" %in% colnames(dataframe) == TRUE){dataframe$study_media <- as.character(dataframe$study_media)}
  if("known_alpha" %in% colnames(dataframe) == TRUE){dataframe$known_alpha <- as.numeric(dataframe$known_alpha)}
  if("sample_ID"  %in% colnames(dataframe) == TRUE){dataframe$sample_ID <- as.character(dataframe$sample_ID)}
  if("concentration_upper" %in% colnames(dataframe) == TRUE){dataframe$concentration_upper <- as.numeric(dataframe$concentration_upper)}
  if("concentration_lower" %in% colnames(dataframe) == TRUE){dataframe$concentration_lower <- as.numeric(dataframe$concentration_lower)}
  if("error_SD" %in% colnames(dataframe) == TRUE){dataframe$error_SD <- as.numeric(dataframe$error_SD)}
  if("error_upper" %in% colnames(dataframe) == TRUE){dataframe$error_upper <- as.numeric(dataframe$error_upper)}
  if("error_lower" %in% colnames(dataframe) == TRUE){dataframe$error_lower <- as.numeric(dataframe$error_lower)}
  dataframeclean <- mutate_all(dataframe, cleantext) 
  #metric <- "length (um)"
  if("study_media" %in% colnames(dataframe) == TRUE){
    if(metric == "length (um)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "length", "length_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "length",
             "sd" = "length_sd")}
    if(metric == "width (um)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "width", "width_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "length",
             "sd" = "width_sd")}
    if(metric == "mass (ug)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "mass", "mass_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "mass",
             "sd" = "mass_sd")}
    if(metric == "volume (um3)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "volume", "volume_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "volume",
             "sd" = "volume_sd")}
    if(metric == "surface area (um2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "surface_area", "surface_area_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "surface_area",
             "sd" = "surface_area_sd")}
    if(metric == "specific surface area (g/m2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "specific_surface_area", "specific_surface_area_sd")], by = "study_media", all.x=TRUE)
    dataframeclean <- dataframeclean %>%
      rename("alpha" = "specific_surface_area",
             "sd" = "specific_surface_area_sd")}
    alpha_lower <- (dataframeclean$alpha) - (dataframeclean$sd)
    alpha_upper <- (dataframeclean$alpha) + (dataframeclean$sd)
    dataframeclean <- dataframeclean %>% 
      add_column(alpha_lower = alpha_lower,
                 alpha_upper = alpha_upper) %>%
      select(-study_media, -sd)
  }else{
    dataframeclean <- dataframeclean %>% 
      add_column(alpha = NA,
                 alpha_lower = NA,
                 alpha_upper = NA
      )
  }
  
  if("sample_ID" %in% colnames(dataframeclean) == TRUE){
    unique_bins <- dataframeclean %>% count(sample_ID, size_min, size_max)
    bin_numbers <- unique_bins %>% count(sample_ID)
    alpha_bins <- bin_numbers %>% filter(n >= 5)
    if(nrow(alpha_bins) >= 1){
      unique_alpha <- data.frame()
      for(x in 1:nrow(alpha_bins)){
        x = 1
        sample_name <- alpha_bins$sample_ID[[x]]
        sample_bins <- dataframeclean %>% filter(sample_ID == sample_name)
        midpoint <- (as.numeric(sample_bins$size_min) + as.numeric(sample_bins$size_max))/2
        sample_bins <- sample_bins %>%
          add_column(midpoint = midpoint,
                     alpha_calc_lower = NA,
                     alpha_calc_upper = NA) %>%
          select(sample_ID, midpoint, concentration_particle_vol, alpha, alpha_calc_lower, alpha_calc_upper)
        sample_bins$log_size <- log10(sample_bins$midpoint)
        sample_bins$concentration_particle_vol <- as.numeric(sample_bins$concentration_particle_vol)
        sample_bins$log_abundance <- log10(sample_bins$concentration_particle_vol)
        r1model <- lm(log_abundance ~ log_size, data = sample_bins)
        alpha <- -(as.numeric(coef(r1model)[2]))
        sample_bins$alpha <- alpha
        r1model_sum <- summary.lm(r1model)
        error <- as.numeric(coef(r1model_sum)[4])
        sample_bins$alpha_calc_lower <- alpha - error
        sample_bins$alpha_calc_upper <- alpha + error
        sample_bins <- sample_bins %>% select(sample_ID, alpha, alpha_calc_lower, alpha_calc_upper)
        sample_bins <- unique(sample_bins)
        unique_alpha <- rbind(unique_alpha, sample_bins)
        unique_alpha <- unique_alpha %>% rename(alpha_calc = alpha)
      }
      dataframeclean <- dataframeclean %>% left_join(unique_alpha, by = "sample_ID")
      for(x in 1:nrow(dataframeclean)){
        if(! is.na(dataframeclean[x, "alpha_calc"])){
          dataframeclean[x, "alpha"] <- dataframeclean[x, "alpha_calc"]
          dataframeclean[x, "alpha_lower"] <- dataframeclean[x, "alpha_calc_lower"]
          dataframeclean[x, "alpha_upper"] <- dataframeclean[x, "alpha_calc_upper"]
        }
      }
      dataframeclean <- dataframeclean %>% select(-c(alpha_calc, alpha_calc_lower, alpha_calc_upper))
    }
  }
  
  if("known_alpha" %in% colnames(dataframeclean) == TRUE){
    for(x in 1:nrow(dataframeclean)){
      if(! is.na(dataframeclean[x, "known_alpha"])){
        known_alpha_val <- as.numeric(dataframeclean[x, "known_alpha"])
        dataframeclean[x, "alpha"] <- (dataframeclean[x, "known_alpha"])
        if(metric == "length (um)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * length_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * length_coef)}
        if(metric == "width (um)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * width_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * width_coef)}
        if(metric == "mass (ug)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * mass_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * mass_coef)}
        if(metric == "volume (um3)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * volume_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * volume_coef)}
        if(metric == "surface area (um2)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * surface_area_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * surface_area_coef)}
        if(metric == "specific surface area (g/m2)"){dataframeclean[x, "alpha_lower"] <- known_alpha_val - (known_alpha_val * specific_surface_area_coef)
        dataframeclean[x, "alpha_upper"] <- known_alpha_val + (known_alpha_val * specific_surface_area_coef)}
      }
    }
    dataframeclean <- dataframeclean %>% select(-known_alpha)
  }
  
  for(x in 1:nrow(dataframeclean)){
    if(is.na(dataframeclean[x, "alpha"])){
      dataframeclean[x, "alpha"] <- 1.6
      if(metric == "length (um)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * length_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * length_coef)}
      if(metric == "width (um)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * width_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * width_coef)}
      if(metric == "mass (ug)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * mass_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * mass_coef)}
      if(metric == "volume (um3)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * volume_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * volume_coef)}
      if(metric == "surface area (um2)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * surface_area_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * surface_area_coef)}
      if(metric == "specific surface area (g/m2)"){dataframeclean[x, "alpha_lower"] <- 1.6 - (1.6 * specific_surface_area_coef)
      dataframeclean[x, "alpha_upper"] <- 1.6 + (1.6 * specific_surface_area_coef)}
    }
  }
  
  dataframeclean <- dataframeclean %>%
    add_column(concentration_lower = NA,
               concentration_upper = NA)
  
  if("sample_ID" %in% colnames(dataframeclean) == TRUE){
    unique_bins <- dataframeclean %>% count(sample_ID, size_min, size_max)
    bin_numbers <- unique_bins %>% count(sample_ID)
    bins_add <- bin_numbers %>% filter(n >= 2)
    
    dataframeclean_ <- dataframeclean %>% distinct(sample_ID, .keep_all = T)
    
    if(nrow(bins_add) >= 1){
      sample_add <- data.frame()
      for(x in 1:nrow(bins_add)){
        #x = 1
        sample_name <- bins_add$sample_ID[[x]]
        sample_bins <- dataframeclean %>% filter(sample_ID == sample_name)
        sample_bins$size_min <- as.numeric(sample_bins$size_min)
        sample_bins$size_max <- as.numeric(sample_bins$size_max)
        sample_bins$concentration_particle_vol <- as.numeric(sample_bins$concentration_particle_vol)
        row <- which(grepl(sample_name, dataframeclean_$sample_ID))
        dataframeclean_$size_min[[row]] <- min(sample_bins$size_min)
        dataframeclean_$size_max[[row]] <- max(sample_bins$size_max)
        dataframeclean_$concentration_particle_vol[[row]] <- sum(sample_bins$concentration_particle_vol)
        if("error_SD" %in% colnames(dataframeclean) == TRUE && !(is.na(sample_bins$error_SD[[x]]))){
          sample_bins$error_SD <- as.numeric(sample_bins$error_SD)
          dataframeclean_$concentration_upper[[row]] <- dataframeclean_$concentration_particle_vol[[row]] + (max(sample_bins$error_SD))
          dataframeclean_$concentration_lower[[row]] <- dataframeclean_$concentration_particle_vol[[row]] - (max(sample_bins$error_SD))
        }
        
        if("error_lower" %in% colnames(dataframeclean) == TRUE && "error_upper" %in% colnames(dataframeclean) == TRUE && !(is.na(sample_bins$error_upper[[x]])) && !(is.na(sample_bins$error_lower[[x]]))){
          sample_bins$error_upper <- as.numeric(sample_bins$error_upper)
          sample_bins$error_lower <- as.numeric(sample_bins$error_lower)
          dataframeclean_$concentration_upper[[row]] <- sum(sample_bins$error_upper)
          dataframeclean_$concentration_lower[[row]] <- sum(sample_bins$error_lower)
        }
      }
    }
    
    dataframeclean <- dataframeclean_
    ID <- dataframe %>% select(sample_ID)
    ID_clean <- mutate_all(ID, cleantext) 
    unique_ID <- unique(ID)
    unique_ID_clean <- unique(ID_clean)
    ID_raw_clean <- data.table(unique_ID = unique_ID, unique_ID_clean = unique_ID_clean) %>%
      rename(unique_ID = unique_ID.sample_ID,
             unique_ID_clean = unique_ID_clean.sample_ID)
    
    unique_ID_clean <- as.list(unique_ID_clean$sample_ID)
    dataframeclean <- dataframeclean %>% arrange(factor(sample_ID, levels = unique_ID_clean))
    dataframeclean$alpha <- as.numeric(dataframeclean$alpha)
  }
  
  for(x in 1:nrow(dataframeclean)){
    if(is.na(dataframeclean$concentration_upper[[x]])){
      dataframeclean$concentration_upper[[x]] <- dataframeclean$concentration_particle_vol[[x]]
    }
    if(is.na(dataframeclean$concentration_lower[[x]])){
      dataframeclean$concentration_lower[[x]] <- dataframeclean$concentration_particle_vol[[x]]
    }
  }
  
  dataframeclean <- dataframeclean %>%
    add_column(correction_factor = NA,
               correction_factor_lower = NA,
               correction_factor_upper = NA,
               corrected_concentration = NA,
               corrected_concentration_lower = NA,
               corrected_concentration_upper = NA)
  
  
  # corrected_min <- 1
  # corrected_max <- 5000
  #  x1D_set = 1
  #  x2D_set = 5000
  #Extrapolated parameters
  x1D_set = as.numeric(corrected_min) #lower limit default extrapolated range is 1 um
  x2D_set = as.numeric(corrected_max) #upper limit default extrapolated range is 5 mm
  
  for(x in 1:nrow(dataframeclean)) {
    #mean alpha
    x1M_set = as.numeric(dataframeclean$size_min[[x]])
    x2M_set = as.numeric(dataframeclean$size_max[[x]])
    alpha = as.numeric(dataframeclean$alpha[[x]])
    if(alpha == 1){
      CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
      CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
      CF <- (as.numeric(CFL) + as.numeric(CFU))/2
    }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                      x2M = x2M_set, #upper measured length
                      x1D = x1D_set, #default lower size range
                      x2D = x2D_set,  #default upper size range
                      a = alpha #alpha
                      )
    }
    
    dataframeclean$correction_factor[[x]] <- as.numeric(CF)
    dataframeclean$corrected_concentration[[x]] <- as.numeric(dataframeclean$correction_factor[[x]]) * as.numeric(dataframeclean$concentration_particle_vol[[x]])
    
    #min alpha
    alpha = as.numeric(dataframeclean$alpha_lower[[x]])
    if(alpha == 1){
      CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
      CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
      CF <- (as.numeric(CFL) + as.numeric(CFU))/2
    }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                      x2M = x2M_set, #upper measured length
                      x1D = x1D_set, #default lower size range
                      x2D = x2D_set,  #default upper size range
                      a = alpha #alpha
    )
    }
    dataframeclean$correction_factor_lower[[x]] <- as.numeric(CF)
    dataframeclean$corrected_concentration_lower[[x]] <- as.numeric(dataframeclean$correction_factor_lower[[x]]) * as.numeric(dataframeclean$concentration_lower[[x]])
    
    #max alpha
    alpha = as.numeric(dataframeclean$alpha_upper[[x]])
    if(alpha == 1){
      CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
      CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
      CF <- (as.numeric(CFL) + as.numeric(CFU))/2
    }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                      x2M = x2M_set, #upper measured length
                      x1D = x1D_set, #default lower size range
                      x2D = x2D_set,  #default upper size range
                      a = alpha #alpha
    )
    }
    dataframeclean$correction_factor_upper[[x]] <- as.numeric(CF)
    dataframeclean$corrected_concentration_upper[[x]] <- as.numeric(dataframeclean$correction_factor_upper[[x]]) * as.numeric(dataframeclean$concentration_upper[[x]])
  }
  dataframeclean <- dataframeclean %>% 
    select(-c(size_min, size_max, concentration_lower, concentration_upper)) %>%
    mutate_if(is.numeric, signif, digits=3)
  
  return(dataframeclean)
}


#dataframe <- read.csv("tests/rescaling_particle.csv")
correctionFactor_particle <- function(dataframe, corrected_min, corrected_max, binning_type, bin_number){
  
  dataframe$length_um <- as.numeric(dataframe$length_um)
  dataframe$sample_ID <- as.character(dataframe$sample_ID)
  if("sample_volume" %in% colnames(dataframe) == TRUE){
    dataframe$sample_volume <- as.numeric(dataframe$sample_volume)}
  
  unique_sample_ID <- unique(dataframe$sample_ID)
  unique_sample_ID <- as.data.frame(unique_sample_ID) %>%
    rename(sample_ID = unique_sample_ID)
  unique_alpha <- data.frame()
  
  for(x in 1:nrow(unique_sample_ID)){
   #x = 1
    subset <- filter(dataframe, sample_ID == unique_sample_ID$sample_ID[[x]])
    sample_size <- as.numeric(nrow(subset))
    #bin_number = 5
    #binning_type = "sd"
    int <- classify_intervals(subset$length_um, n = bin_number, style = binning_type)
    #int <- classify_intervals(subset$length_um, n = 5, style = "quantile")
    midpoints <- get_midpoints(x = int)
    midpoints <- as.data.frame(midpoints)
    freq <- midpoints %>% count(midpoints) %>%
      rename(length_um = midpoints,
             abundance = n)
    freq$log_size <- log10(freq$length_um)
    freq$log_abundance <- log10(freq$abundance)
    r1model <- lm(log_abundance ~ log_size, data = freq)
    alpha <- -(as.numeric(coef(r1model)[2]))
    r1model_sum <- summary.lm(r1model)
    #error <- (1/sqrt(sample_size))/2
    error <- as.numeric(coef(r1model_sum)[4])
    alpha_lower <- alpha - error
    alpha_upper <- alpha + error
    subset_ <- subset %>% add_column(alpha = alpha,
                                     alpha_lower = alpha_lower,
                                     alpha_upper = alpha_upper) %>% 
      select(sample_ID, alpha, alpha_lower, alpha_upper)
    subset_ <- unique(subset_)
    unique_alpha <- rbind(unique_alpha, subset_)
  }
  
  dataframe <- left_join(dataframe, unique_alpha, by = "sample_ID")
  
  if("sample_volume" %in% colnames(dataframe) == TRUE){
    dataframe_ <- data.table()
    
    for(x in 1:nrow(unique_sample_ID)){
      #x = 2
      subset <- filter(dataframe, sample_ID == unique_sample_ID$sample_ID[[x]])
      particle_number <- subset %>% count(sample_volume) %>%
        rename(particle_number = n)
      concentration <- (as.numeric(particle_number$particle_number))/(as.numeric(particle_number$sample_volume))
      subset <- subset %>% add_column(concentration = concentration,
                                      correction_factor = NA,
                                      correction_factor_lower = NA,
                                      correction_factor_upper = NA,
                                      corrected_concentration = NA,
                                      corrected_concentration_lower = NA,
                                      corrected_concentration_upper = NA)
      #x1D_set = 1
      #x2D_set = 5000
      #Extrapolated parameters
      x1D_set = as.numeric(corrected_min) #lower limit default extrapolated range is 1 um
      x2D_set = as.numeric(corrected_max) #upper limit default extrapolated range is 5 mm
      
      #mean alpha
      x1M_set = as.numeric(min(subset$length_um))
      x2M_set = as.numeric(max(subset$length_um))
      alpha = as.numeric(subset$alpha[1])
      if(alpha == 1){
        CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
        CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
        CF <- (as.numeric(CFL) + as.numeric(CFU))/2
      }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                        x2M = x2M_set, #upper measured length
                        x1D = x1D_set, #default lower size range
                        x2D = x2D_set,  #default upper size range
                        a = alpha #alpha
      )
      }
      
      subset$correction_factor <- as.numeric(CF)
      subset$corrected_concentration <- as.numeric(subset$correction_factor) * as.numeric(subset$concentration)
      
      #min alpha
      alpha = as.numeric(subset$alpha_lower[1])
      if(alpha == 1){
        CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
        CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
        CF <- (as.numeric(CFL) + as.numeric(CFU))/2
      }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                        x2M = x2M_set, #upper measured length
                        x1D = x1D_set, #default lower size range
                        x2D = x2D_set,  #default upper size range
                        a = alpha #alpha
      )
      }
      subset$correction_factor_lower <- as.numeric(CF)
      subset$corrected_concentration_lower <- as.numeric(subset$correction_factor_lower) * as.numeric(subset$concentration)
      
      #max alpha
      alpha = as.numeric(subset$alpha_upper[1])
      if(alpha == 1){
        CFL <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 0.999)
        CFU <- CFfnx(x1M = x1M_set, x2M = x2M_set, x1D = x1D_set, x2D = x2D_set, a = 1.001)
        CF <- (as.numeric(CFL) + as.numeric(CFU))/2
      }else{CF <- CFfnx(x1M = x1M_set,#lower measured length
                        x2M = x2M_set, #upper measured length
                        x1D = x1D_set, #default lower size range
                        x2D = x2D_set,  #default upper size range
                        a = alpha #alpha
      )
      }
      subset$correction_factor_upper <- as.numeric(CF)
      subset$corrected_concentration_upper <- as.numeric(subset$correction_factor_upper) * as.numeric(subset$concentration)
      
      
      
      dataframe_ <- rbind(dataframe_, subset)
      
    }
    #dataframe_ <- dataframe_ %>% select(-sample_volume)
  }
  #dataframe <- dataframe_ %>% distinct(sample_ID, alpha, .keep_all = TRUE) %>% select(-length_um)
  dataframe <- dataframe_
  
  dataframe <- dataframe %>%
    mutate_if(is.numeric, signif, digits=3)
  
  return(dataframe)
}

#create function to extract midpoints from binning outputs
get_midpoints <- function(x, dp=2){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, dp))
}

#Make df for alpha values
study_media <- c("marinesurface","freshwatersurface","marinesediment","freshwatersediment","effluent", "biota", "drinkingwater")
length <- c(2.07, 2.64, 2.57, 3.25, 2.54, 2.59, 1.64)
length_sd <- c(0.03, 0.01, 0.20, 0.19, 0.01, 0.04, 0.55)
width <- c(1.96, 2.70, 2.51, 2.87, 2.55, 2.52, NA)
width_sd <- c(0.026, 0.008, 0.091, 0.167, 0.015, 0.07, NA)
mass <- c(1.32, 1.65, 1.50, 1.56, 1.40, 1.41, NA)
mass_sd <- c(0.009, 0.071, 0.026, 0.077, 0.002, 0.060, NA)
volume <- c(1.48, 1.68, 1.50, 1.53, 1.45, 1.40, NA)
volume_sd <- c(0.063, 0.081, 0.023, 0.013, 0.004, 0.004, NA)
surface_area <- c(1.50, 2.00, 1.75, 1.89, 1.73, 1.69, NA)
surface_area_sd <- c(0.009, 0.065, 0.050, 0.055, 0.005, 0.023, NA)
specific_surface_area <- c(1.98, 2.71, 2.54, 2.82, 2.58, 2.46, NA)
specific_surface_area_sd <- c(0.297, 0.009, 0.082, 0.096, 0.013, 0.092, NA)

alpha_vals <- data.frame(study_media = study_media,
                         length = length,
                         length_sd = length_sd,
                         width = width,
                         width_sd = width_sd,
                         mass=mass,
                         mass_sd = mass_sd,
                         volume = volume,
                         volume_sd = volume_sd,
                         surface_area = surface_area,
                         surface_area_sd = surface_area_sd,
                         specific_surface_area = specific_surface_area,
                         specific_surface_area_sd = specific_surface_area_sd
)

#Find average coefficient of variability to create upper and lower alpha vals from given values and for a = 1.6
length_coef <- as.numeric(mean(length_sd/length)) #0.0289907
width_coef <- as.numeric(mean(width_sd/width)) #0.02405526
mass_coef <- as.numeric(mean(mass_sd/mass)) #0.02675376
volume_coef <- as.numeric(mean(volume_sd/volume)) #0.02003795
surface_area_coef <- as.numeric(mean(surface_area_sd/surface_area)) #0.0187786
specific_surface_area_coef <- as.numeric(mean(specific_surface_area_sd/specific_surface_area)) #0.0436807


use_cases <- read.csv("data/Item_Use_Case.csv")
prime_unclassifiable <- read.csv("data/PrimeUnclassifiable.csv")
PrimeUnclassifiable <- read.csv("data/PrimeUnclassifiable.csv")

primeItems <- read.csv("data/PrimeItems.csv")
primeMaterials <- read.csv("data/PrimeMaterials.csv")

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
  sample_size = length(data)
  population_size = 10000
  1.96*abs(sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1)))
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
    select(from, Key, Count) %>%
    add_row(from = "trash", Key = "missing", Count = sum(DF$Count, na.rm = T) - sum(.$Count, na.rm = T))%>%
    mutate(Count = Count/sum(Count, na.rm = T))
  
  
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
              min_prop = mean(totalsum, na.rm = T) - confidence_interval_width(totalsum), 
              max_prop = mean(totalsum, na.rm = T) + confidence_interval_width(totalsum))
  
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
  
  values[df_join_boot$mean_prop < 0.07] <- NA
  
  plot_ly() %>%
    add_trace(
      labels = df_join_boot$to,
      parents = df_join_boot$from,
      type = "sunburst",
      maxdepth = 6,
      domain = list(column = 1), 
      branchvalues = 'total',
      texttemplate = values,
      values = df_join_boot$mean_prop) %>%
    layout(colorway = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD"))
    
}

#brewer.pal(n = 10, name = "Set3")

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
trash_mass <- read.csv("data/trash_mass.csv")
aliasclean <- mutate_all(alias, cleantext)
aliascleani <- mutate_all(aliasi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)
microcolorclean <- mutate_all(microcolor, cleantext)
trash_mass_clean <- mutate_all(trash_mass, cleantext)

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

#data tables for count to mass conversion
#Make polymer-density dataframe
#Output correct survey sheet
polymer_db_ <- data.frame(polymer_db)
polymer_db_$material <- cleantext(polymer_db_$material)
polymer_db_$density <- as.numeric(polymer_db_$density)
polymer_db_$lower_conf <- as.numeric(polymer_db_$lower_conf)
polymer_db_$upper_conf <- as.numeric(polymer_db_$upper_conf)
density_mg_um_3 <- polymer_db_$density * 1e-9
density_min <- polymer_db_$lower_conf * 1e-9
density_max <- polymer_db_$upper_conf * 1e-9
polymer_db_ <- polymer_db_ %>%
  mutate(density_mg_um_3 = density_mg_um_3,
         density_max = density_max,
         density_min = density_min)
polymer_db_ <- polymer_db_ %>% 
  left_join(Materials_Alias, by = c("material" = "Alias")) %>%
  select(-c("Material"))
polymer_density <- polymer_db_ %>%
  select(material, density_mg_um_3, density_max, density_min, readable)

#Add +/- 5% error for measured particle dimensions
morphology <- c("fragment","sphere","fiber","film","foam")
meas_min <- 0.95
meas_max <- 1.05
L_min <- c(0.95, 0.95, 0.95, 0.95, 0.95)
L_max <- c(1.05, 1.05, 1.05, 1.05, 1.05)

#Min and max values given in Kooi Koelmans
W_min <- c(0.1,0.60,0.001,0.1,0.1)
W_max <- c(1,1,0.5,1,1)
#W_mean <- (as.numeric(W_min) + as.numeric(W_max))/2
#W_sd <- (as.numeric(W_max) - as.numeric(W_min))/6
H_min <- c(0.01,0.36,0.001,0.001,0.01)
H_max <- c(1,1,0.5,0.1,1)
#H_mean <- (as.numeric(H_min) + as.numeric(H_max))/2
#H_sd <- (as.numeric(H_max) - as.numeric(H_min))/6

#Assuming min and max encompass 99.7% of normal distribution, calculate 95% confidence interval
#W_min <- as.numeric(quantile(rnorm(n = 100000, mean = W_mean, sd = W_sd), probs = c(0.025)))
#W_max <- as.numeric(quantile(rnorm(n = 100000, mean = W_mean, sd = W_sd), probs = c(0.975)))
#H_min <- as.numeric(quantile(rnorm(n = 100000, mean = H_mean, sd = H_sd), probs = c(0.025)))
#H_max <- as.numeric(quantile(rnorm(n = 100000, mean = H_mean, sd = H_sd), probs = c(0.975)))

morphology_shape <- data.frame(morphology=morphology,
                               L_min=L_min,
                               L_max=L_max,
                               W_min=W_min,
                               W_max=W_max,
                               H_min=H_min,
                               H_max=H_max
)

#convert morphologies in TT to morphologies with defined dimensions
morphology <- c("fiber", "nurdle", "foam", "sphere", "line", "bead", "sheet", "film", "fragment", "rubberyfragment", "fiberbundle")
morph_dimension <- c("fiber", "sphere", "foam", "sphere", "fiber", "sphere", "film", "film", "fragment", "fragment", "film")
morph_conversion <- data.frame(morphology = morphology,
                               morph_dimension = morph_dimension)



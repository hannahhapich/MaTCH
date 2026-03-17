library(tidyr)
library(skimr)
library(chRoma)
library(tibble)
library(dplyr)
library(data.table)
library(data.tree)
library(plotly)
library(classInt)
library(aws.s3)
library(digest)
library(shiny)
library(DT)
library(shinyTree)
library(shinyWidgets)
library(bs4Dash)
library(purrr)
library(shinyFeedback)

source("Equations.R")

color <- read.csv("data/Microplastics_Color.csv")


#Data for embeddings generation via chRoma - Category-Specific Versions
# Trash vector databases
items_vectorDB_trash <- readRDS(file = "data/items_vectorDB_trash.rda")
materials_vectorDB_trash <- readRDS(file = "data/materials_vectorDB_trash.rda")

# Microplastic vector databases
items_vectorDB_microplastic <- readRDS(file = "data/items_vectorDB_microplastic.rda")
materials_vectorDB_microplastic <- readRDS(file = "data/materials_vectorDB_microplastic.rda")

# Keep unified versions for backward compatibility (fallback)
items_vectorDB <- items_vectorDB_trash
materials_vectorDB <- materials_vectorDB_trash

Sys.setenv(OPENAI_API_KEY = readLines("data/openai.txt", warn = FALSE))
creds <- read.csv("data/s3_cred.csv")
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = creds$Access.key.ID,
  "AWS_SECRET_ACCESS_KEY" = creds$Secret.access.key,
  "AWS_DEFAULT_REGION" = "us-east-2"
)

# Note: VectorDB filtering happens through alias joins in merge_terms, not by filtering the DB itself
# This preserves the vectorDB structure needed for chRoma functions like query_collection()

merge_terms <- function(file_paths, materials_vectorDB, items_vectorDB, alias, aliasi, use_cases, prime_unclassifiable, type = "trash") {
  # Use category-specific vectorDBs based on type parameter
  if (type == "microplastics") {
    materials_vectorDB <- materials_vectorDB_microplastic
    items_vectorDB <- items_vectorDB_microplastic
  } else {  # Default to trash
    materials_vectorDB <- materials_vectorDB_trash
    items_vectorDB <- items_vectorDB_trash
  }
  if (type == "microplastics") {
    # These will be overridden in server.R when calling the function
  }
  
  clean_and_prepare_data <- function(dataframe, column, descriptor) {
    if (!column %in% colnames(dataframe)) {
      stop(paste("Column", column, "not found in dataframe"))
    }
    
    if (!"Alias" %in% colnames(descriptor)) {
      stop("Column 'Alias' not found in alias dataframe")
    }
    
    dataframe <- dataframe %>%
      mutate(!!sym(column) := as.character(!!sym(column))) %>%
      mutate(!!sym(column) := cleantext(!!sym(column)))
    
    key <- inner_join(dataframe %>% select(!!sym(column)), descriptor, by = setNames("Alias", column)) %>% distinct()
    
    left <- anti_join(dataframe %>% select(!!sym(column)), descriptor, by = setNames("Alias", column)) %>%
      distinct() %>% drop_na() %>% rename(text = !!sym(column)) %>% as.data.table()
    
    return(list(dataframe = dataframe, key = key, left = left))
  }
  
  process_unknowns <- function(left, vectorDB, descriptor, column, key) {
    if (nrow(left) > 0) {
      new_vDB <- add_collection(metadata = left)
      # Pull 15 aliases to allow deduplication down to 5 unique PRIME terms
      unknown_key <- query_collection(db = vectorDB, query_embeddings = new_vDB, top_n = 15, type = "dotproduct") %>%
        left_join(vectorDB$metadata, by = c("db_id" = "id")) %>%
        rename(Alias = text) %>%
        left_join(new_vDB$metadata, by = c("query_id" = "id")) %>%
        rename(!!sym(column) := text)
      
      embedding <- unknown_key %>%
        add_column(rank = rep(seq(15), nrow(unknown_key) / 15)) %>%
        select(Alias, !!sym(column), rank) %>%
        pivot_wider(names_from = rank, values_from = Alias) %>%
        rename(!!paste0(column, "_raw") := 1,
               !!paste0(column, "_match_1") := 2,
               !!paste0(column, "_match_2") := 3,
               !!paste0(column, "_match_3") := 4,
               !!paste0(column, "_match_4") := 5,
               !!paste0(column, "_match_5") := 6,
               !!paste0(column, "_match_6") := 7,
               !!paste0(column, "_match_7") := 8,
               !!paste0(column, "_match_8") := 9,
               !!paste0(column, "_match_9") := 10,
               !!paste0(column, "_match_10") := 11,
               !!paste0(column, "_match_11") := 12,
               !!paste0(column, "_match_12") := 13,
               !!paste0(column, "_match_13") := 14,
               !!paste0(column, "_match_14") := 15,
               !!paste0(column, "_match_15") := 16)
      
      unknown_key <- unknown_key %>% distinct(!!sym(column), .keep_all = TRUE) %>%
        select(Alias, !!sym(column))
      
      readable_col <- ifelse(column == "material", "Material", "Item")
      key_full <- unknown_key %>%
        inner_join(descriptor, by = c("Alias")) %>%
        select(!!sym(column), !!sym(readable_col), readable) %>%
        bind_rows(key)
      
      return(list(key = key_full, embedding = embedding))
    }
    return(NULL)
  }
  
  # Clean and prepare data
  material_data <- clean_and_prepare_data(file_paths, "material", alias)
  
  # Only process morphology if the column exists
  if("morphology" %in% colnames(file_paths)){
    morphology_data <- clean_and_prepare_data(file_paths, "morphology", aliasi)
  } else {
    morphology_data <- list(dataframe = file_paths %>% mutate(morphology = NA), key = data.frame(), left = data.frame())
  }
  
  # Process unknown materials
  
  material_results <- process_unknowns(material_data$left, materials_vectorDB, alias, "material", material_data$key)
  if (!is.null(material_results)) {
    material_data$key <- material_results$key 
    material_embedding <- material_results$embedding
  } else {
    # Create empty material_embedding with match columns for consistency with downstream functions
    material_embedding <- material_data$dataframe %>%
      select(material) %>%
      rename(material_raw = material) %>%
      mutate(material_match_1 = NA, material_match_2 = NA, material_match_3 = NA, 
             material_match_4 = NA, material_match_5 = NA, material_match_6 = NA,
             material_match_7 = NA, material_match_8 = NA, material_match_9 = NA,
             material_match_10 = NA, material_match_11 = NA, material_match_12 = NA,
             material_match_13 = NA, material_match_14 = NA, material_match_15 = NA) %>%
      distinct()
  }
  
  # Process unknown morphologies (only if morphology column exists)
  if("morphology" %in% colnames(file_paths)){
    morphology_results <- process_unknowns(morphology_data$left, items_vectorDB, aliasi, "morphology", morphology_data$key)
    if (!is.null(morphology_results)) {
      morphology_data$key <- morphology_results$key
      morphology_embedding <- morphology_results$embedding
    } else {
      # Create empty morphology_embedding with match columns for consistency with downstream functions
      morphology_embedding <- morphology_data$dataframe %>%
        select(morphology) %>%
        rename(morphology_raw = morphology) %>%
        mutate(morphology_match_1 = NA, morphology_match_2 = NA, morphology_match_3 = NA, 
               morphology_match_4 = NA, morphology_match_5 = NA, morphology_match_6 = NA,
               morphology_match_7 = NA, morphology_match_8 = NA, morphology_match_9 = NA,
               morphology_match_10 = NA, morphology_match_11 = NA, morphology_match_12 = NA,
               morphology_match_13 = NA, morphology_match_14 = NA, morphology_match_15 = NA) %>%
        distinct()
    }
  }
  
  material_data$dataframe$material <- cleantext(material_data$dataframe$material)
  
  # Only clean morphology if it exists
  if("morphology" %in% colnames(material_data$dataframe)){
    material_data$dataframe$morphology <- cleantext(material_data$dataframe$morphology)
  }
  
  # Replace old material with merged material
  dataframeclean <- material_data$dataframe %>%
    left_join(material_data$key, by = "material") %>%
    select(-any_of("Material")) %>%
    rename(material_new = readable)
  
  # Only join morphology if it exists
  if("morphology" %in% colnames(dataframeclean)){
    dataframeclean <- dataframeclean %>%
      left_join(morphology_data$key, by = c("morphology")) %>%
      select(-any_of("Item")) %>%
      rename(morphology_new = readable) %>%
      select(any_of(c("material", "morphology", "material_new", "morphology_new"))) %>%
      unique()
  } else {
    dataframeclean <- dataframeclean %>%
      select(any_of(c("material", "material_new"))) %>%
      unique()
  }
  
  if ("material_embedding" %in% ls()) {
    dataframeclean <- dataframeclean %>% left_join(material_embedding, by = c("material" = "material_raw"))
  }
  if ("morphology_embedding" %in% ls() && "morphology" %in% colnames(dataframeclean)) {
    dataframeclean <- dataframeclean %>% left_join(morphology_embedding, by = c("morphology" = "morphology_raw"))
  }
  
  materialclean <- cleantext(file_paths$material)
  
  # Only clean morphology if it exists
  if("morphology" %in% colnames(file_paths)){
    morphologyclean <- cleantext(file_paths$morphology)
  } else {
    morphologyclean <- NA
  }
  
  # Handle rename and join - only rename morphology if it exists
  if("morphology" %in% colnames(file_paths)){
    dataframe <- file_paths %>%
      rename(material_raw = material, morphology_raw = morphology) %>%
      mutate(material = materialclean, morphology = morphologyclean) %>%
      left_join(dataframeclean, by = c("material", "morphology")) %>%
      select(-any_of(c("material", "morphology"))) %>%
      rename(material = material_new, morphology = morphology_new)
  } else {
    dataframe <- file_paths %>%
      rename(material_raw = material) %>%
      mutate(material = materialclean) %>%
      left_join(dataframeclean, by = c("material")) %>%
      select(-any_of(c("material"))) %>%
      rename(material = material_new)
  }
  
  dataframe <- dataframe %>%
    select(any_of(c("morphology_raw", "material_raw", "morphology", "material")), everything())
  return(dataframe)
}

.confidence_interval_width <- function(proportion, sample_size, population_size){
  1.96*sqrt((1/sample_size)*proportion * (1-proportion) * (population_size-sample_size)/(population_size-1))
}

polymer_class_rename <- function(df) {
  df$polymer[df$polymer == ""] <- 0
  df$polymer_class[df$polymer_class == ""] <- 0
  df <- df %>%
    mutate(material = ifelse(polymer == 0 & polymer_class != 0, polymer_class, polymer))
  return(df)
}


# Main function to convert particle count to mass
particle_count_mass <- function(dataframe, morphology_shape, polymer_density, trash_mass_clean, polymer_avg_decision, morph_weight, sample_weight, fiber_min, fiber_med, fiber_max, film_min, film_med, film_max, fiber_width_switch){
  
  # ===== COLUMN MAPPING LOGIC =====
  # Map alternative column names to standardized names (but keep originals for output)
  # These mappings are treated internally, original names returned at the end
  
  if ("max_length_um" %in% colnames(dataframe) && !("length_um" %in% colnames(dataframe))) {
    dataframe$length_um <- dataframe$max_length_um
  }
  
  if ("min_length_um" %in% colnames(dataframe) && !("width_um" %in% colnames(dataframe))) {
    dataframe$width_um <- dataframe$min_length_um
  }
  
  if ("material_class" %in% colnames(dataframe) && !("material" %in% colnames(dataframe))) {
    dataframe$material <- dataframe$material_class
  }
  
  # ===== TYPE CONVERSIONS =====
  if("length_um" %in% colnames(dataframe) == TRUE){dataframe$length_um <- as.numeric(dataframe$length_um)
  dataframe$length_um[is.na(dataframe$length_um)] <- 0
  }
  if("morphology" %in% colnames(dataframe) == TRUE){dataframe$morphology <- as.character(dataframe$morphology)}
  if("material" %in% colnames(dataframe) == TRUE){dataframe$material <- as.character(dataframe$material)}
  if("width_um" %in% colnames(dataframe) == TRUE){dataframe$width_um <- as.numeric(dataframe$width_um)}
  if("height_um" %in% colnames(dataframe) == TRUE){dataframe$height_um <- as.numeric(dataframe$height_um)}
  if("density" %in% colnames(dataframe) == TRUE){dataframe$density <- as.numeric(dataframe$density)}
  if("area_um2" %in% colnames(dataframe) == TRUE){dataframe$area_um2 <- as.numeric(dataframe$area_um2)}
  if("perimeter_um" %in% colnames(dataframe) == TRUE){dataframe$perimeter_um <- as.numeric(dataframe$perimeter_um)}
  if("circularity" %in% colnames(dataframe) == TRUE){dataframe$circularity <- as.numeric(dataframe$circularity)}
  if("Dx" %in% colnames(dataframe) == TRUE){dataframe$Dx <- as.numeric(dataframe$Dx)}
  if("Dy" %in% colnames(dataframe) == TRUE){dataframe$Dy <- as.numeric(dataframe$Dy)}
  
  if("sample_id" %in% colnames(dataframe) == TRUE){dataframe$sample_id <- as.character(dataframe$sample_id)}
  
  # Only convert PS + foam to EPS if both material and morphology columns exist
  if(("material" %in% colnames(dataframe)) && ("morphology" %in% colnames(dataframe))){
    dataframe <- dataframe %>%
      mutate(material = ifelse(material == "polystyrene" & morphology == "foam", "EPS", material))
  }
  
  dataframe <- left_join(dataframe, polymer_density, by = c("material" = "readable"), copy = F)
  # Only remove material.y if it exists
  dataframe <- dataframe %>%
    select(-any_of("material.y"))
  
  dataframeclean <- dataframe %>% mutate(material = cleantext(material))
  
  # Only cleantext morphology if it exists
  if("morphology" %in% colnames(dataframeclean)){
    dataframeclean <- dataframeclean %>% mutate(morphology = cleantext(morphology))
    
    dataframeclean <- left_join(dataframeclean, morphology_shape, by = "morphology", copy = F)
  }
  
  # Remove old morphology_shape dimension columns (no longer used in new volume calculation)
  dataframeclean <- dataframeclean %>%
    select(-any_of(c("L_min", "L_max", "W_min", "W_max", "H_min", "H_max")))
  
  
  fiber_min <- as.numeric(fiber_min)
  fiber_med <- as.numeric(fiber_med)
  fiber_max <- as.numeric(fiber_max)
  
  film_min <- as.numeric(film_min)
  film_med <- as.numeric(film_med)
  film_max <- as.numeric(film_max)
  
  dataframeclean <- dataframeclean %>% mutate(row_id = row_number())
  
  # ===== VOLUME CALCULATION LOGIC =====
  # Initialize volume columns (will only keep if at least one volume calculated)
  dataframeclean$volume_um3 <- NA_real_
  dataframeclean$min_volume_um3 <- NA_real_
  dataframeclean$max_volume_um3 <- NA_real_
  dataframeclean$volume_calculation_method <- as.character(NA)
  
  # Initialize mass columns so they exist even if no volumes are calculated
  dataframeclean$min_mass_mg <- NA_real_
  dataframeclean$mean_mass_mg <- NA_real_
  dataframeclean$max_mass_mg <- NA_real_
  
  # Row-by-row volume calculation with preference ordering
  for (row_idx in 1:nrow(dataframeclean)) {
    morph <- dataframeclean$morphology[row_idx]
    vol_result <- NULL
    method_used <- NULL
    
    # ===== FRAGMENT PREFERENCES =====
    if (identical(morph, "fragment")) {
      # Preference #1: L, W, A
      if (all(c("length_um", "width_um", "area_um2") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
          !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
          !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- chen_fragment(L = dataframeclean$length_um[row_idx], 
                                    W = dataframeclean$width_um[row_idx], 
                                    A = dataframeclean$area_um2[row_idx])
        method_used <- "Chen Fragment (L,W,A)"
      }
      # Preference #2: Dx, Dy, P
      else if (all(c("Dx", "Dy", "perimeter_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
               !is.na(dataframeclean$perimeter_um[row_idx]) && dataframeclean$perimeter_um[row_idx] > 0) {
        vol_result <- barchiesi(L = dataframeclean$Dx[row_idx], 
                               W = dataframeclean$Dy[row_idx], 
                               P = dataframeclean$perimeter_um[row_idx])
        method_used <- "Barchiesi (Dx,Dy,P)"
      }
      # Preference #3: L, W, P
      else if (all(c("length_um", "width_um", "perimeter_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
               !is.na(dataframeclean$perimeter_um[row_idx]) && dataframeclean$perimeter_um[row_idx] > 0) {
        vol_result <- barchiesi(L = dataframeclean$length_um[row_idx], 
                               W = dataframeclean$width_um[row_idx], 
                               P = dataframeclean$perimeter_um[row_idx])
        method_used <- "Barchiesi (L,W,P)"
      }
      # Preference #4: Dx, Dy
      else if (all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- tanoiri_fragment(L = dataframeclean$Dx[row_idx], 
                                       W = dataframeclean$Dy[row_idx])
        method_used <- "Tanoiri Fragment (Dx,Dy)"
      }
      # Preference #5: L, W
      else if (all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- tanoiri_fragment(L = dataframeclean$length_um[row_idx], 
                                       W = dataframeclean$width_um[row_idx])
        method_used <- "Tanoiri Fragment (L,W)"
      }
      # Preference #6: L
      else if ("length_um" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0) {
        vol_result <- fragment_geo(L = dataframeclean$length_um[row_idx])
        method_used <- "Simon Geometric (L only)"
      }
      # Preference #7: Dx
      else if ("Dx" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0) {
        vol_result <- fragment_geo(L = dataframeclean$Dx[row_idx])
        method_used <- "Simon Geometric (Dx only)"
      }
      # Preference #8: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina (A)"
      }
    }
    
    # ===== PELLET PREFERENCES =====
    else if (identical(morph, "pellet")) {
      # Preference #1: Dx, Dy, C (with circularity ranges)
      if (all(c("Dx", "Dy", "circularity") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
          !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
          !is.na(dataframeclean$circularity[row_idx])) {
        circ <- dataframeclean$circularity[row_idx]
        if (circ >= 0.5 && circ < 0.75) {
          vol_result <- chen_pellet_a(L = dataframeclean$Dx[row_idx], 
                                      W = dataframeclean$Dy[row_idx], 
                                      C = circ)
          method_used <- "Chen Pellet A (Dx,Dy,C)"
        } else if (circ >= 0.75 && circ <= 1.1) {
          vol_result <- chen_pellet_b(W = dataframeclean$Dy[row_idx], 
                                      C = circ)
          method_used <- "Chen Pellet B (Dy,C)"
        }
      }
      # Preference #2: L, W, C (with circularity ranges)
      else if (all(c("length_um", "width_um", "circularity") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
               !is.na(dataframeclean$circularity[row_idx])) {
        circ <- dataframeclean$circularity[row_idx]
        if (circ >= 0.5 && circ < 0.75) {
          vol_result <- chen_pellet_a(L = dataframeclean$length_um[row_idx], 
                                      W = dataframeclean$width_um[row_idx], 
                                      C = circ)
          method_used <- "Chen Pellet A (L,W,C)"
        } else if (circ >= 0.75 && circ <= 1.1) {
          vol_result <- chen_pellet_b(W = dataframeclean$width_um[row_idx], 
                                      C = circ)
          method_used <- "Chen Pellet B (W,C)"
        }
      }
      # Preference #3: Dx, Dy
      else if (all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- tanoiri_pellet(L = dataframeclean$Dx[row_idx], 
                                     W = dataframeclean$Dy[row_idx])
        method_used <- "Tanoiri Pellet (Dx,Dy)"
      }
      # Preference #4: L, W
      else if (all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- tanoiri_pellet(L = dataframeclean$length_um[row_idx], 
                                     W = dataframeclean$width_um[row_idx])
        method_used <- "Tanoiri Pellet (L,W)"
      }
      # Preference #5: Dx
      else if ("Dx" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0) {
        vol_result <- simon(L = dataframeclean$Dx[row_idx], 
                           W = dataframeclean$Dx[row_idx])
        method_used <- "Simon (Dx as L,W)"
      }
      # Preference #6: L
      else if ("length_um" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0) {
        vol_result <- simon(L = dataframeclean$length_um[row_idx], 
                           W = dataframeclean$length_um[row_idx])
        method_used <- "Simon (L as L,W)"
      }
      # Preference #7: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina (A)"
      }
    }
    
    # ===== FILM PREFERENCES =====
    else if (identical(morph, "film")) {
      # Preference #1: L, W, H
      if (all(c("length_um", "width_um", "height_um") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
          !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
          !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- film_geo(L = dataframeclean$length_um[row_idx], 
                              W = dataframeclean$width_um[row_idx], 
                              H = dataframeclean$height_um[row_idx])
        method_used <- "Film Geo (L,W,H measured)"
      }
      # Preference #2: Dx, Dy, H
      else if (all(c("Dx", "Dy", "height_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
               !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- film_geo(L = dataframeclean$Dx[row_idx], 
                              W = dataframeclean$Dy[row_idx], 
                              H = dataframeclean$height_um[row_idx])
        method_used <- "Film Geo (Dx,Dy,H measured)"
      }
      # Preference #3: L, W, H inputs
      else if (all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- film_geo_H(L = dataframeclean$length_um[row_idx], 
                                W = dataframeclean$width_um[row_idx], 
                                H = film_med, H_min = film_min, H_max = film_max)
        method_used <- "Film Geo H (L,W,H literature)"
      }
      # Preference #4: Dx, Dy, H inputs
      else if (all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- film_geo_H(L = dataframeclean$Dx[row_idx], 
                                W = dataframeclean$Dy[row_idx], 
                                H = film_med, H_min = film_min, H_max = film_max)
        method_used <- "Film Geo H (Dx,Dy,H literature)"
      }
      # Preference #5: L, H inputs (assuming W from L * 0.45)
      else if ("length_um" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0) {
        vol_result <- film_geo_WH(L = dataframeclean$length_um[row_idx], 
                                 H = film_med, H_min = film_min, H_max = film_max)
        method_used <- "Film Geo WH (L,H literature,W proj)"
      }
      # Preference #6: Dx, H inputs (assuming W from Dx * 0.45)
      else if ("Dx" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0) {
        vol_result <- film_geo_WH(L = dataframeclean$Dx[row_idx], 
                                 H = film_med, H_min = film_min, H_max = film_max)
        method_used <- "Film Geo WH (Dx,H literature,W proj)"
      }
      # Preference #7: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina (A)"
      }
    }
    
    # ===== FIBER PREFERENCES =====
    else if (identical(morph, "fiber")) {
      fiber_switch <- fiber_width_switch
      
      # Preference #1: L, W, H AND fiber_width_switch == FALSE
      if (fiber_switch == FALSE &&
          all(c("length_um", "width_um", "height_um") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
          !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
          !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- fiber_geo(L = dataframeclean$length_um[row_idx], 
                               W = dataframeclean$width_um[row_idx], 
                               H = dataframeclean$height_um[row_idx])
        method_used <- "Fiber Geo (L,W,H measured)"
      }
      # Preference #2: Dx, Dy, H AND fiber_width_switch == FALSE
      else if (fiber_switch == FALSE &&
               all(c("Dx", "Dy", "height_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
               !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- fiber_geo(L = dataframeclean$Dx[row_idx], 
                               W = dataframeclean$Dy[row_idx], 
                               H = dataframeclean$height_um[row_idx])
        method_used <- "Fiber Geo (Dx,Dy,H measured)"
      }
      # Preference #3: Dx, Dy AND fiber_width_switch == FALSE
      else if (fiber_switch == FALSE &&
               all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- chen_fiber(Dx = dataframeclean$Dx[row_idx], 
                                Dy = dataframeclean$Dy[row_idx])
        method_used <- "Chen Fiber (Dx,Dy)"
      }
      # Preference #4: L, W AND fiber_width_switch == FALSE
      else if (fiber_switch == FALSE &&
               all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- fiber_geo_H(L = dataframeclean$length_um[row_idx], 
                                W = dataframeclean$width_um[row_idx])
        method_used <- "Fiber Geo H (L,W)"
      }
      # Preference #5: Dx, Dy AND fiber_width_switch == FALSE
      else if (fiber_switch == FALSE &&
               all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- fiber_geo_H(L = dataframeclean$Dx[row_idx], 
                                W = dataframeclean$Dy[row_idx])
        method_used <- "Fiber Geo H (Dx,Dy)"
      }
      # Preference #6: L AND fiber_width_switch == TRUE
      else if (fiber_switch == TRUE &&
               "length_um" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0) {
        vol_result <- fiber_geo_WH(L = dataframeclean$length_um[row_idx], 
                                  W = fiber_med, W_min = fiber_min, W_max = fiber_max)
        method_used <- "Fiber Geo WH (L,W literature)"
      }
      # Preference #7: Dx AND fiber_width_switch == TRUE
      else if (fiber_switch == TRUE &&
               "Dx" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0) {
        vol_result <- fiber_geo_WH(L = dataframeclean$Dx[row_idx], 
                                  W = fiber_med, W_min = fiber_min, W_max = fiber_max)
        method_used <- "Fiber Geo WH (Dx,W literature)"
      }
      # Preference #8: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina (A)"
      }
    }
    
    # ===== SPHERE PREFERENCES =====
    else if (identical(morph, "sphere")) {
      # Preference #1: L, W, H
      if (all(c("length_um", "width_um", "height_um") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
          !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
          !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- sphere_geo(L = dataframeclean$length_um[row_idx], 
                                W = dataframeclean$width_um[row_idx], 
                                H = dataframeclean$height_um[row_idx])
        method_used <- "Sphere Geo (L,W,H)"
      }
      # Preference #2: Dx, Dy, H
      else if (all(c("Dx", "Dy", "height_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
               !is.na(dataframeclean$height_um[row_idx]) && dataframeclean$height_um[row_idx] > 0) {
        vol_result <- sphere_geo(L = dataframeclean$Dx[row_idx], 
                                W = dataframeclean$Dy[row_idx], 
                                H = dataframeclean$height_um[row_idx])
        method_used <- "Sphere Geo (Dx,Dy,H)"
      }
      # Preference #3: L, W
      else if (all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- sphere_geo_H(L = dataframeclean$length_um[row_idx], 
                                  W = dataframeclean$width_um[row_idx])
        method_used <- "Sphere Geo H (L,W)"
      }
      # Preference #4: Dx, Dy
      else if (all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- sphere_geo_H(L = dataframeclean$Dx[row_idx], 
                                  W = dataframeclean$Dy[row_idx])
        method_used <- "Sphere Geo H (Dx,Dy)"
      }
      # Preference #5: L
      else if ("length_um" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0) {
        vol_result <- sphere_geo_WH(L = dataframeclean$length_um[row_idx])
        method_used <- "Sphere Geo WH (L)"
      }
      # Preference #6: Dx
      else if ("Dx" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0) {
        vol_result <- sphere_geo_WH(L = dataframeclean$Dx[row_idx])
        method_used <- "Sphere Geo WH (Dx)"
      }
      # Preference #7: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina (A)"
      }
    }
    
    # ===== GENERIC/UNKNOWN MORPHOLOGY PREFERENCES =====
    else {
      # Preference #1: Dx, Dy, P
      if (all(c("Dx", "Dy", "perimeter_um") %in% colnames(dataframeclean)) &&
          !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
          !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0 &&
          !is.na(dataframeclean$perimeter_um[row_idx]) && dataframeclean$perimeter_um[row_idx] > 0) {
        vol_result <- barchiesi(L = dataframeclean$Dx[row_idx], 
                               W = dataframeclean$Dy[row_idx], 
                               P = dataframeclean$perimeter_um[row_idx])
        method_used <- "Barchiesi Generic (Dx,Dy,P)"
      }
      # Preference #2: L, W, P
      else if (all(c("length_um", "width_um", "perimeter_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0 &&
               !is.na(dataframeclean$perimeter_um[row_idx]) && dataframeclean$perimeter_um[row_idx] > 0) {
        vol_result <- barchiesi(L = dataframeclean$length_um[row_idx], 
                               W = dataframeclean$width_um[row_idx], 
                               P = dataframeclean$perimeter_um[row_idx])
        method_used <- "Barchiesi Generic (L,W,P)"
      }
      # Preference #3: Dx, Dy
      else if (all(c("Dx", "Dy") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$Dx[row_idx]) && dataframeclean$Dx[row_idx] > 0 &&
               !is.na(dataframeclean$Dy[row_idx]) && dataframeclean$Dy[row_idx] > 0) {
        vol_result <- simon(L = dataframeclean$Dx[row_idx], 
                           W = dataframeclean$Dy[row_idx])
        method_used <- "Simon Generic (Dx,Dy)"
      }
      # Preference #4: L, W
      else if (all(c("length_um", "width_um") %in% colnames(dataframeclean)) &&
               !is.na(dataframeclean$length_um[row_idx]) && dataframeclean$length_um[row_idx] > 0 &&
               !is.na(dataframeclean$width_um[row_idx]) && dataframeclean$width_um[row_idx] > 0) {
        vol_result <- simon(L = dataframeclean$length_um[row_idx], 
                           W = dataframeclean$width_um[row_idx])
        method_used <- "Simon Generic (L,W)"
      }
      # Preference #5: A
      else if ("area_um2" %in% colnames(dataframeclean) &&
               !is.na(dataframeclean$area_um2[row_idx]) && dataframeclean$area_um2[row_idx] > 0) {
        vol_result <- medina(A = dataframeclean$area_um2[row_idx])
        method_used <- "Medina Generic (A)"
      }
    }
    
    # Store volume results if one was calculated
    if (!is.null(vol_result)) {
      dataframeclean$volume_um3[row_idx] <- vol_result[1]
      dataframeclean$min_volume_um3[row_idx] <- vol_result[2]
      dataframeclean$max_volume_um3[row_idx] <- vol_result[3]
      dataframeclean$volume_calculation_method[row_idx] <- method_used
    }
  }
  
  # Only keep volume columns if at least one volume was calculated
  has_volume <- any(!is.na(dataframeclean$volume_um3), na.rm = TRUE)
  if (!has_volume) {
    dataframeclean <- dataframeclean %>%
      select(-volume_um3, -min_volume_um3, -max_volume_um3, -volume_calculation_method)
  } else {
    # Remove the internal tracking column (keep volumes though)
    dataframeclean <- dataframeclean %>%
      select(-volume_calculation_method)
  }
  
  
  
  
  # Only calculate mass if volumes were calculated
  if(has_volume){
    if("density" %in% colnames(dataframeclean)){
      for(x in 1:nrow(dataframeclean)){
        if(is.na(dataframeclean[x, "density"]) && is.na(dataframeclean[x, "density_max"])) {
          dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x, "min_volume_um3"])
          dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x,"volume_um3"])
          dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x, "max_volume_um3"])
        }else if(is.na(dataframeclean[x, "density"]) && !is.na(dataframeclean[x, "density_max"])){
          dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_min"]) * as.numeric(dataframeclean[x, "min_volume_um3"])
          dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x,"volume_um3"])
          dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_max"]) * as.numeric(dataframeclean[x, "max_volume_um3"])
        }else{
          dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x, "min_volume_um3"])
          dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x,"volume_um3"])
          dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density"]) * as.numeric(dataframeclean[x, "max_volume_um3"])
        }
      }
    }else{
      for(x in 1:nrow(dataframeclean)){
        if(is.na(dataframeclean[x, "density_max"]) && is.na(dataframeclean[x, "density_min"])){
          dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x, "min_volume_um3"])
          dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x,"volume_um3"])
          dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x, "max_volume_um3"])
        }else{
          dataframeclean[x, "min_mass_mg"] <- as.numeric(dataframeclean[x, "density_min"]) * as.numeric(dataframeclean[x, "min_volume_um3"])
          dataframeclean[x, "mean_mass_mg"] <- as.numeric(dataframeclean[x, "density_mg_um3"]) * as.numeric(dataframeclean[x,"volume_um3"])
          dataframeclean[x, "max_mass_mg"] <- as.numeric(dataframeclean[x, "density_max"]) * as.numeric(dataframeclean[x, "max_volume_um3"])
        }
      }
    }
  }
  
  dataframeclean_particles <- data.frame(dataframeclean)
  
  # Only join trash_mass_clean if it's provided and morphology column exists
  if(!is.null(trash_mass_clean) && "morphology" %in% colnames(dataframeclean_particles)){
    dataframeclean_particles <- dataframeclean_particles %>%
      left_join(trash_mass_clean, by = c("morphology" = "items",
                                         "material" = "material"))
  }
  
  # Only convert weight_estimate_g if the column exists
  if("weight_estimate_g" %in% colnames(dataframeclean_particles)){
    dataframeclean_particles$weight_estimate_g <- as.numeric(dataframeclean_particles$weight_estimate_g)
  }
  
  # Only update mean_mass_mg if it exists (i.e., volumes were calculated)
  if("mean_mass_mg" %in% colnames(dataframeclean_particles) && "weight_estimate_g" %in% colnames(dataframeclean_particles)){
    for(x in 1:nrow(dataframeclean_particles)){
      if(! is.na(dataframeclean_particles[x, "weight_estimate_g"])){
        dataframeclean_particles[x, "mean_mass_mg"] <- (dataframeclean_particles[x, "weight_estimate_g"]*1000)
      }
    }
  }
  
  if(polymer_avg_decision == T && "volume_um3" %in% colnames(dataframeclean_particles)){
    dataframeclean_particles$density_mg_um3 <- as.numeric(dataframeclean_particles$density_mg_um3)
    dataframeclean_particles$volume_um3 <- as.numeric(dataframeclean_particles$volume_um3)
    dataframeclean_particles$mean_mass_mg <- as.numeric(dataframeclean_particles$mean_mass_mg)
    dataframeclean_particles$density_max <- as.numeric(dataframeclean_particles$density_max)
    dataframeclean_particles$density_min <- as.numeric(dataframeclean_particles$density_min)
    dataframeclean_particles_avg <- dataframeclean_particles
    for(x in 1:nrow(dataframeclean_particles)){
      if(is.na(dataframeclean_particles$mean_mass_mg[[x]]) && ! is.na(dataframeclean_particles$volume_um3[[x]])){
        if(morph_weight == F && sample_weight == T && "sample_id" %in% colnames(dataframeclean_particles) == TRUE){
          dataframeclean_particles_avg <- dataframeclean_particles %>% filter(sample_id == dataframeclean_particles$sample_id[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um3, na.rm = T)
          dataframeclean_particles$density_max[[x]] <- mean(dataframeclean_particles_avg$density_max, na.rm = T)
          dataframeclean_particles$density_min[[x]] <- mean(dataframeclean_particles_avg$density_min, na.rm = T)
          dataframeclean_particles$density_mg_um3[[x]] <- density
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*density
          dataframeclean_particles$max_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_max[[x]])
          dataframeclean_particles$min_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_min[[x]])
        }else if(morph_weight == T && sample_weight == F && "morphology" %in% colnames(dataframeclean_particles) || morph_weight == T && sample_weight == T && !("sample_id" %in% colnames(dataframeclean_particles)) && "morphology" %in% colnames(dataframeclean_particles)){
          dataframeclean_particles_avg <- dataframeclean_particles %>% filter(morphology == dataframeclean_particles$morphology[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um3, na.rm = T)
          dataframeclean_particles$density_max[[x]] <- mean(dataframeclean_particles_avg$density_max, na.rm = T)
          dataframeclean_particles$density_min[[x]] <- mean(dataframeclean_particles_avg$density_min, na.rm = T)
          dataframeclean_particles$density_mg_um3[[x]] <- density
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*density
          dataframeclean_particles$max_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_max[[x]])
          dataframeclean_particles$min_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_min[[x]])
        }else if(morph_weight == T && sample_weight == T && "sample_id" %in% colnames(dataframeclean_particles) && "morphology" %in% colnames(dataframeclean_particles)){
          dataframeclean_particles_avg <- dataframeclean_particles %>% filter(morphology == dataframeclean_particles$morphology[[x]])
          dataframeclean_particles_avg <- dataframeclean_particles_avg %>% filter(sample_id == dataframeclean_particles$sample_id[[x]])
          density <- mean(dataframeclean_particles_avg$density_mg_um3, na.rm = T)
          dataframeclean_particles$density_max[[x]] <- mean(dataframeclean_particles_avg$density_max, na.rm = T)
          dataframeclean_particles$density_min[[x]] <- mean(dataframeclean_particles_avg$density_min, na.rm = T)
          dataframeclean_particles$density_mg_um3[[x]] <- density
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*density
          dataframeclean_particles$max_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_max[[x]])
          dataframeclean_particles$min_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_min[[x]])
        }else if(morph_weight == F && sample_weight == F){
          density <- mean(dataframeclean_particles$density_mg_um3, na.rm = T)
          dataframeclean_particles$density_mg_um3[[x]] <- density
          dataframeclean_particles$density_max[[x]] <- mean(dataframeclean_particles_avg$density_max, na.rm = T)
          dataframeclean_particles$density_min[[x]] <- mean(dataframeclean_particles_avg$density_min, na.rm = T)
          dataframeclean_particles$mean_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*density
          dataframeclean_particles$max_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_max[[x]])
          dataframeclean_particles$min_mass_mg[[x]] <- (dataframeclean_particles$volume_um3[[x]])*(dataframeclean_particles$density_min[[x]])
        }
      }
    }
  }
  
  dataframeclean_particles <- dataframeclean_particles %>% 
    select(-any_of(c("weight_estimate_g")))
  
  if("material_raw" %in% colnames(dataframe) && "morphology_raw" %in% colnames(dataframe)){
    dataframeclean_particles <- dataframeclean_particles %>% 
      select(-any_of(c("material", "morphology")))
    
    dataframe <- dataframe %>% select(any_of(c("material", "morphology", "material_raw", "morphology_raw"))) %>%
      distinct()
    
    dataframeclean_particles <- dataframeclean_particles %>% 
      left_join(dataframe, by = c("material_raw", "morphology_raw"))
  }
  
  # Remove internal tracking columns
  dataframeclean_particles <- dataframeclean_particles %>%
    select(-any_of(c("row_id")))
  
  return(dataframeclean_particles)
}


concentration_count_mass <- function(dataframe, morphology_shape, polymer_density, corrected_DF, trash_mass_clean, fiber_min, fiber_med, fiber_max){
  dataframe$particle_concentration <- as.numeric(dataframe$particle_concentration)
  dataframe$sample_id <- as.character(dataframe$sample_id)
  if("morphology" %in% colnames(dataframe) == TRUE){dataframe$morphology <- as.character(dataframe$morphology)}
  if("material" %in% colnames(dataframe) == TRUE){dataframe$material <- as.character(dataframe$material)}
  if("density" %in% colnames(dataframe) == TRUE){dataframe$density <- as.numeric(dataframe$density)}
  if("error_SD" %in% colnames(dataframe) == TRUE){dataframe$error_SD <- as.numeric(dataframe$error_SD)}
  if("error_upper" %in% colnames(dataframe) == TRUE){dataframe$error_upper <- as.numeric(dataframe$error_upper)}
  if("error_lower" %in% colnames(dataframe) == TRUE){dataframe$error_lower <- as.numeric(dataframe$error_lower)}
  dataframeclean <- dataframe
  
  corrected_DF <- corrected_DF %>% select(sample_id, alpha, alpha_upper, alpha_lower)
  corrected_DF$alpha <- as.numeric(corrected_DF$alpha)
  corrected_DF$alpha_upper <- as.numeric(corrected_DF$alpha_upper)
  corrected_DF$alpha_lower <- as.numeric(corrected_DF$alpha_lower)
  dataframeclean <- dataframeclean %>% left_join(corrected_DF, by = "sample_id")
  
  dataframeclean$min_length_um_um <- as.numeric(dataframeclean$min_length_um)
  dataframeclean$max_length_um_um <- as.numeric(dataframeclean$max_length_um)
  
  unique_IDs <- dataframeclean %>% select(sample_id) %>% unique()
  concentration_mass <- data.frame()
  concentration_mass_trash <- data.frame()
  for(x in 1:nrow(unique_IDs)){
    #x = 1
    print(x)
    ID <- unique_IDs[x, "sample_id"]
    data_ID <- dataframeclean %>% filter(sample_id == ID)
    data_ID <- discard(data_ID, ~all(is.na(.)))
    
    if("min_length_um" %in% colnames(data_ID) == TRUE && "max_length_um" %in% colnames(data_ID) == TRUE){
      concentration_ID <- data_ID %>% select(sample_id, particle_concentration, min_length_um, max_length_um, alpha) %>% filter(!is.na(min_length_um)) %>% filter(!is.na(max_length_um)) %>% unique()
    }else{
      concentration_ID <- data_ID %>% select(sample_id, particle_concentration) %>% unique()
    }
    
    if("morphology" %in% colnames(data_ID) == TRUE && "morphology_percent" %in% colnames(data_ID) == TRUE){
      data_ID$morphology_percent <- as.numeric(data_ID$morphology_percent)
      morph_ID <- data_ID %>% select(morphology, morphology_percent) %>% mutate(morphology_percent = morphology_percent/100) %>% unique() %>% filter_all(all_vars(!is.na(.)))
    }else{
      morphology <- c("fiber", "fragment", "sphere", "film", "foam")
      morphology_percent <- c(0.485/0.95, 0.31/0.95, 0.065/0.95, 0.055/0.95, 0.035/0.95)
      morph_ID <- data.frame(morphology, morphology_percent)
    }
    
    if("material" %in% colnames(data_ID) == TRUE && "material_percent" %in% colnames(data_ID) == TRUE){
      data_ID$material_percent <- as.numeric(data_ID$material_percent)
      material_ID <- data_ID %>% select(material, material_percent) %>% mutate(material_percent = material_percent/100) %>% unique() %>% filter_all(all_vars(!is.na(.)))
    }else{
      material <- c("PE", "PP", "PET", "PA", "PS", "PVC", "PVA")
      material_percent <- c(0.25/0.845, 0.145/0.845, 0.165/0.845, 0.12/0.845, 0.085/0.845, 0.02/0.845, 0.06/0.845)
      material_ID <- data.frame(material, material_percent)
    }
    
    # Function to generate power law distributed random numbers
    generate_power_law <- function(n, xmin, xmax, alpha) {
      u <- runif(n) # Generate uniform random numbers
      x <- ((xmax^(1 - alpha) - xmin^(1 - alpha)) * u + xmin^(1 - alpha))^(1 / (1 - alpha))
      return(x)
    }
    
    concentration_ID <- concentration_ID %>% add_column(min_concentration_um3_vol = NA, mean_concentration_um3_vol = NA, max_concentration_um3_vol = NA, min_concentration_mg_vol = NA, mean_concentration_mg_vol = NA, max_concentration_mg_vol = NA)
    #y = 1
    for(y in 1:nrow(concentration_ID)){
      if("min_length_um" %in% colnames(data_ID) == TRUE && "max_length_um" %in% colnames(data_ID) == TRUE){
        # Parameters
        total <- as.numeric(concentration_ID[y, "particle_concentration"])
        ifelse(total <10 || total > 1000, n <- 1000, n <- total)
        xmin <- concentration_ID[y, "min_length_um"]
        xmax <- concentration_ID[y, "max_length_um"]
        alpha <- concentration_ID[y, "alpha"]
        
        # Generate the data
        set.seed(123)
        particle_lengths <- generate_power_law(n, xmin, xmax, alpha)
        
        # Calculate the number of particles for each morphology and material
        morph_ID <- morph_ID %>% mutate(count = round(n * morphology_percent))
        material_ID <- material_ID %>% mutate(count = round(n * material_percent))
        
        # Create a vector of morphologies and materials according to the calculated counts
        morphologies <- rep(morph_ID$morphology, morph_ID$count)
        if(length(morphologies) < n){morphologies <- append(morphologies, sample(morphologies, n-length(morphologies)))}
        if(length(morphologies) > n){length(morphologies) <- n}
        materials <- rep(material_ID$material, material_ID$count)
        if(length(materials) < n){materials <- append(materials, sample(materials, n-length(materials)))}
        if(length(materials) > n){length(materials) <- n}
        # Randomly shuffle the morphologies and materials to mix them up
        set.seed(123) # For reproducibility
        morphologies <- sample(morphologies)
        set.seed(123) # For reproducibility
        materials <- sample(materials)
        
      }else{
        #For trash data, don't perform the inverse power dist, don't mix up material and morphology types
        particle_lengths = NA
        # Calculate the number of particles for each morphology and material
        n <- as.numeric(concentration_ID[y, "particle_concentration"])
        morph_ID <- morph_ID %>% mutate(count = round(n * morphology_percent))
        material_ID <- material_ID %>% mutate(count = round(n * material_percent))
        
        # Create a vector of morphologies and materials according to the calculated counts
        morphologies <- rep(morph_ID$morphology, morph_ID$count)
        materials <- rep(material_ID$material, material_ID$count)
      }
      
      # Combine particle lengths with morphologies
      particle_data <- data.frame(
        length_um = particle_lengths,
        morphology = morphologies,
        material = materials
      )
      
      
      if("density" %in% colnames(data_ID) == T){
        density_ID <- data_ID %>% select(material, density) %>% unique() %>% filter_all(all_vars(!is.na(.)))
        particle_data <- particle_data %>% left_join(density_ID, by = "material")
      }
      
      particle_mass <- particle_count_mass(dataframe = particle_data, morphology_shape = morphology_shape, polymer_density = polymer_density, trash_mass_clean = trash_mass_clean, 
                          polymer_avg_decision = F, morph_weight = F, sample_weight = F, fiber_min = fiber_min, fiber_med = fiber_med, fiber_max = fiber_max)
      particle_mass <- particle_mass %>% filter(!is.na(mean_mass_mg))
      
      concentration_ID[y, "min_concentration_um3_vol"] <- sum(particle_mass$min_volume_um3)
      concentration_ID[y, "mean_concentration_um3_vol"] <- sum(particle_mass$volume_um3)
      concentration_ID[y, "max_concentration_um3_vol"] <- sum(particle_mass$max_volume_um3)
      concentration_ID[y, "min_concentration_mg_vol"] <- sum(particle_mass$min_mass_mg)
      concentration_ID[y, "mean_concentration_mg_vol"] <- sum(particle_mass$mean_mass_mg)
      concentration_ID[y, "max_concentration_mg_vol"] <- sum(particle_mass$max_mass_mg)
      
      if(total <1 || total > 1000){concentration_ID <- concentration_ID %>% mutate(min_concentration_um3_vol = (min_concentration_um3_vol/n)*total,
                                                      mean_concentration_um3_vol = (mean_concentration_um3_vol/n)*total,
                                                      max_concentration_um3_vol = (max_concentration_um3_vol/n)*total,
                                                      min_concentration_mg_vol = (min_concentration_mg_vol/n)*total,
                                                      mean_concentration_mg_vol = (mean_concentration_mg_vol/n)*total,
                                                      max_concentration_mg_vol = (max_concentration_mg_vol/n)*total)}
      
    }
    
    if("min_length_um" %in% colnames(concentration_ID) == FALSE && "max_length_um" %in% colnames(concentration_ID) == FALSE){
      concentration_mass_trash <- rbind(concentration_mass_trash, concentration_ID) %>% discard(~all(is.na(.)))
    }else{
      concentration_mass <- rbind(concentration_mass, concentration_ID)
      
    }
  }
  
  dataframe <- dataframe %>% left_join(concentration_mass, by = c("sample_id", "particle_concentration", "min_length_um", "max_length_um")) %>% select(-alpha)
  if(nrow(concentration_mass_trash) > 0){
    dataframe <- dataframe %>% 
      left_join(concentration_mass_trash, by = c("sample_id", "particle_concentration")) %>% 
      mutate(mean_concentration_mg_vol = coalesce(mean_concentration_mg_vol.y, mean_concentration_mg_vol.x)) %>%
      select(-c(mean_concentration_mg_vol.y, mean_concentration_mg_vol.x))
  }
  
  return(dataframe)
}

correctionFactor_conc <- function(dataframe, alpha_vals, metric, corrected_min, corrected_max){
  dataframe$particle_concentration <- as.numeric(dataframe$particle_concentration)
  dataframe$min_length_um <- as.numeric(dataframe$min_length_um)
  dataframe$max_length_um <- as.numeric(dataframe$max_length_um)
  dataframe$sample_id <- as.character(dataframe$sample_id)
  if("study_media" %in% colnames(dataframe) == TRUE){dataframe$study_media <- as.character(dataframe$study_media)}
  if("known_alpha" %in% colnames(dataframe) == TRUE){dataframe$known_alpha <- as.numeric(dataframe$known_alpha)}
  if("sample_id"  %in% colnames(dataframe) == TRUE){dataframe$sample_id <- as.character(dataframe$sample_id)}
  if("error_SD" %in% colnames(dataframe) == TRUE){dataframe$error_SD <- as.numeric(dataframe$error_SD)}
  if("error_upper" %in% colnames(dataframe) == TRUE){dataframe$error_upper <- as.numeric(dataframe$error_upper)}
  if("error_lower" %in% colnames(dataframe) == TRUE){dataframe$error_lower <- as.numeric(dataframe$error_lower)}
  dataframeclean <- dataframe
  if("study_media" %in% colnames(dataframe) == TRUE){
    dataframeclean <- dataframeclean %>% 
      rename("study_media_clean" = "study_media") 
    study_media <- cleantext(dataframeclean$study_media_clean)
    dataframeclean <- dataframeclean %>% 
      add_column(study_media = study_media)
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
      select(-study_media, -sd) %>%
      rename("study_media" = "study_media_clean")
  }else{
    dataframeclean <- dataframeclean %>% 
      add_column(alpha = NA,
                 alpha_lower = NA,
                 alpha_upper = NA
      )
  }
  
  if("sample_id" %in% colnames(dataframeclean) == TRUE){
    unique_bins <- dataframeclean %>% count(sample_id, min_length_um, max_length_um)
    bin_numbers <- unique_bins %>% count(sample_id)
    alpha_bins <- bin_numbers %>% filter(n >= 5)
    if(nrow(alpha_bins) >= 1){
      unique_alpha <- data.frame()
      for(x in 1:nrow(alpha_bins)){
        sample_name <- alpha_bins$sample_id[[x]]
        sample_bins <- dataframeclean %>% filter(sample_id == sample_name)
        midpoint <- (as.numeric(sample_bins$min_length_um) + as.numeric(sample_bins$max_length_um))/2
        sample_bins <- sample_bins %>%
          add_column(midpoint = midpoint,
                     alpha_calc_lower = NA,
                     alpha_calc_upper = NA) %>%
          select(sample_id, midpoint, particle_concentration, alpha, alpha_calc_lower, alpha_calc_upper)
        sample_bins$log_size <- log10(sample_bins$midpoint)
        sample_bins$particle_concentration <- as.numeric(sample_bins$particle_concentration)
        sample_bins$log_abundance <- log10(sample_bins$particle_concentration)
        r1model <- lm(log_abundance ~ log_size, data = sample_bins)
        alpha <- -(as.numeric(coef(r1model)[2]))
        sample_bins$alpha <- alpha
        r1model_sum <- summary.lm(r1model)
        error <- as.numeric(coef(r1model_sum)[4])
        sample_bins$alpha_calc_lower <- alpha - error
        sample_bins$alpha_calc_upper <- alpha + error
        sample_bins <- sample_bins %>% select(sample_id, alpha, alpha_calc_lower, alpha_calc_upper)
        sample_bins <- unique(sample_bins)
        unique_alpha <- rbind(unique_alpha, sample_bins)
      }
      unique_alpha <- unique_alpha %>% rename(alpha_calc = alpha)
      dataframeclean <- dataframeclean %>% left_join(unique_alpha, by = "sample_id")
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
      dataframeclean[x, "alpha_lower"] <- 1.1 #1.6 - 0.5
      dataframeclean[x, "alpha_upper"] <- 2.1 #1.6 + 0.5
    }
  }
  
  dataframeclean <- dataframeclean %>%
    add_column(concentration_lower = NA,
               concentration_upper = NA)
  
  if("sample_id" %in% colnames(dataframeclean) == TRUE){
    unique_bins <- dataframeclean %>% count(sample_id, min_length_um, max_length_um)
    bin_numbers <- unique_bins %>% count(sample_id)
    bins_add <- bin_numbers %>% filter(n >= 2)
    
    dataframeclean_ <- dataframeclean %>% distinct(sample_id, .keep_all = T)
    
    if(nrow(bins_add) >= 1){
      sample_add <- data.frame()
      for(x in 1:nrow(bins_add)){
        #x = 1
        sample_name <- bins_add$sample_id[[x]]
        sample_bins <- dataframeclean %>% filter(sample_id == sample_name)
        sample_bins$min_length_um <- as.numeric(sample_bins$min_length_um)
        sample_bins$max_length_um <- as.numeric(sample_bins$max_length_um)
        sample_bins$particle_concentration <- as.numeric(sample_bins$particle_concentration)
        row <- which(grepl(sample_name, dataframeclean_$sample_id))
        dataframeclean_$min_length_um[[row]] <- min(sample_bins$min_length_um)
        dataframeclean_$max_length_um[[row]] <- max(sample_bins$max_length_um)
        dataframeclean_$particle_concentration[[row]] <- sum(sample_bins$particle_concentration)
        if("error_SD" %in% colnames(dataframeclean) == TRUE && !(is.na(sample_bins$error_SD[[1]]))){
            sample_bins$error_SD <- as.numeric(sample_bins$error_SD)
            dataframeclean_$particle_concentration <- as.numeric(dataframeclean_$particle_concentration)
            dataframeclean_$concentration_upper[[row]] <- dataframeclean_$particle_concentration[[row]] + (max(sample_bins$error_SD))
            dataframeclean_$concentration_lower[[row]] <- dataframeclean_$particle_concentration[[row]] - (max(sample_bins$error_SD))
        }
        
        if("error_lower" %in% colnames(dataframeclean) == TRUE && "error_upper" %in% colnames(dataframeclean) == TRUE && !(is.na(sample_bins$error_upper[[1]])) && !(is.na(sample_bins$error_lower[[1]]))){
          sample_bins$error_upper <- as.numeric(sample_bins$error_upper)
          sample_bins$error_lower <- as.numeric(sample_bins$error_lower)
          dataframeclean_$concentration_upper[[row]] <- sum(sample_bins$error_upper)
          dataframeclean_$concentration_lower[[row]] <- sum(sample_bins$error_lower)
        }
      }
    }
    
    dataframeclean <- dataframeclean_
    ID <- dataframe %>% select(sample_id)
    ID_clean <- mutate_all(ID, cleantext) 
    unique_ID <- unique(ID)
    unique_ID_clean <- unique(ID_clean)
    ID_raw_clean <- data.table(unique_ID = unique_ID, unique_ID_clean = unique_ID_clean) %>%
      rename(unique_ID = unique_ID.sample_id,
             unique_ID_clean = unique_ID_clean.sample_id)
    
    unique_ID_clean <- as.list(unique_ID_clean$sample_id)
    dataframeclean <- dataframeclean %>% arrange(factor(sample_id, levels = unique_ID_clean))
    dataframeclean$alpha <- as.numeric(dataframeclean$alpha)
  }
  
  for(x in 1:nrow(dataframeclean)){
    if(is.na(dataframeclean$concentration_upper[[x]])){
      dataframeclean$concentration_upper[[x]] <- dataframeclean$particle_concentration[[x]]
    }
    if(is.na(dataframeclean$concentration_lower[[x]])){
      dataframeclean$concentration_lower[[x]] <- dataframeclean$particle_concentration[[x]]
    }
  }
  
  dataframeclean <- dataframeclean %>%
    add_column(correction_factor = NA,
               correction_factor_lower = NA,
               correction_factor_upper = NA,
               corrected_concentration = NA,
               corrected_concentration_lower = NA,
               corrected_concentration_upper = NA)
  
  #Extrapolated parameters
  x1D_set = as.numeric(corrected_min) #lower limit default extrapolated range is 1 um
  x2D_set = as.numeric(corrected_max) #upper limit default extrapolated range is 5 mm
  
  for(x in 1:nrow(dataframeclean)) {
    #mean alpha
    x1M_set = as.numeric(dataframeclean$min_length_um[[x]])
    x2M_set = as.numeric(dataframeclean$max_length_um[[x]])
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
    dataframeclean$corrected_concentration[[x]] <- as.numeric(dataframeclean$correction_factor[[x]]) * as.numeric(dataframeclean$particle_concentration[[x]])
    
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
    select(-c(concentration_lower, concentration_upper))
  
  # Final rounding: alpha values to 3 decimals max (without trailing zeros)
  dataframeclean <- dataframeclean %>% mutate(
    alpha = ifelse(!is.na(alpha), round(alpha, 3), alpha),
    alpha_lower = ifelse(!is.na(alpha_lower), round(alpha_lower, 3), alpha_lower),
    alpha_upper = ifelse(!is.na(alpha_upper), round(alpha_upper, 3), alpha_upper)
  )
  
  return(dataframeclean)
}

correctionFactor_particle <- function(dataframe, corrected_min, corrected_max, binning_type, bin_number){
  
  dataframe$length_um <- as.numeric(dataframe$length_um)
  dataframe$sample_id <- as.character(dataframe$sample_id)
  if("sample_volume" %in% colnames(dataframe) == TRUE){
    dataframe$sample_volume <- as.numeric(dataframe$sample_volume)}
  
  unique_sample_id <- unique(dataframe$sample_id)
  unique_sample_id <- as.data.frame(unique_sample_id) %>%
    rename(sample_id = unique_sample_id)
  unique_alpha <- data.frame()
  
  for(x in 1:nrow(unique_sample_id)){
    subset <- filter(dataframe, sample_id == unique_sample_id$sample_id[[x]])
    subset <- subset %>% filter(!is.na(length_um))
    sample_size <- as.numeric(nrow(subset))
    if(sample_size != 0){
      int <- classify_intervals(subset$length_um, n = bin_number, style = binning_type)
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
      error <- as.numeric(coef(r1model_sum)[4])
      alpha_lower <- alpha - error
      alpha_upper <- alpha + error
      subset_ <- subset %>% add_column(alpha = alpha,
                                       alpha_lower = alpha_lower,
                                       alpha_upper = alpha_upper) %>%
        select(sample_id, alpha, alpha_lower, alpha_upper)
      subset_ <- unique(subset_)
      unique_alpha <- rbind(unique_alpha, subset_)
    }

  }
  unique_alpha <- unique_alpha %>% unique()


  
  
  
  dataframe <- left_join(dataframe, unique_alpha, by = "sample_id")
  
  if("sample_volume" %in% colnames(dataframe) == TRUE){
    dataframe_ <- data.table()
    
    for(x in 1:nrow(unique_sample_id)){
      subset <- filter(dataframe, sample_id == unique_sample_id$sample_id[[x]])
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
    dataframe <- dataframe_
  }
  
  return(dataframe)
}

#create function to extract midpoints from binning outputs
get_midpoints <- function(x){
  lower <- as.numeric(gsub(",.*","",gsub("\\(|\\[|\\)|\\]","", x)))
  upper <- as.numeric(gsub(".*,","",gsub("\\(|\\[|\\)|\\]","", x)))
  return(round(lower+(upper-lower)/2, 2))
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

#Find average coefficient of variability to create upper and lower alpha vals from given values
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
      select(Class, Count)
    
    df_join <- AggregateTrees(DF = df_subset, Alias = Group_Alias, Hierarchy = Group_Hierarchy) %>%
      mutate(from = ifelse(from == "trash", type, from)) %>%
      bind_rows(df_join)
  }
  
  df_join_boot <- df_join %>%
    group_by(from, to) %>%
    summarise(mean_prop = mean(totalsum, na.rm = T)
              )
  
}

sunburstplot <-function(df_join_boot){
  
  values <- paste(df_join_boot$to, 
                  "<br>", 
                  round(df_join_boot$mean_prop, 2) * 100, 
                  "%", 
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


###create function to derive correction factor (CF) from Koelmans et al (equation 2)
CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set default values to convert ranges to (1-5,000 um) #5mm is upper default 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

#create function to locate mean value across bounded inverse power function (mean value theorum for integrals)
mean_val = function(a, #alpha, either solved for or default 1.6
                 x_b, #upper lim provided
                 x_a #lower lim provided
                 ){
  if(a == 1){
    c = ((1/(x_b - x_a))*(log(x_b) - log(x_a)))^(-1)
  }else{
    f_b = ((x_b)^(-a+1))/(-a+1)
    f_a = ((x_a)^(-a+1))/(-a+1)
    f_c = (1/(x_b - x_a))*(f_b - f_a)
    c = (abs(f_c))^(-(1/a)) 
  }
  return(c)}

#Files for tool - TRASH versions (default for backward compatibility)
alias <- read.csv("data/PrimeMaterials_trash.csv")
hierarchy <- read.csv("data/MaterialsHierarchy_trash.csv")
aliasi <- read.csv("data/PrimeItems_trash.csv")
hierarchyi <- read.csv("data/ITEMSHierarchy_trash.csv")
aliasclean <- mutate_all(alias, cleantext)
aliascleani <- mutate_all(aliasi, cleantext)
hierarchyclean <- mutate_all(hierarchy, cleantext)
hierarchycleani <- mutate_all(hierarchyi, cleantext)

#Files for tool - MICROPLASTICS versions
alias_microplastic <- read.csv("data/PrimeMaterials_microplastic.csv")
hierarchy_microplastic <- read.csv("data/MaterialsHierarchy_microplastic.csv")
aliasi_microplastic <- read.csv("data/PrimeItems_microplastic.csv")
hierarchyi_microplastic <- read.csv("data/ITEMSHierarchy_microplastic.csv")
aliasclean_microplastic <- mutate_all(alias_microplastic, cleantext)
aliascleani_microplastic <- mutate_all(aliasi_microplastic, cleantext)
hierarchyclean_microplastic <- mutate_all(hierarchy_microplastic, cleantext)
hierarchycleani_microplastic <- mutate_all(hierarchyi_microplastic, cleantext)

#Shared files
microcolor <- read.csv("data/Microplastics_Color.csv")
trash_mass <- read.csv("data/trash_mass.csv")
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

#Files for display - TRASH versions (default for backward compatibility)
Materials_Alias <- read.csv("data/PrimeMaterials_trash.csv")
Materials_Hierarchy <- read.csv("data/MaterialsHierarchy_trash.csv")
Items_Alias <- read.csv("data/PrimeItems_trash.csv")
Items_Hierarchy <- read.csv("data/ITEMSHierarchy_trash.csv")

#Files for display - MICROPLASTICS versions
Materials_Alias_microplastic <- read.csv("data/PrimeMaterials_microplastic.csv")
Materials_Hierarchy_microplastic <- read.csv("data/MaterialsHierarchy_microplastic.csv")
Items_Alias_microplastic <- read.csv("data/PrimeItems_microplastic.csv")
Items_Hierarchy_microplastic <- read.csv("data/ITEMSHierarchy_microplastic.csv")

#Shared relation files
Material_Item_Relation <- read.csv("data/MaterialItemRelationship.csv")
Brand_Manufacturer_Relation <- read.csv("data/BrandManufacturer.csv")
Brand_Item_Relation <- read.csv("data/BrandItem.csv")
NOAA <- read.csv("data/NOAA.csv")
Micro_Color_Display <-read.csv("data/Microplastics_Color.csv")
polymer_db <- read.csv("data/median_polymer_density.csv")
#polymer_db <- read.csv("data/10_90_Percentile_Polymer_Density.csv")

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

ItemsAlias_sunburst <- read.csv("data/PrimeItems_trash.csv")%>%
  rename(Key = Item) %>%
  select(-readable)
ItemsAlias_sunburst_microplastic <- read.csv("data/PrimeItems_microplastic.csv")%>%
  rename(Key = Item) %>%
  select(-readable)
MaterialsAlias_sunburst <- read.csv("data/PrimeMaterials_trash.csv") %>%
  rename(Key = Material) %>%
  select(-readable)
MaterialsAlias_sunburst_microplastic <- read.csv("data/PrimeMaterials_microplastic.csv") %>%
  rename(Key = Material) %>%
  select(-readable)

#Test data
particle_testData <- read.csv("tests/rescaling_particle.csv", check.names = FALSE)
sample_testData <- read.csv("tests/count_mass_concentration.csv", check.names = FALSE)
trash_particle_testData <- read.csv("tests/trash_particle_mass.csv", check.names = FALSE)

#data tables for count to mass conversion
#Make polymer-density dataframe
#Output correct survey sheet
polymer_db_ <- data.frame(polymer_db)
polymer_db_$material <- cleantext(polymer_db_$material)
polymer_db_$density <- as.numeric(polymer_db_$density)
polymer_db_$lower_conf <- as.numeric(polymer_db_$lower_conf)
polymer_db_$upper_conf <- as.numeric(polymer_db_$upper_conf)
density_mg_um3 <- polymer_db_$density * 1e-9
density_min <- polymer_db_$lower_conf * 1e-9
density_max <- polymer_db_$upper_conf * 1e-9
polymer_db_ <- polymer_db_ %>%
  mutate(density_mg_um3 = density_mg_um3,
         density_max = density_max,
         density_min = density_min)
polymer_db_ <- polymer_db_ %>% 
  left_join(Materials_Alias_microplastic, by = c("material" = "Alias")) %>%
  select(-c("Material"))
polymer_density <- polymer_db_ %>%
  select(material, density_mg_um3, density_max, density_min, readable)

#Average densities for parent polymer classes
polymer_parents <- polymer_db_ %>% left_join(MaterialsHierarchy_sunburst, by = c("material" = "to")) %>%
  rename("parent" = "from") %>%
  drop_na(parent)
polymer_parents <- polymer_parents %>%
  group_by(parent) %>%
  summarize(density_mg_um3 = mean(density_mg_um3, na.rm=TRUE), density_max = mean(density_max, na.rm=TRUE), density_min = mean(density_min, na.rm=TRUE)) %>%
  rename(Material = parent)
polymer_parents <- polymer_parents %>%
  left_join(primeMaterials, by = "Material") %>%
  select(-Alias) %>%
  rename(material = Material)
  
#Join density dbs
polymer_density <- rbind(polymer_density, polymer_parents)
polymer_density <- polymer_density %>% distinct(material, .keep_all = TRUE)

#Add +/- 5% error for measured particle dimensions
morphology <- c("fragment","sphere","film","foam")
meas_min <- 0.95
meas_max <- 1.05
L_min <- c(0.95, 0.95, 0.95, 0.95)
L_max <- c(1.05, 1.05, 1.05, 1.05)

#Min and max values given in Kooi Koelmans
W_min <- c(0.1,0.60,0.1,0.1)
W_max <- c(1,1,1,1)
H_min <- c(0.01,0.36,0.001,0.01)
H_max <- c(1,1,0.1,1)

morphology_base <- data.frame(morphology=morphology,
                               L_min=L_min,
                               L_max=L_max,
                               W_min=W_min,
                               W_max=W_max,
                               H_min=H_min,
                               H_max=H_max
)

#convert morphologies in TT to morphologies with defined dimensions
morphology_unique <- c("fiber", "nurdle", "foam", "sphere", "line", "bead", "pellet", "sheet", "film", "fragment", "rubberyfragment", "fiberbundle", "film+fragment")
morphology <- c("fiber", "pellet", "fragment", "sphere",  "fiber", "pellet", "pellet", "film", "film", "fragment", "fragment", "film", "fragment")
morph_conversion <- data.frame(morphology = morphology,
                               morphology_unique = morphology_unique)
morphology_shape <- morph_conversion %>% left_join(morphology_base, by = "morphology") %>% select(-morphology) %>% rename(morphology = morphology_unique)

#polymer abundance from Burns et al 2018/Bond et al 2018 (ref in Kooi Koelmans 2019 for continuous polymer distribution)
polymer <- c("PE", "PP", "PET", "PA", "PS", "PVC", "PVA")
#Abundance provided does not equal 100, so taken as a proportion
abundance <- c(0.25/0.845, 0.145/0.845, 0.165/0.845, 0.12/0.845, 0.085/0.845, 0.02/0.845, 0.06/0.845)
polymer_abundance <- data.frame(polymer = polymer,
                                abundance = abundance)
polymer_abundance <- polymer_abundance %>% left_join(polymer_density, by = c("polymer" = "readable")) %>% select(-material)
polymer_abundance <- polymer_abundance %>% mutate(density_mg_um3 = density_mg_um3 * abundance,
                                                  density_max = density_max * abundance,
                                                  density_min = density_min * abundance)

polymer_abundance <- polymer_abundance %>% add_row(polymer = "average", 
                                                   density_mg_um3 = sum(polymer_abundance$density_mg_um3), 
                                                   density_max = sum(polymer_abundance$density_max), 
                                                   density_min = sum(polymer_abundance$density_min))
# c(1.148166e-09, 1.523589e-09, 1.037747e-09)



#morphology abundance from Burns et al 2018/Bond et al 2018 (ref in Kooi Koelmans 2019 for continuous Corey Shape Factor distribution)
morphology <- c("fiber", "fragment", "sphere", "film", "foam")
#Abundance provided does not equal 100, so taken as a proportion
abundance <- c(0.485/0.95, 0.31/0.95, 0.065/0.95, 0.055/0.95, 0.035/0.95)
morphology_abundance <- data.frame(morphology = morphology,
                                   abundance = abundance)
morphology_abundance <- morphology_abundance %>% left_join(morphology_shape, by = "morphology")
morphology_abundance <- morphology_abundance %>% mutate(L_min = L_min * abundance, 
                                                        L_max = L_max * abundance,
                                                        W_min = W_min * abundance,
                                                        W_max = W_max * abundance,
                                                        H_min = H_min * abundance,
                                                        H_max = H_max * abundance)
morphology_abundance <- morphology_abundance %>% add_row(morphology = "average", 
                                                         L_min = sum(morphology_abundance$L_min), 
                                                         L_max = sum(morphology_abundance$L_max), 
                                                         W_min = sum(morphology_abundance$W_min),
                                                         W_max = sum(morphology_abundance$W_max),
                                                         H_min = sum(morphology_abundance$H_min),
                                                         H_max = sum(morphology_abundance$H_max))
#c(0.950, 1.05000000, 0.0836684211, 0.74473684, 2.883158e-02, 0.692631579)





#Start server

server <- function(input,output,session) {

  options(shiny.maxRequestSize = 100*1024^2) # 100 MB
  
  ###START MERGING TOOL
  
  #Plot new merged data as sunburst plots
  #Material Sunburst Plot ----
  
  output$plot1 <- renderPlotly({
    req(input$particleData)
    req(convertedTermsSelect())
    dataframe <- convertedTermsSelect()
    if("material" %in% colnames(dataframe)){
      dataframe <- dataframe %>% rename(Class = material)
      if("material_percent" %in% colnames(dataframe)){
        dataframe <- dataframe %>% rename(Count = material_percent) %>%
                      select(Class, Count)
        dataframe <- dataframe %>% group_by(Class) %>%
          summarise_at(c("Count"), sum)

      }else{
        dataframe <- dataframe %>% add_column(Count = 1) %>%
          select(Class, Count)
        dataframe <- dataframe %>% group_by(Class) %>%
          summarise_at(c("Count"), sum)
      }

      Material_DF_group <- dataframe

      material_grouped <- grouped_uncertainty(DF_group = Material_DF_group, Group_Alias = MaterialsAlias_sunburst, Group_Hierarchy = MaterialsHierarchy_sunburst, type = "material")

      #Making readable alias display for sunburst plot
      primeMaterials_SB <- primeMaterials %>%
        add_row(Material = "material", Alias = "material", readable = "material") %>%
        add_row(Material = "trash", Alias = "trash", readable = "trash")
      material_grouped_readable <- left_join(material_grouped, primeMaterials_SB, by = c("from" = "Alias"))
      material_grouped_readable <- material_grouped_readable %>%
        ungroup() %>%
        select(-c("Material", "from"))
      material_grouped_readable <- material_grouped_readable %>%
        rename(from = readable) %>%
        left_join(primeMaterials_SB, by = c("to" = "Alias"))
      material_grouped_readable <- material_grouped_readable %>%
        ungroup() %>%
        select(-c("Material", "to"))
      material_grouped_readable <- material_grouped_readable %>%
        rename(to = readable) %>%
        group_by(from, to)

      Materials_Plot <- sunburstplot(df_join_boot = material_grouped_readable)
      print(Materials_Plot)
    }



  })
  
  
  
  #Item Sunburst Plot ----
  output$plot2 <- renderPlotly({
    
    req(input$particleData)
    req(convertedTermsSelect())
    dataframe <- convertedTermsSelect()
    if("morphology" %in% colnames(dataframe)){
      dataframe <- dataframe %>% rename(Class = morphology)
      if("morphology_percent" %in% colnames(dataframe)){
        dataframe <- dataframe %>% rename(Count = morphology_percent) %>%
          select(Class, Count)
        dataframe <- dataframe %>% group_by(Class) %>%
          summarise_at(c("Count"), sum)
        
      }else{
        dataframe <- dataframe %>% add_column(Count = 1) %>%
          select(Class, Count)
        dataframe <- dataframe %>% group_by(Class) %>%
          summarise_at(c("Count"), sum)
      }
      
      Item_DF_group <- dataframe
      
      #Item prop uncertainty
      item_grouped <- grouped_uncertainty(DF_group = Item_DF_group, Group_Alias = ItemsAlias_sunburst, Group_Hierarchy = ItemsHierarchy_sunburst, type = "items")
      
      #Making readable alias display for sunburst plot
      primeItems_SB <- primeItems %>%
        add_row(Item = "items", Alias = "items", readable = "items") %>%
        add_row(Item = "trash", Alias = "trash", readable = "trash")
      item_grouped_readable <- left_join(item_grouped, primeItems_SB, by = c("from" = "Alias"))
      item_grouped_readable <- item_grouped_readable %>% 
        ungroup() %>%
        select(-c("Item", "from")) 
      item_grouped_readable <- item_grouped_readable %>%
        rename(from = readable) %>%
        left_join(primeItems_SB, by = c("to" = "Alias"))
      item_grouped_readable <- item_grouped_readable %>% 
        ungroup() %>%
        select(-c("Item", "to")) 
      item_grouped_readable <- item_grouped_readable %>%
        rename(to = readable) %>%
        group_by(from, to)
      
      Items_Plot <- sunburstplot(df_join_boot = item_grouped_readable)
      print(Items_Plot)
    }
    
  })
  
  ###END MERGING TOOL
  
  #MaTCH Tool
  convertedTerms <- reactive({
    req(input$particleData)
    infile <- input$particleData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file)
    
    if("polymer" %in% colnames(dataframe) && "polymer_class" %in% colnames(dataframe)){
      dataframe <- dataframe %>% add_column(material = NA)
      dataframe <- polymer_class_rename(dataframe)
      dataframe$material[dataframe$material == 0] <- NA
    }
    
    if("material" %in% colnames(dataframe)){
      # Call merge_terms to process material (and morphology if present)
      # This ensures material_raw, material_match_*, and morphology_raw, morphology_match_* columns are created
      dataframe2 <- merge_terms(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias_microplastic, aliasi = aliasi_microplastic, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable, type = "microplastics")
    }else{dataframe2 <- dataframe}
    
    return(dataframe2)
    
  })
  
  aliasDisplay <- reactive({
    req(input$particleData)
    req(convertedTerms())
    dataframe <- convertedTerms()
    
    has_material <- "material" %in% colnames(dataframe) && "material_match_1" %in% colnames(dataframe)
    has_morphology <- "morphology" %in% colnames(dataframe) && "morphology_match_1" %in% colnames(dataframe)
    
    if (has_material && has_morphology) {
      dataframe2 <- dataframe %>% select(material_raw, material, starts_with("material_match"), morphology_raw, morphology, starts_with("morphology_match"))
    } else if (has_material) {
      dataframe2 <- dataframe %>% select(material_raw, material, starts_with("material_match"))
      # Don't remove match columns even if all NA - they're needed by materialDisplay()
    } else if (has_morphology) {
      dataframe2 <- dataframe %>% select(morphology_raw, morphology, starts_with("morphology_match"))
    } else {
      dataframe2 <- data.frame()
    }
    return(dataframe2)
    
  })
  
  # Reactive expressions for material and morphology dropdowns
  materialDisplay <- reactive({
    req(input$particleData)
    req(aliasDisplay())
    dataframe_mat <- as.data.frame(aliasDisplay())
    
    if("material" %in% colnames(dataframe_mat) && "material_match_1" %in% colnames(dataframe_mat)){
      # Select only unique material_raw values (deduplicate if any duplicates exist)
      dataframe_mat2 <- dataframe_mat %>% 
        select(material_raw, material, starts_with("material_match")) %>% 
        filter(!(is.na(material_match_1))) %>% 
        distinct(material_raw, .keep_all = TRUE) %>%  # Ensure one row per material_raw
        add_column(Prime_Material = NA, input_id = NA)
      
      for (i in 1:nrow(dataframe_mat2)) {
        # Create stable ID based on material_raw hash
        stable_id <- paste0("material_select_", digest::digest(dataframe_mat2[i, 1], algo = "md5"))
        
        # Collect all match aliases (material_match_1 through material_match_15)
        alias_cols <- names(dataframe_mat2) %>% grep("^material_match", ., value = TRUE)
        alias_choices <- as.character(dataframe_mat2[i, alias_cols])
        alias_choices <- alias_choices[!is.na(alias_choices)]
        
        # Join with descriptor to get readable PRIME terms and rank by appearance (ranking preserved from embedding)
        choices_df <- data.frame(Alias = alias_choices, rank = seq_along(alias_choices)) %>%
          left_join(Materials_Alias_microplastic, by = "Alias") %>%
          drop_na(Material) %>%  # Remove aliases not found in microplastic alias list
          group_by(Material) %>%
          slice(1) %>%  # First occurrence of each PRIME term (best ranking)
          ungroup() %>%
          slice(1:5) %>%  # Keep top 5 unique PRIME terms
          arrange(rank)   # Re-order by original ranking
        
        # Create named vector: display readable names, but store Alias values
        choice_vector <- setNames(choices_df$Alias, choices_df$readable)
        
        dataframe_mat2$input_id[i] <- stable_id
        dataframe_mat2$Prime_Material[i] <- as.character(selectInput(
          stable_id, 
          "", 
          choices = choice_vector, 
          selected = choice_vector[1], 
          width = "200px"
        ))
      }
      dataframe_mat2 <- dataframe_mat2 %>% 
        select(material_raw, input_id, Prime_Material) %>%
        rename(alias = Prime_Material)
    }else{
      # Return empty dataframe with expected columns if no matches found
      dataframe_mat2 <- data.frame(material_raw = character(), input_id = character(), alias = character())
    }
    
    return(dataframe_mat2)
    
  })
  
  morphologyDisplay <- reactive({
    req(input$particleData)
    req(aliasDisplay())
    dataframe_morph <- as.data.frame(aliasDisplay())
    
    if("morphology" %in% colnames(dataframe_morph) && "morphology_match_1" %in% colnames(dataframe_morph)){
      # Select only unique morphology_raw values (deduplicate if any duplicates exist)
      dataframe_morph2 <- dataframe_morph %>% 
        select(morphology_raw, morphology, starts_with("morphology_match")) %>% 
        filter(!(is.na(morphology_match_1))) %>% 
        distinct(morphology_raw, .keep_all = TRUE) %>%  # Ensure one row per morphology_raw
        add_column(Prime_Morphology = NA, input_id = NA)
      
      for (i in 1:nrow(dataframe_morph2)) {
        # Create stable ID based on morphology_raw hash
        stable_id <- paste0("morph_select_", digest::digest(dataframe_morph2[i, 1], algo = "md5"))
        
        # Collect all match aliases (morphology_match_1 through morphology_match_15)
        alias_cols <- names(dataframe_morph2) %>% grep("^morphology_match", ., value = TRUE)
        alias_choices <- as.character(dataframe_morph2[i, alias_cols])
        alias_choices <- alias_choices[!is.na(alias_choices)]
        
        # Join with descriptor to get readable PRIME terms and rank by appearance (ranking preserved from embedding)
        choices_df <- data.frame(Alias = alias_choices, rank = seq_along(alias_choices)) %>%
          left_join(Items_Alias_microplastic, by = "Alias") %>%
          drop_na(Item) %>%  # Remove aliases not found in microplastic alias list
          group_by(Item) %>%
          slice(1) %>%  # First occurrence of each PRIME term (best ranking)
          ungroup() %>%
          slice(1:5) %>%  # Keep top 5 unique PRIME terms
          arrange(rank)   # Re-order by original ranking
        
        # Create named vector: display readable names, but store Alias values
        choice_vector <- setNames(choices_df$Alias, choices_df$readable)
        
        dataframe_morph2$input_id[i] <- stable_id
        dataframe_morph2$Prime_Morphology[i] <- as.character(selectInput(
          stable_id, 
          "", 
          choices = choice_vector, 
          selected = choice_vector[1], 
          width = "200px"
        ))
      }
      dataframe_morph2 <- dataframe_morph2 %>% 
        select(morphology_raw, input_id, Prime_Morphology) %>%
        rename(alias = Prime_Morphology)
    }else{
      # Return empty dataframe with expected columns if no morphology data
      dataframe_morph2 <- data.frame(morphology_raw = character(), input_id = character(), alias = character())
    }
    
    return(dataframe_morph2)
    
  })
  
  # TRASH-SPECIFIC DROPDOWN FUNCTIONS
  # Simplified trash material display - just shows available matches
  materialDisplayTrash <- reactive({
    req(input$particleDataTrash)
    req(aliasDisplayTrash())
    dataframe_mat <- as.data.frame(aliasDisplayTrash())
    
    if("material" %in% colnames(dataframe_mat) && "material_match_1" %in% colnames(dataframe_mat)){
      # Select only unique material_raw values (deduplicate if any duplicates exist)
      dataframe_mat2 <- dataframe_mat %>% 
        select(material_raw, material, starts_with("material_match")) %>% 
        filter(!(is.na(material_match_1))) %>% 
        distinct(material_raw, .keep_all = TRUE) %>%  # Ensure one row per material_raw
        add_column(Prime_Material = NA, input_id = NA)
      
      for (i in 1:nrow(dataframe_mat2)) {
        # Create stable ID based on material_raw hash
        stable_id <- paste0("material_select_trash_", digest::digest(dataframe_mat2[i, 1], algo = "md5"))
        
        # Collect all match aliases (material_match_1 through material_match_15)
        alias_cols <- names(dataframe_mat2) %>% grep("^material_match", ., value = TRUE)
        alias_choices <- as.character(dataframe_mat2[i, alias_cols])
        alias_choices <- alias_choices[!is.na(alias_choices)]
        
        # Join with descriptor to get readable PRIME terms using TRASH alias table
        choices_df <- data.frame(Alias = alias_choices, rank = seq_along(alias_choices)) %>%
          left_join(Materials_Alias, by = "Alias") %>%
          drop_na(Material) %>%  # Remove aliases not found in trash alias list
          group_by(Material) %>%
          slice(1) %>%  # First occurrence of each PRIME term (best ranking)
          ungroup() %>%
          slice(1:5) %>%  # Keep top 5 unique PRIME terms
          arrange(rank)   # Re-order by original ranking
        
        # Create named vector: display readable names, but store Alias values
        choice_vector <- setNames(choices_df$Alias, choices_df$readable)
        
        dataframe_mat2$input_id[i] <- stable_id
        dataframe_mat2$Prime_Material[i] <- as.character(selectInput(
          stable_id, 
          "", 
          choices = choice_vector, 
          selected = choice_vector[1], 
          width = "100%"
        ))
      }
      dataframe_mat2 <- dataframe_mat2 %>% 
        select(material_raw, input_id, Prime_Material) %>%
        rename(alias = Prime_Material)
    }else{dataframe_mat2 <- data.frame(NA)}
    
    return(dataframe_mat2)
  })
  
  morphologyDisplayTrash <- reactive({
    req(input$particleDataTrash)
    req(aliasDisplayTrash())
    dataframe_morph <- as.data.frame(aliasDisplayTrash())
    
    if("morphology" %in% colnames(dataframe_morph) && "morphology_match_1" %in% colnames(dataframe_morph)){
      # Select only unique morphology_raw values (deduplicate if any duplicates exist)
      dataframe_morph2 <- dataframe_morph %>% 
        select(morphology_raw, morphology, starts_with("morphology_match")) %>% 
        filter(!(is.na(morphology_match_1))) %>% 
        distinct(morphology_raw, .keep_all = TRUE) %>%  # Ensure one row per morphology_raw
        add_column(Prime_Morphology = NA, input_id = NA)
      
      for (i in 1:nrow(dataframe_morph2)) {
        # Create stable ID based on morphology_raw hash
        stable_id <- paste0("morph_select_trash_", digest::digest(dataframe_morph2[i, 1], algo = "md5"))
        
        # Collect all match aliases (morphology_match_1 through morphology_match_15)
        alias_cols <- names(dataframe_morph2) %>% grep("^morphology_match", ., value = TRUE)
        alias_choices <- as.character(dataframe_morph2[i, alias_cols])
        alias_choices <- alias_choices[!is.na(alias_choices)]
        
        # Join with descriptor to get readable PRIME terms using TRASH alias table
        choices_df <- data.frame(Alias = alias_choices, rank = seq_along(alias_choices)) %>%
          left_join(Items_Alias, by = "Alias") %>%
          drop_na(Item) %>%  # Remove aliases not found in trash alias list
          group_by(Item) %>%
          slice(1) %>%  # First occurrence of each PRIME term (best ranking)
          ungroup() %>%
          slice(1:5) %>%  # Keep top 5 unique PRIME terms
          arrange(rank)   # Re-order by original ranking
        
        # Create named vector: display readable names, but store Alias values
        choice_vector <- setNames(choices_df$Alias, choices_df$readable)
        
        dataframe_morph2$input_id[i] <- stable_id
        dataframe_morph2$Prime_Morphology[i] <- as.character(selectInput(
          stable_id, 
          "", 
          choices = choice_vector, 
          selected = choice_vector[1], 
          width = "100%"
        ))
      }
      dataframe_morph2 <- dataframe_morph2 %>% 
        select(morphology_raw, input_id, Prime_Morphology) %>%
        rename(alias = Prime_Morphology)
    }else{dataframe_morph2 <- data.frame(NA)}
    
    return(dataframe_morph2)
    
  })
  
  
  #Reactive expressions for material and morphology selections
  materialSelect <- reactive({
    req(input$particleData)
    req(materialDisplay())
    data <- materialDisplay()
    
    if("alias" %in% colnames(data) && nrow(data) > 0){
      # Get unique material_raw values and their corresponding stable IDs
      selection_data <- data %>%
        select(material_raw, input_id) %>%
        distinct() %>%
        mutate(selection = sapply(input_id, function(id) {
          val <- input[[id]]
          if (is.null(val)) NA_character_ else as.character(val)
        }, USE.NAMES = FALSE))
      slct <- data %>% 
        left_join(selection_data %>% select(material_raw, selection), by = "material_raw") %>%
        mutate(selection = as.character(selection))
    }else{slct <- data.frame()}
    
    return(slct)
    
  })

  morphologySelect <- reactive({
    req(input$particleData)
    req(morphologyDisplay())
    data <- morphologyDisplay()
    
    if("alias" %in% colnames(data) && nrow(data) > 0){
      # Get unique morphology_raw values and their corresponding stable IDs
      selection_data <- data %>%
        select(morphology_raw, input_id) %>%
        distinct() %>%
        mutate(selection = sapply(input_id, function(id) {
          val <- input[[id]]
          if (is.null(val)) NA_character_ else as.character(val)
        }, USE.NAMES = FALSE))
      slct <- data %>%
        left_join(selection_data %>% select(morphology_raw, selection), by = "morphology_raw") %>%
        mutate(selection = as.character(selection))
    }else{slct <- data.frame()}
    return(slct)
    
    
  })
  
  # TRASH-SPECIFIC ALIAS AND SELECTION FUNCTIONS
  aliasDisplayTrash <- reactive({
    req(input$particleDataTrash)
    req(convertedTermsTrash())
    dataframe <- convertedTermsTrash()
    
    if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "material_match_1" %in% colnames(dataframe) && "morphology_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(material_raw, material, starts_with("material_match"), morphology_raw, morphology, starts_with("morphology_match"))
    }else if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "material_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(material_raw, material, starts_with("material_match"))
      dataframe2 <- dataframe2[,colSums(is.na(dataframe2))<nrow(dataframe2)]
    }else if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "morphology_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(morphology_raw, morphology, starts_with("morphology_match"))
    }else{dataframe2 <- data.frame(NA)}
    return(dataframe2)
    
  })
  
  # Initialize reactive values for trash selections
  trash_material_selections <- reactiveValues()
  trash_morphology_selections <- reactiveValues()
  
  observe({
    req(materialDisplayTrash())
    data <- materialDisplayTrash()
    if("input_id" %in% colnames(data)) {
      for(id in unique(data$input_id)) {
        trash_material_selections[[id]] <- input[[id]]
      }
    }
  })
  
  observe({
    req(morphologyDisplayTrash())
    data <- morphologyDisplayTrash()
    if("input_id" %in% colnames(data)) {
      for(id in unique(data$input_id)) {
        trash_morphology_selections[[id]] <- input[[id]]
      }
    }
  })

  materialSelectTrash <- reactive({
    req(input$particleDataTrash)
    req(materialDisplayTrash())
    data <- materialDisplayTrash()
    
    if("alias" %in% colnames(data) && nrow(data) > 0){
      # Get unique material_raw values and their corresponding stable IDs
      selection_data <- data %>%
        select(material_raw, input_id) %>%
        distinct() %>%
        mutate(selection = sapply(input_id, function(id) {
          val <- input[[id]]
          if (is.null(val)) NA_character_ else as.character(val)
        }, USE.NAMES = FALSE))
      slct <- data %>% 
        left_join(selection_data %>% select(material_raw, selection), by = "material_raw")
    }else{slct <- data.frame()}
    
    return(slct)
  })

  morphologySelectTrash <- reactive({
    req(input$particleDataTrash)
    req(morphologyDisplayTrash())
    data <- morphologyDisplayTrash()
    
    if("alias" %in% colnames(data) && nrow(data) > 0){
      # Get unique morphology_raw values and their corresponding stable IDs
      selection_data <- data %>%
        select(morphology_raw, input_id) %>%
        distinct() %>%
        mutate(selection = sapply(input_id, function(id) {
          val <- input[[id]]
          if (is.null(val)) NA_character_ else as.character(val)
        }, USE.NAMES = FALSE))
      slct <- data %>%
        left_join(selection_data %>% select(morphology_raw, selection), by = "morphology_raw")
    }else{slct <- data.frame()}
    return(slct)
  })

  convertedTermsSelect <- reactive({
    req(input$particleData)
    req(isTruthy(morphologySelect()) || isTruthy(materialSelect()))
    dataframe <- convertedTerms()
    
    if ("material_match_1" %in% colnames(dataframe) && nrow(materialSelect()) > 0) {
      mat_select <- materialSelect()
      if (!is.null(mat_select$selection) && is.character(mat_select$selection)) {
        key <- mat_select %>%
          filter(!is.na(selection)) %>%
          select(material_raw, selection) %>%
          distinct() %>%
          left_join(Materials_Alias_microplastic, by = c("selection" = "Alias")) %>%
          select(material_raw, readable) %>%
          rename(material_select = readable)
        
        dataframe <- dataframe %>%
          select(-starts_with("material_match")) %>%
          left_join(key, by = "material_raw")
        
        dataframe <- dataframe %>%
          mutate(material = ifelse(!is.na(material_select), material_select, material)) %>%
          select(-material_select)
      }
    }
    
    if ("morphology_match_1" %in% colnames(dataframe) && nrow(morphologySelect()) > 0) {
      morph_select <- morphologySelect()
      if (!is.null(morph_select$selection) && is.character(morph_select$selection)) {
        key <- morph_select %>%
          filter(!is.na(selection)) %>%
          select(morphology_raw, selection) %>%
          distinct() %>%
          left_join(Items_Alias_microplastic, by = c("selection" = "Alias")) %>%
          select(morphology_raw, readable) %>%
          rename(morphology_select = readable)
        
        dataframe <- dataframe %>%
          select(-starts_with("morphology_match")) %>%
          left_join(key, by = "morphology_raw")
        
        dataframe <- dataframe %>%
          mutate(morphology = ifelse(!is.na(morphology_select), morphology_select, morphology)) %>%
          select(-morphology_select)
      }
    }
    return(dataframe)
    
  })
  #cleaneddataframe <- dataframe2
  #dataframe <- cleaneddataframe
  convertedParticles <- reactive({
    req(input$particleData)
    req(convertedTermsSelect())
    
    withProgress(message = 'Processing data...', value = 0, {
    dataframe <- convertedTermsSelect()
    
    if(("particle_concentration" %in% colnames(dataframe)) == F){
      dataframe2 <- particle_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density, trash_mass_clean = NULL, 
                                        polymer_avg_decision = input$polymer_avg_decision, morph_weight = input$morph_weight, sample_weight = input$sample_weight,
                                        fiber_min = input$fiber_min, fiber_med = input$fiber_med, fiber_max = input$fiber_max,
                                        film_min = input$film_min, film_med = input$film_med, film_max = input$film_max, fiber_width_switch = input$fiber_auto_width)
      # Handle optional morphology_raw and material_raw columns
      if("morphology_raw" %in% colnames(dataframe2)) {
        dataframe2 <- dataframe2 %>% select(morphology_raw, everything())
      }
      if("material_raw" %in% colnames(dataframe2)) {
        dataframe2 <- dataframe2 %>% select(material_raw, everything())
      }
      if("morphology" %in% colnames(dataframe2)) {
        dataframe2 <- dataframe2 %>% select(morphology, everything())
      }
      if("material" %in% colnames(dataframe2)) {
        dataframe2 <- dataframe2 %>% select(material, everything())
      }
      
      
      if(all(is.na(dataframe2$length_um))){
        # Remove columns that may not exist (use any_of to avoid errors)
        cols_to_remove <- c("length_um", "min_volume_um3", "volume_um3", "max_volume_um3", "min_mass_mg", "max_mass_mg")
        dataframe2 <- dataframe2 %>%
          select(-any_of(cols_to_remove))
      }
      
      incProgress(0.3, detail = "Completed particle mass calculation")
    }else{dataframe2 <- dataframe}
      
    if("particle_concentration" %in% colnames(dataframe) && "min_length_um" %in% colnames(dataframe) && "max_length_um" %in% colnames(dataframe) && "sample_id" %in% colnames(dataframe)){
      dataframe <- dataframe %>% mutate(particle_concentration = replace_na(particle_concentration, 0))
      dataframe3 <- correctionFactor_conc(dataframe = dataframe, alpha_vals = alpha_vals, metric = input$concentration_type, corrected_min = input$corrected_min, corrected_max = input$corrected_max)
      #dataframe3 <- correctionFactor_conc(dataframe = dataframe, alpha_vals = alpha_vals, metric = "length (um)", corrected_min = 1, corrected_max = 100)
      incProgress(0.3, detail = "Completed concentration size rescaling")
      if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
        dataframe4 <- concentration_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density, corrected_DF = dataframe3, trash_mass_clean = trash_mass_clean,
                                               fiber_min = input$fiber_min, fiber_med = input$fiber_med, fiber_max = input$fiber_max)
        dataframe3 <- dataframe3 %>% select(sample_id, alpha, alpha_upper, alpha_lower, correction_factor, correction_factor_upper, correction_factor_lower, corrected_concentration, corrected_concentration_upper, corrected_concentration_lower)
        dataframe3 <- dataframe3 %>% rename(corrected_particle_concentration = corrected_concentration)
        dataframe4 <- dataframe4 %>% left_join(dataframe3, by = "sample_id")
        
        incProgress(0.6, detail = "Completed concentration mass calculation")
      }else{
        dataframe4 <- dataframe3
      }
      
    }else{dataframe4 <- dataframe2} 
    
    if("length_um" %in% colnames(dataframe) && "sample_id" %in% colnames(dataframe)){
      dataframe5 <- correctionFactor_particle(dataframe = dataframe, corrected_min = input$corrected_min, corrected_max = input$corrected_max, binning_type = input$binning_type, bin_number = input$bin_number)
      if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
        # Note: Only keep the volume and mass columns from particle_count_mass output
        # Old dimension columns (W_min, W_max, H_min, H_max, etc.) no longer exist with new volume calculation
        dataframe5 <- cbind(dataframe5, dataframe2)
      
      }
      incProgress(0.6, detail = "Completed particle size rescaling")
    }else{dataframe5 <- dataframe4}
    
    incProgress(1, detail = "Complete")
    
    dataframe5 <- dataframe5 %>%
      select_if(~ !all(is.na(.)))
    
    return(dataframe5)
    })
    
  })
  
  # CONCENTRATION TAB REACTIVE FUNCTIONS
  
  convertedTermsConc <- reactive({
    req(input$particleDataConc)
    infile <- input$particleDataConc
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file)
    
    # Add column renaming logic here (similar to convertedTerms)
    
    if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
      # Microplastic - Concentration tab: use microplastic-specific aliases and full vectorDBs
      # Filtering happens through alias joins in merge_terms function
      dataframe2 <- merge_terms(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias_microplastic, aliasi = aliasi_microplastic, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable, type = "microplastics")
    }else{dataframe2 <- dataframe}
    
    return(dataframe2)
  })
  
  convertedParticlesConc <- reactive({
    req(input$particleDataConc)
    req(convertedTermsConc())
    
    withProgress(message = 'Processing concentration data...', value = 0, {
      dataframe <- convertedTermsConc()
      incProgress(1, detail = "Complete")
      return(dataframe)
    })
  })
  output$contents5Conc <- DT::renderDT(
    convertedParticlesConc(),
    rownames = FALSE,
    options = list(scrollX = TRUE, pageLength = 5)
  )
  
  output$contents8Conc <- DT::renderDT(
    {req(convertedTermsConc())
     req("material" %in% colnames(convertedTermsConc()))
     convertedTermsConc() %>% select(material) %>% distinct()},
    rownames = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$contents9Conc <- DT::renderDT(
    {req(convertedTermsConc())
     req("morphology" %in% colnames(convertedTermsConc()))
     convertedTermsConc() %>% select(morphology) %>% distinct()},
    rownames = FALSE,
    options = list(scrollX = TRUE)
  )
  
  output$plot1Conc <- renderPlotly({
    req(convertedParticlesConc())
    dataframe <- convertedParticlesConc()
    if("material" %in% colnames(dataframe)){
      return(plotly_empty(type = "scatter"))
    }else{
      return(plotly_empty(type = "scatter"))
    }
  })
  
  output$plot2Conc <- renderPlotly({
    req(convertedParticlesConc())
    dataframe <- convertedParticlesConc()
    if("morphology" %in% colnames(dataframe)){
      return(plotly_empty(type = "scatter"))
    }else{
      return(plotly_empty(type = "scatter"))
    }
  })
  
  output$downloadDataConc <- downloadHandler(
    filename = function() {
      paste("concentration_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(convertedParticlesConc(), file, row.names = FALSE)
    }
  )
  
  # TRASH TAB REACTIVE FUNCTIONS
  
  convertedTermsTrash <- reactive({
    req(input$particleDataTrash)
    infile <- input$particleDataTrash
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file)
    
    # Standardize column names (similar to convertedTerms)
    if("Sample ID" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('sample_id' = 'Sample ID')}
    if("Concentration (particles/volume)" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('particle_concentration' = 'Concentration (particles/volume)')}
    if("Material" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('material' = 'Material')}
    if("Material %" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('material_percent' = 'Material %')}
    if("Morphology" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('morphology' = 'Morphology')}
    if("Morphology %" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('morphology_percent' = 'Morphology %')}
    if("Study Media" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('study_media' = 'Study Media')}
    if("Min Length (microns)" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('min_length_um' = 'Min Length (microns)')}
    if("Max Length (microns)" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('max_length_um' = 'Max Length (microns)')}
    if("Known Alpha Value" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('known_alpha' = 'Known Alpha Value')}
    if("Concentration Upper (particles/volume)" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('error_upper' = 'Concentration Upper (particles/volume)')}
    if("Concentration Lower (particles/volume)" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('error_lower' = 'Concentration Lower (particles/volume)')}
    if("Concentration Standard Deviation" %in% colnames(dataframe)){dataframe <- dataframe %>% rename('error_SD' = 'Concentration Standard Deviation')}
    
    if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
      # Trash tab: use trash-specific aliases and full vectorDBs
      # Filtering happens through alias joins in merge_terms function
      dataframe2 <- merge_terms(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias, aliasi = aliasi, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable, type = "trash")
    }else{dataframe2 <- dataframe}
    
    return(dataframe2)
  })
  
  convertedTermsSelectTrash <- reactive({
    req(input$particleDataTrash)
    req(isTruthy(materialSelectTrash()) || isTruthy(morphologySelectTrash()))
    dataframe <- convertedTermsTrash()
    
    # Apply material selections from dropdown
    if ("material_match_1" %in% colnames(dataframe)) {
      mat_select <- materialSelectTrash()
      if(nrow(mat_select) > 0 && "selection" %in% colnames(mat_select)){
        key <- mat_select %>%
          left_join(Materials_Alias, by = c("selection" = "Alias")) %>%
          select(material_raw, readable) %>%
          rename(material_select = readable)
        
        dataframe <- dataframe %>%
          select(-starts_with("material_match")) %>%
          left_join(key, by = "material_raw")
        
        dataframe <- dataframe %>%
          mutate(material = ifelse(!is.na(material_select), material_select, material)) %>%
          select(-material_select)
      } else {
        dataframe <- dataframe %>%
          select(-starts_with("material_match"))
      }
    }
    
    # Apply morphology selections from dropdown
    if ("morphology_match_1" %in% colnames(dataframe)) {
      morph_select <- morphologySelectTrash()
      if(nrow(morph_select) > 0 && "selection" %in% colnames(morph_select)){
        key <- morph_select %>%
          left_join(Items_Alias, by = c("selection" = "Alias")) %>%
          select(morphology_raw, readable) %>%
          rename(morphology_select = readable)
        
        dataframe <- dataframe %>%
          select(-starts_with("morphology_match")) %>%
          left_join(key, by = "morphology_raw")
        
        dataframe <- dataframe %>%
          mutate(morphology = ifelse(!is.na(morphology_select), morphology_select, morphology)) %>%
          select(-morphology_select)
      } else {
        dataframe <- dataframe %>%
          select(-starts_with("morphology_match"))
      }
    }
    
    return(dataframe)
  })
  
  convertedParticlesTrash <- reactive({
    req(input$particleDataTrash)
    req(convertedTermsSelectTrash())
    
    withProgress(message = 'Processing trash data...', value = 0, {
      dataframe <- convertedTermsSelectTrash()
      incProgress(0.5, detail = "Joining with mass data")
      
      # Join with trash mass database based on material AND morphology
      if ("material" %in% colnames(dataframe) && "morphology" %in% colnames(dataframe)) {
        # Use cleaned versions for joining, but preserve readable names for display
        dataframe <- dataframe %>%
          mutate(
            material_clean = cleantext(material),
            morphology_clean = cleantext(morphology)
          ) %>%
          left_join(trash_mass_clean, by = c("material_clean" = "material", "morphology_clean" = "items")) %>%
          select(-material_clean, -morphology_clean)
      }
      
      # Reorder columns for display
      dataframe <- dataframe %>%
        select(material, morphology, material_raw, morphology_raw, everything())
      
      incProgress(1, detail = "Complete")
      return(dataframe)
    })
  })
  
  output$contents5Trash <- DT::renderDT(
    convertedParticlesTrash(),
    rownames = FALSE,
    options = list(
      scrollX = TRUE,
      pageLength = 10,
      columnDefs = list(
        list(targets = '_all', className = 'dt-left')
      )
    )
  )
  
  output$contents8Trash = DT::renderDataTable(
    materialDisplayTrash(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap", rownames = F,
    options = list(
      dom = 'f', 
      paging = FALSE,
      columnDefs = list(
        list(
          targets = 1,
          visible = FALSE
        ),
        list(
          targets = '_all',
          width = '200px'
        )
      ),
      searching = F,
      ordering = FALSE
    ),
    callback = JS("// Destroy existing Bootstrap popovers to prevent stuck overlays
      $('[data-bs-toggle=\"popover\"]').each(function() {
        var popover = bootstrap.Popover.getInstance(this);
        if (popover) popover.dispose();
      });
      
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  output$contents9Trash = DT::renderDataTable(
    morphologyDisplayTrash(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap", rownames = F,
    options = list(
      dom = 'f', 
      paging = FALSE,
      columnDefs = list(
        list(
          targets = 1,
          visible = FALSE
        ),
        list(
          targets = '_all',
          width = '200px'
        )
      ),
      searching = F,
      ordering = FALSE
    ),
    callback = JS("// Destroy existing Bootstrap popovers to prevent stuck overlays
      $('[data-bs-toggle=\"popover\"]').each(function() {
        var popover = bootstrap.Popover.getInstance(this);
        if (popover) popover.dispose();
      });
      
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  output$plot1Trash <- renderPlotly({
    req(convertedParticlesTrash())
    dataframe <- convertedParticlesTrash()
    if("material" %in% colnames(dataframe)){
      return(plotly_empty(type = "scatter"))
    }else{
      return(plotly_empty(type = "scatter"))
    }
  })
  
  output$plot2Trash <- renderPlotly({
    req(convertedParticlesTrash())
    dataframe <- convertedParticlesTrash()
    if("morphology" %in% colnames(dataframe)){
      return(plotly_empty(type = "scatter"))
    }else{
      return(plotly_empty(type = "scatter"))
    }
  })
  
  output$downloadDataTrash <- downloadHandler(
    filename = function() {
      paste("trash_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(convertedParticlesTrash(), file, row.names = FALSE)
    }
  )
  
  output$downloadTrashTestData <- downloadHandler(
    filename = function() {
      paste("trash_particle_test_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trash_particle_testData, file, row.names = FALSE)
    }
  )
  
  #Test data
  
  observeEvent(input$reporting_level, {
    if(input$reporting_level == "Sample (particles/volume)"){
      updateCheckboxGroupInput(inputId = "characteristics",
                               choices = c("Material Proportion" = "material",
                                           "Morphologic Proportion" = "morph",
                                           "Study Media" = "media",
                                           "Min/Max Particle Size Range" = "range"))
      
      updateCheckboxGroupInput(inputId = "advanced", label = "",
                               choices = c("Concentration Size Bins" = "binned",
                                           "Known Alpha Value" = "alpha",
                                           "Known Concentration Error (+/- SD)" = "sd_error",
                                           "Known Concentration Error (lower and upper bounds)" = "error"))
    }
    if(input$reporting_level == "Particle"){
      updateCheckboxGroupInput(inputId = "characteristics",
                               choices = c("Material" = "material_p",
                                           "Morphology" = "morph_p",
                                           "Particle Length (microns)" = "length_p",
                                           "Sample ID" = "sample",
                                           "Projected Area" = "area",
                                           "Perimeter" = "perimeter",
                                           "Circularity" = "circularity",
                                           "Ellipsoid major axis" = "Dx",
                                           "Ellipsoid minor axis" = "Dy"))
      
      updateCheckboxGroupInput(inputId = "advanced", label = "",
                               choices = c("Particle Width (microns)" = "width_p",
                                           "Particle Height (microns)" = "height_p",
                                           "Sample Volume" = "volume",
                                           "Particle Density (mg/microns3)" = "density_p"))
    }
  }
  )
  
  testData <- reactive({
    req(input$reporting_level)
    data = data.frame(matrix(ncol = 0, nrow = 3))
    if(input$reporting_level == "Sample (particles/volume)"){
      particle_concentration = c(100, 100, 100)
      sample_id = c("test", "test", "test")
      data <- data %>% add_column("particle_concentration" = particle_concentration, "sample_id" = sample_id)
      if ("material" %in% as.vector(input$characteristics)){material = c("PE", "LDPE", NA)
        material_percent = c(70, 30, NA)
        data <- add_column(data, "material" = material, "material_percent" = material_percent)}
      if ("morph" %in% as.vector(input$characteristics)){morphology = c("fiber", "fragment", "film")
        morphology_percent = c(70, 20, 10)
        data <- add_column(data, "morphology" = morphology, "morphology_percent" = morphology_percent)}
      if ("media" %in% as.vector(input$characteristics)){study_media = c("marine surface", "marine surface", "marine surface")
        data <- add_column(data, "study_media" = study_media)}
      if ("range" %in% as.vector(input$characteristics) || "binned" %in% as.vector(input$advanced)){min_length_um = c(50, 201, 1001)
        max_length_um = c(200, 1000, 5000)
        data <- add_column(data, "min_length_um" = min_length_um, "max_length_um" = max_length_um)}
      if ("alpha" %in% as.vector(input$advanced)){known_alpha = c(1.80, 1.80, 1.80)
        data <- add_column(data, "known_alpha" = known_alpha)}
      if ("sd_error" %in% as.vector(input$advanced)){error_SD = c(18, 18, 18)
      data <- add_column(data, "error_SD" = error_SD)}
      if ("error" %in% as.vector(input$advanced)){error_upper = c(110, 110, 110)
      error_lower = c(90, 90, 90)
      data <- add_column(data, "error_upper" = error_upper,
                         "error_lower" = error_lower)}
    }
    
    if(input$reporting_level == "Particle"){
      if ("material_p" %in% as.vector(input$characteristics)){material = c("PE", "LDPE", "PET")
      data <- add_column(data, "material" = material)}
      if ("morph_p" %in% as.vector(input$characteristics)){morphology = c("fiber", "fragment", "film")
      data <- add_column(data, "morphology" = morphology)}
      if ("length_p" %in% as.vector(input$characteristics)){length_um = c(120, 70, 80)
      data <- add_column(data, "length_um" = length_um)}
      if ("sample" %in% as.vector(input$characteristics)){sample_id = c("test", "test", "test")
      data <- add_column(data, "sample_id" = sample_id)}
      if ("area" %in% as.vector(input$characteristics)){area_um2 = c(120, 70, 80)
      data <- add_column(data, "area_um2" = area_um2)}
      if ("perimeter" %in% as.vector(input$characteristics)){perimeter_um = c(120, 70, 80)
      data <- add_column(data, "perimeter_um" = perimeter_um)}
      if ("circularity" %in% as.vector(input$characteristics)){circularity = c(0.8, 0.9, 0.7)
      data <- add_column(data, "circularity" = circularity)}
      if ("Dx" %in% as.vector(input$characteristics)){Dx = c(120, 70, 80)
      data <- add_column(data, "Dx" = Dx)}
      if ("Dy" %in% as.vector(input$characteristics)){Dy = c(60, 35, 40)
      data <- add_column(data, "Dy" = Dy)}
      if ("width_p" %in% as.vector(input$advanced)){width_um = c(NA, 30, NA)
      data <- add_column(data, "width_um" = width_um)}
      if ("height_p" %in% as.vector(input$advanced)){height_um = c(20, NA, NA)
      data <- add_column(data, "height_um" = height_um)}
      if ("volume" %in% as.vector(input$advanced)){sample_volume = c(80, 80, 80)
      data <- add_column(data, "sample_volume" = sample_volume)}
      if ("density_p" %in% as.vector(input$advanced)){density = c(NA, 0.00000000098, NA)
      data <- add_column(data, "density" = density)}
    }
    
    return(data)
  })
  
  observeEvent(input$characteristics, {
    req(input$characteristics)
    functions_perf <- c()
    if("material" %in% as.vector(input$characteristics) && "morph" %in% as.vector(input$characteristics) || "material_p" %in% as.vector(input$characteristics) && "morph_p" %in% as.vector(input$characteristics)){
      output$function1 <- renderText({paste("-Semantic matching of material and morphology")})
    }else if(!("material" %in% as.vector(input$characteristics)) || !("morph" %in% as.vector(input$characteristics))){
        output$function1 <- renderText("")
    }else if(!("material_p" %in% as.vector(input$characteristics)) || !("morph_p" %in% as.vector(input$characteristics))){
        output$function1 <- renderText("")
    }

    if("material_p" %in% as.vector(input$characteristics) && "morph_p" %in% as.vector(input$characteristics) && "length_p" %in% as.vector(input$characteristics) ||
       "range" %in% as.vector(input$characteristics)){
      output$function2 <- renderText({paste("-Count to mass conversion")})
    }else if(!("sample" %in% as.vector(input$characteristics)) || !("range" %in% as.vector(input$characteristics))){
      output$function2 <- renderText("")
    }else if(!("material_p" %in% as.vector(input$characteristics)) || !("morph_p" %in% as.vector(input$characteristics)) || !("length_p" %in% as.vector(input$characteristics))){
      output$function2 <- renderText("")
    }

    if("range" %in% as.vector(input$characteristics) || "length_p" %in% as.vector(input$characteristics) && "sample" %in% as.vector(input$characteristics) && "volume" %in% as.vector(input$advanced)){
      output$function3 <- renderText({paste("-Perform particle size rescaling")})
    }else if("length_p" %in% as.vector(input$characteristics) && "sample" %in% as.vector(input$characteristics)  && !("volume" %in% as.vector(input$advanced))){
      output$function3 <- renderText({paste("-Calculate correction factor for size rescaling")})
    }else if(!("range" %in% as.vector(input$characteristics)) && input$reporting_level == "Sample (particles/volume)"){
      output$function3 <- renderText("")
    }else if(!("sample" %in% as.vector(input$characteristics)) || !("length_p" %in% as.vector(input$characteristics))){
      output$function3 <- renderText("")
    }


  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  
  
  
  
  #Output tables
  
  output$contents5 <- DT::renderDT(
    convertedParticles(),
    rownames = FALSE,
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'lfrtip',
      scrollX = TRUE,
      check.names = FALSE,
      columnDefs = list(
        list(targets = '_all', className = 'dt-left')
      )
    ),
    selection = 'none'
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cleaned_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(convertedParticles(), file, row.names = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      if(input$download_selection == "Particle Test Data") {
        paste("particle_test_data-", Sys.Date(), ".csv", sep = "")
      } else {
        paste("sample_test_data-", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      if(input$download_selection == "Particle Test Data") {
        write.csv(particle_testData, file, row.names = FALSE)
      } else {
        write.csv(sample_testData, file, row.names = FALSE)
      }
    }
  )
  
  output$downloadParticleTestData <- downloadHandler(
    filename = function() {
      paste("particle_test_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(particle_testData, file, row.names = FALSE)
    }
  )
  
  output$downloadConcentrationTestData <- downloadHandler(
    filename = function() {
      paste("sample_test_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sample_testData, file, row.names = FALSE)
    }
  )
  
  output$contents8 = DT::renderDataTable(
    materialDisplay(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap", rownames = F,
    options = list(dom = 'f', 
                   paging = FALSE, 
                   columnDefs = list(
                     list(
                       targets = 1,  # Hide input_id column (index 1)
                       visible = FALSE
                     ),
                     list(
                       targets = '_all',  
                       width = '200px'  
                        )
                    ), 
                  searching = F, 
                  ordering = FALSE),
    callback = JS("// Destroy existing Bootstrap popovers to prevent stuck overlays
      $('[data-bs-toggle=\"popover\"]').each(function() {
        var popover = bootstrap.Popover.getInstance(this);
        if (popover) popover.dispose();
      });
      
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )

  output$contents9 = DT::renderDataTable(
    morphologyDisplay(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap", rownames = F,
    options = list(dom = 'f', 
                   paging = FALSE, 
                   columnDefs = list(
                     list(
                       targets = 1,  # Hide input_id column (index 1)
                       visible = FALSE
                     ),
                     list(
                       targets = '_all',  
                       width = '200px'  
                     )
                   ),
                   searching = F, 
                   ordering = FALSE),
    callback = JS("// Destroy existing Bootstrap popovers to prevent stuck overlays
      $('[data-bs-toggle=\"popover\"]').each(function() {
        var popover = bootstrap.Popover.getInstance(this);
        if (popover) popover.dispose();
      });
      
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  

  
  output$testDataDownload <- DT:: renderDataTable(server = F,
                                      datatable({
                                        testData()
                                      }, 
                                      options = list(
                                        paging = F,
                                        searching = F,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = T,
                                        dom = 'Bfrtip',
                                        check.names = FALSE
                                      ),
                                      class = "display",
                                      style="bootstrap",
                                      rownames = F,))
  
  
  output$downloadTemplate <- downloadHandler(    
    filename = function() {
      paste("data_template-", Sys.Date(), ".csv", sep='')
    },
    content = function(file) {
      write.csv(testData(), file, row.names = FALSE)
    }
  )
  
  
}





#Start server

server <- function(input,output,session) {

###Find more and less specific items and materials
  df <- reactive({
    req(input$df)
    infile <- input$df
    df <- fread(infile$datapath)
    dataframe<- as.data.frame(df)%>%
      select(material, items)
    dataframe$material <- as.character(dataframe$material)
    dataframe$items <- as.character(dataframe$items)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
  #Material query tool cleaning
    for(row in 1:nrow(dataframeclean)) {
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "PrimeMaterial"] <- NA
        dataframe[row, "MoreSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      #if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
      #  dataframe[row, "MoreSpecificMaterial"] <- "Unclassifiable"
      #  dataframe[row, "PrimeMaterial"] <- NA
      #  #next #Corrects for cases when unclassifiable
      #}
      
      #Identify Alias Row and Alias name in database
      Primename <- unique(aliasclean[unname(unlist(apply(aliasclean, 2, function(x) which(x == dataframeclean[row,"material"], arr.ind = T)))), "Material"])
      
      if(length(Primename) == 0 ){
          #Create new embedding
        new_material <- data.table(text = dataframeclean[row,"material"])
        new_material_vDB <- add_collection(metadata = new_material)
        similarity <- query_collection(db = materials_vectorDB, query_embeddings = new_material_vDB, top_n = 15, type = "dotproduct") %>%
          left_join(materials_vectorDB$metadata, by = c("db_id" = "id")) %>%
          rename(Alias = text)
        similarity <-  left_join(similarity, primeMaterials, by = "Alias", relationship = "many-to-many")
        top_five <- head(unique(similarity$Material), n=5)
          match1 <- top_five[[1]]
          match2 <- top_five[[2]]
          match3 <- top_five[[3]]
          match4 <- top_five[[4]]
          match5 <- top_five[[5]]
          
          dataframe[row, "PrimeMaterial"] <- as.character(selectInput(paste("sel", row, sep = ""), "", choices = c(match1, match2, match3, match4, match5), width = "100px"))
      }
      
      else{
        dataframe[row, "PrimeMaterial"] <- Primename
      }
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no val. 
      }
      
      
      if(hierarchycolumnnum == ncol(hierarchyclean)) next #Corrects for cases when the value is already the most specific material.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #Less Specific Materials
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"material"]) | dataframeclean[row,"material"] == "") {
        dataframe[row, "LessSpecificMaterial"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "LessSpecificMaterial"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchyclean, 1, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T)))))))
      
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificMaterial"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchyclean, 2, function(x) which(x == dataframe[row, "PrimeMaterial"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchy[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificMaterial"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #row = 1
    
    #find all more specific items
    for(row in 1:nrow(dataframeclean)) { 
      
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "PrimeItem"] <- NA
        dataframe[row, "MoreSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      #if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
      #  dataframe[row, "MoreSpecificItem"] <- "Unclassifiable"
      #  dataframe[row, "PrimeItem"] <- NA
      #  next #Corrects for cases when unclassifiable
      #}
      
      #Identify Alias Row and Alias name
      Primename <- unique(aliascleani[unname(unlist(apply(aliascleani, 2, function(x) which(x == dataframeclean[row,"items"], arr.ind = T)))), "Item"])
      
      if(length(Primename) == 0){
        #Create new embedding
        new_item <- data.table(text = dataframeclean[row,"items"])
        new_item_vDB <- add_collection(metadata = new_item)
        similarity <- query_collection(db = items_vectorDB, query_embeddings = new_item_vDB, top_n = 15, type = "dotproduct") %>%
          left_join(items_vectorDB$metadata, by = c("db_id" = "id")) %>%
          rename(Alias = text)
        similarity <-  left_join(similarity, primeItems, by = "Alias", relationship = "many-to-many")
        top_five <- head(unique(similarity$Item), n=5)
        match1 <- top_five[[1]]
        match2 <- top_five[[2]]
        match3 <- top_five[[3]]
        match4 <- top_five[[4]]
        match5 <- top_five[[5]]
       
        dataframe[row, "PrimeItem"] <- as.character(selectInput(paste("sel", row, sep = ""), "", choices = c(match1, match2, match3, match4, match5), width = "100px"))
        
      }
      
      else{
        dataframe[row, "PrimeItem"] <- Primename
      }
      
      
      
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "MoreSpecificItem"] <- "NO VAL IN DATABASE"
        
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == ncol(hierarchycleani)) next #Corrects for cases when the value is already the most specific.
      
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, (hierarchycolumnnum + 1):ncol(hierarchyclean)]))))
      
      #Push values into datatable
      dataframe[row, "MoreSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    #find all less specific items
    for(row in 1:nrow(dataframe)) { 
      if(is.na(dataframeclean[row,"items"]) | dataframeclean[row,"items"] == "") {
        dataframe[row, "LessSpecificItem"] <- NA
        next #Corrects for cases when there is no val. 
      }
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "LessSpecificItem"] <- "Unclassifiable"
        next #Corrects for cases when unclassifiable
      }
      
      #Identify Alias Row and Alias name
      #Identify Column Name in Hierarchy
      hierarchycolumnnum <- as.numeric(gsub("[[:alpha:]]", "", unique(names(unlist(apply(hierarchycleani, 1, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T)))))))
      if(length(hierarchycolumnnum) == 0) {
        dataframe[row, "LessSpecificItem"] <- "NO VAL IN DATABASE"
        next #Corrects for cases when there is no value.
      }
      
      if(hierarchycolumnnum == 1) next #Corrects for cases when the value is already the least specific.
      hierarchyrows <- unname(unlist(apply(hierarchycleani, 2, function(x) which(x == dataframe[row, "PrimeItem"], arr.ind = T))))
      
      #Get higher values
      vals <- tolower(unique(unname(unlist(hierarchyi[hierarchyrows, 1:(hierarchycolumnnum - 1)]))))
      
      #Push values into datatable
      dataframe[row, "LessSpecificItem"] <- paste(vals[!is.na(vals)], collapse = " | ")
      
    }
    
    return(dataframe)
  })
  
  columnnames <- reactive({
    return(input$variable)
  })
  
  ###START MERGING TOOL

  # df_ <- reactive({
  #   req(input$df_)
  #   
  #   merge_data(file_paths = input$df_$datapath, 
  #              materials_vectorDB = materials_vectorDB, 
  #              items_vectorDB = items_vectorDB,
  #              alias = alias, 
  #              aliasi = aliasi, 
  #              use_cases = use_cases, 
  #              prime_unclassifiable = prime_unclassifiable)
  # })
  
  #Share data ----
  observeEvent(input$df, {
    req(input$share_decision0)
    put_object(
      file = file.path(as.character(input$df$datapath)),
      object = paste0("df_", digest(input$df$datapath), "_", gsub(".*/", "", as.character(input$df$name))),
      bucket = "trashtaxonomy"
    )
  })

  observeEvent(input$particleData, {
    req(input$share_decision1)
    put_object(
      file = file.path(as.character(input$particleData$datapath)),
      object = paste0("particleData_", digest(input$particleData$datapath), "_", gsub(".*/", "", as.character(input$particleData$name))),
      bucket = "trashtaxonomy"
    )
  })
  
  observeEvent(input$concentrationData, {
    req(input$share_decision2)
    put_object(
      file = file.path(as.character(input$concentrationData$datapath)),
      object = paste0("concentrationData_", digest(input$concentrationData$datapath), "_", gsub(".*/", "", as.character(input$concentrationData$name))),
      bucket = "trashtaxonomy"
    )
  })

  
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
  
  selectSurvey <- reactive({
    data = data.frame()
    if(input$sizeRange == "Micro"){data = MicroOnly
    data = as.data.frame(data)
    survey_columns <- c("material","items","color","size")
    colnames(data) = c("material","items","color","size")
    return(data)}
    if(input$specificity == "More Specific"){data = AllMore
    data = as.data.frame(data)
    survey_columns <- c("use", "material","items","count")
    colnames(data) = c("use", "material","items","count")}
    if(input$specificity == "Less Specific"){data = AllLess
    data = as.data.frame(data)
    survey_columns <- c("use", "material","items","count")
    colnames(data) = c("use", "material","items","count")}
    
    if(input$media == "Surface Water"){
      req(input$specificity)
      data <- data %>% filter(!use == "large",
                              !use == "vehicledebris")
      if(input$specificity == "More Specific"){
        data <- data %>% filter(!items == "bricks, cinderblocks, chunks of cement",
                                !items == "piping",
                                !items == "traffic cones",
                                !items == "appliances",
                                !items == "anchor")
      }
    }
    
    if(input$sizeRange == "Macro"){
      req(input$specificity)
      data <- data %>% filter(!use == "microplastics")
    }
    
    if(input$environments == "Marine/Estuarine"){
      req(input$specificity)
      data <- data %>% filter(!use == "gardening&farmingrelated",
                              !use == "officesupplies",
                              !use == "safetyrelated",
                              !use == "constructionmaterials")
    }
    
    if(input$environments == "Riverine"){
      req(input$specificity)
      data <- data %>% filter(!use == "ocean/waterwayactivities",
                              !use == "scuba&snorkelgear,masks,snorkels,fins")
    }
    
    if(input$environments == "Terrestrial"){
      req(input$specificity)
      data <- data %>% filter(!use == "fishinggear",
                              !use == "scuba&snorkelgear,masks,snorkels,fins",
                              !use == "shorelineandrecreationalactivites",
                              !use == "ocean/waterwayactivities")
    }
    
    

    
    return(data)
  })
  
  #MaTCH Tool
  
  #dataframe_ex <- dataframe
  
  convertedTerms <- reactive({
    req(input$particleData)
    infile <- input$particleData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file)
    
    if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
      dataframe2 <- merge_terms(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias, aliasi = aliasi, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable)
    }else{dataframe2 <- dataframe}
    
    return(dataframe2)
    
  })
  
  aliasDisplay <- reactive({
    req(input$particleData)
    #dataframe <- dataframe2
    dataframe <- convertedTerms()
    
    if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "material_match_1" %in% colnames(dataframe) && "morphology_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(material_raw, material, material_match_1, material_match_2, material_match_3, material_match_4, material_match_5, morphology_raw, morphology, morphology_match_1, morphology_match_2, morphology_match_3, morphology_match_4, morphology_match_5)
      dataframe2 <- dataframe2[,colSums(is.na(dataframe2))<nrow(dataframe2)]
    }else if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "material_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(material_raw, material, material_match_1, material_match_2, material_match_3, material_match_4, material_match_5)
      dataframe2 <- dataframe2[,colSums(is.na(dataframe2))<nrow(dataframe2)]
    }else if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "morphology_match_1" %in% colnames(dataframe)){
      dataframe2 <- dataframe %>% select(morphology_raw, morphology, morphology_match_1, morphology_match_2, morphology_match_3, morphology_match_4, morphology_match_5)
      dataframe2 <- dataframe2[,colSums(is.na(dataframe2))<nrow(dataframe2)]
    }else{dataframe2 <- data.frame(NA)}
    
    return(dataframe2)
    
  })
  
  materialDisplay <- reactive({
    req(input$particleData)
    req(aliasDisplay())
    dataframe_mat <- as.data.frame(aliasDisplay())
    if("morphology" %in% colnames(dataframe_mat) && "material" %in% colnames(dataframe_mat) && "material_match_1" %in% colnames(dataframe_mat)){
      dataframe_mat2 <- dataframe_mat %>% select(material_raw, material, material_match_1, material_match_2, material_match_3, material_match_4, material_match_5)
      dataframe_mat2 <- dataframe_mat2 %>% filter(!(is.na(material_match_1))) %>% add_column(Prime_Material = NA)
      for (i in 1:nrow(dataframe_mat2)) {
        dataframe_mat2$Prime_Material[i] <- as.character(selectInput(paste0("sel", i), "", choices = c(dataframe_mat2[i, 3], dataframe_mat2[i, 4], dataframe_mat2[i, 5], dataframe_mat2[i, 6], dataframe_mat2[i, 7]), selected = dataframe_mat2[i, 3], width = "100px"))
        print(dataframe_mat2$Prime_Material[i])
        }
      print(paste0("sel", i))
      dataframe_mat2 <- dataframe_mat2 %>% select(-c(material, material_match_1, material_match_2, material_match_3, material_match_4, material_match_5)) %>%
        rename(alias = Prime_Material)
    }else{dataframe_mat2 <- data.frame(NA)}
    return(dataframe_mat2)
    
  })
  
  materialSelect <- reactive({
    req(input$particleData)
    req(materialDisplay())
    data <- materialDisplay()
    
    if("alias" %in% colnames(data)){
      selection <- as.character(sapply(1:nrow(data), function(i) input[[paste0("sel", i)]]))
      slct <- data %>% add_column(selection = selection)
    }else{slct <- data.frame(NA)}
    
    return(slct)
    
  })
  
  morphologyDisplay <- reactive({
    req(input$particleData)
    req(aliasDisplay())
    dataframe_morph <- as.data.frame(aliasDisplay())
    
    if("morphology" %in% colnames(dataframe_morph) && "material" %in% colnames(dataframe_morph) && "morphology_match_1" %in% colnames(dataframe_morph)){
      dataframe_morph2 <- dataframe_morph %>% select(morphology_raw, morphology, morphology_match_1, morphology_match_2, morphology_match_3, morphology_match_4, morphology_match_5)
      dataframe_morph2 <- dataframe_morph2 %>% filter(!(is.na(morphology_match_1)))
      for (i in 1:nrow(dataframe_morph2)) {
        dataframe_morph2$Prime_Morphology[i] <- as.character(selectInput(paste0("sel2", i), "", choices = c(dataframe_morph[i, 10], dataframe_morph[i, 11], dataframe_morph[i, 12], dataframe_morph[i, 13], dataframe_morph[i, 14]), selected = dataframe_morph[i, 10], width = "100px"))
        print(dataframe_morph2$Prime_Morphology[i])
        }
      dataframe_morph2 <- dataframe_morph2 %>% select(-c(morphology, morphology_match_1, morphology_match_2, morphology_match_3, morphology_match_4, morphology_match_5)) %>%
        rename(alias = Prime_Morphology)
    }else{dataframe_morph2 <- data.frame(NA)}
    return(dataframe_morph2)
    
  })
  
  morphologySelect <- reactive({
    req(input$particleData)
    req(morphologyDisplay())
    data <- morphologyDisplay()
    
    if("alias" %in% colnames(data)){
      selection <- as.character(sapply(1:nrow(data), function(i) input[[paste0("sel2", i)]]))
      slct <- data %>% add_column(selection = selection)
    }else{slct <- data.frame(NA)}
    
    return(slct)
    
  })
  convertedTermsSelect <- reactive({
    req(input$particleData)
    req(isTruthy(morphologySelect()) || isTruthy(materialSelect()))
    req(input$bin_number)
    req(input$binning_type)
    dataframe <- convertedTerms()
    
    if("material_match_1" %in% colnames(dataframe)){
      key <- as.data.frame(materialSelect())
      key <- key %>% left_join(Materials_Alias, by = c("selection" = "Alias"))
      key <- key %>% select(material_raw, readable) %>% rename(material_select = readable)
      dataframe <- dataframe %>% select(-c(material_match_1, material_match_2, material_match_3, material_match_4, material_match_5)) %>%
        left_join(key, by = "material_raw")
      for(x in 1:nrow(dataframe)){
        if(!(is.na(dataframe$material_select[[x]]))){
          dataframe$material[[x]] <- dataframe$material_select[[x]]
        }
      }
      dataframe <- dataframe %>% select(-material_select)
    }else{dataframe <- dataframe}
    
    if("morphology_match_1" %in% colnames(dataframe)){
      key <- as.data.frame(morphologySelect())
      key <- key %>% left_join(Items_Alias, by = c("selection" = "Alias"))
      key <- key %>% select(morphology_raw, readable) %>% rename(morphology_select = readable)
      dataframe <- dataframe %>% select(-c(morphology_match_1, morphology_match_2, morphology_match_3, morphology_match_4, morphology_match_5)) %>%
        left_join(key, by = "morphology_raw")
      for(x in 1:nrow(dataframe)){
        if(!(is.na(dataframe$morphology_select[[x]]))){
          dataframe$morphology[[x]] <- dataframe$morphology_select[[x]]
        }
      }
      dataframe <- dataframe %>% select(-morphology_select)
    }else{dataframe <- dataframe}
    
    print(dataframe)
    
  })
  
  
  convertedParticles <- reactive({
    req(input$particleData)
    req(convertedTermsSelect())
    dataframe <- convertedTermsSelect()
    
    #dataframe <- dataframe2
    if("morphology" %in% colnames(dataframe) && "length_um" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
      dataframe2 <- particle_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density, trash_mass_clean = trash_mass_clean, 
                                        polymer_avg_decision = input$polymer_avg_decision, morph_weight = input$morph_weight, sample_weight = input$sample_weight)
      # dataframe2 <- particle_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density, trash_mass_clean = trash_mass_clean,
      #                                   polymer_avg_decision = T, morph_weight = T, sample_weight = F)
      dataframe2 <- dataframe2 %>% select(morphology_raw, everything()) 
      dataframe2 <- dataframe2 %>% select(material_raw, everything()) 
      dataframe2 <- dataframe2 %>% select(morphology, everything()) 
      dataframe2 <- dataframe2 %>% select(material, everything())
    }else{dataframe2 <- dataframe}
      
    if("concentration_particle_vol" %in% colnames(dataframe) && "size_min" %in% colnames(dataframe) && "size_max" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
      #dataframe3 <- correctionFactor_conc(dataframe = dataframe, alpha_vals = alpha_vals, metric = "length (um)", corrected_min = 1, corrected_max = 5000)
      dataframe3 <- correctionFactor_conc(dataframe = dataframe, alpha_vals = alpha_vals, metric = input$concentration_type, corrected_min = input$corrected_min, corrected_max = input$corrected_max)
      dataframe4 <- concentration_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density, corrected_DF = dataframe3)
      #dataframe4 <- dataframeclean_particles
      #dataframe3 <- dataframeclean
      dataframe3 <- dataframe3 %>% select(sample_ID, alpha, alpha_upper, alpha_lower, correction_factor, correction_factor_upper, correction_factor_lower, corrected_concentration, corrected_concentration_upper, corrected_concentration_lower)
      dataframe3 <- dataframe3 %>% rename(corrected_concentration_particle_vol = corrected_concentration)
      dataframe4 <- dataframe4 %>% left_join(dataframe3, by = "sample_ID")
    }else{dataframe4 <- dataframe2} 
    
    
    if("length_um" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
      #dataframe5 <- correctionFactor_particle(dataframe = dataframe, corrected_min = 1, corrected_max = 5000, binning_type = "jenks", bin_number = 5)
      dataframe5 <- correctionFactor_particle(dataframe = dataframe, corrected_min = input$corrected_min, corrected_max = input$corrected_max, binning_type = input$binning_type, bin_number = input$bin_number)
      if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
        dataframe2 <- dataframe2 %>% select(volume_min_um_3, volume_mean_um_3, volume_max_um_3, min_mass_mg, mean_mass_mg, max_mass_mg)
        dataframe5 <- cbind(dataframe5, dataframe2)
      }
    }else{dataframe5 <- dataframe4}
    
    return(dataframe5)
    
    })
  
  #dataframe <- dataframe4
  #summaryResults <- eventReactive(input$particleData, {
  summaryResults <- reactive({
    req(input$particleData)
    dataframe <- convertedParticles()
    #dataframe <- dataframe5
     if("volume_mean_um_3" %in% colnames(dataframe) && "mean_mass_mg" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
       summary_table <- dataframe %>% group_by(sample_ID) %>%
         summarise_at(c("volume_min_um_3", "volume_mean_um_3", "volume_max_um_3", "min_mass_mg", "mean_mass_mg", "max_mass_mg"), mean)
       summary_table <- summary_table %>%
         add_column(volume_upper = as.numeric(summary_table$volume_max_um_3) - as.numeric(summary_table$volume_mean_um_3),
                    volume_lower = as.numeric(summary_table$volume_mean_um_3) - as.numeric(summary_table$volume_min_um_3),
                    mass_upper = as.numeric(summary_table$max_mass_mg) - as.numeric(summary_table$mean_mass_mg),
                    mass_lower = as.numeric(summary_table$mean_mass_mg) - as.numeric(summary_table$min_mass_mg)) %>%
         select(-c(volume_min_um_3, volume_max_um_3, min_mass_mg, max_mass_mg))
       summary_table <- summary_table %>%
         add_column(volume_error = (as.numeric(summary_table$volume_upper) + as.numeric(summary_table$volume_lower))/2,
                    mass_error = (as.numeric(summary_table$mass_upper) + as.numeric(summary_table$mass_lower))/2) %>%
         select(-c(volume_upper, volume_lower, mass_upper, mass_lower))
       summary_table <- summary_table %>%
         mutate_if(is.numeric, signif, digits=3) %>%
         mutate_if(is.numeric, formatC, format = "e",  digits=3)
       summary_table <- summary_table %>%
         unite(average_mass_mg, c(mean_mass_mg, mass_error), sep = "+/-") %>%
         unite(average_volume_um_3, c(volume_mean_um_3, volume_error), sep = "+/-") 
       summary_table1 <- summary_table
     }
    if("volume_mean_um_3" %in% colnames(dataframe) && "mean_mass_mg" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe) == FALSE){
       summary_table <- dataframe %>% group_by(morphology) %>%
         summarise_at(c("volume_min_um_3", "volume_mean_um_3", "volume_max_um_3", "min_mass_mg", "mean_mass_mg", "max_mass_mg"), mean)
       summary_table <- summary_table %>%
         add_column(volume_upper = as.numeric(summary_table$volume_max_um_3) - as.numeric(summary_table$volume_mean_um_3),
                    volume_lower = as.numeric(summary_table$volume_mean_um_3) - as.numeric(summary_table$volume_min_um_3),
                    mass_upper = as.numeric(summary_table$max_mass_mg) - as.numeric(summary_table$mean_mass_mg),
                    mass_lower = as.numeric(summary_table$mean_mass_mg) - as.numeric(summary_table$min_mass_mg)) %>%
         select(-c(volume_min_um_3, volume_max_um_3, min_mass_mg, max_mass_mg))
       summary_table <- summary_table %>%
         add_column(volume_error = (as.numeric(summary_table$volume_upper) + as.numeric(summary_table$volume_lower))/2,
                    mass_error = (as.numeric(summary_table$mass_upper) + as.numeric(summary_table$mass_lower))/2) %>%
         select(-c(volume_upper, volume_lower, mass_upper, mass_lower))
       summary_table <- summary_table %>%
         mutate_if(is.numeric, signif, digits=3) %>%
         mutate_if(is.numeric, formatC, format = "e",  digits=3)
       
       summary_table$average_mass_mg <- paste(summary_table$mean_mass_mg, summary_table$mass_error, sep="+/-")
       summary_table$average_volume_um_3 <- paste(summary_table$volume_mean_um_3, summary_table$volume_error, sep="+/-")
       summary_table <- summary_table %>%
         select(-c(mean_mass_mg, mass_error, volume_mean_um_3, volume_error))
     }else if("alpha" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe) && "sample_volume" %in% colnames(dataframe) && "mean_concentration_mg_vol" %in% colnames(dataframe) == FALSE){
       summary_table <- dataframe %>% group_by(sample_ID) %>%
         summarise_at(c("concentration", "alpha", "alpha_upper", "alpha_lower", "corrected_concentration", "corrected_concentration_upper", "corrected_concentration_lower"), mean)
       summary_table <- summary_table %>%
         add_column(alpha_error = ((as.numeric(summary_table$alpha_upper) - as.numeric(summary_table$alpha)) + (as.numeric(summary_table$alpha) - as.numeric(summary_table$alpha_lower)))/2,
                    concentration_error = ((as.numeric(summary_table$corrected_concentration_upper) - as.numeric(summary_table$corrected_concentration)) + (as.numeric(summary_table$corrected_concentration) - as.numeric(summary_table$corrected_concentration_lower)))/2) %>%
         select(-c(alpha_upper, alpha_lower, corrected_concentration_upper, corrected_concentration_lower)) %>%
         rename(alpha_ = alpha,
                corrected_concentration_ = corrected_concentration)
       summary_table <- summary_table %>%
         mutate_if(is.numeric, signif, digits=3) %>%
         mutate_if(is.numeric, formatC, format = "e",  digits=3)
       summary_table$alpha <- paste(summary_table$alpha_, summary_table$alpha_error, sep="+/-")
       summary_table$corrected_concentration <- paste(summary_table$corrected_concentration_, summary_table$concentration_error, sep="+/-")
       summary_table <- summary_table %>%
         select(-c(alpha_, alpha_error, corrected_concentration_, concentration_error))
     }else if("alpha" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe) && "sample_volume" %in% colnames(dataframe) == FALSE && "mean_concentration_mg_vol" %in% colnames(dataframe) == FALSE){
       summary_table <- dataframe %>% group_by(sample_ID) %>%
         summarise_at(c("concentration_particle_vol", "alpha", "alpha_upper", "alpha_lower"), mean)
       summary_table <- summary_table %>%
         add_column(alpha_error = ((as.numeric(summary_table$alpha_upper) - as.numeric(summary_table$alpha)) + (as.numeric(summary_table$alpha) - as.numeric(summary_table$alpha_lower)))/2) %>%
         select(-c(alpha_upper, alpha_lower)) %>%
         rename(alpha_ = alpha)
       summary_table <- summary_table %>%
         mutate_if(is.numeric, signif, digits=3) %>%
         mutate_if(is.numeric, formatC, format = "e",  digits=3)
       summary_table$alpha <- paste(summary_table$alpha_, summary_table$alpha_error, sep="+/-")
       summary_table <- summary_table %>%
         select(-c(alpha_, alpha_error))
     }else if("concentration_particle_vol" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe) && "mean_concentration_mg_vol" %in% colnames(dataframe)){
       summary_table <- dataframe %>% group_by(sample_ID) %>%
         summarise_at(c("concentration_particle_vol", "min_concentration_mg_vol", "mean_concentration_mg_vol", "max_concentration_mg_vol", "volume_min_um_3", "volume_max_um_3", "volume_mean_um_3",
                        "alpha", "alpha_upper", "alpha_lower", "corrected_concentration_particle_vol", "corrected_concentration_upper", "corrected_concentration_lower"), mean)
       summary_table <- summary_table %>%
         add_column(volume_error = ((as.numeric(summary_table$volume_max_um_3) - as.numeric(summary_table$volume_mean_um_3)) + (as.numeric(summary_table$volume_mean_um_3) - as.numeric(summary_table$volume_min_um_3)))/2,
                    mass_error = ((as.numeric(summary_table$max_concentration_mg_vol) - as.numeric(summary_table$mean_concentration_mg_vol)) + (as.numeric(summary_table$mean_concentration_mg_vol) - as.numeric(summary_table$min_concentration_mg_vol)))/2,
                    alpha_error = ((as.numeric(summary_table$alpha_upper) - as.numeric(summary_table$alpha)) + (as.numeric(summary_table$alpha) - as.numeric(summary_table$alpha_lower)))/2,
                    corrected_concentration_error = ((as.numeric(summary_table$corrected_concentration_upper) - as.numeric(summary_table$corrected_concentration_particle_vol)) + (as.numeric(summary_table$corrected_concentration_particle_vol) - as.numeric(summary_table$corrected_concentration_lower)))/2) %>%
         select(-c(volume_max_um_3, volume_min_um_3, max_concentration_mg_vol, min_concentration_mg_vol, alpha_upper, alpha_lower, corrected_concentration_upper, corrected_concentration_lower))
       summary_table <- summary_table %>%
         mutate_if(is.numeric, signif, digits=3) %>%
         mutate_if(is.numeric, formatC, format = "e",  digits=3)
       summary_table$concentration_um_3_vol <- paste(summary_table$volume_mean_um_3, summary_table$volume_error, sep="+/-")
       summary_table$concentration_mg_vol <- paste(summary_table$mean_concentration_mg_vol, summary_table$mass_error, sep="+/-")
       summary_table$alpha <- paste(summary_table$alpha, summary_table$alpha_error, sep="+/-")
       summary_table$corrected_concentration_particle_vol <- paste(summary_table$corrected_concentration_particle_vol, summary_table$corrected_concentration_error, sep="+/-")
       summary_table <- summary_table %>%
         select(-c(volume_mean_um_3, volume_error, mean_concentration_mg_vol, corrected_concentration_error, alpha_error, mass_error, corrected_concentration_error))
     }
    
    if("volume_mean_um_3" %in% colnames(dataframe) && "mean_mass_mg" %in% colnames(dataframe) && "length_um" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
      summary_table <- summary_table %>% left_join(summary_table1, by = "sample_ID")
    }else{summary_table <- summary_table}
    
    
    
    return(summary_table)

   })
  
  #Test data
  
  observeEvent(input$reporting_level, {
    if(input$reporting_level == "Sample (particles/volume)"){
      updateCheckboxGroupInput(inputId = "characteristics",
                               choices = c("Material Proportion" = "material",
                                           "Morphologic Proportion" = "morph",
                                           "Study Media" = "media",
                                           "Min/Max Particle Size Range" = "range",
                                           "Average Particle Length (um)" = "length"))
      
      updateCheckboxGroupInput(inputId = "advanced", label = "",
                               choices = c("Concentration Size Bins" = "binned",
                                           "Average Particle Width (um)" = "width",
                                           "Average Particle Height (um)" = "height",
                                           "Known Alpha Value" = "alpha",
                                           "Average Particle Density (mg/um3)" = "density",
                                           "Known Error (+/- SD)" = "sd_error",
                                           "Known Error (lower and upper bounds)" = "error"))
    }
    if(input$reporting_level == "Particle"){
      updateCheckboxGroupInput(inputId = "characteristics",
                               choices = c("Material" = "material_p",
                                           "Morphology" = "morph_p",
                                           "Particle Length (um)" = "length_p",
                                           "Sample ID" = "sample"))
      
      updateCheckboxGroupInput(inputId = "advanced", label = "",
                               choices = c("Particle Width (um)" = "width_p",
                                           "Particle Height (um)" = "height_p",
                                           "Sample Volume" = "volume",
                                           "Particle Density (mg/um3)" = "density_p"))
    }
  }
  )
  
  testData <- reactive({
    req(input$reporting_level)
    data = data.frame(matrix(ncol = 0, nrow = 3))
    if(input$reporting_level == "Sample (particles/volume)"){
      concentration_particle_vol = c(100, 100, 100)
      sample_ID = c("test", "test", "test")
      data <- data %>% add_column(concentration_particle_vol = concentration_particle_vol, sample_ID = sample_ID)
      if ("material" %in% as.vector(input$characteristics)){material = c("PE", "LDPE", NA)
        material_percent = c(70, 30, NA)
        data <- add_column(data, material = material, material_percent = material_percent)}
      if ("morph" %in% as.vector(input$characteristics)){morphology = c("fiber", "fragment", "film")
        morphology_percent = c(70, 20, 10)
        data <- add_column(data, morphology = morphology, morphology_percent = morphology_percent)}
      if ("media" %in% as.vector(input$characteristics)){study_media = c("marine surface", "marine surface", "marine surface")
        data <- add_column(data, study_media = study_media)}
      if ("range" %in% as.vector(input$characteristics) || "binned" %in% as.vector(input$advanced)){size_min = c(50, 201, 1001)
        size_max = c(200, 1000, 5000)
        data <- add_column(data, size_min = size_min, size_max = size_max)}
      # if ("length" %in% as.vector(input$characteristics)){avg_length_um = c(100, 100, 100)
      #   data <- add_column(data, avg_length_um = avg_length_um)}
      if ("width" %in% as.vector(input$advanced)){avg_width_um = c(20, 20, 20)
        data <- add_column(data, avg_width_um = avg_width_um)}
      if ("height" %in% as.vector(input$advanced)){avg_height_um = c(80, 80, 80)
        data <- add_column(data, avg_height_um = avg_height_um)}
      if ("alpha" %in% as.vector(input$advanced)){known_alpha = c(1.80, 1.80, 1.80)
        data <- add_column(data, known_alpha = known_alpha)}
      if ("density" %in% as.vector(input$advanced)){avg_density = c(0.00000000098,0.00000000098, 0.00000000098)
        data <- add_column(data, avg_density = avg_density)}
      if ("sd_error" %in% as.vector(input$advanced)){error_SD = c(18, 18, 18)
      data <- add_column(data, error_SD = error_SD)}
      if ("error" %in% as.vector(input$advanced)){error_upper = c(110, 110, 110)
      error_lower = c(90, 90, 90)
      data <- add_column(data, error_upper = error_upper,
                         error_lower = error_lower)}
    }
    
    if(input$reporting_level == "Particle"){
      if ("material_p" %in% as.vector(input$characteristics)){material = c("PE", "LDPE", "PET")
      data <- add_column(data, material = material)}
      if ("morph_p" %in% as.vector(input$characteristics)){morphology = c("fiber", "fragment", "film")
      data <- add_column(data, morphology = morphology)}
      if ("length_p" %in% as.vector(input$characteristics)){length_um = c(120, 70, 80)
      data <- add_column(data, length_um = length_um)}
      if ("sample" %in% as.vector(input$characteristics)){sample_ID = c("test", "test", "test")
      data <- add_column(data, sample_ID = sample_ID)}
      if ("width_p" %in% as.vector(input$advanced)){width_um = c(NA, 30, NA)
      data <- add_column(data, width_um = width_um)}
      if ("height_p" %in% as.vector(input$advanced)){height_um = c(20, NA, NA)
      data <- add_column(data, height_um = height_um)}
      if ("volume" %in% as.vector(input$advanced)){sample_volume = c(80, 80, 80)
      data <- add_column(data, sample_volume = sample_volume)}
      if ("density_p" %in% as.vector(input$advanced)){density = c(NA, 0.00000000098, NA)
      data <- add_column(data, density = density)}
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
  
  output$contents <- renderDataTable(#server = F,
                                     datatable({
                                       df()[, c("material","items",  input$variable)]
                                     }, 
                                     extensions = 'Buttons',
                                     options = list(
                                       paging = TRUE,
                                       searching = TRUE,
                                       fixedColumns = TRUE,
                                       autoWidth = TRUE,
                                       ordering = TRUE,
                                       dom = 'Bfrtip',
                                       buttons = c('copy', 'csv', 'excel', 'pdf')
                                     ),
                                     class = "display",
                                     style="bootstrap"))
  
  output$contents1 <- DT :: renderDataTable(
                                      datatable({df()[, c("material","PrimeMaterial")] %>% distinct()},
                                                extensions = 'Buttons',
                                                class = "display",
                                                style="bootstrap",
                                                escape = FALSE,
                                                options = list(server = FALSE, dom="Bfrtip", paging=TRUE, ordering=TRUE, buttons=c('copy', 'csv', 'excel', 'pdf')),
                                                callback = JS("table.rows().every(function(row, tab, row) {
                                              var $this = $(this.node());
                                              $this.attr('id', this.data()[0]);
                                              $this.addClass('shiny-input-container');
                                            });
                                            Shiny.unbindAll(table.table().node());
                                            Shiny.bindAll(table.table().node());"))
                                      
                                      )
                                      
  
  output$contents2 <- renderDataTable(#server = F, 
                                      datatable({df()[, c("items","PrimeItem")] %>% distinct()},
                                                extensions = 'Buttons',
                                                class = "display",
                                                style="bootstrap",
                                                escape = FALSE,
                                                options = list(server = FALSE, dom="Bfrtip", paging=TRUE, ordering=TRUE, buttons=c('copy', 'csv', 'excel', 'pdf')),
                                                callback = JS("table.rows().every(function(row, tab, row) {
                                              var $this = $(this.node());
                                              $this.attr('id', this.data()[0]);
                                              $this.addClass('shiny-input-container');
                                            });
                                            Shiny.unbindAll(table.table().node());
                                            Shiny.bindAll(table.table().node());"))
  )
  
  output$contents3 <- renderDataTable(#server = F, 
                                      datatable({
                                        df_()[, c("Use", "Material", "Item", "count", "proportion", "min_proportion", "max_proportion")]
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        server = F, 
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap")
  )
  
  output$contents4 <- renderDataTable(datatable({
                                        selectSurvey()
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        server = F, 
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents5 <- renderDataTable(server = F,
                                      datatable({
                                        convertedParticles()
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents7 <- renderDataTable(server = F,
                                      datatable({
                                        summaryResults()
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents8 = DT::renderDataTable(
    materialDisplay(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap",
    options = list(dom = 'f', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )
  
  output$contents9 = DT::renderDataTable(
    morphologyDisplay(), escape = FALSE, selection = 'none', server = FALSE, style="bootstrap",
    options = list(dom = 'f', paging = FALSE, ordering = FALSE),
    callback = JS("table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-container');
      });
      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());")
  )

  # output$contents9 <- renderDataTable(
  #   datatable({morphologyDisplay()},
  #             class = "display",
  #             style="bootstrap",
  #             escape = FALSE,
  #             #server = FALSE,
  #             options = list(dom="f", paging=TRUE, ordering=TRUE),
  #             callback = JS("table.rows().every(function(row, tab, row) {
  #                                             var $this = $(this.node());
  #                                             $this.attr('id', this.data()[0]);
  #                                             $this.addClass('shiny-input-container');
  #                                           });
  #                                           Shiny.unbindAll(table.table().node());
  #                                           Shiny.bindAll(table.table().node());"))
  #)
  # output$contents9 <- renderDataTable(server = F,
  #                                     datatable({
  #                                       morphologyDisplay()
  #                                     }, 
  #                                     extensions = 'Buttons',
  #                                     options = list(
  #                                       paging = TRUE,
  #                                       searching = TRUE,
  #                                       fixedColumns = TRUE,
  #                                       autoWidth = TRUE,
  #                                       ordering = TRUE,
  #                                       dom = 'Bfrtip',
  #                                       buttons = c('copy', 'csv', 'excel')
  #                                     ),
  #                                     class = "display",
  #                                     style="bootstrap"))
  
  output$testDataDownload <- renderDataTable(server = F,
                                      datatable({
                                        testData()
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$contents6 <- renderDataTable(#server = F, 
                                      datatable({
                                        correctionFactor()[, c("study_media", "concentration", "concentration_units", "size_min", "size_max",  "alpha", "correction_factor", "corrected_concentration")]
                                      }, 
                                      extensions = 'Buttons',
                                      options = list(
                                        paging = TRUE,
                                        searching = TRUE,
                                        fixedColumns = TRUE,
                                        autoWidth = TRUE,
                                        ordering = TRUE,
                                        dom = 'Bfrtip',
                                        buttons = c('copy', 'csv', 'excel', 'pdf')
                                      ),
                                      class = "display",
                                      style="bootstrap"))
  
  output$download_data <- downloadHandler(
    filename = "test_data.csv",
    content = function(file) {
      if(input$download_selection == "Particle Test Data") {fwrite(particle_testData, file)}
      if(input$download_selection == "Sample Test Data") {fwrite(sample_testData, file)}
    })
  
  
  output$downloadData1 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData2 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Materials_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Materials_Hierarchy, file, row.names=FALSE)
    }
  )
  
  output$materialhierarchy <- renderTree({
    Materials_hierarchy
  }
  )
  
  output$itemshierarchy <- renderTree({
    Items_hierarchy
  }
  )
  
  output$downloadData3 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Alias)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Alias, file, row.names=FALSE)
    }
  )
  
  output$downloadData4 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Items_Hierarchy)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Items_Hierarchy, file, row.names=FALSE)
    }
  )
  
  
  output$downloadData5 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Material_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Material_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData6 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Manufacturer_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Manufacturer_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData7 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(Brand_Item_Relation)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Brand_Item_Relation, file, row.names=FALSE)
    }
  )
  
  output$downloadData8 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(PrimeUnclassifiable)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(PrimeUnclassifiable, file, row.names=FALSE)
    }
  )
  
  output$downloadData9 <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(polymer_db)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(polymer_db, file, row.names=FALSE)
    }
  )
  
  output$downloadtest <- downloadHandler(    
    filename = function() {
      paste(deparse(substitute(NOAA)), '.csv', sep='')
    },
    content = function(file) {
      write.csv(NOAA, file, row.names=FALSE)
    }
  )
  
  output$table1 = DT::renderDataTable({
    Materials_Alias
  }, style="bootstrap")
  
  output$table2 = DT::renderDataTable({
    Materials_Hierarchy
  }, style="bootstrap")
  
  output$table3 = DT::renderDataTable({
    Items_Alias
  }, style="bootstrap")
  
  output$table4 = DT::renderDataTable({
    Items_Hierarchy
  }, style="bootstrap")
  
  output$table5 = DT::renderDataTable({
    Material_Item_Relation
  }, style="bootstrap")
  
  output$table6 = DT::renderDataTable({
    Brand_Manufacturer_Relation
  }, style="bootstrap")
  
  output$table7 = DT::renderDataTable({
    Brand_Item_Relation
  }, style="bootstrap")
  
  output$table8 = DT::renderDataTable({
    PrimeUnclassifiable
  }, style="bootstrap")
  
  output$table9 = DT::renderDataTable({
    polymer_db
  }, style="bootstrap")
  
  color <- read.csv("data/Microplastics_Color.csv")
  output$table12 = DT::renderDataTable({
    color
  }, style="bootstrap")
  
  #embeddings <- mongo(url = readLines("data/embeddings_mdb.rtf", warn = FALSE))
  
  
  
}





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
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"material"])) {
        dataframe[row, "MoreSpecificMaterial"] <- "Unclassifiable"
        dataframe[row, "PrimeMaterial"] <- NA
        #next #Corrects for cases when unclassifiable
      }
      
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
      
      if(any(PrimeUnclassifiable == dataframeclean[row,"items"])) {
        dataframe[row, "MoreSpecificItem"] <- "Unclassifiable"
        dataframe[row, "PrimeItem"] <- NA
        next #Corrects for cases when unclassifiable
      }
      
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

  df_ <- reactive({
    req(input$df_)
    merge_data(file_paths = input$df_$datapath, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB,alias = alias, aliasi = aliasi, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable)
  })
  
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
    req(input$df_)
    
    dataframe <- as.data.frame(df_() %>% 
                                 rename(material = Material, 
                                        items = Item) %>%
                                 select(material, items, count))
    
    Material_DF_group <- dataframe %>%
      rename(Count = count) %>%
      rename(Class = material)
    
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
  })
  
  
  
  #Item Sunburst Plot ----
  output$plot2 <- renderPlotly({
    req(input$df_)
    
    dataframe <- as.data.frame(df_() %>% 
                                 rename(material = Material, 
                                        items = Item) %>%
                                 select(material, items, count))
    
    Item_DF_group <- dataframe %>%
      rename(Count = count) %>%
      rename(Class = items)
    
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
  
  #particle count-volume-mass converter
  
  convertedParticles <- reactive({
    req(input$particleData)
    infile <- input$particleData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file)
    
    dataframe <- read.csv("tests/Sample_Dist_Data.csv")
    
    if("width_um" %in% colnames(dataframe) == TRUE){dataframe <- dataframe %>%
      select(length_um, width_um, morphology, polymer)
    dataframe$width_um <- as.numeric(dataframe$width_um)
    }else{dataframe <- dataframe %>%
      select(length_um, morphology, polymer)}
    
    dataframe$length_um <- as.numeric(dataframe$length_um)
    dataframe$morphology <- as.character(dataframe$morphology)
    dataframe$polymer <- as.character(dataframe$polymer)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    #convert morphologies in TT to morphologies with defined dimensions
    morphology <- c("fiber", "nurdle", "foam", "sphere", "line", "bead", "sheet", "film", "fragment", "rubberyfragment", "fiberbundle")
    morph_dimension <- c("fiber", "sphere", "foam", "sphere", "fiber", "sphere", "film", "film", "fragment", "fragment", "film")
    morph_conversion <- data.frame(morphology = morphology,
                                   morph_dimension = morph_dimension)
    dataframeclean <- left_join(dataframeclean, morph_conversion, by = "morphology", copy = FALSE)
    for(x in 1:nrow(dataframeclean)){
      if(is.na(dataframeclean[x,"morph_dimension"])){
        dataframeclean[x,"morph_dimension"] <- dataframeclean[x,"morphology"]
      }
    }
    dataframeclean <- dataframeclean %>%
      select(-morphology)
    dataframeclean <- dataframeclean %>%
      rename(morphology = morph_dimension)
    
    #Make polymer-density dataframe
    #Output correct survey sheet
    polymer_db <- data.frame(polymer_db)
    polymer_db$polymer <- cleantext(polymer_db$polymer)
    polymer_db$density <- as.numeric(polymer_db$density)
    density_mg_um_3 <- polymer_db$density * 1e-9
    polymer_db <- polymer_db %>%
      mutate(density_mg_um_3 = density_mg_um_3)
    polymer_density <- polymer_db %>%
      select(polymer, density_mg_um_3)
    
    #Make morphology dimension dataframe
    morphology <- c("fragment","sphere","fiber","film","foam")
    L_min <- c(0.95,0.95,0.95,0.95,0.95)
    L_max <- c(1.05,1.05,1.05,1.05,1.05)
    W_meas_min <- c(0.95,0.95,0.95,0.95,0.95)
    W_meas_max <- c(1.05,1.05,1.05,1.05,1.05)
    
    #Min and max values given in Kooi Koelmans
    W_min <- c(0.1,0.60,0.001,0.1,0.1)
    W_max <- c(1,1,0.5,1,1)
    W_mean <- (as.numeric(W_min) + as.numeric(W_max))/2
    W_sd <- (as.numeric(W_max) - as.numeric(W_min))/6
    H_min <- c(0.01,0.36,0.001,0.001,0.01)
    H_max <- c(1,1,0.5,0.1,1)
    H_mean <- (as.numeric(H_min) + as.numeric(H_max))/2
    H_sd <- (as.numeric(H_max) - as.numeric(H_min))/6
    
    #Assuming min and max encompass 99.7% of normal distribution, calculate 95% confidence interval
    W_min <- as.numeric(quantile(rnorm(n = 100000, mean = W_mean, sd = W_sd), probs = c(0.025)))
    W_max <- as.numeric(quantile(rnorm(n = 100000, mean = W_mean, sd = W_sd), probs = c(0.975)))
    H_min <- as.numeric(quantile(rnorm(n = 100000, mean = H_mean, sd = H_sd), probs = c(0.025)))
    H_max <- as.numeric(quantile(rnorm(n = 100000, mean = H_mean, sd = H_sd), probs = c(0.975)))
    
    morphology_shape <- data.frame(morphology=morphology,
                                   L_min=L_min,
                                   L_max=L_max,
                                   W_min=W_min,
                                   W_max=W_max,
                                   H_min=H_min,
                                   H_max=H_max
    )
    
    dataframeclean <- left_join(dataframeclean, morphology_shape, by = "morphology", copy = F)
    dataframeclean <- left_join(dataframeclean, polymer_density, by = "polymer", copy = F)
    if("width_um" %in% colnames(dataframeclean) == TRUE) {
      dataframeclean <- data.frame(dataframeclean) %>%
        mutate(L_min = as.numeric(L_min) * as.numeric(length_um),
               L_mean = as.numeric(length_um),
               L_max = as.numeric(L_max) * as.numeric(length_um),
               W_meas_min = as.numeric(W_meas_min) * as.numeric(width_um),
               W_meas_mean = as.numeric(width_um),
               W_meas_max = as.numeric(W_meas_max) * as.numeric(width_um),
               H_min = as.numeric(H_min) * as.numeric(length_um),
               H_mean = (as.numeric(H_min) + as.numeric(H_max))/2 ,
               H_max = as.numeric(H_max) * as.numeric(length_um))
      dataframeclean <- data.frame(dataframeclean) %>%
        mutate(volume_min_um_3 = L_min * W_meas_min* H_min,
               volume_mean_um_3 = L_mean * W_meas_mean* H_mean,
               volume_max_um_3 = L_max * W_meas_max * H_max) 
    }else{dataframeclean <- data.frame(dataframeclean) %>%
      mutate(L_min = as.numeric(L_min) * as.numeric(length_um),
             L_mean = as.numeric(length_um),
             L_max = as.numeric(L_max) * as.numeric(length_um),
             W_min = as.numeric(W_min) * as.numeric(length_um),
             W_mean = (as.numeric(W_min) + as.numeric(W_max))/2 ,
             W_max = as.numeric(W_max) * as.numeric(length_um),
             H_min = as.numeric(H_min) * as.numeric(length_um),
             H_mean = (as.numeric(H_min) + as.numeric(H_max))/2 ,
             H_max = as.numeric(H_max) * as.numeric(length_um))
    dataframeclean <- data.frame(dataframeclean) %>%
      mutate(volume_min_um_3 = L_min * W_min* H_min,
             volume_mean_um_3 = L_mean * W_mean* H_mean,
             volume_max_um_3 = L_max * W_max * H_max) 
    }
    
    dataframeclean_particles <- data.frame(dataframeclean) %>%
      mutate(min_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_min_um,
             mean_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_mean_um,
             max_mass_mg = dataframeclean$density_mg_um_3 * dataframeclean$volume_max_um)
    
    dataframeclean_particles <- dataframeclean_particles %>%
      left_join(trash_mass_clean, by = c("morphology" = "item",
                                         "polymer" = "material"))
    dataframeclean_particles$weight_estimate_g <- as.numeric(dataframeclean_particles$weight_estimate_g)
    for(x in 1:nrow(dataframeclean_particles)){
      if(! is.na(dataframeclean_particles[x, "weight_estimate_g"])){
        dataframeclean_particles[x, "mean_mass_mg"] <- (dataframeclean_particles[x, "weight_estimate_g"]*1000)
      }
    }
    
    dataframeclean_particles <- dataframeclean_particles %>% select(-"weight_estimate_g")
    
    return(dataframeclean_particles)
    
    })
  
  
  correctionFactor <- reactive({
    req(input$calculate_distribution)
    req(input$concentrationData)
    req(input$concentration_type)
    req(input$corrected_min)
    req(input$corrected_max)
    
    #clean incoming data
    infile <- input$concentrationData
    file <- fread(infile$datapath)
    dataframe <- as.data.frame(file) %>%
      select(study_media, concentration, size_min, size_max, concentration_units)
    dataframe$concentration <- as.numeric(dataframe$concentration)
    dataframe$size_min <- as.numeric(dataframe$size_min)
    dataframe$size_max <- as.numeric(dataframe$size_max)
    dataframe$study_media <- as.character(dataframe$study_media)
    dataframe$concentration_units <- as.character(dataframe$concentration_units)
    dataframeclean <- mutate_all(dataframe, cleantext) 
    
    #Make df for alpha values
    study_media <- c("marinesurface","freshwatersurface","marinesediment","freshwatersediment","effluent", "biota")
    length <- c(2.07, 2.64, 2.57, 3.25, 2.54, 2.59)
    mass <- c(1.32, 1.65, 1.50, 1.56, 1.40, 1.41)
    volume <- c(1.48, 1.68, 1.50, 1.53, 1.45, 1.40)
    surface_area <- c(1.50, 2.00, 1.75, 1.89, 1.73, 1.69)
    specific_surface_area <- c(1.98, 2.71, 2.54, 2.82, 2.58, 2.46)
    
    alpha_vals <- data.frame(study_media=study_media,
                             length=length,
                             mass=mass,
                             volume=volume,
                             surface_area=surface_area,
                             specific_surface_area=specific_surface_area
    )
    
    if(input$concentration_type == "length (um)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "length")], by = "study_media", all.x=TRUE)
                                                    dataframeclean <- dataframeclean %>%
                                                      rename("alpha" = "length")}
    if(input$concentration_type == "mass (ug)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "mass")], by = "study_media", all.x=TRUE)
                                                  dataframeclean <- dataframeclean %>%
                                                    rename("alpha" = "mass")}
    if(input$concentration_type == "volume (um3)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "volume")], by = "study_media", all.x=TRUE)
                                                    dataframeclean <- dataframeclean %>%
                                                      rename("alpha" = "volume")}
    if(input$concentration_type == "surface area (um2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "surface_area")], by = "study_media", all.x=TRUE)
                                                          dataframeclean <- dataframeclean %>%
                                                            rename("alpha" = "surface_area")}
    if(input$concentration_type == "specific surface area (g/m2)"){dataframeclean <- merge(x = dataframeclean, y = alpha_vals[ , c("study_media", "specific_surface_area")], by = "study_media", all.x=TRUE)
                                                                    dataframeclean <- dataframeclean %>%
                                                                      rename("alpha" = "specific_surface_area")}
    
    dataframeclean <- dataframeclean %>%
      add_column(correction_factor = NA,
                 corrected_concentration = NA)
    
    
    #Extrapolated parameters
    x1D_set = as.numeric(input$corrected_min) #lower limit default extrapolated range is 1 um
    x2D_set = as.numeric(input$corrected_max) #upper limit default extrapolated range is 5 mm
    
    for(x in 1:nrow(dataframeclean)) {
      x1M_set = as.numeric(dataframeclean$size_min[[x]])
      x2M_set = as.numeric(dataframeclean$size_max[[x]])
      alpha = as.numeric(dataframeclean$alpha[[x]])
      
       CF <- CFfnx(x1M = x1M_set,#lower measured length
                   x2M = x2M_set, #upper measured length
                   x1D = x1D_set, #default lower size range
                   x2D = x2D_set,  #default upper size range
                   a = alpha #alpha for count 
                                                     
      )
       
       CF <- as.numeric(CF)
       
       CF <- format(round(CF, 2), nsmall = 2)
         
      
      dataframeclean$correction_factor[[x]] <- CF
      
      dataframeclean$corrected_concentration[[x]] <- as.numeric(dataframeclean$correction_factor[[x]]) * as.numeric(dataframeclean$concentration[[x]])
      
    }
    
    return(dataframeclean)
  })
  
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
  
  output$contents5 <- renderDataTable(datatable({
                                        convertedParticles()[, c("length_um", "morphology", "polymer", "L_mean", "H_mean", "volume_mean_um_3", "mean_mass_mg")]
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
  

  
  # output$plot3 <- renderPlot({
  #   req(convertedParticles())
  #   ggplot(convertedParticles(), aes(x = morphology, y = volume_mean_um_3, fill = factor(morphology))) +
  #     geom_flat_violin(
  #       position = position_nudge(x = 0.1),
  #       alpha = 0.5,
  #       scale = "width",
  #       trim = FALSE,
  #       width = 0.8,
  #       lwd = 1,
  #     ) +
  #     geom_boxplot(
  #       width = 0.12,
  #       outlier.shape = 8,
  #       outlier.color = "navy",
  #       alpha = 1
  #     ) +
  #     stat_dots(
  #       position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
  #       dotsize = 15,
  #       side = "left",
  #       justification = 1.1,
  #       binwidth = 0.08,
  #       alpha = 1.0
  #     ) +
  #     scale_fill_brewer(palette = "Spectral") +
  #     labs(
  #       title = "Particle Volume by Morphology Type",
  #       x = "Morphology",
  #       y = "Volume (um3)",
  #       fill = "Morphology"
  #     ) +
  #     coord_flip() +
  #     dark_theme_gray() +
  #     theme(
  #       axis.text = element_text(size = 15),
  #       axis.title = element_text(size = 18),
  #       plot.title = element_text(size = 18)
  #     )
  # })
  # 
  # output$plot4 <- renderPlot({
  #   req(convertedParticles())
  #   ggplot(convertedParticles(), aes(x = polymer, y = mean_mass_mg, fill = factor(polymer))) +
  #     geom_flat_violin(
  #       position = position_nudge(x = 0.1),
  #       alpha = 0.5,
  #       scale = "width",
  #       trim = FALSE,
  #       width = 0.8,
  #       lwd = 1,
  #     ) +
  #     geom_boxplot(
  #       width = 0.12,
  #       outlier.shape = 8,
  #       outlier.color = "navy",
  #       alpha = 1
  #     ) +
  #     stat_dots(
  #       position = position_jitterdodge(jitter.width = 1, dodge.width = 0.4, jitter.height = 10),
  #       dotsize = 15,
  #       side = "left",
  #       justification = 1.1,
  #       binwidth = 0.08,
  #       alpha = 1.0
  #     ) +
  #     scale_fill_brewer(palette = "Spectral") +
  #     labs(
  #       title = "Particle Mass by Polymer Type",
  #       x = "Polymer",
  #       y = "Mass (mg)",
  #       fill = "Polymer"
  #     ) +
  #     coord_flip() +
  #     dark_theme_gray() +
  #     theme(
  #       axis.text = element_text(size = 15),
  #       axis.title = element_text(size = 18),
  #       plot.title = element_text(size = 18)
  #     )
  # })
  # 
  # 
  # 
  # output$downloadPlot3 <- downloadHandler(
  #   filename = function() { "particle_volume_plot.pdf" },
  #   content = function(file) {
  #     pdf(file, paper = "default")
  #     plot(plot3())
  #     dev.off()
  #   }
  # )
  # 
  # output$downloadPlot4 <- downloadHandler(
  #   filename = function() { "particle_mass_plot.pdf" },
  #   content = function(file) {
  #     pdf(file, paper = "default")
  #     plot(plot4())
  #     dev.off()
  #   }
  # )
  
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
  
  #embeddings <- mongo(url = readLines("data/embeddings_mdb.rtf", warn = FALSE))
  
  
  
}





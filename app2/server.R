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
    
    #file_paths = test
    #merge_data(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias, aliasi = aliasi, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable)
    
    
     if("morphology" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
       dataframe2 <- merge_terms(file_paths = dataframe, materials_vectorDB = materials_vectorDB, items_vectorDB = items_vectorDB, alias = alias, aliasi = aliasi, use_cases = use_cases, prime_unclassifiable = prime_unclassifiable)
     }else{dataframe2 <- dataframe}
    
    if("morphology" %in% colnames(dataframe) && "length_um" %in% colnames(dataframe) && "material" %in% colnames(dataframe)){
      dataframe3 <- particle_count_mass(dataframe = dataframe2, morphology_shape = morphology_shape, polymer_density = polymer_density, trash_mass_clean = trash_mass_clean)
    }else{dataframe3 <- dataframe2}
      
    if("concentration_particle_vol" %in% colnames(dataframe) && "avg_length_um" %in% colnames(dataframe) && "material" %in% colnames(dataframe) && "morphology" %in% colnames(dataframe) &&
       "material_percent" %in% colnames(dataframe) && "morphology_percent" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
      dataframe4 <- concentration_count_mass(dataframe = dataframe, morphology_shape = morphology_shape, polymer_density = polymer_density)
    }else{dataframe4 <- dataframe3} 
    
    if("concentration_particle_vol" %in% colnames(dataframe) && "size_min" %in% colnames(dataframe) && "size_max" %in% colnames(dataframe)){
      dataframe5 <- correctionFactor_conc(dataframe = dataframe, alpha_vals = alpha_vals, metric = input$concentration_type, corrected_min = input$corrected_min, corrected_max = input$corrected_max)
    }else{dataframe5 <- dataframe4} 
    
    if("length_um" %in% colnames(dataframe) && "sample_ID" %in% colnames(dataframe)){
      dataframe6 <- correctionFactor_particle(dataframe = dataframe, corrected_min = input$corrected_min, corrected_max = input$corrected_max, binning_type = input$binning_type, bin_number = input$bin_number)
    }else{dataframe6 <- dataframe5}
    
    return(dataframe6)
    
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





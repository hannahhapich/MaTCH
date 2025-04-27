library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(readr)
library(Hmisc)
library(ggrepel)
library(data.table)
library(dplyr)
library(tidyr)
library(swfscMisc)
library(circular)
library(data.tree)
library(plotly)
library(ggforce)
library(ggdark)
library(tidyverse)
library(tidyquant)
library(ggdist)
library(ggthemes)

#Set working directory


#Morphology proportions before and after (stacked bar plot)
morphology_summary_all <- read.csv("figures/data/morphology_summary.csv")
morphology_summary_all <- morphology_summary_all %>% arrange(desc(correction))
morph_barplot <- ggplot(morphology_summary_all, aes(x = reorder(correction, proportion), y=proportion, fill=morphology)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(~Source) +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Morphology") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text =  element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/morph_barplot.png", plot = morph_barplot, width = 6.75, height = 4, dpi = 600)

#Material proportions before and after (stacked bar plot)
material_summary_all <- read.csv("figures/data/material_summary.csv")
palette <- colorRampPalette(RColorBrewer::brewer.pal(12,name = 'Set3'))(length(unique(material_summary_all$material)))
material_barplot <- ggplot(material_summary_all, aes(x = reorder(correction,+proportion), y=proportion, fill=material)) + 
  geom_bar(position="stack", stat="identity") +
  facet_grid(~Source) +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Material") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text =  element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/material_barplot.png", plot = material_barplot, width = 6.75, height = 4, dpi = 600)

#Comparability Metric Plot
AverageForOrganizationsTotal <- read.csv("figures/data/comparability_cluster.csv")
comparability_cluster <- ggplot(AverageForOrganizationsTotal, aes(x = ComparabilityItemsEncompassingOthers, y = ComparabilityMaterialsEncompassingOthers)) + 
  geom_point(color = "black", size = 3, alpha = 0.5)  +
  labs(x = "Morphology Comparability (mean decimal %)", y = "Material Comparability (mean decimal %)") + 
  scale_x_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1)) + scale_y_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1))+
  scale_fill_viridis_c(breaks = c(1:10)) + 
  coord_equal() +
  theme_minimal() + 
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

ggsave("figures/comparability_cluster.png", plot = comparability_cluster, width = 6, height = 6, dpi = 1200)


#Files and functions for sunburst plots
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

hierarchyi <- read.csv("figures/data/ITEMSHierarchyLower.csv")
hierarchycleani <- mutate_all(hierarchyi, cleantext)
ItemsHierarchy_sunburst <- data.frame(matrix(ncol=2, dimnames = list("", c("from", "to"))))
for(y in 1:ncol(hierarchycleani)){
  for(x in 1:nrow(hierarchycleani)){
    ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst) + 1, 2] <- hierarchycleani[x,y]
    if(!is.na(hierarchycleani[x,y]) && y == 1){ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst), 1] <- paste("Trash")}
    if(!is.na(hierarchycleani[x,y]) && y != 1){ItemsHierarchy_sunburst[nrow(ItemsHierarchy_sunburst), 1] <- paste(hierarchycleani[x, y -1])}
  }
  ItemsHierarchy_sunburst <- ItemsHierarchy_sunburst %>% distinct() %>% drop_na()
}
hierarchy <- read.csv("figures/data/MaterialsHierarchyLower.csv")
hierarchyclean <- mutate_all(hierarchy, cleantext)
MaterialsHierarchy_sunburst <- data.frame(matrix(ncol=2, dimnames = list("", c("from", "to"))))
for(y in 1:ncol(hierarchyclean)){
  for(x in 1:nrow(hierarchyclean)){
    MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst) + 1, 2] <- hierarchyclean[x,y]
    if(!is.na(hierarchyclean[x,y]) && y == 1){MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst), 1] <- paste("Trash")}
    if(!is.na(hierarchyclean[x,y]) && y != 1){MaterialsHierarchy_sunburst[nrow(MaterialsHierarchy_sunburst), 1] <- paste(hierarchyclean[x, y -1])}
  }
  MaterialsHierarchy_sunburst <- MaterialsHierarchy_sunburst %>% distinct() %>% drop_na()
}

ItemsAlias <- read.csv("figures/data/PrimeItems.csv")
ItemsAlias_sunburst <- read.csv("figures/data/PrimeItems.csv")%>%
  rename(Key = Item) %>%
  select(-readable)
MaterialsAlias <- read.csv("figures/data/PrimeMaterials.csv")
MaterialsAlias_sunburst <- read.csv("figures/data/PrimeMaterials.csv") %>%
  rename(Key = Material) %>%
  select(-readable)

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
      values = df_join_boot$mean_prop,
      insidetextorientation='horizontal') %>%
    layout(colorway = c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD"))
  
}

concentrations <- read.csv("figures/data/Concentration_Data_Matched.csv")
concentrations <- concentrations %>% select(-c(Min.Length..microns. ,
                                               Max.Length..microns. ,
                                               Min.Concentration..microns3.volume. , 
                                               Concentration..microns3.volume. ,
                                               Max.Concentration..microns3.volume. ,
                                               Min.Concentration..mg.volume. ,
                                               Concentration..mg.volume. ,
                                               Max.Concentration..mg.volume. ,
                                               Alpha ,
                                               Correction.Factor ,
                                               Max.Corrected.Concentration..particles.volume. ,
                                               Min.Corrected.Concentration..particles.volume.
)) %>%
  rename(concentration = Concentration..particles.volume. ,
         sample_ID = Sample.ID ,
         study_media = Study.Media ,
         corrected_concentration = Corrected.Concentration..particles.volume. ,
  )

#Microplastic rescaling raw morphology term pie chart
morphology_ratios <- read.csv("figures/data/morphology_all.csv") %>% left_join(concentrations, by = "sample_ID")
morphology_ratios <- morphology_ratios %>% add_column(morph_concentration_reported = morphology_ratios$morph_ratio * morphology_ratios$concentration,
                                                      morph_concentration_corrected = morphology_ratios$morph_ratio * morphology_ratios$corrected_concentration)
dataframe <- morphology_ratios %>% select(morphology, morphology_raw, morph_concentration_reported) %>% rename(items_raw = morphology_raw,
                                                                                                               items = morphology,
                                                                                                               count = morph_concentration_reported)


palette <- colorRampPalette(RColorBrewer::brewer.pal(12,name = 'Set3'))(length(unique(dataframe$items_raw)))
dataframe_summary <- aggregate(dataframe$count, by=list(Morphology=dataframe$items_raw), FUN=sum)
dataframe_summary <- dataframe_summary %>% arrange(desc(x))
morph_pie <- ggplot(dataframe_summary, aes(x= "", y= x, fill=reorder(Morphology, (-x)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = palette) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.box.margin=margin(-25,-25,-25,-25))

ggsave("figures/microplastic_morphology_pie.png", plot = morph_pie, width = 6, height = 6, dpi = 1200)

#Morphology Sunburst Plot
dataframe <- mutate_all(dataframe, cleantext)
Item_DF_group <- dataframe %>%
  rename(Count = count,
         Class = items) %>%
  select(-items_raw)
ItemsAlias_sunburst[duplicated(ItemsAlias_sunburst$Alias), ]

#Takes a bit to run...
item_grouped <- grouped_uncertainty(DF_group = Item_DF_group, Group_Alias = ItemsAlias_sunburst, Group_Hierarchy = ItemsHierarchy_sunburst, type = "items")

##Making readable alias display for sunburst plot
primeItems_SB <- ItemsAlias %>%
  add_row(Item = "items", Alias = "items", readable = "items") %>%
  add_row(Item = "trash", Alias = "trash", readable = "trash")
item_grouped_readable <- left_join(item_grouped, primeItems_SB, by = c("from" = "Alias"))
item_grouped_readable <- item_grouped_readable %>%
  ungroup() %>%
  select(-c("Item", "from"))
item_grouped_readable <- item_grouped_readable %>%
  rename(from = readable) %>%
  left_join(ItemsAlias, by = c("to" = "Alias"))
item_grouped_readable <- item_grouped_readable %>%
  ungroup() %>%
  select(-c("Item", "to"))
item_grouped_readable <- item_grouped_readable %>%
  rename(to = readable) %>%
  group_by(from, to)

Items_Plot <- sunburstplot(df_join_boot = item_grouped_readable)
print(Items_Plot)


#Microplastic rescaling raw material term pie chart
material_ratios <- read.csv("figures/data/material_all.csv") %>% left_join(concentrations, by = "sample_ID")
material_ratios <- material_ratios %>% add_column(material_concentration_reported = material_ratios$material_ratio * material_ratios$concentration,
                                                  material_concentration_corrected = material_ratios$material_ratio * material_ratios$corrected_concentration)
dataframe <- material_ratios %>% select(material, material_raw, material_concentration_reported) %>% rename(count = material_concentration_reported)

palette <- colorRampPalette(RColorBrewer::brewer.pal(12,name = 'Set3'))(length(unique(dataframe$material_raw)))
dataframe_summary <- aggregate(dataframe$count, by=list(Polymer=dataframe$material_raw), FUN=sum)
dataframe_summary <- dataframe_summary %>% arrange(desc(x))
dataframe_summary[dataframe_summary$Polymer < 100] <- NA
mat_pie <- ggplot(dataframe_summary, aes(x= "", y= x, fill=reorder(Polymer, (-x)))) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = palette) +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12))

ggsave("figures/microplastic_material_pie.png", plot = mat_pie, width = 9, height = 6, dpi = 1200)

dataframe <- mutate_all(dataframe, cleantext)

Material_DF_group <- dataframe %>%
  rename(Count = count) %>%
  rename(Class = material)

#Takes a bit to run...
material_grouped <- grouped_uncertainty(DF_group = Material_DF_group, Group_Alias = MaterialsAlias_sunburst, Group_Hierarchy = MaterialsHierarchy_sunburst, type = "material")

#Making readable alias display for sunburst plot
primeMaterials_SB <- MaterialsAlias %>%
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


#Concentration Boxplots

concentration_all <- data.frame() %>% add_column(Source = NA,
                                                  concentration = NA)
for(x in 1:nrow(concentrations)){
  concentrations_row <- data.frame(Source =  rep(NA, 2),
                                   concentration =  rep(NA, 2))
  if(concentrations[x, "study_media"] == "drinkingwater"){
    concentrations_row[1, "Source"] <- "Drinking Water Reported"
    concentrations_row[2, "Source"] <- "Drinking Water Corrected"
  }else{
  concentrations_row[1, "Source"] <- "River Reported"
  concentrations_row[2, "Source"] <- "River Corrected"}
  
  concentrations_row[1, "concentration"] <- concentrations[x, "concentration"]
  concentrations_row[2, "concentration"] <- concentrations[x, "corrected_concentration"]
  concentration_all <- rbind(concentration_all, concentrations_row)
}

median_concentration <- concentration_all %>%
  group_by(Source) %>%
  summarize(median_concentration = median(concentration, na.rm = TRUE))

concentration_all$Source <- factor(concentration_all$Source, 
                                   levels = c("Drinking Water Corrected", "Drinking Water Reported", 
                                              "River Corrected", "River Reported"))

concentration_correction_boxplot <- ggplot(concentration_all, aes(fill = Source, x = Source, y = concentration)) +
  geom_boxplot()+
  scale_y_continuous(trans='log10')+
  scale_fill_manual(values = c("#009999","#8DD3C7", "#FB8072","#FF9999")) +
  theme_tq() +
  theme(plot.margin = margin(t = 20,  
                             r = 50, 
                             b = 40, 
                             l = 10)) +
  labs(x = NULL, y = NULL, title = NULL) +
  
  coord_flip() +
  theme(
  legend.position = "none"        
  )

ggsave("figures/concentration_correction_boxplot.png", plot = concentration_correction_boxplot, width = 6.75, height = 4, dpi = 600)


#Summary stats on trash morphology
trash_data <- read.csv("figures/data/Cowger_Trash_Data_Matched.csv") %>% 
  rename(material = Material,
         items = Morphology,
         weight_estimate_g = Mass..mg.) %>%
  mutate(weight_estimate_g = weight_estimate_g/1000) %>%
  select(material, items, weight_estimate_g)
item_mass <- trash_data %>% 
  group_by(items) %>% 
  summarise(weight_estimate_g = sum(weight_estimate_g))%>%
  arrange(desc(weight_estimate_g))
mass_sum <- sum(item_mass$weight_estimate_g)
item_mass <- item_mass %>%
  add_column(percent = ((item_mass$weight_estimate_g)/mass_sum)*100,
             correction = "Mass") %>%
  select(-weight_estimate_g)
item_mass_remainder <- item_mass %>% filter (percent < 1.5)

item_mass_remainder_row <- c("remaining items", sum(item_mass_remainder$percent), "Mass")
item_mass <- item_mass %>% filter(percent > 1.5)
item_mass <- rbind(item_mass, item_mass_remainder_row)

item_count <- trash_data %>%
  add_column(count = 1) %>%
  group_by(items) %>% 
  summarise(count = sum(count))%>%
  arrange(desc(count))
count_sum <- sum(item_count$count)
item_count <- item_count %>%
  add_column(percent = ((item_count$count)/count_sum)*100,
             correction = "Count") %>%
  select(-count)
item_count_remainder <- item_count %>% filter (percent < 2.5)

item_count_remainder_row <- c("remaining items", sum(item_count_remainder$percent), "Count")
item_count <- item_count %>% filter(percent > 2.5)
item_count <- rbind(item_count, item_count_remainder_row)

morphology_summary <- rbind(item_mass, item_count)

#Make morphology stacked bar plot for macro debris count to mass conversion
morphology_summary <- morphology_summary %>% arrange(desc(correction))
morphology_summary <- morphology_summary %>% left_join(ItemsAlias, by = c("items" = "Alias"))
morphology_summary <- morphology_summary %>% select(-c("items", "Item")) %>% rename(items = readable)
morphology_summary$percent <- as.numeric(morphology_summary$percent)
for(x in 1:nrow(morphology_summary)){
  if(is.na(morphology_summary$items[[x]])){
    morphology_summary$items[[x]] <- "remaining items"
  }
}


set3_colors <- brewer.pal(n = 12, name = "Set3")

print(set3_colors)

macro_debris_morphology_bar <- ggplot(morphology_summary, aes(x = correction, y=percent, fill=reorder(items, (-percent)))) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Morphology") +
  scale_fill_manual(
    values = c(
      "other" = "#8DD3C7",
      "wrappers" = "#D9D9D9",
      "remaining items" = "#FB8072",
      "fragments(trash)" = "#FFFFB3",
      "cups" = "#CCEBC5",
      "bags" = "#FDB462",
      "packaging" = "#BEBADA",
      "lids" = "#FCCDE5"
    ),
    labels = c(
      "other" = "other",
      "wrappers" = "wrappers",
      "remaining items" = "remaining\nitems",
      "fragments(trash)" = "fragment\n(trash)",
      "cups" = "cups",
      "bags" = "bags",
      "packaging" = "packaging",
      "lids" = "lids"
    )) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/macro_debris_morphology_bar.png", plot = macro_debris_morphology_bar, width = 4.4, height = 4, dpi = 600)

#Summary stats on trash material
material_mass <- trash_data %>% 
  group_by(material) %>% 
  summarise(weight_estimate_g = sum(weight_estimate_g))%>%
  arrange(desc(weight_estimate_g))
mass_sum <- sum(material_mass$weight_estimate_g)
material_mass <- material_mass %>%
  add_column(percent = ((material_mass$weight_estimate_g)/mass_sum)*100,
             correction = "Mass") %>%
  select(-weight_estimate_g)

material_count <- trash_data %>% 
  add_column(count = 1) %>%
  group_by(material) %>% 
  summarise(count = sum(count))%>%
  arrange(desc(count))
count_sum <- sum(material_count$count)
material_count <- material_count %>%
  add_column(percent = ((material_count$count)/count_sum)*100,
             correction = "Count") %>%
  select(-count)

material_summary <- rbind(material_mass, material_count)

#Make material stacked bar plot for macro debris count to mass conversion
material_summary <- material_summary %>% arrange(desc(correction))
material_summary <- material_summary %>% left_join(MaterialsAlias, by = c("material" = "Alias"))
material_summary <- material_summary %>% select(-c("material", "Material")) %>% rename(material = readable)
material_summary$percent <- as.numeric(material_summary$percent)

macro_debris_material_bar <- ggplot(material_summary, aes(x = correction, y=percent, fill=reorder(material, (-percent)))) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Material") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/macro_debris_material_bar.png", plot = macro_debris_material_bar, width = 4.4, height = 4, dpi = 600)


#Summary stats on microparticle morphology
microparticle_data <- read.csv("figures/data/Jarlskog_Microparticle_Data_Matched.csv") %>% 
  rename(material = Material,
         items = Morphology,
         count = Concentration..particles.volume. ,
         mass = Concentration..mg.volume.) %>%
  select(material, items, count, mass)

item_mass <- microparticle_data %>% 
  group_by(items) %>% 
  summarise(mass = sum(mass))%>%
  arrange(desc(mass))
mass_sum <- sum(item_mass$mass)
item_mass <- item_mass %>%
  add_column(percent = ((item_mass$mass)/mass_sum)*100,
             correction = "Mass") %>%
  select(-mass)

item_count <- microparticle_data %>%
  group_by(items) %>% 
  summarise(count = sum(count))%>%
  arrange(desc(count))
count_sum <- sum(item_count$count)
item_count <- item_count %>%
  add_column(percent = ((item_count$count)/count_sum)*100,
             correction = "Count") %>%
  select(-count)

morphology_summary <- rbind(item_mass, item_count)

#Make morphology stacked bar plot for micro debris count to mass conversion
morphology_summary <- morphology_summary %>% arrange(desc(correction))
morphology_summary <- morphology_summary %>% left_join(ItemsAlias, by = c("items" = "Alias"))
morphology_summary <- morphology_summary %>% select(-c("items", "Item")) %>% rename(items = readable)
morphology_summary$percent <- as.numeric(morphology_summary$percent)

palette <- colorRampPalette(RColorBrewer::brewer.pal(12,name = 'Set3'))(length(unique(morphology_summary$items)))
micro_debris_morphology_bar <- ggplot(morphology_summary, aes(x = correction, y=percent, fill=reorder(items, (-percent)))) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Morphology") +
  scale_fill_manual(values = palette) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/micro_debris_morphology_bar.png", plot = micro_debris_morphology_bar, width = 4.4, height = 4, dpi = 600)


#Summary stats on microparticle material
material_mass <- microparticle_data %>% 
  group_by(material) %>% 
  summarise(mass = sum(mass))%>%
  arrange(desc(mass))
mass_sum <- sum(material_mass$mass)
material_mass <- material_mass %>%
  add_column(percent = ((material_mass$mass)/mass_sum)*100,
             correction = "Mass") %>%
  select(-mass)

material_count <- microparticle_data %>% 
  group_by(material) %>% 
  summarise(count = sum(count))%>%
  arrange(desc(count))
count_sum <- sum(material_count$count)
material_count <- material_count %>%
  add_column(percent = ((material_count$count)/count_sum)*100,
             correction = "Count") %>%
  select(-count)

material_summary <- rbind(material_mass, material_count)

#Make material stacked bar plot for micro debris count to mass conversion
material_summary <- material_summary %>% arrange(desc(correction))
material_summary$percent <- as.numeric(material_summary$percent)

set3_colors <- brewer.pal(n = 7, name = "Set3")

print(set3_colors)

micro_debris_material_bar <- ggplot(material_summary, aes(x = correction, y=percent, fill=reorder(material, (-percent)))) + 
  geom_bar(position="stack", stat="identity") +
  ylab("Percentage") +
  xlab("") + 
  labs(fill = "Material") +
  scale_fill_manual(
    values = c(
      "tire wear & asphalt" = "#8DD3C7",
      "asphalt" = "#FFFFB3",
      "tire wear" = "#BEBADA",
      "metal" = "#FB8072",
      "organic" = "#80B1D3",
      "paint" = "#FDB462",
      "glass" = "#B3DE69"
    ),
    labels = c(
      "tire wear & asphalt" = "tire wear\n& asphalt",
      "asphalt" = "asphalt",
      "tire wear" = "tire wear",
      "metal" = "metal",
      "organic" = "organic",
      "paint" = "paint",
      "glass" = "glass"
    )) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

ggsave("figures/micro_debris_material_bar.png", plot = micro_debris_material_bar, width = 4.4, height = 4, dpi = 600)



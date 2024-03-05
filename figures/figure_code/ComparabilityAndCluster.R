#Libraries ----

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggpol)
library(ggExtra)
library(data.table)
library(factoextra)
library(tidyverse)
library(cluster)
library(tidyverse)
library(ggrepel)
library(gridExtra)
library(cluster) 
library(ggfortify)
library(corrplot)
library(FactoMineR)
library(Factoshiny)
library(ggpubr)

#Functions ----
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}

#Set working directory

#Datasets ----

MaterialsAlias <- read.csv("data/PrimeMaterials.csv")
ItemsAlias <- read.csv("data/PrimeItems.csv")
ItemsHierarchy <- read.csv("data/ITEMSHierarchyLower.csv")
MaterialsHierarchy <- read.csv("data/MaterialsHierarchyLower.csv")

#Analysis ----
#*Data cleanup ----
ItemsAliasClean <- ItemsAlias %>%
  mutate_all(cleantext) %>%
  distinct() %>%
  mutate(RowId = 1:nrow(.))

MaterialsAliasClean <- MaterialsAlias %>%
  mutate_all(cleantext) %>%
  distinct()%>%
  mutate(RowIdMaterials = 1:nrow(.))

ItemsHierarchyClean <- mutate_all(ItemsHierarchy, cleantext)
  
MaterialHierarchyClean <- mutate_all(MaterialsHierarchy, cleantext)

drinkingwater_morpology <- read.csv("data/microplastics_rescaling/drinkingwater_samples_morphology.csv")
river_morphology <- read.csv("data/microplastics_rescaling/river_samples_morphology.csv")
drinkingwater_material <- read.csv("data/microplastics_rescaling/drinkingwater_samples_material.csv")
river_material <- read.csv("data/microplastics_rescaling/river_samples_material.csv")

##Morphology
#make drinking water morphology data into long format, join with corrected concentration
long_dw_morph <- gather(drinkingwater_morpology, morphology, morph_ratio, Fragment:Rubbery_Fragment) %>%
  drop_na(morph_ratio) %>%
  select(DOI, morphology)
long_riv_morph <- gather(river_morphology, morphology, morph_ratio, fiber:fragment) %>%
  drop_na(morph_ratio) %>%
  select(DOI, morphology)
long_morph <- rbind(long_dw_morph, long_riv_morph)

Combo <- as.data.frame(t(combn(unique(long_morph$DOI), 2)), stringsAsFactors = F)

ComboComparability <- long_morph %>%
  filter(!is.na(morphology)) %>%
  group_by(morphology, DOI) %>%
  summarise(count = n()) %>%
  mutate(count = 1) %>%
  ungroup() 

#*Processing the comparability metric for items ----
#This takes a little while.
for(row in 1:nrow(Combo)) {
  totalcomboright <- filter(ComboComparability, DOI == Combo[row,2]) 
  totalcomboleft <- filter(ComboComparability, DOI == Combo[row,1]) 
  
  
  joinedsum <- filter(ComboComparability, DOI %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(morphology) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2) %>%
    mutate(count = 1)
  
  totalcombo <- filter(ComboComparability, DOI %in% c(Combo[row,1], Combo[row,2])) 
  joinedsumcombo <- filter(ComboComparability, DOI %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(morphology) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2)
  
  Combo[row, "ComparabilityItems"] <- sum(joinedsumcombo$sum)/sum(totalcombo$count)
  Combo[row, "ComparabilityItemsRight"] <- sum(joinedsum$count)/sum(totalcomboright$count)
  Combo[row, "ComparabilityItemsLeft"] <- sum(joinedsum$count)/sum(totalcomboleft$count)
  
}

#*Processing the comparability metric for materials----

long_dw_material <- gather(drinkingwater_material, material, material_ratio, PEST:PPE) %>%
  drop_na(material_ratio) %>%
  select(DOI, material)
long_riv_material <- gather(river_material, material, material_ratio, polymeric:urethane) %>%
  drop_na(material_ratio) %>%
  select(DOI, material)
long_material <- rbind(long_dw_material, long_riv_material)

ComboComparabilityMaterials <- long_material %>%
  filter(!is.na(material)) %>%
  group_by(material, DOI) %>%
  summarise(count = n()) %>%
  mutate(count = 1) %>%
  ungroup() 

#This is a little slow
for(row in 1:nrow(Combo)) {
  totalcombo <- filter(ComboComparabilityMaterials, DOI %in% c(Combo[row,1], Combo[row,2])) 
  
  joinedsumcombo <- filter(ComboComparabilityMaterials, DOI %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(material) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2)
  
  totalcomboright <- filter(ComboComparabilityMaterials, DOI == Combo[row,2]) 
  totalcomboleft <- filter(ComboComparabilityMaterials, DOI == Combo[row,1]) 
  
  
  joinedsum <- filter(ComboComparabilityMaterials, DOI %in% c(Combo[row,1], Combo[row,2])) %>%
    group_by(material) %>%
    summarise(sum = sum(count)) %>%
    filter(sum == 2) %>%
    mutate(count = 1)
  
  Combo[row, "ComparabilityMaterials"] <- sum(joinedsumcombo$sum)/sum(totalcombo$count)
  Combo[row, "ComparabilityMaterialsRight"] <- sum(joinedsum$count)/sum(totalcomboright$count)
  Combo[row, "ComparabilityMaterialsLeft"] <- sum(joinedsum$count)/sum(totalcomboleft$count)
}

AverageForOrganizations <- Combo %>%
  gather(Column, Organization, -ComparabilityItemsLeft, -ComparabilityItemsRight, -ComparabilityMaterials, -ComparabilityMaterialsRight, -ComparabilityMaterialsLeft, -ComparabilityItems) %>%
  mutate(ComparabilityItemsEncompassingOthers = ifelse(Column == "V1", ComparabilityItemsRight, ComparabilityItemsLeft)) %>%
  mutate(ComparabilityMaterialsEncompassingOthers = ifelse(Column == "V1", ComparabilityMaterialsRight, ComparabilityMaterialsLeft)) %>%
  select(Organization, ComparabilityItems, ComparabilityItemsEncompassingOthers, ComparabilityMaterials, ComparabilityMaterialsEncompassingOthers)

AverageForOrganizations[is.na(AverageForOrganizations)]<-0 #Change NA values to 0

#Mean of all the comparability statistics we ran for each organization----
AverageForOrganizationsTotal <- AverageForOrganizations %>%
  group_by(Organization) %>%
  summarize_all(mean)

AverageForOrganizationsTotal <- rbind(AverageForOrganizationsTotal, list('embedding_match_1', 0.7392, 0.7392, 0.8116, 0.8116))
AverageForOrganizationsTotal <- rbind(AverageForOrganizationsTotal, list('embedding_match_5', 0.8905, 0.8905, 0.9273, 0.9273))

AverageForOrganizationsTotal <- read.csv("data/comparability_cluster.csv")
#Comparability Metric Plot ----
plot <- ggplot(AverageForOrganizationsTotal, aes(x = ComparabilityItemsEncompassingOthers, y = ComparabilityMaterialsEncompassingOthers)) + 
  geom_point(color = "black", size = 3, alpha = 0.5)  +
  labs(x = "Morphology Comparability (mean decimal %)", y = "Material Comparability (mean decimal %)") + 
  scale_x_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1)) + scale_y_continuous(breaks = c(seq(0,1, by = 0.1)), limits = c(0,1))+
  scale_fill_viridis_c(breaks = c(1:10)) + 
  coord_equal() +
  theme_minimal() + 
  theme(axis.title = element_text(size = 14),
                        axis.text = element_text(size = 12))
print(plot)
#Stats of the comparability metric ----

#Mean comparability for items and material types ----
mean(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers, na.rm = T)
mean(AverageForOrganizations$ComparabilityItemsEncompassingOthers, na.rm = T)

#Number of comparisons which are 100 percent compatible ----
length(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers[AverageForOrganizations$ComparabilityMaterialsEncompassingOthers == 1])
length(AverageForOrganizations$ComparabilityItemsEncompassingOthers[AverageForOrganizations$ComparabilityItemsEncompassingOthers == 1])

#Number of comparisons ----
nrow(AverageForOrganizations)

#Number of comparisons which are 0 percent compatible ----
length(AverageForOrganizations$ComparabilityMaterialsEncompassingOthers[AverageForOrganizations$ComparabilityMaterialsEncompassingOthers == 0])
length(AverageForOrganizations$ComparabilityItemsEncompassingOthers[AverageForOrganizations$ComparabilityItemsEncompassingOthers == 0])

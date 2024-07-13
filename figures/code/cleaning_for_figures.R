
library(dplyr)
library(data.table)
library(DT)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggforce)
library(ggdark)
library(ggdist)
library(tibble)
library(plotly)
library(readr)
library(ggthemes)
library(rlang)
library(PupillometryR)
library(gridExtra)
library(grid)



#Data cleaning function
cleantext <- function(x) {
  x <- tolower(gsub("[[:space:]]", "", x))
  ifelse(x == "", NA, x)
}
  
###CATEGORICAL VARIABLE ANALYSIS
#read in data

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

##Morphology Data
morphology_ratios <- read.csv("figures/data/morphology_all.csv") %>% left_join(concentrations, by = "sample_ID")
morphology_ratios <- morphology_ratios %>% add_column(morph_concentration_reported = morphology_ratios$morph_ratio * morphology_ratios$concentration,
                                                      morph_concentration_corrected = morphology_ratios$morph_ratio * morphology_ratios$corrected_concentration)
  
##Material Data
material_ratios <- read.csv("figures/data/material_all.csv") %>% left_join(concentrations, by = "sample_ID")
material_ratios <- material_ratios %>% add_column(material_concentration_reported = material_ratios$material_ratio * material_ratios$concentration,
                                                  material_concentration_corrected = material_ratios$material_ratio * material_ratios$corrected_concentration)



#Calculate Morphology
morphology_concentrations <- morphology_ratios %>% select(morphology, study_media, morph_concentration_reported, morph_concentration_corrected)
morphology_drinking <- morphology_ratios %>% filter(study_media == "drinkingwater")
morphology_drinking_total_before <- sum(morphology_drinking$morph_concentration_reported)
morphology_drinking_total_after <- sum(morphology_drinking$morph_concentration_corrected)
morphology <- c("Fiber", "Film", "Foam", "Fragment", "Nurdle", "Sphere")
morphology_summary <- data.frame(morphology) %>%
  add_column(before_total = c(0,0,0,0,0,0),
             after_total = c(0,0,0,0,0,0),
             before_percent = c(0,0,0,0,0,0),
             after_percent = c(0,0,0,0,0,0))
for(x in 1:nrow(morphology_drinking)){
  if(morphology_drinking$morphology[[x]] == "Fiber"){morphology_summary[1,2] <- paste(as.numeric(morphology_summary[1,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[1,3] <- paste(as.numeric(morphology_summary[1,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
  if(morphology_drinking$morphology[[x]] == "Film"){morphology_summary[2,2] <- paste(as.numeric(morphology_summary[2,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[2,3] <- paste(as.numeric(morphology_summary[2,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
  if(morphology_drinking$morphology[[x]] == "Foam"){morphology_summary[3,2] <- paste(as.numeric(morphology_summary[3,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[3,3] <- paste(as.numeric(morphology_summary[3,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
  if(morphology_drinking$morphology[[x]] == "Fragment"){morphology_summary[4,2] <- paste(as.numeric(morphology_summary[4,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[4,3] <- paste(as.numeric(morphology_summary[4,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
  if(morphology_drinking$morphology[[x]] == "Nurdle"){morphology_summary[5,2] <- paste(as.numeric(morphology_summary[5,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[5,3] <- paste(as.numeric(morphology_summary[5,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
  if(morphology_drinking$morphology[[x]] == "Sphere"){morphology_summary[6,2] <- paste(as.numeric(morphology_summary[6,2]) + as.numeric(morphology_drinking$morph_concentration_reported[[x]]))
  morphology_summary[6,3] <- paste(as.numeric(morphology_summary[6,3]) + as.numeric(morphology_drinking$morph_concentration_corrected[[x]]))}
}

morphology_summary$before_percent <- (as.numeric(morphology_summary$before_total)/as.numeric(morphology_drinking_total_before))*100
morphology_summary$after_percent <- (as.numeric(morphology_summary$after_total)/as.numeric(morphology_drinking_total_after))*100
morphology_summary_drinkingwater <- morphology_summary



morphology_river <- morphology_concentrations %>% filter(study_media != "drinkingwater")

morphology_river_total_before <- sum(morphology_river$morph_concentration_reported)
morphology_river_total_after <- sum(morphology_river$morph_concentration_corrected)
morphology <- c("Fiber", "Film", "Foam", "Fragment", "Nurdle", "Sphere")
morphology_summary <- data.frame(morphology) %>%
  add_column(before_total = c(0,0,0,0,0,0),
             after_total = c(0,0,0,0,0,0),
             before_percent = c(0,0,0,0,0,0),
             after_percent = c(0,0,0,0,0,0))
for(x in 1:nrow(morphology_river)){
  if(morphology_river$morphology[[x]] == "Fiber"){morphology_summary[1,2] <- paste(as.numeric(morphology_summary[1,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[1,3] <- paste(as.numeric(morphology_summary[1,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
  if(morphology_river$morphology[[x]] == "Film"){morphology_summary[2,2] <- paste(as.numeric(morphology_summary[2,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[2,3] <- paste(as.numeric(morphology_summary[2,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
  if(morphology_river$morphology[[x]] == "Foam"){morphology_summary[3,2] <- paste(as.numeric(morphology_summary[3,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[3,3] <- paste(as.numeric(morphology_summary[3,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
  if(morphology_river$morphology[[x]] == "Fragment"){morphology_summary[4,2] <- paste(as.numeric(morphology_summary[4,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[4,3] <- paste(as.numeric(morphology_summary[4,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
  if(morphology_river$morphology[[x]] == "Nurdle"){morphology_summary[5,2] <- paste(as.numeric(morphology_summary[5,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[5,3] <- paste(as.numeric(morphology_summary[5,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
  if(morphology_river$morphology[[x]] == "Sphere"){morphology_summary[6,2] <- paste(as.numeric(morphology_summary[6,2]) + as.numeric(morphology_river$morph_concentration_reported[[x]]))
  morphology_summary[6,3] <- paste(as.numeric(morphology_summary[6,3]) + as.numeric(morphology_river$morph_concentration_corrected[[x]]))}
}

morphology_summary$before_percent <- (as.numeric(morphology_summary$before_total)/as.numeric(morphology_river_total_before))*100
morphology_summary$after_percent <- (as.numeric(morphology_summary$after_total)/as.numeric(morphology_river_total_after))*100
morphology_summary_river <- morphology_summary

morphology_summary_all <-rbind(morphology_summary_drinkingwater, morphology_summary_river)
source <- c("Drinking Water", "Drinking Water", "Drinking Water", "Drinking Water", "Drinking Water", "Drinking Water", "River", "River", "River", "River", "River", "River")
morphology_summary_all <- morphology_summary_all %>% add_column(Source = source) %>% select(-c(before_total, after_total)) %>% rename(Before = before_percent, After = after_percent)
morphology_summary_all_long <- gather(morphology_summary_all, correction, proportion, Before:After)
write.csv(morphology_summary_all_long, "figures/data/morphology_summary.csv")

#Calculate Material

material_clean <- material_ratios %>% select(material, study_media, material_concentration_reported, material_concentration_corrected)
material_drinking <- material_clean %>% filter(study_media == "drinkingwater")
material_river <- material_clean %>% filter(study_media != "drinkingwater")

material_drinking_total_before <- sum(as.numeric(material_drinking$material_concentration_reported))
material_drinking_total_after <- sum(as.numeric(material_drinking$material_concentration_corrected))
material_river_total_before <- sum(as.numeric(material_river$material_concentration_reported))
material_river_total_after <- sum(as.numeric(material_river$material_concentration_corrected))

material_merged_summary_before <- aggregate(material_drinking$material_concentration_reported, by=list(material = material_drinking$material), FUN=sum)
material_merged_summary_before$x <- (as.numeric(material_merged_summary_before$x)/as.numeric(material_drinking_total_before))*100
material_merged_summary_before <- material_merged_summary_before %>% rename(proportion = x) %>% add_column(Source = "Drinking Water", correction = "Before")

material_merged_summary_before_riv <- aggregate(material_river$material_concentration_reported, by=list(material = material_river$material), FUN=sum)
material_merged_summary_before_riv$x <- (as.numeric(material_merged_summary_before_riv$x)/as.numeric(material_river_total_before))*100
material_merged_summary_before_riv <- material_merged_summary_before_riv %>% rename(proportion = x) %>% add_column(Source = "River", correction = "Before")

material_merged_summary_after <- aggregate(material_drinking$material_concentration_corrected, by=list(material = material_drinking$material), FUN=sum)
material_merged_summary_after$x <- (as.numeric(material_merged_summary_after$x)/as.numeric(material_drinking_total_after))*100
material_merged_summary_after <- material_merged_summary_after %>% rename(proportion = x) %>% add_column(Source = "Drinking Water", correction = "After")

material_merged_summary_after_riv <- aggregate(material_river$material_concentration_corrected, by=list(material = material_river$material), FUN=sum)
material_merged_summary_after_riv$x <- (as.numeric(material_merged_summary_after_riv$x)/as.numeric(material_river_total_after))*100
material_merged_summary_after_riv <- material_merged_summary_after_riv %>% rename(proportion = x) %>% add_column(Source = "River", correction = "After")

material_summary_all <- rbind(material_merged_summary_before, material_merged_summary_before_riv, material_merged_summary_after, material_merged_summary_after_riv)
write.csv(material_summary_all, "figures/data/material_summary.csv")

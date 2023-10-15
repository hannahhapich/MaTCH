library(data.table)
library(poweRlaw)

#Polymers
curb_materials_2_20 <- c("glass", "metallic", "organic", "paint", "tire wear & asphalt", "mineral")
track_materials_2_20 <- c("glass", "metallic", "organic", "paint", "tire wear & asphalt", "mineral")
curb_materials_20_125 <- c("glass", "metallic", "organic", "paint", "asphalt", "tire wear", "mineral")
track_materials_20_125 <- c("glass", "metallic", "organic", "paint", "asphalt", "tire wear", "mineral")

curb_prob_2_20 <- as.numeric(c(0.000228, 0.0117, 0.0125, 0.00942, 0.188, 0.778))
track_prob_2_20 <- c(0.00E+00, 1.17E-02, 8.10E-03, 5.17E-03, 1.29E-01, 8.46E-01)
curb_prob_20_125 <- c(0.00E+00, 1.75E-03, 2.17E-02, 2.65E-03, 6.93E-02, 1.08E-01, 7.96E-01)
track_prob_20_125 <- c(0.00E+00, 3.72E-03, 2.68E-02, 1.75E-03, 4.00E-02, 3.33E-02, 8.94E-01)

curb_materials_simulated_2_20 <- sample(x = curb_materials_2_20, size = 100000, replace = T, prob = curb_prob_2_20)
track_materials_simulated_2_20 <- sample(x = track_materials_2_20, size = 100000, replace = T, prob = track_prob_2_20)
curb_materials_simulated_20_125 <- sample(x = curb_materials_20_125, size = 100000, replace = T, prob = curb_prob_20_125)
track_materials_simulated_20_125 <- sample(x = track_materials_20_125, size = 100000, replace = T, prob = track_prob_20_125)


#Sizes
# Particle size distribution, set seed for reproducibility
set.seed(123)
particle_sizes <- rplcon(1000000, xmin = 1, alpha = 1.6)
particle_sizes_simulated <- data.frame(sizes = particle_sizes)
particle_sizes_2_20 <- filter(particle_sizes_simulated, sizes <= 20 & sizes >= 2)
particle_sizes_20_125 <- filter(particle_sizes_simulated, sizes <= 125 & sizes > 20)

#Sample from each particle size group for each sample
set.seed(456)
curb_sizes_2_20 <- particle_sizes_2_20[sample(nrow(particle_sizes_2_20), 100000), ]
set.seed(789)
track_sizes_2_20 <- particle_sizes_2_20[sample(nrow(particle_sizes_2_20), 100000), ]
set.seed(101)
curb_sizes_20_125 <- particle_sizes_20_125[sample(nrow(particle_sizes_20_125), 100000), ]
set.seed(112)
track_sizes_20_125 <- particle_sizes_20_125[sample(nrow(particle_sizes_20_125), 100000), ]

#Morphology assumptions
materials <- c("glass", "metallic", "organic", "paint", "asphalt", "tire wear", "mineral", "tire wear & asphalt")
morphology <- c("fragment", "fragment", "fragment", "film", "fragment", "fragment", "fragment", "fragment")
morph_conversion <- data.frame (materials = materials,
                                morphology = morphology)

#Datasets
simulated_curb_2_20 <- data.table(materials = curb_materials_simulated_2_20, sizes = curb_sizes_2_20) %>%
  left_join(morph_conversion, by = "materials")
simulated_track_2_20 <- data.table(materials = track_materials_simulated_2_20, sizes = track_sizes_2_20) %>%
  left_join(morph_conversion, by = "materials")
simulated_curb_20_125 <- data.table(materials = curb_materials_simulated_20_125, sizes = curb_sizes_20_125) %>%
  left_join(morph_conversion, by = "materials")
simulated_track_20_125 <- data.table(materials = track_materials_simulated_20_125, sizes = track_sizes_20_125) %>%
  left_join(morph_conversion, by = "materials")

#Save datasets
write.csv(simulated_curb_2_20, "data/simulated_data/simulated_curb_2_20.csv")
write.csv(simulated_track_2_20, "data/simulated_data/simulated_track_2_20.csv")
write.csv(simulated_curb_20_125, "data/simulated_data/simulated_curb_20_125.csv")
write.csv(simulated_track_20_125, "data/simulated_data/simulated_track_20_125.csv")









CFfnx = function(a, #default alpha from Koelmans et al (2020)
                 x2D, #set detault values to convert ranges to (1-5,000 um) #5mm is upper defuault 
                 x1D, #1 um is lower default size
                 x2M, x1M){
  CF = (x2D^(1-a)-x1D^(1-a))/(x2M^(1-a)-x1M^(1-a)) 
  return(CF)}

CF <- CFfnx(x1M = 2,#lower measured length
            x2M = 20, #upper measured length
            x1D = 1, #default lower size range
            x2D = 20,  #default upper size range
            a = 1.6 #alpha for count 
            
)
print(CF)
#3.166285

CF <- CFfnx(x1M = 20,#lower measured length
            x2M = 125, #upper measured length
            x1D = 20, #default lower size range
            x2D = 5000,  #default upper size range
            a = 1.6 #alpha for count 
            
)
print(CF)
#1.051974








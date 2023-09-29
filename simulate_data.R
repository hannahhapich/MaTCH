library(data.table)

#Colors
colors <- c("red", "blue", "green")
color_probs <- c(0.3, 0.5, 0.2)

colors_simulated <- sample(colors, size = 10000, replace = T, prob = color_probs)

#Polymers
polymer <- c("PE", "PVC", "PS")
polymer_probs <- c(0.2, 0.7, 0.1)

polymers_simulated <- sample(polymer, size = 10000, replace = T, prob = polymer_probs)

#Sizes
particle_sizes_simulated <- rpois(10000, lambda = 0.5)

#Dataset
simulated_dataset <- data.table(colors = colors_simulated, polymers = polymers_simulated, sizes = particle_sizes_simulated)

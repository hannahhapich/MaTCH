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


#Rescale code from Nizamali
n = 10000
df <-data.frame(n = c(1:n),
                Alpha = numeric(n),
                Size = numeric(n),
                W_L = numeric(n),
                H_L = numeric(n),
                Dens = numeric(n))

#### ======SIZE======= ####

# parameters from Kooi et al 2021
m.alpha.w = 2.64 
sd.alpha.w = 0.01
min.alpha.w = 1.01
max.alpha.w = 2.56
size.min = 1    #um
size.max = 5000 #um

# Alpha (-)
df$Alpha <- rtnorm(n = n, mean = m.alpha.w, sd = sd.alpha.w, lower = min.alpha.w, upper = max.alpha.w)

# Size (um)
for(i in 1:n){
  success <- FALSE
  while(!success){
    U = runif(1, 0, 1) 
    size = size.min*(1-U)^(1/(1-df$Alpha[i]))
    success <- size <= 5000}
  df[which(df$n == i),]$Size= size
}

p <- ggplot(df, aes(x=Size)) + 
  geom_density() + 
  scale_x_continuous(trans='log10')

p






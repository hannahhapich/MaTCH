library(data.table)

#Colors
colors <- c("red", "blue", "green")
color_probs <- c(0.3, 0.5, 0.2)

colors_simulated <- sample(colors, size = 10000, replace = T, prob = color_probs)

#Polymers
curb_materials_2_20 <- c("glass", "metallic", "organic", "paint", "tire wear & asphalt", "mineral")
track_materials_2_20 <- c("glass", "metallic", "organic", "paint", "tire wear & asphalt", "mineral")
curb_materials_20_125 <- c("glass", "metallic", "organic", "paint", "asphalt", "tire wear", "mineral")
track_materials_20_125 <- c("glass", "metallic", "organic", "paint", "asphalt", "tire wear", "mineral")

curb_prob_2_20 <- c(0.000228, 0.0117, 0.0125, 0.00942, 0.188, 0.778)
track_prob_2_20 <- c(0.00E+00, 1.17E-02, 8.10E-03, 5.17E-03, 1.29E-01, 8.46E-01)
curb_prob_20_125 <- c(0.00E+00, 1.75E-03, 2.17E-02, 2.65E-03, 6.93E-02, 1.08E-01, 7.96E-01)
track_prob_20_125 <- c(0.00E+00, 3.72E-03, 2.68E-02, 1.75E-03, 4.00E-02, 3.33E-02, 8.94E-01)

curb_materials_simulated_2_20 <- sample.int(curb_materials_2_20, size = 1030000000000, replace = T, prob = curb_prob_2_20)

#Sizes
particle_sizes_simulated <- rpois(10000, lambda = 0.5)

#Dataset
simulated_dataset <- data.table(colors = colors_simulated, polymers = polymers_simulated, sizes = particle_sizes_simulated)


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

#Rescale code from Nizamali
n = 100000
df <-data.frame(n = c(1:n),
                Alpha = numeric(n),
                Size = numeric(n))

#### ======SIZE======= ####

# parameters from Kooi et al 2021
m.alpha.w = 1.6
sd.alpha.w = 0.05
min.alpha.w = 1.5
max.alpha.w = 1.7
size.min = 1    #um
size.max = 20 #um

# Alpha (-)
df$Alpha <- rtnorm(n = n, mean = m.alpha.w, sd = sd.alpha.w, lower = min.alpha.w, upper = max.alpha.w)

# Size (um)
for(i in 1:n){
  success <- FALSE
  while(!success){
    U = runif(1, 0, 1) 
    size = size.min*(1-U)^(1/(1-df$Alpha[i]))
    success <- size <= size.max}
  df[which(df$n == i),]$Size= size
}

p <- ggplot(df, aes(x=Size)) + 
  geom_density() #+ 
  #scale_x_continuous(trans='log10')

p







#L = length (max feret)
#W = width (min feret)
#H = height (measured third axis)
#Dx = ellipsoid major axis (separate metric from image J analogous to length)
#Dy = ellipsoid minor axis (separate metric from image J analogous to width)
#C = circularity (between 0 and ~1.1; should be 1 but image J pixelation can yield values slightly higher)
#A = area
#P = perimeter

#Chen (2024) Equations (https://doi.org/10.1021/acs.est.4c01031) ----
##For pellet (req: Dx OR L, Dy OR W, C; 0.5 </= C < 0.75) (citation: Chen 2024, https://doi.org/10.1021/acs.est.4c01031)
chen_pellet_a <- function(L, W, C){
  V = (0.115)*(pi)*(L)*(W)^2
  #Symmetric error range based on the magnitude of Chen's mean error (-7.1%).
  V_min = V - ((V)*(0.071))
  V_max = V + ((V)*(0.071))
  return(c(V, V_min, V_max))
}

##For pellet (req: Dw OR L, Dy OR W, C; 0.75 </= C </= 1.10; max should be 1.0 but image J pixelation can yield values slightly higher) (citation: Chen 2024, https://doi.org/10.1021/acs.est.4c01031)
chen_pellet_b <- function(W, C){
  V = (pi/6)*(W)^3
  # Chen reports mean errors of +2.0% and -3.1% for model 13 in the applicable circularity strata. Preserve the directional endpoints.
  V_min = V - ((V)*(0.031))
  V_max = V + ((V)*(0.020))
  return(c(V, V_min, V_max))
}

##For fiber (req: Dx, Dy) (citation: Chen 2024, https://doi.org/10.1021/acs.est.4c01031)
chen_fiber <- function(Dx, Dy){
  V = (0.012)*(pi)*(Dx)*(Dy)*sqrt(Dx * Dy)
  # Chen reports mean +/- SEM = -0.2 +/- 9.5%. Preserves the resulting directional endpoints (-9.7%, +9.3%) as a performance envelope.
  V_min = V - ((V)*(0.097))
  V_max = V + ((V)*(0.093))
  return(c(V, V_min, V_max))
}


##For fragment (req: L, W, A) (citation: Chen 2024, https://doi.org/10.1021/acs.est.4c01031)
chen_fragment <- function(L, W, A){
  V = (0.144)*(A)*sqrt(L * W)
  # Chen reports mean +/- SEM = 0.9 +/- 5.0%. Preserves the resulting directional endpoints (-4.1%, +5.9%) as a performance envelope.
  V_min = V - ((V)*(0.041))
  V_max = V + ((V)*(0.059))
  return(c(V, V_min, V_max))
}



#Barchiesi (2023) Equations (https://doi.org/10.1021/acs.est.3c03620) ----
#For all/unspecified morphology (req: Dx OR L,Dy OR W, P) (citation: Barchiesi 2023, https://doi.org/10.1021/acs.est.3c03620)
barchiesi <- function(L, W, P){
  V = ((((pi)*((3*((L/2) + (W/2))) - sqrt((((3)*(L/2) + (W/2)) * ((L/2) + ((3)*(W/2)))))))/P)^3) * (4/3) * (pi) * (L/2) * (W/2) * ((W/L) * (W/2))
  #Range of Vmodeled/Vmeasured from Barchiesi 2023
  V_min = V * 0.99
  V_max = V * 1.06
  return(c(V, V_min, V_max))
}



#Tanoiri (2021) Equations (https://doi.org/10.1016/j.marpolbul.2021.112749) ----
#For fragment (req: Dx OR L, Dy OR W) (citation: Tanoiri 2021, https://doi.org/10.1016/j.marpolbul.2021.112749)
tanoiri_fragment <- function(L, W){
  V = (4/3)*(pi)*(L/2)*(W/2) * (0.372 * (W/2))
  #Range of Vmodeled/Vmeasured from Barchiesi 2023
  V_min = V * (0.98)
  V_max = V * (1.02)
  return(c(V, V_min, V_max))
}

#For pellets (req: Dx OR L, Dy OR W) (citation: PE/PP-pellet model from Tanoiri 2021, https://doi.org/10.1016/j.marpolbul.2021.112749)
tanoiri_pellet <- function(L, W){
  #Uses Tanoiri's better-supported PE/PP-pellet coefficient as its default Tanoiri pellet pathway. The third axis is 0.565 times the major axis; this is not an automatic material-specific classification.
  V = (4/3)*(pi)*(L/2)*(W/2) * (0.565 * (L/2))
  #Range of Vmodeled/Vmeasured from Barchiesi 2023
  V_min = V * (0.98)
  V_max = V * (1.02)
  return(c(V, V_min, V_max))
}



#Simon (2018) Equations (https://doi.org/10.1016/j.watres.2018.05.019) ----
#For all/unspecified morphology (req: Dx OR L, Dy OR W) (citation: Simon 2018, https://doi.org/10.1016/j.watres.2018.05.019, with bias correction modification from Barchiesi 2023, https://doi.org/10.1021/acs.est.3c03620)
simon <- function(L, W){
  V = (4/3)*(pi)*(L/2)*(W/2)*((W/L)*(W/2))
  #As every test showed Simon (2018) to overestimate, mean and max values will both be set to Simon (2018)
  #Min is set to the bias corrected estimate (Simon (2018) output divided by the maximum measured overestimate found in Barchiesi test, 1.57)
  V_min = V/1.57
  V_max = V
  return(c(V, V_min, V_max))
}

#For fragments (req: L OR Dx) (citation: ellipsoid equation from Simon 2018 https://doi.org/10.1016/j.watres.2018.05.019, with bias correction modification from Barchiesi 2023, https://doi.org/10.1021/acs.est.3c03620, and W projections from Kooi and Koelmans 2019 (https://doi.org/10.1021/acs.estlett.9b00379))
fragment_geo <- function(L){
  W = L * 0.45
  W_min = L * 0.1
  W_max = L
  V = (4/3)*(pi)*(L/2)*(W/2)*((W/L)*(W/2))
  V_min = ((4/3)*(pi)*(L/2)*(W_min/2)*((W_min/L)*(W_min/2)))/1.57
  V_max = (4/3)*(pi)*(L/2)*(W_max/2)*((W_max/L)*(W_max/2))
  return(c(V, V_min, V_max))
}

#For pellets (req: L OR Dx) (citation: ellipsoid equation from Simon 2018 https://doi.org/10.1016/j.watres.2018.05.019, with bias correction modification from Barchiesi 2023, https://doi.org/10.1021/acs.est.3c03620, and W = L because pellets ~circular from 2D view)
pellets_geo <- function(L){
  V = (4/3)*(pi)*(L/2)*(L/2)*((L/L)*(L/2))
  V_min = V/1.57
  V_max = V
  return(c(V, V_min, V_max))
}


#Medina (2021) Equations (https://dx.doi.org/10.3791/68148) ----
#For all/unspecified morphology (req: A) (citation: Medina 2021, https://dx.doi.org/10.3791/68148, with bias correction modification from Barchiesi 2023, https://doi.org/10.1021/acs.est.3c03620)
medina <- function(A){
  V = (4/3)*(pi)*(sqrt(A/pi))^3
  #As every test showed Medina (2021) to overestimate, mean and max values will both be set to Medina (2021)
  #Min is set to the bias corrected estimate (Medina (2021) output divided by the maximum measured overestimate found in Barchiesi test, 3.82)
  V_min = V/3.82
  V_max = V
  return(c(V, V_min, V_max))
}




#Basic geometry equations ----
#For films (req: Dx OR L, Dy OR W, H) (citation: basic geometry for a sheet)
film_geo <- function(L, W, H){
  V = L * W * H
  #All dimensions measured; do not transfer old +/- 5% length-measurement.
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}

#For films (req: Dx OR L, Dy OR W, with H values from literature estimates) (citation: basic geometry for a sheet)
film_geo_H <- function(L, W, H, H_min, H_max){
  V = L * W * H
  V_min = L * W * H_min
  V_max = L * W * H_max
  return(c(V, V_min, V_max))
}

#For films (req: L) (citation: basic geometry for a sheet, thickness from materials literature, and W projections from Kooi and Koelmans 2019 (https://doi.org/10.1021/acs.estlett.9b00379))
film_geo_WH <- function(L, H, H_min, H_max){
  V = L * (L*0.45) * H
  V_min = L * (L*0.1) * H_min
  V_max = L * L * H_max
  return(c(V, V_min, V_max))
}

#For fibers (req: Dx OR L, Dy OR W, H, and input$fiber_auto_width = OFF) (citation: basic geometry for a cylinder)
fiber_geo <- function(L, W, H){
  r = (W + H)/4
  V = (pi) * (r)^2 * (L)
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}

#For fibers (req: L, W, and input$fiber_auto_width = OFF) (citation: basic geometry for a cylinder)
fiber_geo_H <- function(L, W){
  r = (W)/2
  V = (pi) * (r)^2 * (L)
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}

#For fibers (req: L, can have W or not, but input$fiber_auto_width = ON) (citation: basic geometry for a cylinder)
fiber_geo_WH <- function(L, W, W_min, W_max){
  V = (pi) * ((W/2))^2 * (L)
  V_min = (pi) * ((W_min/2))^2 * (L)
  V_max = (pi) * ((W_max/2))^2 * (L)
  return(c(V, V_min, V_max))
}

#For spheres (req: L OR Dx, Y OR Dy, H) (citation: basic geometry for a sphere)
sphere_geo <- function(L, W, H){
  r = (L + W + H)/6
  V = (4/3) * (pi) * (r)^3
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}

#For spheres (req: L OR Dx, Y OR Dy) (citation: basic geometry for a sphere)
sphere_geo_H <- function(L, W){
  r = (L + W)/4
  V = (4/3) * (pi) * (r)^3
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}

#For spheres (req: L OR Dx) (citation: basic geometry for a sphere)
sphere_geo_WH <- function(L){
  r = (L)/2
  V = (4/3) * (pi) * (r)^3
  V_min = V
  V_max = V
  return(c(V, V_min, V_max))
}










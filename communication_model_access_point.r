# Communication model

# Code Parameters

# R_ki is the maximum upload data rate between a device k and access point i

# Simulation Parameters
# source("parameters.r")
# dev_power <- p_i

# # Known/Calculated Parameters

# sigma <- 8 # Standard Deviation
# mean <- 0 # Mean
# d_km <- seq(0.1, 10, by = 0.1) # Sequence of distance in Km

# Path Loss Model
path_loss <- function(d_km, N) {
  result <- 140.7 + 36.7 * log10(d_km) + N
  return(result)
}

# Maximum Upload Data Rate using Shannon Equation
upload_data_rate_device_ap <- function(W, dev_power, noise_power,
                              d_km, mean, sigma) {
  # Normal Distribution
  N <- dlnorm(d_km, mean, sigma, TRUE)
  PL <- path_loss(d_km, N)
  result <- W * log2(1 + (dev_power - PL) / noise_power)
  return(result)
}

# N <- dlnorm(d_km, mean, sigma, TRUE)
# PL <- path_loss(d_km, N)
# R_ki <- upload_data_rate_device_ap(W, dev_power, noise_power, d_km, mean, sigma)

# Plot Data
#plot(d_km, PL, main = "Path Loss Model")
#print(R_ki)
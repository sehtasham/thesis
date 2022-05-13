# Simulation Parameters

lambda <- seq(0.1, 0.7, by = 0.1) # task generation rate
p_o <- seq(0.1, 0.7, by = 0.1) # percentage of the tasks that will be offloaded to the edge server
D <- 4 * (10 ^ 6) # task size in bits
v <- 500 # number of CPU cycles required to process 1 bit data of task
f_l <- 500 * (10 ^ 6) # CPU cycles per seconds of the IoT device
f_e <- 10 * (10 ^ 9) # CPU cycles per seconds of the edge server
C_e <- (10 ^ -25) # IoT Computation capability
p_i <- (100 ^ -3)  # IoT device transmit power (mW)
W <- (10 ^ 5) # Wireless Channel Bandwidth (MHz)
noise_power <- (-174 + 10 * log10(W))
sigma_2 <- noise_power ^ 2
R <- 100 # to be calculated using different channel models
G <- seq(0.5, 1.5, by = 0.5) * (10 ^ -5) # Channel Gain different states
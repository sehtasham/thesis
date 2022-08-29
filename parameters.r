# Simulation Parameters

lambda <- seq(0.1, 0.7, by = 0.1) # task generation rate
p_o <- seq(0.1, 0.7, by = 0.1) # percentage of the tasks that will be offloaded to the edge server
D <- 4 * (10 ^ 6) # task size in bits
v <- 500 # number of CPU cycles required to process 1 bit data of task
f_l <- 500 * (10 ^ 6) # CPU cycles per seconds of the IoT device
f_e <- 4 * (10 ^ 9) # CPU cycles per seconds of the edge server
C_e <- (10 ^ -25) # IoT Computation capability
p_i <- 100 * (10 ^ -3)  # IoT device transmit power (W)
p_i_mW <- 100 #mW
p_i_dbm <-  20 #dbm
W <- (10 ^ 5) # Wireless Channel Bandwidth (MHz)
noise_power <- (-174 + 10 * log10(W)) #dbm
sigma_2 <- noise_power ^ 2
R <- 100 # to be calculated using different channel models
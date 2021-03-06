#                   Communication model
# Time division multiple access(TDMA) is used to manage IoT devices
# access to the shared wireless channel when transmitting data/tasks
# to the edge server.

# Code Parameters

# P_t is the transmission power
# W represents bandwidth
# h is th channel gain from the transmitting IoT device to the edge server
# sigma_2 is the variance of additive white Gaussian noise (AWGN)

# source("parameters.r")
# P_t <- p_i
# h <- G

# Function to calculate Upload Data Rate between device and SBS
upload_data_rate_device_tdma <- function(W, noise_power, h, P_t) {
    tmp <- (P_t * h) / noise_power
    result <- W * log2(1 + tmp)
    return(result)
}

# R_tdma <- upload_data_rate_device_tdma(W, noise_power, h, P_t)

# Energy Consumption Model model


# Code Parameters

# Device denoted by i
# C_e is the effective switched capacitance depending on the chip architecture
# p_i is the transmission power of the IoT node i
# D is the task size in bits
# R is the uplink data rate in bps
# t_o is the delay to offload the task
# f_l is the CPU cycles per seconds of the IoT device
# E_l is the energy cost incurred when a task is processed locally (IoT device)
# E_o is the energy cost incurred at an IoT device when it offloads a task
# v is the number of CPU cycles required to process 1 bit data of task

# Simulation Parameters
C_e <- 10 ^ -25 # IoT Computation capability
p_i <- 100 ^ -3  # IoT device transmit power (mW)
D <- 4 * 10 ^ 6
R <- 100 # to be calculated using different channel models
f_l <- 500 * 10 ^ 6 # CPU cycles per seconds of the IoT device
v <- 500 # number of CPU cycles required to process 1 bit data of task
lambda <- seq(0.1, 0.7, by = 0.1) # task generation rate
p_o <- seq(0.1, 0.7, by = 0.1) # percentage of the tasks that will be offloaded to the edge server

# Function to calculate the time to offload a task
time_to_offload_task <- function(D, R) {
    result <- D / R
    return(result)
}

# Function to calculate the energy cost incurred locally
energy_cost_local <- function(C_e, L, f_l) {
    result <- (1 - p_o) * lambda * C_e * v * D * (f_l ^ 2)
    return(result)
}

# Function to calculate the energy cost incurred when offloading task
energy_cost_offload <- function(p_i, D, R) {
    t_o <- time_to_offload_task(D, R)
    result <- p_o * lambda * p_i * t_o
    return(result)
}

# Computation

t_o <- time_to_offload_task(D, R)
E_l <- energy_cost_local(C_e, D, f_l)
E_o <- energy_cost_offload(p_i, D, R)


print(paste("Time to offload a task = ", t_o))
print(paste("Energy cost incurred when a task is processed locally = ", E_l))
print(paste("Energy cost incurred when offloading a task = ", E_o))

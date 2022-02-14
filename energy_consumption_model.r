# Energy Consumption Model model


# Code Parameters

# Device denoted by i
# C_e is the effective switched capacitance depending on the chip architecture
# p_i is the transmission power of the IoT node i
# D is the task size in bits
# t_o is the transmission delay for a task that is offloaded to an edge server
# f_l is the CPU cycles per seconds of the IoT device
# E_l is the energy cost incurred when a task is processed locally (IoT device)
# E_o is the energy cost incurred at an IoT device when it offloads a task
# v is the number of CPU cycles required to process 1 bit data of task
# lambda is the task generation rate
# p_o is the percentage of the tasks that will be offloaded to the edge server
source("parameters.r")

time_to_offload_task <- function(D, R) {
    result <- D / R
    return(result)
}

# Function to calculate the energy cost incurred locally
energy_cost_local <- function(C_e, L, f_l, v, D, lambda, p_o) {
    result <- (1 - p_o) * lambda * C_e * v * D * (f_l ^ 2)
    return(result)
}

# Function to calculate the energy cost incurred when offloading task
energy_cost_offload <- function(p_i, t_o, lambda, p_o) {
    result <- p_o * lambda * p_i * t_o
    return(result)
}

#outer_energy_cost_local <- function(lambda, p_o) {
#energy_cost_local(C_e, L, f_l, v, D, lambda, p_o)
#}

t_o <- time_to_offload_task(D, R)
E_l <- energy_cost_local(C_e, L, f_l, v, D, lambda, p_o)
E_o <- energy_cost_offload(p_i, t_o, lambda, p_o)

print(paste("Time to offload a task = ", t_o))
print(paste("Energy cost incurred when a task is processed locally = ", E_l))
print(paste("Energy cost incurred when offloading a task = ", E_o))

#z <- outer(lambda, p_o, outer_energy_cost_local)
#persp(lambda, p_o, z,
#zlab = "E_l",
#theta = 30, phi = 15,
#col = "springgreen", shade = 0.5)


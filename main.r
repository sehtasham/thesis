source("parameters.r")
source("task_computational_model.r")
source("energy_consumption_model.r")
source("communication_model_tdma.r")
source("communication_model_uav.r")
source("communication_model_access_point.r")
source("communication_model_node.r")
source("graphs.r")
source("methods.r")

# Function to calculate the time to offload a task
time_to_offload_task <- function(D, R) {
    result <- D / R
    return(result)
}

# For Communication Model TDMA
P_t <- p_i_mW
G <- c(0.5, 1, 1.5) * (10 ^ -5) # Channel Gain different states
h <- G[1]
h <- 10 * log10(h) #Conversion to db
R_tdma <- upload_data_rate_device_tdma(W, noise_power, h, P_t)
to_tdma <- time_to_offload_task(D, R_tdma)

# For Communication Model with AP
sigma <- 8 # Standard Deviation
mean <- 0 # Mean
d_km <- seq(0.1, 10, by = 0.1) # Sequence of distance in Km
dev_power <- p_i_dbm
R_ap <- upload_data_rate_device_ap(W, dev_power,
                                noise_power, d_km[100], mean, sigma)
to_ap <- time_to_offload_task(D, R_ap)

# For communication model with UAV
W_i <- W
alpha_0 <- -50 #dB
L_i <- c(0, 0, 0) #m
L_uav <- c(500, 500, 100) #m
R_uav <- upload_data_rate_device_uav(W_i, p_i, dbm_to_watt(noise_power),
                                dbm_to_watt(alpha_0), L_uav, L_i)
to_uav <- time_to_offload_task(D, R_uav)


# For Communication Model eNodeB
P_i <- p_i
L <- 100
h_i <- 127 + (30 * log10(L))
theta_i <- 0.1
R_node <- upload_data_rate_device_node(W, dbm_to_watt(noise_power), h_i, P_i, theta_i)
to_node <- time_to_offload_task(D, R_node)


t_o <- to_node
print(R_ap)
print(t_o)


#print(expected_delay(D, v, p_o, f_l, f_e, t_o, 0.5))
#print(energy_cost_local(C_e, L, f_l, v, D, 0.5, p_o))
#print(energy_cost_offload(p_i, t_o, 0.5, p_o))



outer_expected_delay <- function(lambda, p_o) {
expected_delay(D, v, p_o, f_l, f_e, t_o, lambda)
}
outer_energy_cost_local <- function(lambda, p_o) {
energy_cost_local(C_e, L, f_l, v, D, lambda, p_o)
}
outer_energy_cost_offload <- function(lambda, p_o) {
energy_cost_offload(p_i, t_o, lambda, p_o)
}
#par(mfrow=c(1, 1))


create_3d_graph(lambda, p_o, outer_energy_cost_local, "Local Energy Cost (J)")
dev.copy(png,'plots/local.png')
dev.off()
create_3d_graph(lambda, p_o, outer_energy_cost_offload, "Offload Energy Cost (J)")
dev.copy(png,'plots/offload.png')
dev.off()
create_3d_graph(lambda, p_o, outer_expected_delay, "Expected Delay (s)")
dev.copy(png,'plots/delay.png')
dev.off()
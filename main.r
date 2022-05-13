source("parameters.r")
source("task_computational_model.r")
source("energy_consumption_model.r")
source("communication_model_tdma.r")
source("communication_model_uav.r")
source("communication_model_access_point.r")
source("graphs.r")
# Function to calculate the time to offload a task
time_to_offload_task <- function(D, R) {
    result <- D / R
    return(result)
}

# For Communication Model TDMA
P_t <- p_i
h <- G[1]
R_tdma <- upload_data_rate_device_tdma(W, noise_power, h, P_t)
to_tdma <- time_to_offload_task(D, R_tdma)

# For Communication Model with AP
sigma <- 8 # Standard Deviation
mean <- 0 # Mean
d_km <- seq(0.1, 10, by = 0.1) # Sequence of distance in Km
R_ki <- upload_data_rate_device_ap(W, dev_power,
                                noise_power, d_km[1], mean, sigma)
to_ap <- time_to_offload_task(D, R_ki)

# For communication model with UAV
W_i <- W
alpha_0 <- -50 #dB
L_i <- c(0, 0, 100)
L_UAV <- c(500, 500, 100)

Ri_UAV <- upload_data_rate_device_uav(W_i, p_i, noise_power,
                                alpha_0, L_UAV, L_i)
to_uav <- time_to_offload_task(D, Ri_UAV)


t_o <- to_uav

outer_expected_delay <- function(lambda, p_o) {
expected_delay(D, v, p_o, f_l, f_e, t_o, lambda)
}
outer_energy_cost_local <- function(lambda, p_o) {
energy_cost_local(C_e, L, f_l, v, D, lambda, p_o)
}
outer_energy_cost_offload <- function(lambda, p_o) {
energy_cost_offload(p_i, t_o, lambda, p_o)
}
par(mfrow=c(1, 3))

create_3d_graph(lambda, p_o, outer_expected_delay, "Expected Delay")
create_3d_graph(lambda, p_o, outer_energy_cost_local, "Energy Cost Local")
create_3d_graph(lambda, p_o, outer_energy_cost_offload, "Energy Cost Offload")